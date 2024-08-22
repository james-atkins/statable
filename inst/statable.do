global STATABLE_RUN : env STATABLE_RUN
global STATABLE_INPUT : env STATABLE_INPUT

while fileexists("$STATABLE_RUN") {
    while (fileexists("$STATABLE_RUN") & !fileexists("$STATABLE_INPUT")) {
        sleep 100
    }

    if fileexists("$STATABLE_INPUT") {
        do "$STATABLE_INPUT"
    }
}
