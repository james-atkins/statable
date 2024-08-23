
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statable - Run Stata commands from R

<!-- badges: start -->
<!-- badges: end -->

statable is a Stata interface for R that enables executing Stata
commands directly from R. It provides the following functionalities:

- Execute Stata commands from R
- Transfer data frames between R and Stata seamlessly
- Integrate Stata “chunks” into knitr or R Markdown documents
- Run multiple Stata sessions at the same time

## Installation

`statable` is not currently on CRAN. You can install the development
version of statable from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("james-atkins/statable")
```

## Getting Started

To run a Stata command from R, use the `stata_run` function. This also
accepts a vector of commands.

``` r
library(statable)

stata_run("sysuse auto")
#> . sysuse auto
#> (1978 Automobile Data)
stata_run(c("summarize", "regress price length weight"))
#> . summarize
#> 
#>     Variable |        Obs        Mean    Std. Dev.       Min        Max
#> -------------+---------------------------------------------------------
#>         make |          0
#>        price |         74    6165.257    2949.496       3291      15906
#>          mpg |         74     21.2973    5.785503         12         41
#>        rep78 |         69    3.405797    .9899323          1          5
#>     headroom |         74    2.993243    .8459948        1.5          5
#> -------------+---------------------------------------------------------
#>        trunk |         74    13.75676    4.277404          5         23
#>       weight |         74    3019.459    777.1936       1760       4840
#>       length |         74    187.9324    22.26634        142        233
#>         turn |         74    39.64865    4.399354         31         51
#> displacement |         74    197.2973    91.83722         79        425
#> -------------+---------------------------------------------------------
#>   gear_ratio |         74    3.014865    .4562871       2.19       3.89
#>      foreign |         74    .2972973    .4601885          0          1
#> . regress price length weight
#> 
#>       Source |       SS           df       MS      Number of obs   =        74
#> -------------+----------------------------------   F(2, 71)        =     18.91
#>        Model |   220725280         2   110362640   Prob > F        =    0.0000
#>     Residual |   414340116        71  5835776.28   R-squared       =    0.3476
#> -------------+----------------------------------   Adj R-squared   =    0.3292
#>        Total |   635065396        73  8699525.97   Root MSE        =    2415.7
#> 
#> ------------------------------------------------------------------------------
#>        price |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
#> -------------+----------------------------------------------------------------
#>       length |  -97.96031    39.1746    -2.50   0.015    -176.0722   -19.84838
#>       weight |   4.699065   1.122339     4.19   0.000     2.461184    6.936946
#>        _cons |   10386.54   4308.159     2.41   0.019     1796.316    18976.76
#> ------------------------------------------------------------------------------
```

To pass data from R to Stata, you can use the function `stata_data_in`.
Likewise, the function `stata_data_out` loads the current Stata dataset
into R.

``` r
data("mtcars")

stata_data_in(mtcars, clear = TRUE)
#> . use "$R_data_frame", clear
stata_run("describe")
#> . describe
#> 
#> Contains data from /tmp/RtmppX45Ji/statable1843655686f85/data_frame184365b4fe3c
#> > 6.dta
#>   obs:            32                          
#>  vars:            11                          23 Aug 2024 16:00
#> -------------------------------------------------------------------------------
#>               storage   display    value
#> variable name   type    format     label      variable label
#> -------------------------------------------------------------------------------
#> mpg             double  %10.0g                
#> cyl             double  %10.0g                
#> disp            double  %10.0g                
#> hp              double  %10.0g                
#> drat            double  %10.0g                
#> wt              double  %10.0g                
#> qsec            double  %10.0g                
#> vs              double  %10.0g                
#> am              double  %10.0g                
#> gear            double  %10.0g                
#> carb            double  %10.0g                
#> -------------------------------------------------------------------------------
#> Sorted by:

stata_run("rename mpg miles_per_gallon")
#> . rename mpg miles_per_gallon
mtcars_stata <- stata_data_out()
#> . save "/tmp/RtmppX45Ji/statable1843655686f85/stata184363f8c4556.dta"
#> file /tmp/RtmppX45Ji/statable1843655686f85/stata184363f8c4556.dta saved

mean(mtcars_stata$miles_per_gallon)
#> [1] 20.09062
```

More generally, you can use the Stata global macros of the form
`$R_dataframe_name`. Behind the scenes, this exports the referenced data
frame to a dta file and sets the value of the global to this path. This
allows you to use data in R with a variety of Stata commands, such as
`merge`, `joinby` or `append`.

``` r
# statable sets the gloal R_mtcars to be equal to the path of a dta file
# containing the data frame mtcars.
stata_run("use $R_mtcars, clear")
#> . use $R_mtcars, clear
stata_run("display _N")
#> . display _N
#> 32

stata_run("append using $R_mtcars")
#> . append using $R_mtcars
stata_run("display _N")
#> . display _N
#> 64
```

Note that R allows both variable names and column names to have dots in
them but this is not allowed by Stata! You will have to rename them
first.

When running `stata_run`, `statable` starts a Stata *session* in the
background. Unless overwise specified, all commands use this same
session but it is possible to have multiple Stata sessions running at
the same time. You can access the default session with
`stata_default_session()`.

``` r
another_session <- stata_start_session()

stata_run("sysuse lifeexp", session = another_session)
#> . sysuse lifeexp
#> (Life expectancy, 1998)
stata_run("describe, simple", session = another_session)
#> . describe, simple
#> region     country    popgrowth  lexp       gnppc      safewater

# Note that the data set loaded in the default session is different to the
# data set in `another_session`.
stata_run("describe, simple")
#> . describe, simple
#> mpg   cyl   disp  hp    drat  wt    qsec  vs    am    gear  carb

stata_close_session(another_session)
```

`statable` integrates with knitr allowing you to run Stata code chunks
in knitr or rmarkdown documents. To do so, you must first import the
package with `library(statable)` so knitr knows what to do with `stata`
chunks. Then add a chunk like the following.

```` default
```{stata}
use $R_mtcars, clear
summarize
```
````
