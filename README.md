
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statable - Run Stata from R

<!-- badges: start -->
<!-- badges: end -->

statable is an R interface to Stata that lets you to combine the
strengths of both R and Stata. You can prepare and manipulate your data
in R, then quickly switch to Stata to perform specialised analyses, all
without leaving your R environment.

statable provides the following functionality:

- Run Stata commands from R
- Transfer datasets between R and Stata
- Run Stata “chunks” in knitr or R Markdown documents
- Work with multiple Stata sessions at the same time

## Installation

`statable` is not currently on CRAN. You can install the development
version from [GitHub](https://github.com/james-atkins/statable) with:

``` r
# install.packages("pak")
pak::pak("james-atkins/statable")
```

## Usage

For more information, please read the [Getting Started
guide](https://statable.jamesatkins.com/articles/statable.html).

``` r
# First, load the package. All statable commands start with `stata_`.
library(statable)

# statable should find Stata automatically but, if necessary, you can
# manually set the path to the Stata executable
# stata_path("C:/Program Files/Stata18/StataSE-64.exe")

# `stata_run()` runs Stata commands in the same session.
# All your data and results stay in one place, allowing you to easily build on
# your previous commands.
stata_run("sysuse auto")
#> ℹ Started Stata session:
#>   '/nix/store/hk6h0ak3ym85b771n6kvk1hcmh4b7rjv-user-environment/bin/stata-se'
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

# To pass data from R to Stata, you can use the function `stata_data_in()`
data("mtcars")
stata_data_in(mtcars, clear = TRUE)
#> . use "$R_data_frame", clear
stata_run("describe")
#> . describe
#> 
#> Contains data from /tmp/Rtmp43oq7Y/statable132588723cb8a5/data_frame13258872ed1
#> > 4a8.dta
#>   obs:            32                          
#>  vars:            11                          02 Sep 2024 18:08
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

# Likewise, `stata_data_out()` returns the current Stata dataset into R
stata_run("rename mpg miles_per_gallon")
#> . rename mpg miles_per_gallon
mtcars_stata <- stata_data_out()
#> . save "/tmp/Rtmp43oq7Y/statable132588723cb8a5/stata1325882c7c2af9.dta"
#> file /tmp/Rtmp43oq7Y/statable132588723cb8a5/stata1325882c7c2af9.dta saved
mean(mtcars_stata$miles_per_gallon)
#> [1] 20.09062

# You can also reference R data frames using Stata globals of the form
# `$R_dataframe_name`.
# This allows you to use data in R with a variety of Stata commands, such as
# `merge`, `joinby` or `append`.
stata_run("use $R_mtcars, clear")
#> . use $R_mtcars, clear
stata_run("summarize")
#> . summarize
#> 
#>     Variable |        Obs        Mean    Std. Dev.       Min        Max
#> -------------+---------------------------------------------------------
#>          mpg |         32    20.09062    6.026948       10.4       33.9
#>          cyl |         32      6.1875    1.785922          4          8
#>         disp |         32    230.7219    123.9387       71.1        472
#>           hp |         32    146.6875    68.56287         52        335
#>         drat |         32    3.596563    .5346787       2.76       4.93
#> -------------+---------------------------------------------------------
#>           wt |         32     3.21725    .9784574      1.513      5.424
#>         qsec |         32    17.84875    1.786943       14.5       22.9
#>           vs |         32       .4375    .5040161          0          1
#>           am |         32      .40625    .4989909          0          1
#>         gear |         32      3.6875    .7378041          3          5
#> -------------+---------------------------------------------------------
#>         carb |         32      2.8125      1.6152          1          8
stata_run("append using $R_mtcars")
#> . append using $R_mtcars
stata_run("display _N")
#> . display _N
#> 64
```

## knitr

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
