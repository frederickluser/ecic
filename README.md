
# Extended Changes-in-Changes (ecic)

**ecic** estimates a changes-in-changes model with multiple periods and cohorts as suggested in Athey and Imbens
([2006](https://onlinelibrary.wiley.com/doi/10.1111/j.1468-0262.2006.00668.x).
They propose to estimate a changes-in-changes model for every valid two-by-two combination of treatment and control groups.
This package implements this, calculates standard errors via bootstrap and plot results for average quantile treatment effects (QTEs).
Coefficients can also be aggregated for each post-treatment period in an event-study-style fashion.

## Installation

You can install **ecic** from Github.

``` r
# install.packages("remotes")
remotes::install_github("frederickluser/ecic")
```

## Example

``` r
library(ecic)

data("dat", package = "ecic")
head(dat)
#>    year countyreal     lpop     lemp first.treat treat
#> 866 2003       8001 5.896761 8.461469        2007     1
#> 841 2004       8001 5.896761 8.336870        2007     1
#> 842 2005       8001 5.896761 8.340217        2007     1
#> 819 2006       8001 5.896761 8.378161        2007     1
#> 827 2007       8001 5.896761 8.487352        2007     1
#> 937 2003       8019 2.232377 4.997212        2007     1

# Estimate the model
mod =
  ecic(
    yvar = lemp, 		# dependent variable
    gvar = first.treat, # group variable
    tvar = year,		# time variable
    ivar = countyreal,	# unit ID
    dat = mpdta, 		# dataset
    boot = "weighted",	# bootstrap proceduce (NULL, "normal", or "weighted")
    nReps = 100		# number of bootstrap runs
    )

cic_summary(mod)
  perc        coefs         se
#> 0.1  0.128731804 0.08624093
#> 0.2  0.006087219 0.04316900
#> 0.3 -0.063364510 0.26915492
#> 0.4 -0.087365098 0.27766957
#> 0.5 -0.121044019 0.32612111
#> 0.6 -0.079813481 0.29355815
#> 0.7 -0.143767555 0.21229327
#> 0.8  0.245438750 0.20500564
#> 0.9 -0.153745906 0.25230691
