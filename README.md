<<<<<<< HEAD
# R mipplot package

## Purpose and Functionality

The mipplot package contains generic functions to produce area/bar/box/line plots of data following IAMC submission format.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```
The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mipplot")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Diego Silva Herran <silva-herran@iges.or.jp>.
=======
# mipplot

Package contains generic functions to produce area/bar/box/line plots of data following IAMC submission format.

## Installation

Install from GitHub (requires [devtools](https://github.com/hadley/devtools) package):

```r
if (!require("devtools")) install.packages("devtools")
library(devtools)
devtools::install_github("UTokyo-mip/mipplot", dependencies = TRUE)
```

## Example

```r
library(mipplot)
mipplot_interactive_plot_line(mipplot::ar5_db_sample09_Wang, mipplot::ar5_db_rule_table_v09_Wang)
```

## Screenshots

<img src=https://raw.githubusercontent.com/UTokyo-mip/mipplot/develop/images/top_screenshot.png width=70.0% alt="screenshot" />

## License

The mipplot R package is open source licensed under the MIT license.

test text: edit by wnag
>>>>>>> develop
