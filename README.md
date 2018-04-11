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

<img src="/images/top_screenshot.png?raw=true" width=70.0% alt="screenshot" />

## License

The mipplot R package is open source licensed under the MIT license.

test text: edit by wnag
