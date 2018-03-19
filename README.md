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
mipplot_interactive_plot_line(mipplot::AR5_sample_data)
```

## Screenshots



## License

The mipplot R package is open source licensed under the MIT license.

test text: edit by wnag
