# R mipplot package

## Purpose and Functionality

The mipplot package contains generic functions to produce area/bar/box/line plots of data following IAMC submission format.


## Installation

#### Windows

The latest release of `mipplot` can be installed from a binary package.

1. Visit the [release page on GitHub](https://github.com/UTokyo-mip/mipplot/tree/master/release).

2. Download the latest **zip** file (binary package).

3. Start R.

4. Set working directory to where the downloaded file is in.

5. Run following commands.
```r
install.packages("tidyverse")
install.packages("mipplot_0.1.0.zip", repos = NULL)
```

Or via `devtools`:

```r
if (!require("devtools")) install.packages("devtools")
library(devtools)
devtools::install_github("UTokyo-mip/mipplot", dependencies = TRUE)
```

#### Mac OS

The latest release of `mipplot` can be installed from a binary package.

1. Visit the [release page on GitHub](https://github.com/UTokyo-mip/mipplot/tree/master/release).

2. Download the latest **tar.gz** file (source package).

3. Start R.

4. Set working directory to where the downloaded file is in.

5. Run following commands.
```r
install.packages("tidyverse")
install.packages("mipplot_0.1.0.tar.gz", repos = NULL)
```

Or via `devtools`:

```r
if (!require("devtools")) install.packages("devtools")
library(devtools)
devtools::install_github("UTokyo-mip/mipplot", dependencies = TRUE)
```

#### Ubuntu (or Debian based distribution)

Run the following commands:

```bash
sudo apt install build-essential gfortran g++ libcurl4-openssl-dev libxml2-dev libssl-dev
```
  This commands install the library which `tidyverse` library depends.

The latest release of `mipplot` can be installed from a tarball.

1. Visit the [release page on GitHub](https://github.com/UTokyo-mip/mipplot/tree/master/release).

2. Download the latest **tar.gz** file (source package).

3. Start R.

4. Set working directory to where the downloaded file is in.

5. Run following commands.
```r
install.packages("tidyverse")
install.packages("mipplot_0.1.0.tar.gz", repos = NULL)
```

Or via `devtools`:

```r
if (!require("devtools")) install.packages("devtools")
library(devtools)
devtools::install_github("UTokyo-mip/mipplot", dependencies = TRUE)
```

## Example

```r
library(mipplot)
mipplot_area(ar5_db_sample_data, ar5_db_sample_rule_table,
  region = "World", scenario = "EMF27-450-FullTech")
```

## Questions / Problems

In case of questions / problems please contact Diego Silva Herran <silva-herran@iges.or.jp>.

## Screenshots

<img src="/images/top_screenshot.png?raw=true" alt="screenshot" />

## License

The mipplot R package is open source licensed under the MIT license.
