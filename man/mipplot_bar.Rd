% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mipplot_bar.R
\name{mipplot_bar}
\alias{mipplot_bar}
\title{Bar plot from IAMC data}
\usage{
mipplot_bar(
  D,
  R,
  region = levels(D$region),
  xby = "scenario",
  target_year = levels(as.factor(D$period)),
  facet_x = NULL,
  facet_y = NULL,
  PRINT_OUT = FALSE,
  DEBUG = TRUE,
  fontsize = 20,
  color_code_specify = TRUE,
  one_hundred_percent_stacked = FALSE,
  axis_scenario_text_angle = 0,
  language = "en"
)
}
\arguments{
\item{D}{A dataframe of IAMC data in tibble format to produce plots.}

\item{R}{A dataframe of data aggregation rules (meta data).}

\item{region}{A list of region.}

\item{xby}{name of axis. the default setting is "scenario".}

\item{target_year}{target year.}

\item{facet_x}{facet_x}

\item{facet_y}{facet_y}

\item{PRINT_OUT}{set TRUE to generate A PDF file.}

\item{DEBUG}{set TRUE to show debug messages.}

\item{fontsize}{size of font in the output plot.}

\item{color_code_specify}{set FALSE if you apply default color palette.}

\item{one_hundred_percent_stacked}{set TRUE if you want a graph of 100\% stacked, set this to TRUE.}

\item{axis_scenario_text_angle}{text angle of x axis}

\item{language}{A string of language. Possible values are "en", "jp",
"es", "zh-cn", "zh-tw". The default value is "en".}
}
\value{
A list of bar plots.
}
\description{
Bar plots using right-hand-side values of
             target additivity rule. The function arguments include the
             input dataframe, labels for the plot/axes/legend, and
             faceting dimensions.
}
\examples{
\donttest{
library(dplyr)
data_subset <- ar5_db_sample_data \%>\%
filter(variable == "Emissions|CO2|Land Use") \%>\%
filter(model \%in\% c("AIM-Enduse 12.1", "GCAM 3.0", "IMAGE 2.4")) \%>\%
filter(scenario \%in\% c("EMF27-450-Conv", "EMF27-450-FullTech"))

mipplot_bar(data_subset, ar5_db_sample_rule_table,
region = c("ASIA"),
target_year = 2005,
one_hundred_percent_stacked = FALSE,
axis_scenario_text_angle = 0,
language = 'en')
}
}
