% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mipplot_area.R
\name{mipplot_area}
\alias{mipplot_area}
\title{Area plot from IAMC data}
\usage{
mipplot_area(
  D,
  R,
  region = levels(D$region),
  scenario = levels(D$scenario),
  facet_x = NULL,
  facet_y = NULL,
  PRINT_OUT = FALSE,
  DEBUG = TRUE,
  fontsize = 20,
  color_code_specify = TRUE,
  one_hundred_percent_stacked = FALSE,
  axis_year_text_angle = 0,
  language = "en"
)
}
\arguments{
\item{D}{A dataframe of IAMC data in tibble format to produce area plots.}

\item{R}{A dataframe of data aggregation rules (meta data).}

\item{region}{A list of regions.}

\item{scenario}{A list of scenario.}

\item{facet_x}{facet_x}

\item{facet_y}{facet_y}

\item{PRINT_OUT}{set TRUE to generate PDF file.}

\item{DEBUG}{set TRUE to show debug messages.}

\item{fontsize}{font size of text.}

\item{color_code_specify}{set FALSE if you apply default color palette.}

\item{one_hundred_percent_stacked}{set TRUE if you want a graph of 100\% stacked, set this to TRUE.}

\item{axis_year_text_angle}{text angle of x axis}

\item{language}{A string of language. Possible values are "en", "jp",
"es", "zh-cn", "zh-tw". The default value is "en".}
}
\value{
A list of area plots.
}
\description{
Area plots using right-hand-side values of target
             additivity rule. The function arguments include the input dataframe,
             labels for the plot/axes/legend, and faceting dimensions
             (two in this version).
}
\examples{
\donttest{
library(dplyr)
data_subset <- ar5_db_sample_data \%>\%
filter(variable == "Emissions|CO2|Land Use") \%>\%
filter(model \%in\% c("AIM-Enduse 12.1", "GCAM 3.0", "IMAGE 2.4")) \%>\%
filter(2005 <= period) \%>\%
filter(period <= 2100)
mipplot_area(data_subset, ar5_db_sample_rule_table,
region = c("ASIA"),
scenario = c("EMF27-450-Conv"),
one_hundred_percent_stacked = FALSE,
axis_year_text_angle = 0,
language = 'en')
}
}
