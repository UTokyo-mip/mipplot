% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mipplot_autofill_color.R
\name{mipplot_autofill_color}
\alias{mipplot_autofill_color}
\title{Complementation of color scheme}
\usage{
mipplot_autofill_color(rule_table_without_colors)
}
\arguments{
\item{rule_table_without_colors}{Incomplete color specification rule table. It dosen't contain "Color_code" column.}
}
\value{
Complete color specification rule table. It is containing "Color_code" column.
However, if color complementation can not be performed
automatically, the return value is an incomplete color specification.
}
\description{
fill colors automatically
}
