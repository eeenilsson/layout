% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout.r
\name{prepare}
\alias{prepare}
\title{Table by group}
\usage{
prepare(object, variable_col, by_col = "all")
}
\arguments{
\item{object}{A data frame containing the layout table body.}

\item{variable_col}{A character element with the name of the column containing variable names.}

\item{by_col}{A character element with specifying the column containing the factor used for grouping. Set to 'all' for single group.}
}
\value{
An S4 object. the object@body contains the table with one row per variable and a set of display columns for each level of the grouping variable.
}
\description{
\code{prepare} prepare formats body from long to wide by given factor and collects variables from original data for later. Output is an S4 object containing the table and ancillary information to be used for table formatting in when printing as html.
}
\examples{
## Create an S4 object, optionally adding specifications for the slots
example_layout <- new('layout',
                     caption = 'Example table',
                     footer = 'Flower petal dimensions by species.',
                     body = dta_length) # using patel length as body
## Prepare the layout object
example_layout <- prepare(example_layout,
                          variable_col= 'key',
                          by_col = "Species")
## Get petal width data
example_layout_width <- new('layout',
                      body = dta_width) 
## Prepare the layout object for petal width
example_layout_width <- prepare(example_layout_width,
                          variable_col= 'key',
                          by_col = "Species")
}
\seealso{
\code{\link{layout_html}} for html tables.

Other table layout: 
\code{\link{add_colgroup}()},
\code{\link{add_cols}()},
\code{\link{add_rowgroup}()},
\code{\link{add_units}()},
\code{\link{firstrow_css}()},
\code{\link{layout_html}()},
\code{\link{order_by_vector}()},
\code{\link{order_layout}()},
\code{\link{print_abbrev}()},
\code{\link{query_abbrev}()}
}
\concept{table layout}
