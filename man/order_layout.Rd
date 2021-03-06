% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout.r
\name{order_layout}
\alias{order_layout}
\title{Order table rows.}
\usage{
order_layout(
  object,
  match_by = object@variable_col,
  order_by = object@.row_order,
  drop = FALSE
)
}
\arguments{
\item{object}{A layout object.}

\item{match_by}{A character element specifying the column name in the object@body that is used for ordering.}

\item{order_by}{A character vector with variable names appearing in the order that is to be applied to the object@body.}

\item{drop}{If \code{TRUE}, variables absent from \code{order_by} are removed from the returned object@body.}
}
\value{
A layout object where the object@body has been reordered.
}
\description{
\code{order_layout} Orders the rows of a layout object table body.
}
\examples{
example_layout <- order_layout(example_layout,
        match_by = 'key',
        order_by = c("Sepal.Length", "Petal.Length"))
}
\seealso{
\code{\link{prepare}} for making a layout object and \code{\link{layout_html}} for printing a layout object to html.

Other table layout: 
\code{\link{add_colgroup}()},
\code{\link{add_cols}()},
\code{\link{add_rowgroup}()},
\code{\link{add_units}()},
\code{\link{firstrow_css}()},
\code{\link{layout_html}()},
\code{\link{order_by_vector}()},
\code{\link{prepare}()},
\code{\link{print_abbrev}()},
\code{\link{query_abbrev}()}
}
\concept{table layout}
