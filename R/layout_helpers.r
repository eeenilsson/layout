### Helper functions for layout

#' @title Query abbreviations
#' @family table layout
#' @seealso \code{\link{html.layout}} for html tables and \code{\link{print_abbrev}} for printing abbrevs to table footer.
#' @description
#' \code{query_abbrev} queries if any of the names in a named list is present in an object including object names. Returns named list with all abbreviations present (via grepl).
#' @param x Object to test.
#' @param y Named list of abbrevs.
#' @return  Named list of abbreviations in object and corresponding explanation
#' @examples None yet.
#' @export
query_abbrev <- function(x, y) {
    # 
    #
    returnthis <- vector()
    for(i in 1: length(y)){
        ifelse(isTRUE(sum(
            sapply(
             x, FUN = function(x) {grepl(names(y[i]), x)}  # test in all columns 
            )) >0
            || sum(grepl(names(y[i]), names(x))) > 0), # test in names
               returnthis <- append(returnthis, y[i]),
               NA
        )
    }
    return(returnthis)
}

#' @title Print abbreviations
#' @family table layout
#' @seealso \code{\link{query_abbrev}} for querying an object for abbrevs 
#' @description
#' \code{print_abbrev} prints abbrevs provided in named list, for table footer.
#' @param x Object to test.
#' @param y Named list of abbrevs.
#' @return  Character string with each abbrev and explanation pair separated by semicolon.
#' @examples None yet.
#' @export
print_abbrev <- function(x){
   # input named list with abbreviations as names and explanations as values
   # returns character string with abbreviations and corresponding explanations
   listabbrev <- vector()
   for(i in 1:length(x)){
    listabbrev <- append(listabbrev,
           paste(
               as.character(names(x[i])),
               x[i],
               sep = ", ")
           )
   }
  return(paste("Abbreviations: ", paste(listabbrev, collapse="; "), sep=""))
}

#' @title Order data frame by vector
#' @family table layout
#' @seealso \code{\link{query_abbrev}} for querying an object for abbrevs 
#' @description
#' \code{order_by_vector} orders a data frame by a character vector.
#' @param x Data frame to order.
#' @param match_by Character element specifying name of column to match by.
#' @param order_by Character vector with character elements in the desired order, matching those in the dataframe column that is to be matched on.
#' @return  Reordered data frame.
#' @examples None yet.
#' @export
order_by_vector <- function(x, match_by, order_by){
    df_temp <- data.frame(
                             odr_by_temp=order_by,
                             odr_temp=1:length(order_by)
    )
    names(df_temp) <- c(match_by, "odr_temp")
    arrange(left_join(x,
                      df_temp
                      ),
            odr_temp
            )%>%select(-odr_temp)
}


