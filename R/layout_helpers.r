### Helper functions for layout

#' @title Query abbreviations
#' @family table layout
#' @seealso \code{\link{html.layout}} for html tables and \code{\link{print_abbrev}} for printing abbrevs to table footer.
#' @description
#' \code{query_abbrev} queries if any of the names in a named list is present in an object including object names, matching only on strings that are separate words (ie starting and ending with blank, comma, parentheris or other separator). Returns named list with all abbreviations present (via grepl).
#' @param x Object to test.
#' @param y Named list of abbrevs.
#' @return  Named list of abbreviations in object and corresponding explanation
#' @examples None yet.
#' @export
## Version 1.1: Added code to look for abbreviation only when being a separate word
query_abbrev <- function(x, y) {
    returnthis <- vector()
    for(i in 1: length(y)){
        ifelse(isTRUE(
            sum(
                sapply(
                    x,
                    FUN = function(x) {
                        grepl(
                            paste('\\b', names(y[i]), '\\b', sep = ''),
                            x
                        )
                    }  # test in all columns 
                )) >0
            || sum(
                   grepl(
                       paste('\\b',names(y[i]), '\\b', sep = ''),
                       names(x)
                   )
               ) > 0
        ), # test in names
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


#' @title Add units to labels
#' @family table layout
#' @description
#' \code{add_units} adds units to variable labels from a named vector matching the names of the labels. 
#' @param x  contains variable labels.
#' @param y contains units to be added, matched by name.
#' @return  Named vector with labels and added units, if available (otherwise "NA" is added).
#' @examples
#' variable_labels <- c(
#'    'apples' = "Apples",
#'    'bnanas' = "Bananas",
#'    'peas' = "Peas"
#')
#'unit_labels <- c(
#'    'apples' = "pcs",
#'    'peas' = "kg"
#')
#'add_units(variable_labels, unit_labels)
#' @export

add_units <- function(x, y){
    ## x and y are named vectors
    ## x contains variable labels
    ## y contains units, matched by name
    TEMP <- plyr::rbind.fill.matrix(t(x), t(y))
    TEMP[is.na(TEMP)] <- "missing_label"
    RESULT <- paste(TEMP[1,], TEMP[2,], sep = ", ")
    names(RESULT) <- colnames(TEMP)
    RESULT <- gsub(", missing_label", "", RESULT) # remove missing
    return(RESULT)
}
