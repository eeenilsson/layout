#' @import dplyr
#' @import htmlTable

## An S4 class to represent a table layout.
#' @slot body A data.frame
#' @slot footer A character
#' @slot abbreviations A character vector
#' @slot cgroup_levels  A character vector
#' @slot display A character vector
#' @slot variable_col A character vector
#' @slot left_col A character vector
#' @slot right_col A character vector
#' @slot n.cgroup A numeric
#' @slot col_order A character vector
#' @slot n.rgroup A numeric
#' @slot rgroup A character vector
#' @slot rgroup_header A list
#' @slot .row_order A character vector hidden
#' @slot labels A character vector
layout <- setClass("layout", representation(
                       caption="character",
                       body="data.frame",
                       footer="character",
                       abbreviations="character",
                       cgroup_levels ="character",
                       display="character",
                       variable_col="character",
                       left_col="character",
                       right_col="character",
                       n.cgroup="numeric",
                       col_order="character",
                       n.rgroup="numeric",
                       rgroup=c("character"),
                       rgroup_header="list",
                       .row_order="character", # hidden
                       group_type="character",
                       labels="character"
                   ))

#' @title Table by group
#' @family table layout
#' @seealso \code{\link{layout_html}} for html tables.
#' @description
#' \code{prepare} prepare formats body from long to wide by given factor and collects variables from original data for later. Output is an S4 object containing the table and ancillary information to be used for table formatting in when printing as html.
#' @param object A data frame containing the layout table body.
#' @param variable_col A character element with the name of the column containing variable names. 
#' @param by_col A character element with specifying the column containing the factor used for grouping. Set to 'all' for single group. 
#' @return An S4 object. the object@body contains the table with one row per variable and a set of display columns for each level of the grouping variable.
#' @examples
#' ## Create an S4 object, optionally adding specifications for the slots
#' example_layout <- new('layout',
#'                      caption = 'Example table',
#'                      footer = 'Flower petal dimensions by species.',
#'                      body = dta_length) # using patel length as body
#'## Prepare the layout object
#' example_layout <- prepare(example_layout,
#'                           variable_col= 'key',
#'                           by_col = "Species")
#' ## Get petal width data
#' example_layout_width <- new('layout',
#'                       body = dta_width) 
#' ## Prepare the layout object for petal width
#' example_layout_width <- prepare(example_layout_width,
#'                           variable_col= 'key',
#'                           by_col = "Species")
#'@export
setGeneric('prepare', # initialize funtion as a generic
           function(object,
                    variable_col,
                    by_col='all'){
               standardGeneric('prepare')
           }
           )
setMethod('prepare', # specify function in relation to object class
          signature(object='layout'),
          function(object, variable_col, by_col){
              if(by_col == 'all'){
                  object@body <- cbind(
                      object@body,
                      all = as.factor(rep("all", times = nrow(object@body)))
                  )
              }
              if(is.null(levels(object@body[[by_col]]))){
                  warning("The BY column must be a factor variable")
              }
                            tbl <- as.data.frame(object@body) # remake?
              slot(object, "cgroup_levels") <-as.character(
                  levels(object@body[[by_col]]))
              slot(object, "display") <- names(tbl)[!names(tbl)%in% names(tbl[c(variable_col, by_col)])]
              slot(object, "variable_col") <- variable_col
              slot(object, "n.cgroup") <- c(
                  1, # for variables col
                  rep(
                      length(object@display), # n display cols
                      times = length(object@cgroup_levels) # times n levels in grouping column
    )
    )
              if(variable_col == 'variable'){
                  warning('variable col should not be named "variable" since it interferes with the re-casting. Please rename it.')
              }
              if(by_col != 'all'){
                  object@body%>%
                      reshape2::melt(., id.vars=c(variable_col, by_col))%>% # needed for cast
                      reshape2::dcast(., # cast to make a set of display cols for each level of by_col
                                      as.formula(
                                          paste(
                                              paste(variable_col, collapse = " + "),
                                              "~",
                                              by_col,
                                              "+",
                                              "variable" # generated by melt
                                          )
                                      ),
                                      id.var = c(variable_col),
                                      xvalue.var = c('value')
                                      ) -> slot(object, "body")
              }else{
                  slot(object, "body") <- object@body[!names(object@body) %in% by_col]
              }
              
              slot(object, "col_order") <- names(object@body)
              return(object)
          }
    )

## problem: Function will not accept tibble, must be converted to data frame

#' @title Order table rows. 
#' @family table layout
#' @seealso \code{\link{prepare}} for making a layout object and \code{\link{layout_html}} for printing a layout object to html.
#' @description
#' \code{order_layout} Orders the rows of a layout object table body.
#' @param object A layout object.
#' @param match_by A character element specifying the column name in the object@body that is used for ordering.
#' @param order_by A character vector with variable names appearing in the order that is to be applied to the object@body.
#' @param drop If \code{TRUE}, variables absent from \code{order_by} are removed from the returned object@body.
#' @return A layout object where the object@body has been reordered. 
#' @examples 
#'example_layout <- order_layout(example_layout,
#'         match_by = 'key',
#'         order_by = c("Sepal.Length", "Petal.Length"))
#' @export
setGeneric('order_layout', # initialize funtion as a generic
           function(object,
                    match_by=object@variable_col, #default order by var col 
                    order_by=object@.row_order,
                    drop=FALSE
                    ){
               standardGeneric('order_layout')
           }
           )
setMethod('order_layout', # specify function in relation to object class
          signature(object='layout'),
          function(object, match_by, order_by, drop){
              if(length(object@n.rgroup)>0){
                  warning("The layout object contains row groups. These can potentially be disrupted by re-ordering the table rows. Use order_by before creating row groups or check results carefully.")
              }else{NULL}
              df_temp <- data.frame(
                  odr_by_temp=order_by,
                  odr_temp=1:length(order_by)
              )
              names(df_temp) <- c(match_by, "odr_temp")
              df_temp <- arrange(inner_join(df_temp, #left => inner join
                                           object@body # changed places here
                                           ),
                                 odr_temp
                                 )%>%select(-odr_temp)
              
              if(!drop){ # keep rows not in vector
                  df_temp <- rbind(
                      df_temp,
                      object@body[
                          !object@body[,object@variable_col] %in% order_by,
                          ]
                  )
              }else{NULL}
              slot(object, "body") <- df_temp
              return(object)
          }
          )
#' @title Add row groups
#' @family table layout
#' @seealso \code{\link{prepare}} for making a layout object and \code{\link{layout_html}} for printing a html table from such an object.
#' @description
#' \code{add_rowgroup} Adds a row group to a layout object and assigns separate row group headers displayed when printing as html. The original object@body will be used as the first row group.
#' @param object A layout object including a body containing the table.
#' @param object2 A layout object including a body containing in its @body the row group to be added.
#' @param rgroup_names A character vector specifying the names (labels) to be used for the row groups.
#' @return An S4 object with rows- and row group specifications added.
#' @examples
#' example_layout <- add_rowgroup(example_layout,
#'                               example_layout_width,
#'                               rgroup_names = c("Sepal Length", "Sepal Width"))
#' @export
setGeneric('add_rowgroup',
           function(object, object2, rgroup_names=''){
               standardGeneric('add_rowgroup')
           }
           )
setMethod('add_rowgroup', 
          signature(object='layout'),
          function(object, object2, rgroup_names){
              object@rgroup = rgroup_names
              if(length(object@n.rgroup)==0){
                  object@n.rgroup = c(nrow(object@body), nrow(object2@body) )  
              }else{
                  object@n.rgroup = c(object@n.rgroup, nrow(object2@body) )    
              }
              header2 <- names(object2@body)
              names(object2@body) <- names(object@body) # to enable rbind
              slot(object, "body") <- rbind(object@body,
                        object2@body)
              firstcol_label <- vector()
              if(length(object@rgroup_header)==0){ # assign rgroup headers
                  firstcol_label <- object@variable_col
              }else{
                  firstcol_label <- object@rgroup_header[[1]][1]
              }
              rgroup_header1 <- c(
                  firstcol_label,
                  object@left_col,
                  rep(object@display, times =  length(object@cgroup_levels)),
                  object@right_col
              )
              if(length(object2@rgroup_header)==0){
                  firstcol_label <- object2@variable_col
              }else{
                  firstcol_label <- object2@rgroup_header[[1]][1]
              }
              rgroup_header2 <- c(
                  firstcol_label,
                  object2@left_col,
                  rep(object2@display, times =  length(object2@cgroup_levels)),
                  object2@right_col
              )
              if(length(object@rgroup_header)<2){
                  slot(object, "rgroup_header") <- list(
                      rgroup_header1,
                      rgroup_header2)
              }else{
                  slot(object, "rgroup_header") <- list(
                      object@rgroup_header,
                      rgroup_header1,
                      rgroup_header2)
              }
              if(rgroup_names[1]==''){
                  object@rgroup <- paste("Group",
                                         1:length(object@rgroup_header))
              }else{
                  object@rgroup <- rgroup_names
              }
              names(object@rgroup) <- NULL
              object@rgroup_header <- setNames(
                  object@rgroup_header,
                  1:length(object@rgroup_header)
              )
              for(i in 1:length(object@rgroup_header)){ # set names for add
                  object@rgroup_header[[i]] <- setNames(
                      object@rgroup_header[[i]],
                      1:length(object@rgroup_header[[i]]))
              }
              return(object)
          }
          )

#' @title Add columns
#' @family table layout
#' @seealso \code{\link{prepare}} for making a layout object.
#' @description
#' \code{add_cols} Adds a data frame to a layout object table body.
#' @param object A layout object including a body containing the table.
#' @param DF A data frame with the columns to be added. The column used for matching is not added. 
#' @param by A character element with the name of the column specifying the column to use for joining (must be named identically in the layout object and the data frame containing the columns to be added).
#' @param position A character value 'left' or 'right' specifying where the new columns should be added. Any specification other than 'right' will be interpreted as 'left'.
#' @return A layout object with new columns added to the object body.
#' @examples
#' example_layout <- add_cols(
#'    example_layout,
#'    dta_valid_n,
#'    by = "key",
#'    position = "left")
#' @export
setGeneric('add_cols', # initialize funtion as a generic
           function(object, DF, by, position = 'left'){
               standardGeneric('add_cols')
           }
           )
setMethod('add_cols', # specify function in relation to object class
          signature(object='layout'),
          function(object, DF, by, position){
              slot(object, "body") <- left_join(object@body, DF, by = by) # add cols
              if(position == 'right'){
                  slot(object, "right_col") <- names(DF)[!names(DF) %in% object@variable_col]
              }else{
                  slot(object, "left_col") <- names(DF)[!names(DF) %in% object@variable_col]
              }
              object@col_order <- c(object@variable_col,
                                    object@left_col,
                                    names(object@body)[!names(object@body) %in% c(object@variable_col, object@left_col, object@right_col)],
                                    object@right_col
                                    )
              slot(object, "n.cgroup") <- c(object@n.cgroup[1],
                                            ifelse(is.na(object@left_col[1]),
                                                   NA,
                                                   length(object@left_col)),
                                            object@n.cgroup[-1],
                                            ifelse(is.na(object@right_col[1]),
                                                   NA,
                                                   length(object@right_col))
                                            )
              slot(object, "n.cgroup") <- na.omit(object@n.cgroup)
              slot(object, "body") <- object@body[, object@col_order] # reorder
              return(object)
          }
          )

## Note: make group objects a sub-category to layout?

#' @title Format first row of each rowgroup
#' @family table layout
#' @seealso \code{\link{prepare}} for making a layout object and \code{\link{layout_html}} for printing a layout object to html.
#' @description
#' \code{firstrow_css} makes a css for first row of each rowgroup in a layout object. The output can be fed to the html function as a css.cell argument.
#' @param object A layout object.
#' @param css_style A character vector with the css style options that arfe to be applied. The default css applied is "font -size: 2em;".
#' @return A matrix specifying the css for each cell in the html table.
#' @examples firstrow_css(example_layout)
#' @export
setGeneric('firstrow_css', # initialize funtion as a generic
           function(object,
                    css_style="font-size: 2em;",
                    ...){
               standardGeneric('firstrow_css')
           }
           )
setMethod('firstrow_css', # specify function in relation to object class
          signature(object='layout'),
          function(object, css_style, ...){
              tmp_matrix <- matrix("",
                                   ncol=ncol(object@body),
                                   nrow=1) # empty css for header
              for(i in 1:length(object@n.rgroup)){
                  tmp <- rbind(
                      rep(
                          css_style,
                          times=ncol(object@body)
                      ),
                      matrix("",
                             ncol=ncol(object@body),
                             nrow=object@n.rgroup[i]-1)
                  )
                  tmp_matrix <- rbind(tmp_matrix, tmp)
              }
              tmp_matrix
          }
          )

#' @title Html table
#' @family table layout
#' @seealso \code{\link{prepare}} for making a layout object.
#' @description
#' \code{layout_html} prints layout object as a html table.
#' @param object A layout object including a body containing the table.
#' @param caption An optional character element for table caption.
#' @param footer An optional character element for table footer. Abbreviations, if used, will also be added to the footer.
#' @param abbreviations An optional named vector referenced by the abbreviations query which matches abbreviations in the table body, variables column, header, column groups and row headers and adds the corresponding explanations to the table footer.
#' @param labels An optional named vector which will be used for replacing  names in variables column, header, column groups and row headers.
#' @param header An optional character vector used for column headers. If header is not specified an there are row groups in the table object, the header will be blank (ie only row group headers displayed).
#' @param signif_digits An optional numeric element. If specified, all numeric columns in table body will be rounded to the specified number of significant digits.
#' @param rgroup An optional character vector specifying replacement row group headers. The default is "Group n".
#' @param rgroup An optional numeric vector specifying replacement length for row groups.
#' @return A html table.
#' @examples
#' html_out <- layout_html(example_layout,
#'     ## rgroup=rgr,
#'     css.rgroup="align-content: center;font-weight: bold;font-size: 1em;",
#'     caption='cap',
#'     footer='footer here',
#'     abbreviations=abbrevs,
#'     labels=varnames,
#'     signif_digits=3
#'     ##css.cell=subheader_css(test3, "align-content: center;font-size: 2em;")
#'     )
#' @export
setGeneric('layout_html', # initialize funtion as a generic
           function(object,
                    caption=NULL,
                    header=NULL,
                    footer=NULL,
                    abbreviations=NULL,
                    labels=NULL,
                    signif_digits=NULL,
                    rgroup=NULL,
                    n.rgroup=NULL,
                    units=NULL,
                    ...){
               standardGeneric('layout_html')
           }
           )
setMethod('layout_html', # specify function in relation to object class
          signature(object='layout'),
          function(object, caption, header, footer, abbreviations, labels, signif_digits, rgroup, n.rgroup, units, ...){
              ## if units specified, add these to labels
              if(!is.null(units)){
                  slot(object, "labels") <- add_units(object@labels, units)
              }
              if(is.null(abbreviations)){ # if no abbreviations stated
                  if(length(object@abbreviations)==0){ # if no existing
                      abbreviations <- c('someabbreviation' = "somelabel")
                  }else{abbreviations <- object@abbreviations}
              }
              if(sum(names(object@body) %in% "var")>0){
                  print("Rename var to variable")
## Not working              names(object@body)[names(object@body) %in% "var"] <- "variable"    
              }
              if(is.null(labels)){ # if no labels stated
                  if(length(object@labels)==0){ # if no existing
                      labels <- c('someterm' = "somelabel")
                  }else{labels <- object@labels}
              }
              if(!is.null(rgroup)){
                  slot(object, "rgroup") <- rgroup
              }else{NULL}
              if(!is.null(n.rgroup)){
                  slot(object, "n.rgroup") <- n.rgroup
              }else{NULL}
              if(!is.null(caption)){
                  slot(object, "caption") <- caption
              }
              if(!is.null(footer)){
                  slot(object, "footer") <- footer
              }else{
                  if(length(object@footer)==0){
                      slot(object, "footer") <- ''
                  }
              }
              slot(object, "abbreviations") <- abbreviations
              slot(object, "labels") <- labels
              if(!is.null(signif_digits)){
                  object@body <- as.data.frame(
                      lapply(object@body,
                             function(x){
                                 if(is.numeric(x) & sum(!x%%1==0)>0){
                                     signif(x, digits = signif_digits)
                                 }else{
                                     x
                                 }
                             }
                             )
                  )
              }else{NULL}
              if(is.null(header)){
                  header_temp <- c(
                  object@variable_col,
                  object@left_col,
                  rep(object@display, times =  length(object@cgroup_levels)),
                  object@right_col
              )
              }else{
                  header_temp <- header
              }
              if(is.null(header)& length(object@rgroup)>0){
                  header_temp <- rep("", times = length(header_temp))
              }else{
                  NULL}              
              ## cgroup <- c(
              ##     "", # none for var col
              ##     if(length(object@left_col)!=0){""}, # none for left col
              ##     object@cgroup_levels,
              ##     if(length(object@right_col)!=0){""}
              ## )

              ## test
                  cgroup <- c(
                  "", # none for var col
                  if(length(object@left_col)!=0){""}, # none for left col
                  ifelse(length(object@cgroup_levels) == 1 & object@cgroup_levels == 'all', rep('', times = length(object@cgroup_levels)), object@cgroup_levels),
                  if(length(object@right_col)!=0){""}
              )
              ##

              cgroup <-
                  plyr::revalue(cgroup, object@labels, warn_missing = FALSE) # Rename
              header_temp <-
                  plyr::revalue(header_temp, object@labels, warn_missing = FALSE)
              object@rgroup <-
                  plyr::revalue(object@rgroup, object@labels, warn_missing = FALSE)
              object@rgroup_header <- lapply(object@rgroup_header, function(x){plyr::revalue(x, object@labels)})
              object@body[, object@variable_col] <-
                  plyr::revalue(object@body[, object@variable_col], object@labels, warn_missing = FALSE)
              attr(object@rgroup, "add") <- lapply(object@rgroup_header, `[`, -1)
              table_html <- htmlTable(
                  object@body,
                  align=paste("l", paste(rep('c', ncol(object@body)-1),collapse='')),
                  align.header=paste("l", paste(rep('c', ncol(object@body)-1),collapse='')),
                  rnames = FALSE,
                  header = header_temp,
                  cgroup = na.omit(cgroup),
                  n.cgroup = object@n.cgroup,
                  rgroup=object@rgroup,
                  n.rgroup=object@n.rgroup,
                  caption = object@caption,
                  tfoot = paste(object@footer,
                                " ",
                                print_abbrev(
                                    query_abbrev(
                                        c(
                                            as.character(
                                                object@body[,object@variable_col]),
                                            header_temp,
                                            unlist(object@rgroup_header),
                                            cgroup),
                                        object@abbreviations)
                                ),
                                sep = "")
              )              
              return(table_html)
          })

#' @title Add column group
#' @family table layout
#' @seealso \code{\link{prepare}} for making a layout object.
#' @description
#' \code{add_colgroup} Adds a layout objet to another layout object, as a new column group.
#' @param object A layout object including a body containing the table.
#' @param object2 A layout object including a body containing the table. The column used for matching is not added. 
#' @param by A character element with the name of the column specifying the column to use for joining (must be named identically in the layout object and the data frame containing the columns to be added).
#' @param position A character value 'left' or 'right' specifying where the new columns should be added. Any specification other than 'right' will be interpreted as 'left'.
#' @return A layout object with new column group added.
#' @examples
#' add_colgroup(example_layout, example_layout, position = 'left')
#' @export
#'
setGeneric('add_colgroup', # initialize funtion as a generic
           function(object,
                    object2,
                    ## by = '',
                    position = 'left'){
               standardGeneric('add_colgroup')
           })
setMethod('add_colgroup', # specify function in relation to object class
          signature(object='layout'),
          function(object,
                   object2,
                   ## by,
                   position){
              if(sum(names(object2@body) %in% names(object@body)) > 1){
                  warning("Duplicate column names detected. Please rename.")
              }
              if(object@variable_col[1]!=object2@variable_col[1]){
                  warning("Name of variable column not matching.")
              }              
              by <- object@variable_col[1]
              slot(object, "body") <- left_join(object@body, object2@body, by = by)
              if(position == 'left'){ # put right cols after var + left
                  object@col_order <- c(
                      object@col_order[1:(1+length(object@left_col))], # left
                      object2@col_order[-1], # not var
                      object@col_order[(2+length(object@left_col)):length(object@col_order)]
                  )
                  object@n.cgroup <-c(
                      object@n.cgroup[1:(1+length(object@left_col[1]))], #left
                      object2@n.cgroup[-1],
                      object@n.cgroup[
                      (2+length(object@left_col[1])):length(object@n.cgroup)])
                  object@cgroup_levels <- c(object2@left_col,
                                            object2@cgroup_levels,
                                            object@cgroup_levels
                                            )
              }else{
                  object@col_order <- c(
                      object@col_order[1:(1+length(object@left_col))], # left
                      object@col_order[(2+length(object@left_col)):length(object@col_order)],
                      object2@col_order[-1] # not var   
                  )
                  object@n.cgroup <-c(
                      object@n.cgroup[1:(1+length(object@left_col[1]))],
                      object@n.cgroup[
                      (2+length(object@left_col[1])):length(object@n.cgroup)],
                      object2@n.cgroup[-1]
                  )
                  object@cgroup_levels <- c(object@cgroup_levels,
                                            object2@left_col,
                                            object2@cgroup_levels)

              }
              slot(object, "body") <- object@body[, object@col_order]
              object@body <- object@body[, object@col_order]
              return(object)
          })
