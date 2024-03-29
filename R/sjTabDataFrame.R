#' @title Show (description of) data frame as HTML table
#' @name sjt.df
#' 
#' @seealso \code{\link{sji.viewSPSS}}
#' 
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2014/03/04/beautiful-table-outputs-in-r-part-2-rstats-sjplot/}
#'              }
#'              
#' @description Shows description or the content of data frame (rows and columns) as HTML table,
#'                or saves it as file. Helpful if you want a quick overview of a data frame's 
#'                content. See parameter \code{describe} for details. By default, \code{describe} 
#'                is \code{TRUE} and a description of the data frame is given,
#'                using the \code{\link{describe}} function of the \code{psych} package.
#'
#' @param df A data frame that should be printed.
#' @param describe If \code{TRUE} (default), a description of the data frame's variables is given.
#'          The description is retrieved from the \code{\link{describe}} function of the \code{\link{psych}}
#'          package. If this parameter is \code{FALSE}, the data frame's content (values) is shown.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param alternateRowColors If \code{TRUE}, alternating rows are highlighted with a light gray
#'          background color.
#' @param orderColumn Indicates a column, either by column name or by column index number,
#'          that should be orderd. Default order is ascending, which can be changed with
#'          \code{orderAscending} parameter. Default is \code{NULL}, hence the data frame
#'          is printed with no specific order. See examples for further details.
#' @param orderAscending If \code{TRUE} (default) and \code{orderColumn} is not \code{NULL},
#'          data frame is ordered according to the specified column in an ascending order.
#'          Use \code{FALSE} to apply descending order. See examples for further details.
#' @param title A table caption. By default, \code{title} is \code{NULL}, hence no title will be used.
#' @param stringVariable A string used for the first column name. Default is \code{"Variable"}.
#' @param repeatHeader If \code{TRUE}, the header row will also be added at the bottom at the table. This might
#'          be helpful, if you have longer tables and want to see the column names at the end of the table as well.
#' @param showType If \code{TRUE}, the variable type is shown in a separate row below the column
#'          names.
#' @param showRowNames If \code{TRUE} and \code{describe} is \code{false}, first table column contains row names
#'          of data frame. Use \code{showRowNames=FALSE} to omit first table column with row names.
#' @param showCommentRow If \code{TRUE}, an optional comment line can be added to the end / below
#'          the table. Use \code{commentString} to specify the comment.
#' @param commentString A string that will be added to the end / below the table. Only
#'          applies, if \code{showCommentRow} is \code{TRUE}.
#' @param hideProgressBar If \code{TRUE}, the progress bar that is displayed when creating the
#'          table is hidden. Default in \code{FALSE}, hence the bar is visible.
#' @param encoding The charset encoding used for variable and value labels. Default is \code{"UTF-8"}. Change
#'          encoding if specific chars are not properly displayed (e.g.) German umlauts).
#' @param CSS A \code{\link{list}} with user-defined style-sheet-definitions, according to the official CSS syntax (see
#'          \url{http://www.w3.org/Style/CSS/}). See return value \code{page.style} for details
#'          of all style-sheet-classnames that are used in this function. Parameters for this list need:
#'          \enumerate{
#'            \item the class-names with \code{"css."}-prefix as parameter name and
#'            \item each style-definition must end with a semicolon
#'          } 
#'          You can add style information to the default styles by using a + (plus-sign) as
#'          initial character for the parameter attributes. Examples:
#'          \itemize{
#'            \item \code{css.table='border:2px solid red;'} for a solid 2-pixel table border in red.
#'            \item \code{css.summary='font-weight:bold;'} for a bold fontweight in the summary row.
#'            \item \code{css.arc='color:blue;'} for a blue text color each 2nd row.
#'            \item \code{css.caption='+color:red;'} to add red font-color to the default table caption style.
#'          }
#'          See further examples below and \url{http://rpubs.com/sjPlot/sjtbasics}.
#' @param useViewer If \code{TRUE}, the function tries to show the HTML table in the IDE's viewer pane. If
#'          \code{FALSE} or no viewer available, the HTML table is opened in a web browser.
#' @param no.output If \code{TRUE}, the html-output is neither opened in a browser nor shown in
#'          the viewer pane and not even saved to file. This option is useful when the html output
#'          should be used in \code{knitr} documents. The html output can be accessed via the return
#'          value.
#' @return Invisibly returns a \code{\link{structure}} with
#'          \itemize{
#'            \item the data frame with the description information (\code{data}),
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @examples
#' # init dataset
#' data(efc)
#' 
#' # plot efc-data frame summary
#' \dontrun{
#' sjt.df(efc, alternateRowColors=TRUE)}
#' 
#' # plot content, first 50 rows of first 5 columns of example data set
#' \dontrun{
#' sjt.df(efc[1:50,1:5], describe=FALSE, stringVariable="Observation")}
#' 
#' # plot efc-data frame summary, ordered descending by mean-column
#' \dontrun{
#' sjt.df(efc, orderColumn="mean", orderAscending=FALSE)}
#' 
#' # plot first 20 rows of first 5 columns of example data set,
#' # ordered by column "e42dep" with alternating row colors
#' \dontrun{
#' sjt.df(efc[1:20,1:5], alternateRowColors=TRUE, 
#'        orderColumn="e42dep", describe=FALSE)}
#' 
#' # plot first 20 rows of first 5 columns of example data set,
#' # ordered by 4th column in descending order.
#' \dontrun{
#' sjt.df(efc[1:20,1:5], orderColumn=4, orderAscending=FALSE, describe=FALSE)}
#' 
#' # ---------------------------------------------------------------- 
#' # User defined style sheet
#' # ---------------------------------------------------------------- 
#' \dontrun{
#' sjt.df(efc,
#'        alternateRowColor=TRUE,
#'        CSS=list(css.table="border: 2px solid #999999;",
#'                 css.tdata="border-top: 1px solid;",
#'                 css.arc="color:blue;"))}
#'
#' @export
sjt.df <- function (df,
                    describe=TRUE,
                    file=NULL,
                    alternateRowColors=FALSE,
                    orderColumn=NULL,
                    orderAscending=TRUE,
                    title=NULL,
                    repeatHeader=FALSE,
                    stringVariable="Variable",
                    showType=FALSE,
                    showRowNames=TRUE,
                    showCommentRow=FALSE,
                    commentString="No comment...",
                    hideProgressBar=FALSE,
                    encoding="UTF-8",
                    CSS=NULL,
                    useViewer=TRUE,
                    no.output=FALSE) {
  # -------------------------------------
  # make data frame of single variable, so we have
  # unique handling for the data
  # -------------------------------------
  if (!is.data.frame(df)) {
    stop("Parameter needs to be a data frame!", call.=FALSE)
  }
  # -------------------------------------
  # Description?
  # -------------------------------------
  if (describe) {
    # also include missings
    missings <- apply(df, 2, function(x) sum(is.na(x)))
    # and proportion of missings
    missings.percentage <- round(100*missings/nrow(df),2)
    df <- round(describe(df),2)
    # insert missing variables in data frame
    df <- data.frame(df[,1:2], missings, missings.percentage, df[,3:ncol(df)])
    # proper column name
    colnames(df)[4] <- "missings (percentage)"
  }
  # -------------------------------------
  # Order data set if requested
  # -------------------------------------
  if (!is.null(orderColumn)) {
    # check whether orderColumn is numeric or character
    if (is.character(orderColumn)) {
      # retrieve column that equals orderColumn string
      nr <- which(colnames(df)==orderColumn)
      orderColumn <- as.numeric(nr)
    }
    # check for correct range
    if (is.numeric(orderColumn) && orderColumn>0 && orderColumn<=ncol(df)) {
      # retrieve order
      rfolge <- order(df[,orderColumn])
      # reverse order?
      if (!orderAscending) {
        rfolge <- rev(rfolge)
      }
      # sort dataframe
      df <- df[rfolge,]
    }
  }
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.caption <- "caption"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.arc <- "arc"
  tag.comment <- "comment"
  tag.firsttablerow <- "firsttablerow"
  tag.lasttablerow <- "lasttablerow"
  tag.firsttablecol <- "firsttablecol"
  tag.leftalign <- "leftalign"
  tag.centertalign <- "centertalign"
  css.table <- "border-collapse:collapse; border:none;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.thead <- "border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm;"
  css.tdata <- "padding:0.2cm; text-align:left; vertical-align:top;"
  css.arc <- "background-color:#eaeaea;"
  css.lasttablerow <- "border-top:1px solid; border-bottom: double;"
  css.firsttablerow <- "border-bottom:1px solid black;"
  css.firsttablecol <- ""
  css.leftalign <- "text-align:left;"
  css.centertalign <- "text-align:center;"
  css.comment <- "font-style:italic; border-top:double black; text-align:right;"
  if (showCommentRow && repeatHeader) css.comment <- "font-style:italic; text-align:right;"
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']],1,1)=='+', paste0(css.table, substring(CSS[['css.table']],2)), CSS[['css.table']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']],1,1)=='+', paste0(css.caption, substring(CSS[['css.caption']],2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']],1,1)=='+', paste0(css.thead, substring(CSS[['css.thead']],2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']],1,1)=='+', paste0(css.tdata, substring(CSS[['css.tdata']],2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']],1,1)=='+', paste0(css.arc, substring(CSS[['css.arc']],2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- ifelse(substring(CSS[['css.lasttablerow']],1,1)=='+', paste0(css.lasttablerow, substring(CSS[['css.lasttablerow']],2)), CSS[['css.lasttablerow']])
    if (!is.null(CSS[['css.firsttablerow']])) css.firsttablerow <- ifelse(substring(CSS[['css.firsttablerow']],1,1)=='+', paste0(css.firsttablerow, substring(CSS[['css.firsttablerow']],2)), CSS[['css.firsttablerow']])
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- ifelse(substring(CSS[['css.leftalign']],1,1)=='+', paste0(css.leftalign, substring(CSS[['css.leftalign']],2)), CSS[['css.leftalign']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']],1,1)=='+', paste0(css.centeralign, substring(CSS[['css.centeralign']],2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- ifelse(substring(CSS[['css.firsttablecol']],1,1)=='+', paste0(css.firsttablecol, substring(CSS[['css.firsttablecol']],2)), CSS[['css.firsttablecol']])
    if (!is.null(CSS[['css.comment']])) css.comment <- ifelse(substring(CSS[['css.comment']],1,1)=='+', paste0(css.comment, substring(CSS[['css.comment']],2)), CSS[['css.comment']])
  }
  # -------------------------------------
  # set style sheet
  # -------------------------------------
  page.style <- sprintf("<style>\n%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                        tag.table, css.table, tag.caption, css.caption,
                        tag.thead, css.thead, tag.tdata, css.tdata, tag.arc, css.arc,
                        tag.lasttablerow, css.lasttablerow, tag.firsttablerow, css.firsttablerow,
                        tag.leftalign, css.leftalign, tag.centertalign, css.centertalign,
                        tag.firsttablecol, css.firsttablecol, tag.comment, css.comment)
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n%s\n</head>\n<body>\n", encoding, page.style)
  # -------------------------------------
  # get row and column count of data frame
  # -------------------------------------
  rowcnt <- nrow(df)
  colcnt <- ncol(df)
  # -------------------------------------
  # get row and column names of data frame
  # -------------------------------------
  rnames <- rownames(df)
  cnames <- colnames(df)
  # -------------------------------------
  # start table tag
  # -------------------------------------
  page.content <- "<table>\n"
  # -------------------------------------
  # table caption, variable label
  # -------------------------------------
  if (!is.null(title)) page.content <- paste0(page.content, sprintf("  <caption>%s</caption>\n", title))
  # -------------------------------------
  # header row
  # -------------------------------------
  page.content <- paste0(page.content, "  <tr>\n")
  # first columns are rownames
  if (showRowNames) page.content <- paste0(page.content, sprintf("    <th class=\"thead firsttablerow firsttablecol\">%s</th>\n", stringVariable))
  for (i in 1:colcnt) {
    # check variable type
    vartype <- c("unknown type")
    if (is.character(df[,i])) vartype <- c("character")
    else if (is.factor(df[,i])) vartype <- c("factor")
    else if (is.numeric(df[,i])) vartype <- c("numeric")
    else if (is.atomic(df[,i])) vartype <- c("atomic")
    # column names and variable as table headline
    page.content <- paste0(page.content, sprintf("    <th class=\"thead firsttablerow\">%s", cnames[i]))
    if (showType) page.content <- paste0(page.content, sprintf("<br>(%s)", vartype))
    page.content <- paste0(page.content, "</th>\n")
  }
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  if (!hideProgressBar) pb <- txtProgressBar(min=0, max=rowcnt, style=3)
  # -------------------------------------
  # subsequent rows
  # -------------------------------------
  for (rcnt in 1:rowcnt) {
    # default row string
    arcstring <- ""
    # if we have alternating row colors, set css
    if (alternateRowColors) arcstring <- ifelse(rcnt %% 2 ==0, " arc", "")
    page.content <- paste0(page.content, "  <tr>\n")
    # first table cell is rowname
    if (showRowNames) page.content <- paste0(page.content, sprintf("    <td class=\"tdata leftalign firsttablecol%s\">%s</td>\n", arcstring, rnames[rcnt]))
    # all columns of a row
    for (ccnt in 1:colcnt) {
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata centertalign%s\">%s</td>\n", arcstring, df[rcnt,ccnt]))
    }
    # update progress bar
    if (!hideProgressBar) setTxtProgressBar(pb, rcnt)
    # close row tag
    page.content <- paste0(page.content, "</tr>\n")
  }
  if (!hideProgressBar) close(pb)
  # -------------------------------------
  # repeat header row?
  # -------------------------------------
  if (repeatHeader) {
    page.content <- paste0(page.content, "  <tr>\n")
    if (showRowNames) page.content <- paste0(page.content, sprintf("    <th class=\"thead lasttablerow firsttablecol\">%s</th>\n", stringVariable))
    for (i in 1:colcnt) {
      # check variable type
      vartype <- c("unknown type")
      if (is.character(df[,i])) vartype <- c("character")
      else if (is.factor(df[,i])) vartype <- c("factor")
      else if (is.numeric(df[,i])) vartype <- c("numeric")
      else if (is.atomic(df[,i])) vartype <- c("atomic")
      # column names and variable as table headline
      page.content <- paste0(page.content, sprintf("    <th class=\"thead lasttablerow\">%s", cnames[i]))
      if (showType) page.content <- paste0(page.content, sprintf("<br>(%s)", vartype))
      page.content <- paste0(page.content, "</th>\n")
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # add optional "comment" row
  # -------------------------------------
  if (showCommentRow) {
    page.content <- paste0(page.content, "  <tr>\n")
    if (!showRowNames) colcnt <- colcnt-1
    page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"comment\">%s</td>\n", colcnt+1, commentString))
    # close row tag
    page.content <- paste0(page.content, "</tr>\n")
  }
  # -------------------------------------
  # finish html page
  # -------------------------------------
  page.content <- paste0(page.content, "</table>\n")
  toWrite <- paste0(toWrite, sprintf("%s\n</body></html>", page.content))
  # -------------------------------------
  # replace class attributes with inline style,
  # useful for knitr
  # -------------------------------------
  # copy page content
  # -------------------------------------
  knitr <- page.content
  # -------------------------------------
  # set style attributes for main table tags
  # -------------------------------------
  knitr <- gsub("class=", "style=", knitr)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr)
  knitr <- gsub("<caption", sprintf("<caption style=\"%s\"", css.caption), knitr)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr)
  knitr <- gsub(tag.thead, css.thead, knitr)
  knitr <- gsub(tag.arc, css.arc, knitr)
  knitr <- gsub(tag.comment, css.comment, knitr)
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr)
  knitr <- gsub(tag.firsttablerow, css.firsttablerow, knitr)
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr)
  knitr <- gsub(tag.centertalign, css.centertalign, knitr)
  # -------------------------------------
  # check if html-content should be outputted
  # -------------------------------------
  if (!no.output) {
    # -------------------------------------
    # check if we have filename specified
    # -------------------------------------
    if (!is.null(file)) {
      # write file
      write(knitr, file=file)
    }
    else {
      # else create and browse temporary file
      htmlFile <- tempfile(fileext=".html")
      write(toWrite, file=htmlFile)
      # check whether we have RStudio Viewer
      viewer <- getOption("viewer")
      if (useViewer && !is.null(viewer)) {
        viewer(htmlFile)
      }
      else {
        utils::browseURL(htmlFile)    
      }
      # delete temp file
      # unlink(htmlFile)
    }
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjtdf",
                       list(data = df,
                            page.style = page.style,
                            page.content = page.content,
                            output.complete = toWrite,
                            knitr = knitr)))
}
                     