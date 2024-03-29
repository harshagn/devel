#' @title Show contingency tables as HTML table
#' @name sjt.xtab
#' 
#' @description Shows contingency tables as HTML file in browser or viewer pane, or saves them as file.
#' 
#' @references \itemize{
#'              \item \url{http://rpubs.com/sjPlot/sjtxtab}
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              }
#'              
#' @seealso \code{\link{sjp.xtab}} \cr
#'          \code{\link{sjs.table.values}}
#' 
#' @param var.row Variable that should be displayed in the table rows.
#' @param var.col Variable that should be displayed in the table columns.
#' @param var.grp An optional grouping variable that splits the data into several groups,
#'          depending on the amount of categories. See examples for details.
#' @param weightBy A weight factor that will be applied to weight all cases.
#'          Default is \code{NULL}, so no weights are used.
#' @param digits The amount of digits used for the percentage values inside table cells.
#'          Default is 1.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param variableLabels A character vector of same length as supplied variables, with 
#'          the associated variable names. Following order is needed: name of \code{var.row},
#'          name of \code{var.col}, and - if \code{var.grp} is not \code{NULL} - name of \code{var.grp}.
#'          See examples for more details.
#'          variableLabels are detected automatically, if \code{var.row} or \code{var.col}
#'          have a \code{"variable.label"} attribute (see \code{\link{sji.setVariableLabels}}) for details).
#' @param valueLabels A list of character vectors that indicate the value labels of the supplied
#'          variables. Following order is needed: value labels of \code{var.row},
#'          value labels  of \code{var.col}, and - if \code{var.grp} is not \code{NULL} - 
#'          value labels  of \code{var.grp}. \code{valueLabels} needs to be a \code{\link{list}} object.
#'          See examples for more details.
#' @param breakVariableLabelsAt Wordwrap for variable labels. Determines how many chars of the variable labels are displayed in 
#'          one line and when a line break is inserted. Default is 40.
#' @param breakValueLabelsAt Wordwrap for value labels. Determines how many chars of the value labels are displayed in 
#'          one line and when a line break is inserted. Default is 20.
#' @param stringTotal String label for the total column / row header.
#' @param showCellPerc If \code{TRUE}, cell percentage values are shown.
#' @param showRowPerc If \code{TRUE}, row percentage values are shown.
#' @param showColPerc If \code{TRUE}, column percentage values are shown.
#' @param showExpected If \code{TRUE}, expected values are also shown.
#' @param showHorizontalLine If \code{TRUE}, data rows are separated with a horizontal line.
#' @param showSummary If \code{TRUE} (default), a summary row with Chi-square statistics (see \code{\link{chisq.test}}),
#'          Cramer's V or Phi-value etc. is shown. If a cell contains expected values lower than five (or lower than 10 
#'          if df is 1), the Fisher's excact test (see \code{\link{fisher.test}}) is computed instead of Chi-square test. 
#'          If the table's matrix is larger than 2x2, Fisher's excact test with Monte Carlo simulation is computed.
#' @param showLegend If \code{TRUE} (default), the color legend for coloring observed and expected
#'          values as well as cell, row and column percentages is shown. See \code{tdcol.n},
#'          \code{tdcol.expected}, \code{tdcol.cell}, \code{tdcol.row} and \code{tdcol.col}.
#' @param showNA If \code{TRUE}, \code{\link{NA}}'s (missing values) are also printed in the table.
#' @param labelNA The label for the missing column/row.
#' @param tdcol.n Color for highlighting count (observed) values in table cells. Default is black.
#' @param tdcol.expected Color for highlighting expected values in table cells. Default is cyan.
#' @param tdcol.cell Color for highlighting cell percentage values in table cells. Default is red.
#' @param tdcol.row Color for highlighting row percentage values in table cells. Default is blue.
#' @param tdcol.col Color for highlighting column percentage values in table cells. Default is green.
#' @param highlightTotal If \code{TRUE}, the total column and row will be highlighted with a
#'          different background color. See \code{highlightColor}.
#' @param highlightColor If \code{highlightTotal} is \code{TRUE}, this color value will be used
#'          for painting the background of the total column and row. Default is a light grey.
#' @param percSign The percentage sign that is printed in the table cells, in HTML-format.
#'          Default is \code{"&nbsp;\%"}, hence the percentage sign has a non-breaking-space after
#'          the percentage value.
#' @param hundret Default value that indicates the 100-percent column-sums (since rounding values
#'          may lead to non-exact results). Default is \code{"100.0"}.
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
#'            \item \code{css.lasttablerow='border-bottom: 1px dotted blue;'} for a blue dotted border of the last table row.
#'            \item \code{css.summary='+color:blue;'} to add blue font color style to the summary row.
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
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'          
#' @note The HTML tables can either be saved as file and manually opened (specify parameter \code{file}) or
#'         they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
#'         or opened with the default web browser. Displaying resp. opening a temporary file is the
#'         default behaviour (i.e. \code{file=NULL}). \cr \cr
#'         Since package version 1.3, the parameter \code{valueLabels}, which represent the 
#'         value labels, is retrieved automatically if a) the variables \code{var.col} and \code{var.row} come from a data frame
#'         that was imported with the \code{\link{sji.SPSS}} function (because then value labels are
#'         attached as attributes to the data) or b) when the variables are factors with named factor levels
#'         (e.g., see column \code{group} in dataset \code{\link{PlantGrowth}}). However, you still
#'         can use own parameters variable labels.
#'         
#' @examples 
#' # prepare sample data set
#' data(efc)
#' efc.labels <- sji.getValueLabels(efc)
#' 
#' # print simple cross table w/o labels
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep)}
#'          
#' # print cross table with labels and expected values
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep, 
#'          variableLabels=c("Elder's gender", "Elder's dependency"),
#'          valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
#'          showExpected=TRUE)}
#' 
#' # print minimal cross table with labels, total col/row highlighted
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep, 
#'          variableLabels=c("Elder's gender", "Elder's dependency"),
#'          valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
#'          showHorizontalLine=FALSE,
#'          showCellPerc=FALSE,
#'          highlightTotal=TRUE)}
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- sji.setVariableLabels(efc, sji.getVariableLabels(efc))
#' # print cross table with labels and all percentages
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep,
#'          showRowPerc=TRUE, showColPerc=TRUE)}
#' 
#' # print cross table with labels and all percentages, including
#' # grouping variable
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep, efc$c161sex, 
#'          variableLabels=c("Elder's gender", 
#'                           "Elder's dependency",
#'                           "Carer's gender"), 
#'          valueLabels=list(efc.labels[['e16sex']],
#'                           efc.labels[['e42dep']],
#'                           efc.labels[['c161sex']]),
#'          showRowPerc=TRUE, showColPerc=TRUE)}
#'
#' # ---------------------------------------------------------------- 
#' # User defined style sheet
#' # ---------------------------------------------------------------- 
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep, 
#'          variableLabels=c("Elder's gender", "Elder's dependency"),
#'          valueLabels=list(efc.labels[['e16sex']], efc.labels[['e42dep']]),
#'          CSS=list(css.table="border: 2px solid;",
#'                  css.tdata="border: 1px solid;",
#'                  css.horline="border-bottom: double blue;"))}
#'
#' @export
sjt.xtab <- function (var.row,
                      var.col,
                      var.grp=NULL,
                      weightBy=NULL,
                      digits=1,
                      file=NULL,
                      variableLabels=NULL,
                      valueLabels=NULL,
                      breakVariableLabelsAt=40,
                      breakValueLabelsAt=20,
                      stringTotal="Total",
                      showCellPerc=FALSE,
                      showRowPerc=FALSE,
                      showColPerc=FALSE,
                      showExpected=FALSE,
                      showHorizontalLine=FALSE,
                      showSummary=TRUE,
                      showLegend=TRUE,
                      showNA=FALSE,
                      labelNA="NA",
                      tdcol.n="black",
                      tdcol.expected="#339999",
                      tdcol.cell="#993333",
                      tdcol.row="#333399",
                      tdcol.col="#339933",
                      highlightTotal=FALSE,
                      highlightColor="#f8f8f8",
                      percSign="&nbsp;&#37;",
                      hundret="100.0",
                      encoding="UTF-8",
                      CSS=NULL,
                      useViewer=TRUE,
                      no.output=FALSE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(valueLabels)) {
    valueLabels <- list()
    # --------------------------------------------------------
    # row value labels
    # --------------------------------------------------------
    vl <- autoSetValueLabels(var.row)
    if (is.null(vl)) {
      vl <- sort(unique(na.omit(var.row)))
    }
    valueLabels[[1]] <- vl
    # --------------------------------------------------------
    # column value labels
    # --------------------------------------------------------
    vl <- autoSetValueLabels(var.col)
    if (is.null(vl)) {
      vl <- sort(unique(na.omit(var.col)))
    }
    valueLabels[[2]] <- vl
    # --------------------------------------------------------
    # group value labels
    # --------------------------------------------------------
    if (!is.null(var.grp)) {
      vl <- autoSetValueLabels(var.grp)
      if (is.null(vl)) {
        vl <- sort(unique(na.omit(var.grp)))
      }
      valueLabels[[3]] <- vl
    }
  }
  # -------------------------------------
  # list conversion needed here. in case value labels
  # of only one variable were detected, "valueLabels" is now
  # of type "character", thus length would differ from "valueLabels"'s
  # length if it were a list. needed below.
  # -------------------------------------
  if (!is.null(valueLabels) && !is.list(valueLabels)) valueLabels <- list(valueLabels)
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(variableLabels)) {
    variableLabels <- c()
    vn1 <- autoSetVariableLabels(var.row)
    vn2 <- autoSetVariableLabels(var.col)
    if (!is.null(vn1) && !is.null(vn2)) {
      variableLabels <- c(vn1, vn2)
    }
    if (!is.null(var.grp)) {
      vn3 <- autoSetVariableLabels(var.grp)
      if (!is.null(vn3)) {
        variableLabels <- c(variableLabels, vn3)
      }
    }
  }
  # -------------------------------------
  # init variable labels
  # -------------------------------------
  s.var.row <- s.var.col <- s.var.grp <- NULL
  if(!is.null(variableLabels)) {
    s.var.row <- ifelse(length(variableLabels)>0, variableLabels[1], "var.row")
    s.var.col <- ifelse(length(variableLabels)>1, variableLabels[2], "var.col")
    s.var.grp <- ifelse(length(variableLabels)>2, variableLabels[3], "var.grp")
  }
  else {
    s.var.row <- "var.row"
    s.var.col <- "var.col"
    s.var.grp <- "var.grp"
  }
  # check length of variable labels and split longer strings at into new lines
  if (!is.null(s.var.row)) s.var.row <- sju.wordwrap(s.var.row, breakVariableLabelsAt, "<br>")
  if (!is.null(s.var.col)) s.var.col <- sju.wordwrap(s.var.col, breakVariableLabelsAt, "<br>")
  if (!is.null(s.var.grp)) s.var.grp <- sju.wordwrap(s.var.grp, breakVariableLabelsAt, "<br>")
  # -------------------------------------
  # compute xtab
  # -------------------------------------
  # check if we have missings or not
  # -------------------------------------
  if (showNA) {
    # check if we have weights or not
    if (is.null(weightBy)) {
      # check if we have groupings or not
      if (is.null(var.grp)) {
        tab <- ftable(xtabs(~ addNA(var.row) + addNA(var.col)))
        coladd <- 3
      }
      else {
        tab <- ftable(xtabs(~ addNA(var.grp) + addNA(var.row) + addNA(var.col)))
        coladd <- 4
      }
    }
    else {
      # check if we have groupings or not
      if (is.null(var.grp)) {
        tab <- ftable(xtabs(weightBy ~ addNA(var.row) + addNA(var.col)))
        coladd <- 3
      }
      else {
        tab <- ftable(xtabs(weightBy ~ addNA(var.grp) + addNA(var.row) + addNA(var.col)))
        coladd <- 4
      }
    }
  }
  # -------------------------------------
  # no missings to show here
  # -------------------------------------
  else {
    # check if we have weights or not
    if (is.null(weightBy)) {
      # check if we have groupings or not
      if (is.null(var.grp)) {
        tab <- ftable(xtabs(~ var.row + var.col))
        coladd <- 2
      }
      else {
        tab <- ftable(xtabs(~ var.grp + var.row + var.col))
        coladd <- 3
      }
    }
    else {
      # check if we have groupings or not
      if (is.null(var.grp)) {
        tab <- ftable(xtabs(weightBy ~ var.row + var.col))
        coladd <- 2
      }
      else {
        tab <- ftable(xtabs(weightBy ~ var.grp + var.row + var.col))
        coladd <- 3
      }
    }
  }
  # -------------------------------------
  # compute table percentages
  # -------------------------------------
  tab.values <- sjs.table.values(tab, digits)
  tab.cell <- tab.values$cell
  tab.row <- tab.values$row
  tab.col <- tab.values$col
  tab.expected <- tab.values$expected
  # -------------------------------------
  # determine total number of columns
  # we have an optional column for the grouping variable,
  # a column for var.row labels and the columns for the
  # var.col data. Finally, we have a "total" column
  # -------------------------------------
  totalncol <- ncol(tab)+coladd
  # -------------------------------------
  # init value labels
  # -------------------------------------
  labels.var.row <- labels.var.grp <- labels.var.col <- NULL
  # -------------------------------------
  # check how many value labels have been supplied
  # and set value labels
  # -------------------------------------
  if (length(valueLabels)>0) {
    labels.var.row <- valueLabels[[1]]
  }
  else {
    labels.var.row <- seq_along(unique(na.omit(var.row)))
  }
  if (length(valueLabels)>1) {
    labels.var.col <- valueLabels[[2]]
  }
  else {
    labels.var.col <- seq_along(unique(na.omit(var.col)))
  }
  if (length(valueLabels)>2) {
    labels.var.grp <- valueLabels[[3]]
  }
  else {
    if (is.null(var.grp)) {
      labels.var.grp <- NULL
    }
    else {
      labels.var.grp <- seq_along(unique(na.omit(var.grp)))
    }
  }
  # ------------------------------------------
  # add label for missing colum
  # ------------------------------------------
  if (showNA) {
    labels.var.col <- c(labels.var.col, labelNA)
    labels.var.row <- c(labels.var.row, labelNA)
    if (!is.null(labels.var.grp)) labels.var.grp <- c(labels.var.grp, labelNA)
  }
  # check length of variable labels and split longer strings at into new lines
  if (!is.null(labels.var.row)) labels.var.row <- sju.wordwrap(labels.var.row, breakValueLabelsAt, "<br>")
  if (!is.null(labels.var.col)) labels.var.col <- sju.wordwrap(labels.var.col, breakValueLabelsAt, "<br>")
  if (!is.null(labels.var.grp)) labels.var.grp <- sju.wordwrap(labels.var.grp, breakValueLabelsAt, "<br>")
  # -------------------------------------
  # table init
  # -------------------------------------
  # init web page header
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.firstcolborder <- "firstcolborder"
  tag.secondtablerow <- "secondtablerow"
  tag.leftalign <- "leftalign"
  tag.centeralign <- "centeralign"
  tag.lasttablerow <- "lasttablerow"
  tag.totcol <- "totcol"
  tag.tothi <- "tothi"
  tag.summary <- "summary"
  tag.horline <- "horline"
  tag.td_ex <- "td_ex"
  tag.td_cl <- "td_cl"
  tag.td_rw <- "td_rw"
  tag.td_c <- "td_c"
  tag.td_n <- "td_n"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-top:double; text-align:center; font-style:italic; font-weight:normal;"
  css.tdata <- "padding:0.2cm;"
  css.firstcolborder <- "border-bottom:1px solid;"
  css.secondtablerow <- "border-bottom:1px solid; text-align:center;"
  css.leftalign <- "text-align:left; vertical-align:top;"
  css.centeralign <- "text-align:center;"
  css.lasttablerow <- ifelse(highlightTotal==TRUE, sprintf(" border-bottom:double; background-color:%s;", highlightColor), " border-bottom:double;")
  css.totcol <- ifelse(highlightTotal==TRUE, sprintf(" background-color:%s;", highlightColor), "")
  css.tothi <- "font-weight:bolder; font-style:italic;"
  css.summary <- "text-align:right; font-size:0.9em; font-style:italic;"
  css.horline <- ifelse(showHorizontalLine==TRUE, "border-bottom:1px solid;", "")
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']],1,1)=='+', paste0(css.table, substring(CSS[['css.table']],2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']],1,1)=='+', paste0(css.thead, substring(CSS[['css.thead']],2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']],1,1)=='+', paste0(css.tdata, substring(CSS[['css.tdata']],2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']],1,1)=='+', paste0(css.summary, substring(CSS[['css.summary']],2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- ifelse(substring(CSS[['css.leftalign']],1,1)=='+', paste0(css.leftalign, substring(CSS[['css.leftalign']],2)), CSS[['css.leftalign']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']],1,1)=='+', paste0(css.centeralign, substring(CSS[['css.centeralign']],2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- ifelse(substring(CSS[['css.lasttablerow']],1,1)=='+', paste0(css.lasttablerow, substring(CSS[['css.lasttablerow']],2)), CSS[['css.lasttablerow']])
    if (!is.null(CSS[['css.firstcolborder']])) css.firstcolborder <- ifelse(substring(CSS[['css.firstcolborder']],1,1)=='+', paste0(css.firstcolborder, substring(CSS[['css.firstcolborder']],2)), CSS[['css.firstcolborder']])
    if (!is.null(CSS[['css.secondtablerow']])) css.secondtablerow <- ifelse(substring(CSS[['css.secondtablerow']],1,1)=='+', paste0(css.secondtablerow, substring(CSS[['css.secondtablerow']],2)), CSS[['css.secondtablerow']])
    if (!is.null(CSS[['css.totcol']])) css.totcol <- ifelse(substring(CSS[['css.totcol']],1,1)=='+', paste0(css.totcol, substring(CSS[['css.totcol']],2)), CSS[['css.totcol']])
    if (!is.null(CSS[['css.tothi']])) css.tothi <- ifelse(substring(CSS[['css.tothi']],1,1)=='+', paste0(css.tothi, substring(CSS[['css.tothi']],2)), CSS[['css.tothi']])
    if (!is.null(CSS[['css.horline']])) css.horline <- ifelse(substring(CSS[['css.horline']],1,1)=='+', paste0(css.horline, substring(CSS[['css.horline']],2)), CSS[['css.horline']])
  }
  # -------------------------------------
  # set style sheet
  # -------------------------------------
  page.style <- sprintf("<style>\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>", 
                        tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata, tag.secondtablerow, css.secondtablerow, 
                        tag.leftalign, css.leftalign, tag.centeralign, css.centeralign, tag.lasttablerow, css.lasttablerow, 
                        tag.totcol, css.totcol, tag.tothi, css.tothi,
                        tag.td_n, tdcol.n, tag.td_c, tdcol.cell, tag.td_rw, tdcol.row,
                        tag.td_cl, tdcol.col, tag.td_ex, tdcol.expected, 
                        tag.summary, css.summary, tag.horline, css.horline,
                        tag.firstcolborder, css.firstcolborder)
  # start writing content
  toWrite <- paste(toWrite, page.style)
  toWrite <- paste(toWrite, "\n</head>\n<body>\n")
  # -------------------------------------
  # init first table row
  # -------------------------------------
  page.content <- "<table>\n"
  page.content <- paste(page.content, "  <tr>\n")
  # -------------------------------------
  # check whether we have additional grouping column
  # -------------------------------------
  if (!is.null(var.grp)) {
    page.content <- paste(page.content, sprintf("    <th class=\"thead firstcolborder\" rowspan=\"2\">%s</th>\n", s.var.grp))
  }
  # -------------------------------------
  # column with row-variable-name
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th class=\"thead firstcolborder\" rowspan=\"2\">%s</th>\n", s.var.row))
  # -------------------------------------
  # column with column-variable-name
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th class=\"thead\" colspan=\"%i\">%s</th>\n", length(labels.var.col), s.var.col))
  # -------------------------------------
  # total-column
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th class=\"thead tothi firstcolborder\" rowspan=\"2\">%s</th>\n", stringTotal))
  page.content <- paste(page.content, "  </tr>\n")
  # -------------------------------------
  # init second table row
  # -------------------------------------
  page.content <- paste(page.content, "\n  <tr>\n")
  # -------------------------------------
  # column variable labels
  # -------------------------------------
  for (i in seq_along(labels.var.col)) {
    page.content <- paste(page.content, sprintf("    <td class=\"secondtablerow tdata\">%s</td>\n", labels.var.col[i]))
  }
  page.content <- paste(page.content, "  </tr>\n")
  # -------------------------------------
  # table content
  # -------------------------------------
  # retrieve index colums of group var, if we have any
  # if we have a grouping variable, we need to know at
  # which row a new category of group starts
  # -------------------------------------
  if (is.null(var.grp)) {
    group.var.rows <- NULL
  }
  else {
    group.var.rows <- seq(1,nrow(tab), by=length(labels.var.row))
  }
  # -------------------------------------
  # if we have group vars, we need a repeating counter vor row value labels
  # -------------------------------------
  if (!is.null(group.var.rows)) {
    rowlabelcnt <- rep(1:length(labels.var.row), length(group.var.rows))
  }
  else {
    rowlabelcnt <- 1:length(labels.var.row)
  }
  # -------------------------------------
  # iterate all table data rows
  # -------------------------------------
  for (irow in 1:nrow(tab)) {
    # -------------------------------------
    # start new table row
    # -------------------------------------
    page.content <- paste(page.content, "\n  <tr>")
    # -------------------------------------
    # check for group var label, resp. if group var
    # starts with current row
    # -------------------------------------
    if (any(group.var.rows==irow)) {
      page.content <- paste(page.content, sprintf("\n    <td class=\"tdata leftalign\" rowspan=\"%i\">%s</td>", length(labels.var.row), labels.var.grp[which(group.var.rows==irow)]))
    }
    # -------------------------------------
    # set row variable label
    # -------------------------------------
    page.content <- paste(page.content, sprintf("\n    <td class=\"tdata leftalign\">%s</td>", labels.var.row[rowlabelcnt[irow]]))
    # -------------------------------------
    # iterate all data columns
    # -------------------------------------
    for (icol in 1:ncol(tab)) {
      # -------------------------------------
      # first table cell data contains observed values
      # -------------------------------------
      cellstring <- sprintf("<span class=\"td_n\">%i</span>", tab[irow,icol])
      # -------------------------------------
      # if we have expected values, add them to table cell
      # -------------------------------------
      if (showExpected) {
        cellstring <- paste(cellstring, sprintf("<br><span class=\"td_ex\">%s</span>", tab.expected[irow,icol]), sep="")
      }
      # -------------------------------------
      # if we have row-percentage, add percentage value to table cell
      # -------------------------------------
      if (showRowPerc) {
        cellstring <- paste(cellstring, sprintf("<br><span class=\"td_rw\">%s%s</span>", tab.row[irow,icol],percSign), sep="")
      }
      # -------------------------------------
      # if we have col-percentage, add percentage value to table cell
      # -------------------------------------
      if (showColPerc) {
        cellstring <- paste(cellstring, sprintf("<br><span class=\"td_cl\">%s%s</span>", tab.col[irow,icol], percSign), sep="")
      }
      # -------------------------------------
      # if we have cell-percentage, add percentage value to table cell
      # -------------------------------------
      if (showCellPerc) {
        cellstring <- paste(cellstring, sprintf("<br><span class=\"td_c\">%s%s</span>", tab.cell[irow,icol], percSign), sep="")
      }
      # -------------------------------------
      # write table cell data
      # -------------------------------------
      page.content <- paste(page.content, sprintf("\n    <td class=\"tdata centeralign horline\">%s</td>", cellstring), sep="")
    }
    # -------------------------------------
    # after all data columns have been printed,
    # add a total column
    # -------------------------------------
    # first table cell data contains observed values
    # -------------------------------------
    cellstring <- sprintf("<span class=\"td_n\">%i</span>", rowSums(tab)[irow])
    # if we have expected values, add them to table cell
    if (showExpected) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_ex\">%s</span>", rowSums(tab.expected)[irow]), sep="")
    }
    # if we have row-percentage, add percentage value to table cell
    if (showRowPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_rw\">%s%s</span>", hundret, percSign), sep="")
    }
    # if we have col-percentage, add percentage value to table cell
    if (showColPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_cl\">%s%s</span>", rowSums(tab.cell)[irow], percSign), sep="")
    }
    # if we have cell-percentage, add percentage value to table cell
    if (showCellPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_c\">%s%s</span>", rowSums(tab.cell)[irow], percSign), sep="")
    }
    # write table cell data
    page.content <- paste(page.content, sprintf("\n    <td class=\"tdata centeralign totcol horline\">%s</td>", cellstring), sep="")
    # close table row
    page.content <- paste(page.content, "\n  </tr>\n")
  }
  # ------------------------------
  # start new table row
  # this row contains the total row with sums for all columns
  # ------------------------------
  page.content <- paste(page.content, "\n  <tr>\n    ", sep="")
  # check whether we have group-var, and if not, apply colspan
  if (!is.null(var.grp)) {
    page.content <- paste(page.content, sprintf("<td class=\"tdata lasttablerow leftalign tothi\" colspan=\"2\">%s</td>", stringTotal), sep="")
  }
  else {
    page.content <- paste(page.content, sprintf("<td class=\"tdata lasttablerow leftalign tothi\">%s</td>", stringTotal), sep="")
  }
  # --------------------------
  # iterate all data columns
  # --------------------------
  for (icol in 1:ncol(tab)) {
    # -------------------------------------
    # add total row, first table cell data contains observed values
    # -------------------------------------
    cellstring <- sprintf("<span class=\"td_n\">%i</span>", colSums(tab)[icol])
    # calculate total percentage value
    cellpercval <- round(100*colSums(tab)[icol]/sum(tab),digits)
    # if we have expected values, add them to table cell
    if (showExpected) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_ex\">%s</span>", colSums(tab.expected)[icol]), sep="")
    }
    # if we have row-percentage, add percentage value to table cell
    if (showRowPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_rw\">%s%s</span>", cellpercval, percSign), sep="")
    }
    # if we have col-percentage, add percentage value to table cell
    if (showColPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_cl\">%s%s</span>", hundret, percSign), sep="")
    }
    # if we have cell-percentage, add percentage value to table cell
    if (showCellPerc) {
      cellstring <- paste(cellstring, sprintf("<br><span class=\"td_c\">%s%s</span>", cellpercval, percSign), sep="")
    }
    page.content <- paste(page.content, sprintf("\n    <td class=\"tdata lasttablerow centeralign\">%s</td>", cellstring), sep="")
  }
  # --------------------------
  # the lower right table cell contains the complete
  # total values, i.e. all percentages are 100%
  # --------------------------
  cellstring <- sprintf("%s", sum(tab))
  if (showExpected) cellstring <- paste(cellstring, sprintf("<br>%s", sum(tab.expected)), sep="")
  if (showColPerc) cellstring <- paste(cellstring, sprintf("<br>%s%s", hundret, percSign), sep="")
  if (showRowPerc) cellstring <- paste(cellstring, sprintf("<br>%s%s", hundret, percSign), sep="")
  if (showCellPerc) cellstring <- paste(cellstring, sprintf("<br>%s%s", hundret, percSign), sep="")
  # write table cell data
  page.content <- paste(page.content, sprintf("\n    <td class=\"tdata lasttablerow centeralign\">%s</td>", cellstring), sep="")
  # close table row
  page.content <- paste(page.content, "\n  </tr>\n")
  # -------------------------------------
  # table summary
  # -------------------------------------
  if (showSummary) {
    # start new table row
    page.content <- paste(page.content, "\n  <tr>\n    ", sep="")
    # calculate chi square value
    chsq <- chisq.test(tab)
    fish <- NULL
    # check whether variables are dichotome or if they have more
    # than two categories. if they have more, use Cramer's V to calculate
    # the contingency coefficient
    if (nrow(tab)>2 || ncol(tab)>2) {
      kook <- sprintf("&Phi;<sub>c</sub>=%.3f", sjs.cramer(tab))
      # if minimum expected values below 5, compute fisher's exact test
      if(min(tab.expected)<5 || (min(tab.expected)<10 && chsq$parameter==1)) fish <- fisher.test(tab, simulate.p.value=TRUE)
    }
    else {
      kook <- sprintf("&Phi;=%.3f", sjs.phi(tab))
      # if minimum expected values below 5 and df=1, compute fisher's exact test
      if(min(tab.expected)<5 || (min(tab.expected)<10 && chsq$parameter==1)) fish <- fisher.test(tab)
    }
    # create summary row
    if (is.null(fish)) {
      page.content <- paste(page.content, sprintf("    <td class=\"summary tdata\" colspan=\"%i\">&Chi;<sup>2</sup>=%.3f &middot; df=%i &middot; %s &middot; p=%.3f</td>", totalncol, chsq$statistic, chsq$parameter, kook, chsq$p.value), sep="")
    }
    else {
      page.content <- paste(page.content, sprintf("    <td class=\"summary tdata\" colspan=\"%i\">Fisher's p=%.3f &middot; df=%i &middot; %s</td>", totalncol, fish$p.value, chsq$parameter, kook), sep="")
    }
    # close table row
    page.content <- paste(page.content, "\n  </tr>\n")
  }  
  # -------------------------------------
  # finish table
  # -------------------------------------
  page.content <- paste(page.content, "\n</table>")
  # -------------------------------------
  # print legend
  # -------------------------------------
  if (showLegend) page.content <- paste(page.content, sprintf("<p>\n  <span class=\"td_n\">observed values</span> &middot; \n  <span class=\"td_ex\">expected values</span> &middot; \n  <span class=\"td_rw\">&#37; within %s</span> &middot; \n  <span class=\"td_cl\">&#37; within %s</span> &middot; \n  <span class=\"td_c\">&#37; of total</span>\n</p>", gsub("<br>", " ", s.var.row), gsub("<br>", " ", s.var.col)), "\n")
  # -------------------------------------
  # add table to return value list, so user can access each
  # single frequency table
  # -------------------------------------
  toWrite <- paste(toWrite, page.content, "\n")
  # -------------------------------------
  # finish html page
  # -------------------------------------
  toWrite <- paste0(toWrite, "</body></html>")
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
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr)
  knitr <- gsub(tag.thead, css.thead, knitr)
  knitr <- gsub(tag.secondtablerow, css.secondtablerow, knitr)
  knitr <- gsub(tag.firstcolborder, css.firstcolborder, knitr)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr)  
  knitr <- gsub(tag.centeralign, css.centeralign, knitr)  
  knitr <- gsub(tag.horline, css.horline, knitr)  
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr)  
  knitr <- gsub(tag.totcol, css.totcol, knitr)  
  knitr <- gsub(tag.summary, css.summary, knitr)  
  knitr <- gsub(tag.tothi, css.tothi, knitr)  
  # -------------------------------------
  # replace color-attributes for legend
  # -------------------------------------
  knitr <- gsub(tag.td_ex, sprintf("color:%s;",tdcol.expected), knitr)  
  knitr <- gsub(tag.td_cl, sprintf("color:%s;",tdcol.col), knitr)  
  knitr <- gsub(tag.td_rw, sprintf("color:%s;",tdcol.row), knitr)  
  knitr <- gsub(tag.td_c, sprintf("color:%s;",tdcol.cell), knitr)  
  knitr <- gsub(tag.td_n, sprintf("color:%s;",tdcol.n), knitr)  
  # -------------------------------------
  # check if html-content should be printed
  # -------------------------------------
  if (!no.output) {
    # -------------------------------------
    # check if we have filename specified
    # -------------------------------------
    if (!is.null(file)) {
      # write file
      write(knitr, file=file)
    }
    # -------------------------------------
    # else open in viewer pane
    # -------------------------------------
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
  invisible (structure(class = "sjtxtab",
                       list(page.style = page.style,
                            page.content = page.content,
                            output.complete = toWrite,
                            knitr = knitr)))
}
