#' @title Show principal component analysis as HTML table
#' @name sjt.pca
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2014/03/04/beautiful-table-outputs-in-r-part-2-rstats-sjplot/}
#'              }
#' 
#' @description Performes a principle component analysis on a data frame or matrix and
#'                displays the factor solution as HTML table, or saves them as file. 
#'                \cr \cr In case a data frame is used as 
#'                parameter, the Cronbach's Alpha value for each factor scale will be calculated,
#'                i.e. all variables with the highest loading for a factor are taken for the
#'                reliability test. The result is an alpha value for each factor dimension.
#' 
#' @seealso \code{\link{sjp.pca}} \cr
#'          \code{\link{sjs.reliability}} \cr
#'          \code{\link{sjt.itemanalysis}} \cr
#'          \code{\link{sjs.cronbach}}
#' 
#' @param data A data frame with factors (each columns one variable) that should be used 
#'          to compute a PCA, or a \code{\link{prcomp}} object.
#' @param numberOfFactors A predefined number of factors to use for the calculating the varimax
#'          rotation. By default, this value is \code{NULL} and the amount of factors is
#'          calculated according to the Kaiser-criteria. See paramater \code{plotEigenvalues}.
#' @param factorLoadingTolerance Specifies the minimum difference a variable needs to have between
#'          factor loadings (components) in order to indicate a clear loading on just one factor and not
#'          diffusing over all factors. For instance, a variable with 0.8, 0.82 and 0.84 factor loading 
#'          on 3 possible factors can not be clearly assigned to just one factor and thus would be removed
#'          from the principal component analysis. By default, the minimum difference of loading values
#'          between the highest and 2nd highest factor should be 0.1
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param varlabels The item labels that are printed in the first column. If no item labels are
#'          provided (default), the data frame's column names are used. Item labels must
#'          be a string vector, e.g.: \code{varlabels=c("Var 1", "Var 2", "Var 3")}.
#' @param title A table caption. By default, \code{"Principal Component Analysis (with varimax rotation)"}
#'          is used as the table's title.
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the variable labels are displayed in 
#'          one line and when a line break is inserted. Default is 20.
#' @param digits The amount of digits used the values inside table cells.
#'          Default is 2.
#' @param showCronbachsAlpha If \code{TRUE} (default), the cronbach's alpha value for each factor scale will be calculated,
#'          i.e. all variables with the highest loading for a factor are taken for the
#'          reliability test. The result is an alpha value for each factor dimension.
#'          Only applies when \code{data} is a data frame and no \code{\link{prcomp}} object.
#' @param showMSA If \code{TRUE}, shows an additional column with the measure of sampling adequacy according
#'          dor each component.
#' @param showVariance If \code{TRUE}, the proportions of variances for each component as well as cumulative
#'          variance are shown in the table footer.
#' @param alternateRowColors If \code{TRUE}, alternating rows are highlighted with a light gray
#'          background color.
#' @param stringPov The string for the table row that contains the proportions of variances. By default, 
#'          \code{"Proportion of Variance"} will be used.
#' @param stringCpov The string for the table row that contains the cumulative variances. By default, 
#'          \code{"Cumulative Proportion"} will be used.
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
#'            \item \code{css.cronbach='+color:green;'} to add green color formatting to the Cronbach's Alpha value.
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
#'            \item the complete html-output (\code{output.complete}),
#'            \item the html-table with inline-css for use with knitr (\code{knitr}),
#'            \item the \code{factor.index}, i.e. the column index of each variable with the highest factor loading for each factor and
#'            \item the \code{removed.items}, i.e. which variables have been removed because they were outside of the \code{factorLoadingTolerance}'s range.
#'            }
#'            for further use.
#'
#' @note The HTML tables can either be saved as file and manually opened (specify parameter \code{file}) or
#'         they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
#'         or opened with the default web browser. Displaying resp. opening a temporary file is the
#'         default behaviour (i.e. \code{file=NULL}). \cr \cr
#'         This PCA uses the \code{\link{prcomp}} function and the \code{\link{varimax}} rotation.
#' 
#' @examples
#' # randomly create data frame with 7 items, each consisting of 4 categories
#' likert_4 <- data.frame(sample(1:4, 500, replace=TRUE, prob=c(0.2,0.3,0.1,0.4)),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.5,0.25,0.15,0.1)),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.4,0.15,0.25,0.2)),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.25,0.1,0.4,0.25)),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.1,0.4,0.4,0.1)),
#'                        sample(1:4, 500, replace=TRUE),
#'                        sample(1:4, 500, replace=TRUE, prob=c(0.35,0.25,0.15,0.25)))
#'
#' # Create variable labels
#' colnames(likert_4) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")
#' 
#' # show table
#' \dontrun{
#' sjt.pca(likert_4)}
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' data(efc)
#' 
#' # retrieve variable and value labels
#' varlabs <- sji.getVariableLabels(efc)
#' 
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc)=="c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc)=="c90cop9")
#'  
#' # create data frame with COPE-index scale
#' df <- as.data.frame(efc[,c(start:end)])
#' colnames(df) <- varlabs[c(start:end)]
#' 
#' \dontrun{
#' sjt.pca(df)}
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- sji.setVariableLabels(efc, varlabs)
#' \dontrun{
#' sjt.pca(efc[,c(start:end)])}
#' 
#' @importFrom psych KMO
#' @export
sjt.pca <- function (data,
                     numberOfFactors=NULL,
                     factorLoadingTolerance=0.1,
                     file=NULL, 
                     varlabels=NULL,
                     title="Principal Component Analysis (with varimax rotation)",
                     breakLabelsAt=40,
                     digits=2,
                     showCronbachsAlpha=TRUE,
                     showMSA=FALSE,
                     showVariance=FALSE,
                     alternateRowColors=FALSE,
                     stringPov="Proportion of Variance",
                     stringCpov="Cumulative Proportion",
                     encoding="UTF-8",
                     CSS=NULL,
                     useViewer=TRUE,
                     no.output=FALSE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(varlabels) && is.data.frame(data)) {
    # if yes, iterate each variable
    for (i in 1:ncol(data)) {
      # retrieve variable name attribute
      vn <- autoSetVariableLabels(data[,i])
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        varlabels <- c(varlabels, vn)
      }
      else {
        # else break out of loop
        varlabels <- NULL
        break
      }
    }
  }
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (class(data)=="prcomp") {
    pcadata <- data
    dataframeparam <- FALSE
    showMSA <- FALSE
  }
  else {
    pcadata <- prcomp(na.omit(data), retx=TRUE, center=TRUE, scale.=TRUE)
    dataframeparam <- TRUE
  }
  # -------------------------------------
  # init header
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.caption <- "caption"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.centeralign <- "centeralign"
  tag.cronbach <- "cronbach"  
  tag.msa <- "msa"  
  tag.pov <- "pov"  
  tag.cpov <- "cpov"  
  tag.kmo <- "kmo"  
  tag.arc <- "arc"
  tag.minval <- "minval"
  tag.removable <- "removable"
  tag.firsttablerow <- "firsttablerow"
  tag.firsttablecol <- "firsttablecol"
  css.table <- "border-collapse:collapse; border:none;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.thead <- "border-top:double black; padding:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.centeralign <- "text-align:center;"
  css.cronbach <- "font-style:italic;"  
  css.msa <- "font-style:italic; color:#666666;"  
  css.kmo <- "font-style:italic; border-bottom:double;"  
  css.pov <- "font-style:italic; border-top:1px solid;"  
  css.cpov <- "font-style:italic;"  
  css.minval <- "color:#cccccc;"
  css.arc <- "background-color:#eaeaea;"
  css.removable <- "background-color:#eacccc;"
  css.firsttablerow <- "border-top:1px solid black;"
  css.firsttablecol <- ""
  if (!showMSA && !showCronbachsAlpha) css.cpov <- sprintf("%s border-bottom:double;", css.cpov)
  if (!showMSA && showCronbachsAlpha) css.cronbach <- sprintf("%s border-bottom:double;", css.cronbach)
  if (!showVariance && showCronbachsAlpha) css.cronbach <- sprintf("%s border-top:1px solid;", css.cronbach)
  if (!showVariance && !showCronbachsAlpha) css.kmo <- sprintf("%s border-top:1px solid;",css.kmo)
  if (!showVariance && !showCronbachsAlpha && !showMSA) css.table <- sprintf("%s border-bottom:double;", css.table)
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']],1,1)=='+', paste0(css.table, substring(CSS[['css.table']],2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']],1,1)=='+', paste0(css.thead, substring(CSS[['css.thead']],2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']],1,1)=='+', paste0(css.tdata, substring(CSS[['css.tdata']],2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']],1,1)=='+', paste0(css.caption, substring(CSS[['css.caption']],2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']],1,1)=='+', paste0(css.centeralign, substring(CSS[['css.centeralign']],2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']],1,1)=='+', paste0(css.arc, substring(CSS[['css.arc']],2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.firsttablerow']])) css.firsttablerow <- ifelse(substring(CSS[['css.firsttablerow']],1,1)=='+', paste0(css.firsttablerow, substring(CSS[['css.firsttablerow']],2)), CSS[['css.firsttablerow']])
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- ifelse(substring(CSS[['css.firsttablecol']],1,1)=='+', paste0(css.firsttablecol, substring(CSS[['css.firsttablecol']],2)), CSS[['css.firsttablecol']])
    if (!is.null(CSS[['css.cronbach']])) css.cronbach <- ifelse(substring(CSS[['css.cronbach']],1,1)=='+', paste0(css.cronbach, substring(CSS[['css.cronbach']],2)), CSS[['css.cronbach']])
    if (!is.null(CSS[['css.msa']])) css.msa <- ifelse(substring(CSS[['css.msa']],1,1)=='+', paste0(css.msa, substring(CSS[['css.msa']],2)), CSS[['css.msa']])
    if (!is.null(CSS[['css.kmo']])) css.kmo <- ifelse(substring(CSS[['css.kmo']],1,1)=='+', paste0(css.kmo, substring(CSS[['css.kmo']],2)), CSS[['css.kmo']])
    if (!is.null(CSS[['css.pov']])) css.pov <- ifelse(substring(CSS[['css.pov']],1,1)=='+', paste0(css.pov, substring(CSS[['css.pov']],2)), CSS[['css.pov']])
    if (!is.null(CSS[['css.cpov']])) css.cpov <- ifelse(substring(CSS[['css.cpov']],1,1)=='+', paste0(css.cpov, substring(CSS[['css.cpov']],2)), CSS[['css.cpov']])
    if (!is.null(CSS[['css.minval']])) css.minval <- ifelse(substring(CSS[['css.minval']],1,1)=='+', paste0(css.minval, substring(CSS[['css.minval']],2)), CSS[['css.minval']])
    if (!is.null(CSS[['css.removable']])) css.removable <- ifelse(substring(CSS[['css.removable']],1,1)=='+', paste0(css.removable, substring(CSS[['css.removable']],2)), CSS[['css.removable']])
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                         tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                         tag.cronbach, css.cronbach, tag.minval, css.minval,
                         tag.removable, css.removable, tag.firsttablerow, css.firsttablerow,
                         tag.firsttablecol, css.firsttablecol, tag.centeralign, css.centeralign,
                         tag.msa, css.msa, tag.kmo, css.kmo, tag.caption, css.caption,
                         tag.pov, css.pov, tag.cpov, css.cpov, tag.arc, css.arc)
  # ------------------------
  # start content
  # ------------------------
  toWrite <- paste0(toWrite, page.style)
  toWrite = paste(toWrite, "\n</head>\n<body>", "\n")
  # ----------------------------
  # calculate eigenvalues
  # ----------------------------
  pcadata.eigenval <- pcadata$sdev^2
  # ----------------------------
  # retrieve best amount of factors according
  # to Kaiser-critearia, i.e. factors with eigen value > 1
  # ----------------------------
  pcadata.kaiser <- which(pcadata.eigenval<1)[1]-1
  # --------------------------------------------------------
  # varimax rotation, retrieve factor loadings
  # --------------------------------------------------------
  # check for predefined number of factors
  if (!is.null(numberOfFactors) && is.numeric(numberOfFactors)) {
    pcadata.kaiser <- numberOfFactors
  }
  pcadata.varim = varimaxrota(pcadata, pcadata.kaiser)
  # create data frame with factor loadings
  df <- as.data.frame(pcadata.varim$loadings[,1:ncol(pcadata.varim$loadings)])
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(varlabels)) {
    varlabels <- row.names(df)
  }
  # ----------------------------
  # Prepare length of labels
  # ----------------------------
  if (!is.null(varlabels)) {
    # wrap long variable labels
    varlabels <- sju.wordwrap(varlabels, breakLabelsAt, "<br>")
  }
  # --------------------------------------------------------
  # this function checks which items have unclear factor loadings,
  # i.e. which items do not strongly load on a single factor but
  # may load almost equally on several factors
  # --------------------------------------------------------
  getRemovableItems <- function(dataframe) {
    # clear vector
    removers <- c()
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in 1:nrow(dataframe)) {
      # get factor loadings for each item
      rowval <- as.numeric(abs(df[i,]))
      # retrieve highest loading
      maxload <- max(rowval)
      # retrieve 2. highest loading
      max2load <- sort(rowval, TRUE)[2]
      # check difference between both
      if (abs(maxload-max2load)<factorLoadingTolerance) {
        # if difference is below the tolerance,
        # remeber row-ID so we can remove that items
        # for further PCA with updated data frame
        removers <- c(removers, i)
      }
    }
    # return a vector with index numbers indicating which items
    # have unclear loadings
    return (removers)
  }
  # --------------------------------------------------------
  # this function retrieves a list with the column index ("factor" index)
  # where each case of the data frame has its highedt factor loading.
  # So we know to which "group" (factor dimension) each case of the 
  # data frame belongs to according to the pca results
  # --------------------------------------------------------
  getItemLoadings <- function(dataframe) {
    # clear vector
    itemloading <- c()
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in 1:nrow(dataframe)) {
      # get factor loadings for each item
      rowval <- abs(df[i,])
      # retrieve highest loading and remeber that column
      itemloading <- c(itemloading, which(rowval==max(rowval)))
    }
    # return a vector with index numbers indicating which items
    # loads the highest on which factor
    return (itemloading)
  }
  # --------------------------------------------------------
  # this function calculates the cronbach's alpha value for
  # each factor scale, i.e. all variables with the highest loading
  # for a factor are taken for the reliability test. The result is
  # an alpha value for each factor dimension
  # --------------------------------------------------------
  getCronbach <- function(dataframe, itemloadings) {
    # clear vector
    cbv <- c()
    # iterate all highest factor loadings of items
    for (n in 1:length(unique(itemloadings))) {
      # calculate cronbach's alpha for those cases that all have the
      # highest loading on the same factor
      cbv <- c(cbv, sjs.cronbach(na.omit(dataframe[,which(itemloadings==n)])))
    }
    # cbv now contains the factor numbers and the related alpha values
    # for each "factor dimension scale"
    return(cbv)
  }
  # ----------------------------------
  # Cronbach's Alpha can only be calculated when having a data frame
  # with each component / variable as column
  # ----------------------------------
  if (dataframeparam) {
    # get alpha values
    alphaValues <- getCronbach(data, getItemLoadings(df))
  }
  else {
    cat("\nCronbach's Alpha can only be calculated when having a data frame with each component / variable as column\n")
    alphaValues <- NULL
    showCronbachsAlpha <- FALSE
  }
  # -------------------------------------
  # retrieve those items that have unclear factor loadings, i.e.
  # which almost load equally on several factors. The tolerance
  # that indicates which difference between factor loadings is
  # considered as "equally" is defined via factorLoadingTolerance
  # -------------------------------------
  removableItems <- getRemovableItems(df)
  # -------------------------------------
  # retrieve kmo and msa for data set
  # -------------------------------------
  kmo <- NULL
  if (showMSA) kmo <- KMO(data)
  # -------------------------------------
  # variance
  # -------------------------------------
  pov <- cpov <- NULL
  if (showVariance) {
    pov <- summary(pcadata)$importance[2,1:pcadata.kaiser]
    cpov <- summary(pcadata)$importance[3,1:pcadata.kaiser]
  }
  # -------------------------------------
  # convert data frame, add label names
  # -------------------------------------
  maxdf <- apply(df,1,function(x) max(abs(x)))
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
  # write tr-tag
  page.content <- paste0(page.content, "  <tr>\n")
  # first column
  page.content <- paste0(page.content, "    <th class=\"thead\">&nbsp;</th>\n")
  # iterate columns
  for (i in 1:ncol(df)) {
    page.content <- paste0(page.content, sprintf("    <th class=\"thead\">Component %i</th>\n", i))
  }
  # check if msa column should be shown
  if (showMSA) page.content <- paste0(page.content, "    <th class=\"thead msa\">MSA</th>\n")
  # close table row
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # data rows
  # -------------------------------------
  # iterate all rows of df
  for (i in 1:nrow(df)) {
    # start table row
    rowcss <- ""
    # check for removable items in first row
    if (i %in% removableItems && i==1) rowcss <- " firsttablerow removable"
    # check for removable items in other rows
    if (i %in% removableItems && i!=1) rowcss <- " removable"
    # check for non-removable items in first row
    if (is.na(match(i, removableItems)) && i==1) rowcss <- " firsttablerow"
    # default row string for alternative row colors
    arcstring <- ""
    # if we have alternating row colors, set css
    if (alternateRowColors) arcstring <- ifelse(i %% 2 ==0, " arc", "")
    # write tr-tag with class-attributes
    page.content <- paste0(page.content, "  <tr>\n")
    # print first table cell
    page.content <- paste0(page.content, sprintf("    <td class=\"firsttablecol%s%s\">%s</td>\n", arcstring, rowcss, varlabels[i]))
    # iterate all columns
    for (j in 1:ncol(df)) {
      # start table column
      colcss <- sprintf(" class=\"tdata centeralign%s%s\"", arcstring, rowcss)
      if (maxdf[[i]]!=max(abs(df[i,j]))) colcss <- sprintf(" class=\"tdata centeralign minval%s%s\"", arcstring, rowcss)
      page.content <- paste0(page.content, sprintf("    <td%s>%.*f</td>\n", colcss, digits, df[i,j]))
    }
    # check if msa column should be shown
    if (showMSA) page.content <- paste0(page.content, sprintf("    <td class=\"tdata msa centeralign%s%s\">%.*f</td>\n", arcstring, rowcss, digits, kmo$MSAi[[i]]))
    # close row
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # variance
  # -------------------------------------
  if (showVariance) {
    # write tr-tag with class-attributes
    page.content <- paste0(page.content, "  <tr>\n")
    # first column
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata pov\">%s</td>\n", stringPov))
    # iterate alpha-values
    for (i in 1:length(pov)) {
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign pov\">%.*f&nbsp;%%</td>\n", digits, 100*pov[i]))
    }
    # check if msa column should be shown
    if (showMSA) page.content <- paste0(page.content, "    <td class=\"tdata centeralign pov\"></td>\n")
    page.content <- paste0(page.content, "  </tr>\n  <tr>\n")
    # first column
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata cpov\">%s</td>\n", stringCpov))
    # iterate alpha-values
    for (i in 1:length(pov)) {
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign cpov\">%.*f&nbsp;%%</td>\n", digits, 100*cpov[i]))
    }
    # check if msa column should be shown
    if (showMSA) page.content <- paste0(page.content, "    <td class=\"tdata centeralign cpov\"></td>\n")
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # cronbach's alpha
  # -------------------------------------
  if (showCronbachsAlpha && !is.null(alphaValues)) {
    # write tr-tag with class-attributes
    page.content <- paste0(page.content, "  <tr>\n")
    # first column
    page.content <- paste0(page.content, "    <td class=\"tdata cronbach\">Cronbach's &alpha;</td>\n")
    # iterate alpha-values
    for (i in 1:length(alphaValues)) {
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign cronbach\">%.*f</td>\n", digits, alphaValues[i]))
    }
    # check if msa column should be shown
    if (showMSA) page.content <- paste0(page.content, "    <td class=\"tdata centeralign cronbach\"></td>\n")
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Kaiser-Meyer-Olkin-Kriterium
  # -------------------------------------
  if (showMSA) {
    # write tr-tag with class-attributes
    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, "    <td class=\"tdata kmo\">Kaiser-Meyer-Olkin</td>\n")
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign kmo\" colspan=\"%i\"></td>\n", ncol(df)))
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign kmo\">%.*f</td>\n", digits, kmo$MSA))
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # finish table
  # -------------------------------------
  page.content <- paste(page.content, "\n</table>")
  # -------------------------------------
  # finish html page
  # -------------------------------------
  toWrite <- paste(toWrite, page.content, "\n")
  toWrite <- paste0(toWrite, "</body></html>")
  # -------------------------------------
  # create list with factor loadings that indicate
  # on which column inside the data frame the highest
  # loading is
  # -------------------------------------
  factorindex <- c()
  for (i in 1:nrow(df)) {
    factorindex <- c(factorindex, which.max(abs(df[i,])))
  }
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
  knitr <- gsub(tag.centeralign, css.centeralign, knitr)
  knitr <- gsub(tag.cronbach, css.cronbach, knitr)
  knitr <- gsub(tag.msa, css.msa, knitr)
  knitr <- gsub(tag.pov, css.pov, knitr)
  knitr <- gsub(tag.arc, css.arc, knitr)
  knitr <- gsub(tag.cpov, css.cpov, knitr)
  knitr <- gsub(tag.kmo, css.kmo, knitr)
  knitr <- gsub(tag.minval, css.minval, knitr)  
  knitr <- gsub(tag.removable, css.removable, knitr)  
  knitr <- gsub(tag.firsttablerow, css.firsttablerow, knitr)  
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr)  
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
  invisible (structure(class = "sjtpca",
                       list(page.style = page.style,
                            page.content = page.content,
                            output.complete = toWrite,
                            knitr = knitr,
                            factor.index = factorindex,
                            removed.items = removableItems)))
}
