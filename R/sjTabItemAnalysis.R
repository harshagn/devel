#' @title Show item analysis of an item scale as HTML table
#' @name sjt.itemanalysis
#' @description This function performs an item analysis with certain statistics that are
#'                useful for scale / index development. The resulting tables are shown in the
#'                viewer pane / webbrowser or can be saved as file. Following statistics are 
#'                computed for each item of a data frame:
#'                \itemize{
#'                  \item percentage of missing values
#'                  \item mean value
#'                  \item standard deviation
#'                  \item skew
#'                  \item item difficulty
#'                  \item item discrimination
#'                  \item Cronbach's Alpha if item was removed from scale
#'                  \item mean (or average) inter-item-correlation
#'                }
#'                Optional, following statistics can be computed as well:
#'                \itemize{
#'                  \item kurstosis
#'                  \item Shapiro-Wilk Normality Test
#'                }
#'                If \code{factor.groups} is not \code{NULL}, the data frame \code{df} will be
#'                splitted into groups, assuming that \code{factor.groups} indicate those columns
#'                of the data frame that belong to a certain factor (see return value of function \code{\link{sjt.pca}}
#'                as example for retrieving factor groups for a scale and see examples for more details).
#'
#' @seealso \code{\link{sjs.cronbach}} \cr
#'          \code{\link{sjs.reliability}} \cr
#'          \code{\link{sjs.mic}} \cr
#'          \code{\link{sjp.pca}} \cr
#'          \code{\link{sjt.pca}} \cr
#'          \code{\link{sjt.df}} \cr
#'          \code{\link{sju.mean.n}}
#'          
#' @param df A data frame with items (from a scale)
#' @param factor.groups If not \code{NULL}, the data frame \code{df} will be splitted into sub-groups,
#'          where the item analysis is carried out for each of these groups. Must be a vector of same 
#'          length as \code{ncol(df)}, where each item in this vector represents the group number of
#'          the related columns of \code{df}. See examples for more details.
#' @param factor.groups.titles Titles for each factor group that will be used as table caption for each
#'          component-table. Must be a character vector of same length as \code{length(unique(factor.groups))}.
#'          Default is \code{"auto"}, which means that each table has a standard caption \emph{Component x}.
#'          Use \code{NULL} to suppress table captions.
#' @param scaleItems If \code{TRUE}, the data frame's vectors will be scaled when calculating the
#'          Cronbach's Alpha value (see \code{\link{sjs.reliability}}). Recommended, when 
#'          the variables have different measures / scales.
#' @param minValidRowMeanValue the minimum amount of valid values to compute row means for index scores.
#'          Default is 2, i.e. the return values \code{index.scores} and \code{df.index.scores} are
#'          computed for those items that have at least \code{minValidRowMeanValue} per case (observation, or
#'          technically, row). See \link{sju.mean.n} for details.
#' @param alternateRowColors If \code{TRUE}, alternating rows are highlighted with a light gray
#'          background color.
#' @param orderColumn Indicates a column, either by column name or by column index number,
#'          that should be orderd. Default order is ascending, which can be changed with
#'          \code{orderAscending} parameter. Default is \code{NULL}, hence the results of
#'          the item analysis are  printed with no specific order. See examples in
#'          \code{\link{sjt.df}} for further details.
#' @param orderAscending If \code{TRUE} (default) and \code{orderColumn} is not \code{NULL},
#'          data frame is ordered according to the specified column in an ascending order.
#'          Use \code{FALSE} to apply descending order. See examples in \code{\link{sjt.df}} 
#'          for further details.
#' @param showShapiro If \code{TRUE}, a Shapiro-Wilk normality test is computed for each item.
#'          See \code{\link{shapiro.test}} for details.
#' @param showKurtosis If \code{TRUE}, the kurtosis for each item will also be shown (see \code{\link{kurtosi}}
#'          and \code{\link{describe}} in the \code{psych}-package for more details.
#' @param showCompCorrMat If \code{TRUE} (default), a correlation matrix of each component's
#'          index score is shown. Only applies if \code{factor.groups} is not \code{NULL} and \code{df} has
#'          more than one group. First, for each case (df's row), the sum of all variables (df's columns) is
#'          scaled (using the \code{\link{scale}}-function) and represents a "total score" for
#'          each component (a component is represented by each group of \code{factor.groups}).
#'          After that, each case (df's row) has a scales sum score for each component.
#'          Finally, a correlation of these "scale sum scores" is computed.
#' @param file The destination file, which will be in html-format. If no filepath is specified (default),
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
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
#'            \item \code{css.arc='+font-style:italic;'} to add italic formatting to each 2nd row.
#'          }
#'          See further examples at \url{http://rpubs.com/sjPlot/sjtbasics}.
#' @param useViewer If \code{TRUE}, the function tries to show the HTML table in the IDE's viewer pane. If
#'          \code{FALSE} or no viewer available, the HTML table is opened in a web browser.
#' @param no.output If \code{TRUE}, the html-output is neither opened in a browser nor shown in
#'          the viewer pane and not even saved to file. This option is useful when the html output
#'          should be used in \code{knitr} documents. The html output can be accessed via the return
#'          value.
#' @return (Invisibly) returns a structure with following elements:
#'         \itemize{
#'          \item \code{df.list}: List of data frames with the item analysis for each sub.group (or complete, if \code{factor.groups} was \code{NULL})
#'          \item \code{index.scores}: List of standardized scale / index scores of each case (mean value of all scale items for each case) for each sub-group. Note that NA's are removed from this list. Use \code{df.index.scores} if you want to append the cases' related index scores to the original data frame.
#'          \item \code{df.index.scores}: A data frame with all \code{index.scores} as column variables. While \code{index.scores} don't have NA's included, this data frame's row-length equals to the originals data frame's row-length (and thus can be appended)
#'          \item \code{ideal.item.diff}: List of vectors that indicate the ideal item difficulty for each item in each sub-group. Item difficulty only differs when items have different levels.
#'          \item \code{cronbach.values}: List of Cronbach's Alpha values for the overall item scale for each sub-group.
#'          \item \code{knitr.list}: List of html-tables with inline-css for use with knitr for each table (sub-group)
#'          \item \code{knitr}: html-table of all complete output with inline-css for use with knitr
#'          \item \code{complete.page}: Complete html-output.
#'          }
#'          If \code{factor.groups} was \code{NULL}, each list contains only one elment, since just one
#'          table is printed for the complete scale indicated by \code{df}. If \code{factor.groups} 
#'          is a vector of group-index-values, the lists contain elements for each sub-group.
#' 
#' @note \itemize{
#'          \item The \emph{Shapiro-Wilk Normality Test} (see column \code{W(p)}) tests if an item has a distribution that is significantly different from normal.
#'          \item \emph{Item difficulty} should range between 0.2 and 0.8. Ideal value is \code{p+(1-p)/2} (which mostly is between 0.5 and 0.8).
#'          \item For \emph{item discrimination}, acceptable values are 0.20 or higher; the closer to 1.00 the better. See \code{\link{sjs.reliability}} for more details.
#'          \item In case the total \emph{Cronbach's Alpha} value is below the acceptable cut-off of 0.7 (mostly if an index has few items), the \emph{mean inter-item-correlation} is an alternative measure to indicate acceptability. Satisfactory range lies between 0.2 and 0.4.
#'        }
#' 
#' @references \itemize{
#'              \item Jorion N, Self B, James K, Schroeder L, DiBello L, Pellegrino J (2013) Classical Test Theory Analysis of the Dynamics Concept Inventory. (\url{https://www.academia.edu/4104752/Classical_Test_Theory_Analysis_of_the_Dynamics_Concept_Inventory})
#'              \item Briggs SR, Cheek JM (1986) The role of factor analysis in the development and evaluation of personality scales. Journal of Personality, 54(1), 106-148 (\url{http://onlinelibrary.wiley.com/doi/10.1111/j.1467-6494.1986.tb00391.x/abstract})
#'              \item McLean S et al. (2013) Stigmatizing attitudes and beliefs about bulimia nervosa: Gender, age, education and income variability in a community sample. International Journal of Eating Disorders, doi: 10.1002/eat.22227.
#'              \item Trochim WMK (2008) Types of Reliability. \url{http://www.socialresearchmethods.net/kb/reltypes.php}
#'             }
#' 
#' @examples
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
#' sjt.itemanalysis(df)}
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- sji.setVariableLabels(efc, varlabs)
#' \dontrun{
#' sjt.itemanalysis(efc[,c(start:end)])}
#'   
#' # ---------------------------------------
#' # Compute PCA on Cope-Index, and perform a
#' # item analysis for each extracted factor.
#' # ---------------------------------------
#' \dontrun{
#' factor.groups <- sjt.pca(df, no.output=TRUE)$factor.index
#' sjt.itemanalysis(df, factor.groups)}
#'  
#' @importFrom psych describe
#' @export
sjt.itemanalysis <- function(df,
                             factor.groups=NULL,
                             factor.groups.titles="auto",
                             scaleItems=FALSE,
                             minValidRowMeanValue=2,
                             alternateRowColors=TRUE,
                             orderColumn=NULL,
                             orderAscending=TRUE,
                             showShapiro=FALSE,
                             showKurtosis=FALSE,
                             showCompCorrMat=TRUE,
                             file=NULL,
                             encoding="UTF-8",
                             CSS=NULL,
                             useViewer=TRUE,
                             no.output=FALSE) {
  # -----------------------------------
  # auto-detect variable labels
  # -----------------------------------
  varlabels <- c()
  for (i in 1:ncol(df)) {
    # retrieve variable name attribute
    vn <- autoSetVariableLabels(df[,i])
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
  if (!is.null(varlabels)) colnames(df) <- varlabels
  # -----------------------------------
  # check whether we have (factor) groups
  # for data frame
  # -----------------------------------
  if (is.null(factor.groups)) {
    factor.groups <- rep(1, length.out=ncol(df))
  }
  # data frame with data from item-analysis-output-table
  df.ia <- list()
  # component's correlation matrix
  df.comcor <- list()
  diff.ideal.list <- list()
  index.scores <- list()
  # cronbach's alpha values
  cronbach.total <- list()
  # mean inter-item-correlation values
  mic.total <- list()
  # -----------------------------------
  # retrieve unique factor / group index values
  # -----------------------------------
  findex <- sort(unique(factor.groups))
  # -----------------------------------
  # set titles
  # -----------------------------------
  if (!is.null(factor.groups.titles) && (factor.groups.titles[1]=="auto" || length(factor.groups.titles)!=length(findex))) {
    factor.groups.titles <- sprintf("Component %i", seq_along(findex))
  }
  # -----------------------------------
  # prepare data frame for index-scores, used
  # later
  # -----------------------------------
  df.index.scores <- c()
  # create a column for each sub-group that will contain the index-score-values
  for (i in 1:length(findex)) {
    # first, will with NA. Values follow later
    score <- rep(NA, nrow(df))
    df.index.scores <- cbind(df.index.scores, score)
  }
  # convert to data frame
  df.index.scores <- data.frame(df.index.scores)
  # proper col names
  colnames(df.index.scores) <- sprintf("Score%i", seq_along(1:ncol(df.index.scores)))
  # -----------------------------------
  # iterate all sub-scales (groups)
  # -----------------------------------
  for (i in 1:length(findex)) {
    # -----------------------------------
    # retrieve sub-scale
    # -----------------------------------
    df.sub <- subset(df, select=which(factor.groups==findex[i]))
    # -----------------------------------
    # remember item (column) names for return value
    # return value gets column names of initial data frame
    # -----------------------------------
    df.names <- colnames(df)[which(factor.groups==findex[i])]
    # -----------------------------------
    # retrieve missings for each item
    # -----------------------------------
    missings <- apply(df.sub, 2, function(x) sum(is.na(x)))
    # -----------------------------------
    # retrieve missing percentage for each item
    # -----------------------------------
    missings.prz <- apply(df.sub, 2, function(x) round(100*sum(is.na(x))/length(x),2))
    # -----------------------------------
    # remove missings
    # -----------------------------------
    df.sub <- na.omit(df.sub)
    # -----------------------------------
    # item difficulty
    # -----------------------------------
    itemcnt <- ncol(df.sub)
    difficulty <- apply(df.sub, 2, function(x) round(sum(x)/(max(x)*length(x)),2))
    # -----------------------------------
    # ideal item difficulty
    # -----------------------------------
    fun.diff.ideal <- function(x) {
      p <- 1/max(x)
      return (round(p+(1-p)/2,2))
    }
    diff.ideal <- apply(df.sub, 2, fun.diff.ideal)
    # -----------------------------------
    # get statistics
    # -----------------------------------
    dstat <- describe(df.sub)
    reli <- sjs.reliability(df.sub, scaleItems=scaleItems)
    # -----------------------------------
    # get index score value, by retrieving the row mean
    # -----------------------------------
    item.score <- sju.mean.n(df.sub, minValidRowMeanValue)
    # -----------------------------------
    # store scaled values of each item's total score
    # to compute correlation coefficients between identified components
    # -----------------------------------
    df.subcc <- subset(df, select=which(factor.groups==findex[i]))
    comcor <- scale(apply(df.subcc, 1, sum), center=TRUE, scale=TRUE)
    # -----------------------------------
    # check if we have valid return values from reliability test.
    # In case df had less than 3 columns, NULL is returned
    # -----------------------------------
    if (!is.null(reli)) {
      alpha <- reli[,1]
      itemdis <- reli[,2]
    }
    else {
      alpha <- as.factor(NA)
      itemdis <- as.factor(NA)
    }
    # -----------------------------------
    # create dummy data frame
    # -----------------------------------
    df.dummy <- data.frame(cbind(sprintf("%.2f %%", missings.prz), round(dstat$mean,2), round(dstat$sd,2), round(dstat$skew,2)))
    df.colnames <- c("Missings", "Mean", "SD", "Skew")
    # -----------------------------------
    # include kurtosis statistics
    # -----------------------------------
    if (showKurtosis) {
      df.dummy <- data.frame(cbind(df.dummy, round(dstat$kurtosis,2)))
      df.colnames <- c(df.colnames, "Kurtosis")
    }
    # -----------------------------------
    # include shapiro-wilk normality test
    # -----------------------------------
    if (showShapiro) {
      shaptest.w <- apply(df.sub, 2, function(x) shapiro.test(x)$statistic)
      shaptest.p <- apply(df.sub, 2, function(x) shapiro.test(x)$p.value)
      df.dummy <- data.frame(cbind(df.dummy, sprintf("%.2f (%.3f)", shaptest.w, shaptest.p)))
      df.colnames <- c(df.colnames, "W(p)")
    }
    df.dummy <- data.frame(cbind(df.dummy, difficulty, itemdis, alpha))
    df.colnames <- c(df.colnames, "Item Difficulty", "Item Discrimination", "&alpha; if deleted")
    # -----------------------------------
    # set names of data frame
    # -----------------------------------
    colnames(df.dummy) <- df.colnames
    rownames(df.dummy) <- df.names    
    # -----------------------------------
    # add results to return list
    # -----------------------------------
    df.ia[[length(df.ia)+1]] <- df.dummy
    diff.ideal.list[[length(diff.ideal.list)+1]] <- diff.ideal
    index.scores[[length(index.scores)+1]] <- item.score
    cronbach.total[[length(cronbach.total)+1]] <- sjs.cronbach(df.sub)
    df.comcor[[length(df.comcor)+1]] <- comcor
    # -----------------------------------
    # Mean-interitem-corelation
    # -----------------------------------
    mic.total[[length(mic.total)+1]] <- sjs.mic(df.sub)
  }
  # -----------------------------------
  # create data frame with index scores,
  # including missings
  # -----------------------------------
  for (i in 1:length(index.scores)) {
    # column names equal row-index-values
    index <- as.numeric(names(index.scores[[i]]))
    # fill df with index-score-values
    df.index.scores[index,i] <- index.scores[[i]]
  }
  # -----------------------------------
  # create page with all html content
  # -----------------------------------
  complete.page <- ""
  knitr.list <- list()
  # -----------------------------------
  # iterate all data frames etc.
  # -----------------------------------
  for (i in 1:length(df.ia)) {
    # check if we have titles for each component-table
    if (!is.null(factor.groups.titles)) dftitle <- factor.groups.titles[i]
    # get html-table from data frame
    html <- sjt.df(df.ia[[i]], describe=FALSE, no.output=TRUE, title=dftitle, orderAscending=orderAscending, orderColumn=orderColumn, alternateRowColors=alternateRowColors, CSS=CSS, encoding=encoding, showCommentRow=TRUE, commentString=sprintf("Mean inter-item-correlation=%.3f &middot; Cronbach's &alpha;=%.3f", mic.total[[i]], cronbach.total[[i]]))
    # add to complete html-page
    complete.page <- paste0(complete.page, html$knitr)
    complete.page <- paste0(complete.page, "<p style=\"margin:2em;\">&nbsp;</p>")
    knitr.list[[length(knitr.list)+1]] <- html$knitr
  }
  # -------------------------------------
  # show component correlation table
  # -------------------------------------
  if (showCompCorrMat) {
    # check if we have enough components
    if (length(df.comcor)>1) {
      # copy all component correlation values to a data frame
      df.cc <- data.frame(matrix(unlist(df.comcor), nrow=nrow(df), byrow=FALSE))
      # give proper columm names
      colnames(df.cc) <- sprintf("Component %i", c(1:ncol(df.cc)))
      # compute correlation table, store html result
      html <- sjt.corr(df.cc, missingDeletion="listwise", pvaluesAsNumbers=TRUE, triangle="lower", stringDiagonal=sprintf("&alpha;=%.3f", unlist(cronbach.total)), encoding=encoding, no.output=TRUE)
      # add to html that is printed
      complete.page <- paste0(complete.page, html$knitr)
      knitr.list[[length(knitr.list)+1]] <- html$knitr
    }
  }
  # -------------------------------------
  # wrap html-tags
  # -------------------------------------
  knitr <- complete.page
  complete.page <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n</head>\n<body>\n%s\n</body></html>", encoding, complete.page)
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
      write(complete.page, file=htmlFile)
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
  invisible (list(class="sjtitemanalysis",
                  df.list=df.ia,
                  index.scores=index.scores,
                  df.index.scores=df.index.scores,
                  cronbach.values=cronbach.total,
                  ideal.item.diff=diff.ideal.list,
                  knitr=knitr,
                  knitr.list=knitr.list,
                  complete.page=complete.page))
}
