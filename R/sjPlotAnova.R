# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("xv", "lower", "upper", "pv", "p"))


#' @title Plot One-Way-Anova tables
#' @name sjp.aov1
#' 
#' @description Plot One-Way-Anova table sum of squares (SS) of each factor level (group) 
#'                against the dependent variable. The SS of the factor variable against the 
#'                dependent variable (variance within and between groups) is printed to
#'                the model summary.
#' @seealso \code{\link{sju.aov1.levene}}
#'                
#' @param depVar The dependent variable. Will be used with following formular:
#'          \code{aov(depVar ~ grpVar)}
#' @param grpVar The grouping variable, as unordered factor. Will be used with following formular:
#'          \code{aov(depVar ~ grpVar)}
#' @param meansums If \code{TRUE}, the values reported are the true group mean values. If \code{FALSE} (default),
#'          the values are reported in the standard way, i.e. the values indicate the difference of
#'          the group mean in relation to the intercept (reference group).
#' @param type Indicates Whether the group means should be plotted as \code{"dots"} (aka forest plots, default)
#'          or as \code{"bars"}.
#' @param hideErrorBars If \code{TRUE}, the error bars that indicate the confidence intervals of the group means are not
#'          shown. Only applies if parameter \code{type} is \code{"bars"}. Default value is \code{FALSE}.
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#'          Use \code{"auto"} to automatically detect variable names that will be used as title
#'          (see \code{\link{sji.setVariableLabels}}) for details).
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param axisLabels.y Value labels of the grouping variable \code{grpVar} that are used for labelling the
#'          grouping variable axis. Passed as vector of strings.
#'          Example: \code{axisLabels.y=c("Label1", "Label2", "Label3")}. \cr
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically. See examples below. \cr
#'          Note: In case \code{type} is \code{"bars"}, the \code{grpVar} will be plotted along
#'          the x-axis.
#' @param reverseOrder If \code{TRUE}, the order of the factor categories (groups) is reversed.
#'          Default is \code{FALSE}.
#' @param stringIntercept A string that indicates the reference group (intercept), that is appended to
#'          the value label of the grouping variable. Default is \code{"(Intercept)"}.
#' @param showAxisLabels.y Whether y axis text (category value) should be shown (use \code{TRUE})
#'          or not. Default is \code{TRUE}.
#' @param axisLabelSize The size of value labels in the diagram. Default is 4, recommended values range
#'          between 2 and 8.
#' @param axisLabelColor The color of the category labels (predictor labels). Default is a dark grey (grey30).
#' @param axisTitle.x A label for the x axis. Default is \code{NULL}, which means no x-axis title.
#'          Use \code{"auto"} to automatically detect variable names that will be used as title
#'          (see \code{\link{sji.setVariableLabels}}) for details).
#' @param axisTitleColor The color of the x axis label.
#' @param axisTitleSize The size of the x axis label. Default is 1.2.
#' @param axisLimits Defines the range of the axis where the beta coefficients and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits=c(-0.8,0.8)}.
#' @param valueLabelColor Colour of the values inside the diagram. Only applies, when parameter
#'          \code{showValueLabels} is set to \code{TRUE}. Use any valid colour value, e.g. \code{valueLabelColor="grey50"} or
#'          \code{valueLabelColor=c("#cc3366")}. Default is \code{"grey20"}.
#' @param valueLabelColorNS Colour of the non significant values inside the diagram.
#'          Only applies, when parameter \code{showValueLabels} is set to \code{TRUE}. Use any valid colour value, e.g. 
#'          \code{valueLabelColor="grey50"} or \code{valueLabelColor=c("#cc3366")}. Default is \code{"grey50"}.
#' @param valueLabelSize Size of the value labels. Drfault is 4.5. Recommended Values range from
#'          2 to 8
#' @param valueLabelAlpha The alpha level (transparancy) of the value labels. Default is 0.8, use
#'          any value from 0 to 1.
#' @param axisLabelAngle.x Angle for x-axis-labels, passed as numeric value.
#' @param axisLabelAngle.y Angle for y-axis-labels, passed as numeric value.
#' @param errorBarColor The color of the error bars that indicate the confidence intervalls
#'          of the group means. Default is \code{NULL}, which means that if \code{type} is \code{"dots"},
#'          the \code{pointColor} value will be used as error bar color. In case \code{type} is \code{"bars"},
#'          \code{"black"} will be used as error bar color.
#' @param errorBarWidth The width of the error bar ends. Default is 0.
#' @param errorBarSize The size of the error bar. Default is 0.8.
#' @param errorBarLineType The linetype of error bars. Default is \code{1} (solid line).
#' @param pointColor The colors of the points that indicate the mean-value. \code{pointColor} is a 
#'          vector with two values: the first indicating groups with positive means and the second 
#'          indicating negative means. Default is \code{c("#3366a0", "#aa6633")}.
#' @param pointSize The size of the points that indicate the mean-value. Default is 3.
#' @param barColor The colors of the bars in bar charts. Only applies if parameter \code{type} is \code{"bars"}. \code{barColor} is a 
#'          vector with two values: the first indicating groups with positive means and the second 
#'          indicating negative means. Default is \code{c("#3366a0", "#aa6633")}.
#' @param barWidth The width of the bars in bar charts. Only applies if parameter \code{type} is \code{"bars"}. Default is 0.5
#' @param barAlpha The alpha value of the bars in bar charts. Only applies if parameter \code{type} is \code{"bars"}. Default is 1
#' @param barOutline If \code{TRUE}, each bar gets a colored outline. Only applies if parameter \code{type} is \code{bars}.
#'          Default is \code{FALSE}.
#' @param barOutlineColor The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}.
#'          Default is black.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is \code{NULL}, so \code{\link{pretty}} gridbeaks will be used.
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids)
#'          \item \code{"none"} for no borders, grids and ticks or
#'          \item \code{"themr"} if you are using the \code{ggthemr} package (in such cases, you may use the \code{ggthemr::swatch} function to retrieve theme-colors for the \code{barColor} parameter)
#'          }
#'          See \url{http://rpubs.com/sjPlot/custplot} for details and examples.
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param expand.grid If \code{TRUE}, the plot grid is expanded, i.e. there is a small margin between
#'          axes and plotting region. Default is \code{FALSE}.
#' @param showTickMarks Whether tick marks of axes should be shown or not
#' @param showValueLabels Whether the value labels (mean differences) should be plotted 
#'          to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each category/group should be appended
#'          to values or not.
#' @param showModelSummary If \code{TRUE} (default), a summary of the anova model with 
#'          Sum of Squares between groups (ssb), Sum of Squares within groups (ssw), multiple and adjusted 
#'          R-square and F-Test is printed to the lower right corner
#'          of the diagram. Default is \code{TRUE}.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' data(efc)
#' # note: "grpVar" does not need to be a factor.
#' # coercion to factor is done by the function
#' sjp.aov1(efc$c12hour, efc$e42dep)
#' 
#' 
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' sjp.aov1(efc$c12hour,
#'          as.factor(efc$e42dep),
#'          axisLabels.y=efc.val['e42dep'],
#'          axisTitle.x=efc.var[['c12hour']])
#'          
#' # -------------------------------------------------
#' # auto-detection of value labels and variable names
#' # -------------------------------------------------
#' efc <- sji.setVariableLabels(efc, efc.var)
#' sjp.aov1(efc$c12hour,
#'          efc$e42dep,
#'          title="auto",
#'          axisTitle.x="auto")
#' 
#' sjp.aov1(efc$c12hour,
#'          as.factor(efc$c172code),
#'          axisLabels.y=efc.val['c172code'],
#'          title=efc.var[['c12hour']],
#'          type="bars",
#'          showTickMarks=FALSE,
#'          showModelSummary=FALSE,
#'          axisLabelAngle.x=90)
#'
#' @import ggplot2
#' @export
sjp.aov1 <- function(depVar,
                    grpVar,
                    meansums=FALSE,
                    type="dots",
                    hideErrorBars=FALSE,
                    title=NULL,
                    titleSize=1.3,
                    titleColor="black",
                    axisLabels.y=NULL, 
                    reverseOrder=FALSE,
                    stringIntercept="(Intercept)",
                    showAxisLabels.y=TRUE,
                    axisLabelSize=1.1,
                    axisLabelColor="gray30",
                    axisTitle.x=NULL,
                    axisTitleSize=1.2,
                    axisTitleColor=c("#444444"),
                    axisLimits=NULL,
                    valueLabelColor="grey20",
                    valueLabelColorNS="grey50",
                    valueLabelSize=4.5,
                    valueLabelAlpha=0.8,
                    axisLabelAngle.x=0, 
                    axisLabelAngle.y=0, 
                    errorBarColor=NULL,
                    errorBarWidth=0,
                    errorBarSize=0.8,
                    errorBarLineType=1,
                    pointColor=c("#3366a0", "#aa3333"),
                    pointSize=3,
                    barColor=c("#3366a0", "#aa3333"),
                    barWidth=0.5,
                    barAlpha=1,
                    barOutline=FALSE,
                    barOutlineColor="black",
                    breakTitleAt=50, 
                    breakLabelsAt=12, 
                    gridBreaksAt=NULL,
                    borderColor=NULL, 
                    axisColor=NULL, 
                    theme=NULL,
                    majorGridColor=NULL,
                    minorGridColor=NULL,
                    hideGrid.x=FALSE,
                    hideGrid.y=FALSE,
                    expand.grid=FALSE,
                    showTickMarks=TRUE,
                    showValueLabels=TRUE, 
                    labelDigits=2,
                    showPValueLabels=TRUE,
                    showModelSummary=TRUE,
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) axisLabels.y <- autoSetValueLabels(grpVar)
  if (!is.null(axisTitle.x) && axisTitle.x=="auto") axisTitle.x <- autoSetVariableLabels(depVar)
  if (!is.null(title) && title=="auto") {
    t1 <- autoSetVariableLabels(depVar)
    t2 <- autoSetVariableLabels(grpVar)
    if (!is.null(t1) && !is.null(t2)) {
      title <- paste0(t1, " by ", t2)
    }
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y)) {
    # if labels are lists, unlist
    if (is.list(axisLabels.y)) {
      axisLabels.y <- unlistlabels(axisLabels.y)
    }
    # append "intercept" string, to mark the reference category
    axisLabels.y[1] <- paste(axisLabels.y[1], stringIntercept)
  }
  # --------------------------------------------------------
  # Check if grpVar is factor. If not, convert to factor
  # --------------------------------------------------------
  if (!is.factor(grpVar)) {
    grpVar <- as.factor(grpVar)
  }
  # --------------------------------------------------------
  # Check spelling of type-param
  # --------------------------------------------------------
  if (type=="dot" || type=="d") {
    type <- "dots"
  }
  if (type=="bar" || type=="b") {
    type <- "bars"
  }
  if (expand.grid==TRUE) {
    expand.grid <- waiver()
  }
  else {
    expand.grid <- c(0,0)
  }
  # --------------------------------------------------------
  # set geom colors
  # --------------------------------------------------------
  if (type=="dots") {
    geomcols <- pointColor
  }
  else {
    geomcols <- barColor
  }
  # --------------------------------------------------------
  # check whether we colors for error bars. if not, use point color
  # in case of dots or "black" in case of bars.
  # --------------------------------------------------------
  if (is.null(errorBarColor)) {
    if (type=="dots") {
      errorBarColors <- geomcols
    }
    else {
      errorBarColors <- c("black", "black")
    }
  }
  else {
    errorBarColors <- c(errorBarColor, errorBarColor)
  }
  # --------------------------------------------------------
  # check whether we have x-axis title. if not, use standard
  # value
  # --------------------------------------------------------
  if (is.null(axisTitle.x)) {
    axisTitle.x <- c("")
  }
  # --------------------------------------------------------
  # check whether bars should have an outline
  # --------------------------------------------------------
  if (!barOutline) {
    barOutlineColor <- waiver()
  }  
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sju.wordwrap(axisTitle.x, breakTitleAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)
  }
  # ----------------------------
  # Calculate one-way-anova. Since we have
  # only one group variable, Type of SS does
  # not matter.
  # ----------------------------
  fit <- aov(depVar ~ grpVar)
  # coefficients (group mean)
  means <- summary.lm(fit)$coefficients[,1]
  # p-values of means
  means.p <- summary.lm(fit)$coefficients[,4]
  # lower confidence intervals of coefficients (group mean)
  means.lci <- confint(fit)[,1]
  # upper confidence intervals of coefficients (group mean)
  means.uci <- confint(fit)[,2]
  # ----------------------------
  # Check whether true group means should be reported
  # or the differences of group means in relation to the
  # intercept (reference group). The latter is the default.
  # ----------------------------
  if (meansums) {
    for (i in 2:length(means)) {
      means[i] <- means[i]+means[1]
      means.lci[i] <- means.lci[i]+means[1]
      means.uci[i] <- means.uci[i]+means[1]
    }
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    # sum of squares
    ss <- summary(fit)[[1]]['Sum Sq']
    # multiple r2
    r2 <- summary.lm(fit)$r.squared
    # adj. r2
    r2.adj <- summary.lm(fit)$adj.r.squared
    # get F-statistics
    fstat <- summary.lm(fit)$fstatistic[1]
    # p-value for F-test
    pval <- summary(fit)[[1]]['Pr(>F)'][1,1]
    # indicate significance level by stars
    pan <- c("")
    if (pval<=0.001) {
      pan <- c("***")
    }
    else  if (pval<=0.01) {
      pan <- c("**")
    }
    else  if (pval<=0.05) {
      pan <- c("*")
    }
    # create mathematical term
    modsum <- as.character(as.expression(
      substitute(italic(SS[B]) == ssb * "," ~~ italic(SS[W]) == ssw * "," ~~ R^2 == mr2 * "," ~~ "adj." * R^2 == ar2 * "," ~~ "F" == f * panval,
                 list(ssb=sprintf("%.2f", ss[1,]),
                      ssw=sprintf("%.2f", ss[2,]),
                      mr2=sprintf("%.3f", r2),
                      ar2=sprintf("%.3f", r2),
                      f=sprintf("%.2f", fstat),
                      panval=pan))))
  }
  # ----------------------------
  # print coefficients and p-values in plot
  # ----------------------------
  # init data column for p-values
  ps <- c(round(means,labelDigits))
  # if no values should be shown, clear
  # vector now
  if (!showValueLabels) {
    ps <- rep(c(""), length(ps))
  }
  # --------------------------------------------------------
  # copy p-values into data column
  # --------------------------------------------------------
  if (showPValueLabels) {
    for (i in 1:length(means.p)) {
      if (means.p[i]>=0.05) {
      }
      else if (means.p[i]>=0.01 && means.p[i]<0.05) {
        ps[i] <- paste(ps[i], "*")
      }
      else if (means.p[i]>=0.001 && means.p[i]<0.01) {
        ps[i] <- paste(ps[i], "**")
      }
      else {
        ps[i] <- paste(ps[i], "***")
      }
    }  
  }
  # --------------------------------------------------------
  # check whether order of category items should be reversed
  # or not
  # --------------------------------------------------------
  if (reverseOrder) {
    catorder <- c(length(means):1)
  }
  else {
    catorder <- c(1:length(means))
  }
  # --------------------------------------------------------
  # create new data.frame, since ggplot requires data.frame as parameter
  # The data frame contains means, CI and p-values
  # --------------------------------------------------------
  df <- data.frame(cbind(
    # Append coefficients
    means,
    # append CI
    means.lci,
    means.uci,
    # append p-value
    means.p,
    ps,
    catorder))
  # --------------------------------------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- row.names(df)
  }
  # order labels
  axisLabels.y <- axisLabels.y[catorder]
  # give columns names
  names(df) <- c("means", "lower", "upper", "p", "pv", "xv")
  df$means <- as.numeric(as.character(df$means))
  df$lower <- as.numeric(as.character(df$lower))
  df$upper <- as.numeric(as.character(df$upper))
  df$p <- as.numeric(as.character(df$p))
  df$pv <- as.character(df$pv)
  # bind color values to data frame, because we cannot use several
  # different color aesthetics in ggplot
  df <- cbind(df,
              geocol=ifelse(df$means>=0, geomcols[1], geomcols[2]), 
              labcol=ifelse(df$p<0.05, valueLabelColor, valueLabelColorNS),
              errcol=ifelse(df$means>=0, errorBarColors[1], errorBarColors[2]))
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user-defined range (if "axisLimits"
  # is not NULL)
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    # check whether we have bar chart and error bars hidden
    # in this case, the upper limit does not correspond to the
    # upper CI, but to the highest OR value
    if (type=="bars") {
      # if errorbars are hidden, the axis range is defined
      # by the mean values
      if (hideErrorBars) {
        maxval <- max(df$means)+10
        minval <- min(df$means)
      }
      # if errorbars are shown, axis range is defined
      # by confidence interval
      else {
        maxval <- max(df$upper)
        minval <- min(df$lower)
      }
      # if minval is > 0, set it to zero, so we have a proper baseline
      if (minval>0) minval <- 0
      # if maxval is < 0, set it to zero, so we have a proper baseline
      if (maxval<0) maxval <- 0
    }
    else {
      # else we have confindence intervals displayed, so
      # the range corresponds to the boundaries given by
      # the CI's
      maxval <- max(df$upper)
      minval <- min(df$lower)
    }
    if (maxval>0) limfac <- ifelse(abs(maxval)<5, 5, 10)
    else limfac <- ifelse(abs(minval)<5, 5, 10)
    upper_lim <- ifelse(maxval==0, 0, limfac*ceiling((maxval+1)/limfac))
    lower_lim <- ifelse(minval==0, 0, limfac*floor(minval/limfac))
  }
  else {
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # determine gridbreaks
  if (is.null(gridBreaksAt)) {
    ticks <- pretty(c(lower_lim, upper_lim))
  }
  else {
    ticks <- c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  }
  # --------------------------------------------------------
  # Set theme and default grid colours. grid colours
  # might be adjusted later
  # --------------------------------------------------------
  hideGridColor <- c("white")
  if (is.null(theme)) {
    ggtheme <- theme_gray()
    hideGridColor <- c("gray90")
  }
  else if (theme=="themr") {
    ggtheme <- NULL
  }
  else if (theme=="bw") {
    ggtheme <- theme_bw()
  }
  else if (theme=="classic") {
    ggtheme <- theme_classic()
  }
  else if (theme=="minimal") {
    ggtheme <- theme_minimal()
  }
  else if (theme=="none") {
    ggtheme <- theme_minimal()
    majorGridColor <- c("white")
    minorGridColor <- c("white")
    showTickMarks <-FALSE
  }
  # --------------------------------------------------------
  # Set up grid colours
  # --------------------------------------------------------
  majorgrid <- NULL
  minorgrid <- NULL
  if (!is.null(majorGridColor)) {
    majorgrid <- element_line(colour=majorGridColor)
  }
  if (!is.null(minorGridColor)) {
    minorgrid <- element_line(colour=minorGridColor)
  }
  hidegrid <- element_line(colour=hideGridColor)
  # --------------------------------------------------------
  # Set up visibility of tick marks
  # --------------------------------------------------------
  if (!showTickMarks && !is.null(ggtheme)) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  # --------------------------------------------------------
  # Set up plot padding (margins inside diagram). In case of
  # bars, we don't want margins.
  # --------------------------------------------------------
  if (type=="bars") {
    scaley <- scale_y_continuous(limits=c(lower_lim,upper_lim), expand=expand.grid, breaks=ticks, labels=ticks)    
  }
  else {
    scaley <- scale_y_continuous(limits=c(lower_lim,upper_lim), breaks=ticks, labels=ticks)    
  }
  # --------------------------------------------------------
  # Start plot here!
  # --------------------------------------------------------
  if (type=="dots") {
    anovaplot <- ggplot(df, aes(y=means, x=xv)) +
      # print point
      geom_point(size=pointSize, colour=df$geocol) +
      # and error bar
      geom_errorbar(aes(ymin=lower, ymax=upper), colour=df$errcol, size=errorBarSize, width=errorBarWidth, linetype=errorBarLineType) +
      # Print p-values. With vertical adjustment, so they don't overlap with the errorbars
      geom_text(aes(label=pv, y=means), colour=df$labcol, vjust=-0.8, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
  }
  # --------------------------------------------------------
  # start with bar plots here
  # --------------------------------------------------------
  else if (type=="bars") {
    # check whether we have error bars. if yes, adjust horizontal
    # posizion of labels
    hlabj <- ifelse(hideErrorBars==FALSE, 1.3, 0.5)
    anovaplot <- ggplot(df, aes(y=means, x=xv)) +
      # stat-parameter indicates statistics
      # stat="bin": y-axis relates to count of variable
      # stat="identity": y-axis relates to value of variable
      geom_bar(fill=df$geocol, stat="identity", position="identity", width=barWidth, colour=barOutlineColor, alpha=barAlpha) +
      # print value labels and p-values
      geom_text(aes(label=pv, y=means), colour=df$labcol, vjust=ifelse(df$means>=0, -1, 1), hjust=hlabj, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
    if (hideErrorBars==FALSE) {
      anovaplot <- anovaplot +
        # print confidence intervalls (error bars)
        geom_errorbar(aes(ymin=lower, ymax=upper), colour=df$errcol, width=errorBarWidth, size=errorBarSize, linetype=errorBarLineType)
    }
  }
  # --------------------------------------------------------
  # continue with other plot elements
  # --------------------------------------------------------
  anovaplot <- anovaplot +
    # set y-scale-limits, breaks and tick labels
    scaley +
    # set value labels to x-axis
    scale_x_discrete(labels=axisLabels.y, limits=c(1:nrow(df))) +
    # flip coordinates
    labs(title=title, x=NULL, y=axisTitle.x)
  # apply theme
  if (!is.null(ggtheme)) {
    anovaplot <- anovaplot +
      ggtheme +
      # set axes text and 
      theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
            axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
            axis.text.y = element_text(angle=axisLabelAngle.y),
            axis.text.x = element_text(angle=axisLabelAngle.x),
            plot.title = element_text(size=rel(titleSize), colour=titleColor))
  }
  # --------------------------------------------------------
  # Flip coordinates when we have dots
  # --------------------------------------------------------
  if (type=="dots") {
    anovaplot <- anovaplot + coord_flip()
  }
  # the panel-border-property can only be applied to the bw-theme
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      anovaplot <- anovaplot + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    anovaplot <- anovaplot + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    anovaplot <- anovaplot + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    anovaplot <- anovaplot + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideGrid.x) {
    anovaplot <- anovaplot + 
      theme(panel.grid.major.x = hidegrid,
            panel.grid.minor.x = hidegrid)
  }
  if (hideGrid.y) {
    anovaplot <- anovaplot + 
      theme(panel.grid.major.y = hidegrid,
            panel.grid.minor.y = hidegrid)
  }
  
  
  # check whether modelsummary should be printed
  if (showModelSummary) {
    # add annotations with model summary
    # annotations include intercept-value and model's r-square
    if (type=="dots") {
      anovaplot <- anovaplot + annotate("text", label=modsum, parse=TRUE, x=-Inf, y=Inf, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha, vjust=-0.5, hjust=1.1)
    }
    else {
      anovaplot <- anovaplot + annotate("text", label=modsum, parse=TRUE, x=-Inf, y=Inf, colour=valueLabelColor, size=valueLabelSize, alpha=valueLabelAlpha, vjust=1.2, hjust=-0.02)
    }
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(anovaplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpaov1",
                       list(plot = anovaplot,
                            df = df)))
}



#' @title Plot Levene-Test for One-Way-Anova
#' @name sju.aov1.levene
#' 
#' @description Plot results of Levene's Test for Equality of Variances for One-Way-Anova.
#' @seealso \code{\link{sjp.aov1}}, \code{\link{sjs.chi2.gof}}, \code{\link{sjs.mwu}} and \code{\link{wilcox.test}}, 
#'          \code{\link{ks.test}}, \code{\link{kruskal.test}}, \code{\link{t.test}}, \code{\link{chisq.test}}, 
#'          \code{\link{fisher.test}}
#'           
#' @param depVar The dependent variable. Will be used with following formular:
#'          \code{aov(depVar ~ grpVar)}
#' @param grpVar The grouping variable, as unordered factor. Will be used with following formular:
#'          \code{aov(depVar ~ grpVar)}
#' 
#' @examples
#' data(efc)
#' sju.aov1.levene(efc$c12hour, efc$e42dep)
#' 
#' @export
sju.aov1.levene <- function(depVar, grpVar) {
  # check if grpVar is factor
  if (!is.factor(grpVar)) grpVar <- factor(grpVar)
  # remove missings
  df <- na.omit(data.frame(depVar, grpVar))
  # calculate means
  means <- tapply(df$depVar, df$grpVar, mean)
  depVarNew <- abs(df$depVar - means[df$grpVar])
  cat("\nLevene's Test for Homogeneity of Variances\n------------------------------------------\n")
  fit <- aov(depVarNew ~ df$grpVar)
  print(summary(fit))
  pval <- summary(fit)[[1]]['Pr(>F)'][1,1]
  # print "summary" of test
  cat("\nConclusion:\n")
  if (pval>0.05) {
    cat("Groups are homogeneous. Everything's fine.\n")
  }
  else {
    cat("Groups are not homogeneous!\n")
  }
}
