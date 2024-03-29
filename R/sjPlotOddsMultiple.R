# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("OR", "lower", "upper", "p", "pa", "shape"))

#' @title Plot odds ratios (forest plots) of multiple fitted glm's
#' @name sjp.glmm
#' @references \itemize{
#'              \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
#'              \item \url{http://strengejacke.wordpress.com/2014/01/29/comparing-multiple-glm-in-one-graph-rstats/}
#'             }
#' 
#' @description Plot odds ratios (forest plots) of multiple fitted glm's with confidence intervalls in one plot.
#' @seealso \code{\link{sjp.glm}} \cr
#'          \code{\link{sjt.glm}} \cr
#'          \code{\link{sjp.glm.ma}} \cr
#'          \code{\link{sjp.lmm}}
#' 
#' @param ... One or more fitted glm-objects.
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#' @param titleSize The size of the plot title. Default is 1.3.
#' @param titleColor The color of the plot title. Default is \code{"black"}.
#' @param labelDependentVariables Labels of the dependent variables of all fitted models
#'          which have been used as first parameter(s), provided as char vector.
#' @param legendDepVarTitle A character vector used for the title of the dependent variable's legend.
#'          Default is \code{"Dependent Variables"}.
#' @param legendPValTitle A character vector used for the title of the significance level's legend.
#'          Default is \code{"p-level"}. Only applies if \code{usePShapes} is \code{TRUE}.
#' @param stringModel String constant used as legend text for the model names in case no 
#'          labels for the dependent variables are provided (see \code{labelDependentVariables}).
#'          Default is \code{"Model"}.
#' @param axisLabels.y Labels of the predictor variables (independent vars, odds) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: \code{axisLabels.y=c("Label1", "Label2", "Label3")}
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          \code{list} object with label strings. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param axisLabelSize The size of value labels in the diagram. Default is 1.1, recommended values range
#'          between 0.7 and 3.0
#' @param showAxisLabels.y Whether odds names (predictor labels) should be shown or not.
#' @param showTickMarks Whether tick marks of axes should be shown or not.
#' @param axisTitle.x A label ("title") for the x axis.
#' @param axisTitleColor The color of the x axis label.
#' @param axisTitleSize The size of the x axis label.
#' @param axisLimits Defines the range of the axis where the beta coefficients and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits=c(-0.8,0.8)}.
#' @param axisLabelAngle.x Angle for axis-labels where the odds ratios are printed. Note
#'          that due to the coordinate flip, the acutal y-axis with odds ratios are appearing on the x-axis.
#' @param axisLabelAngle.y Angle for axis-labels where the predictor labels (\code{axisLabels.y}) are printed. Note
#'          that due to the coordinate flip, the acutal x-axis with predictor labels are appearing on the y-axis.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param breakLegendAt Wordwrap for legend, i.e. names of the dependent variables of each fitted model.
#'          See parameter \code{labelDependentVariables}. Determines how many chars of each dependent variable name
#'          is displayed in one line in the legend and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is 0.5
#' @param transformTicks if \code{TRUE}, the grid bars have exponential distances, i.e. they
#'          visually have the same distance from one grid bar to the next. Default is \code{FALSE} which
#'          means that grids are plotted on every \code{gridBreaksAt}'s position, thus the grid bars
#'          become narrower with higher odds ratio values.
#' @param pointSize The size of the points that indicate the beta-value. Default is 3.
#' @param modelPlotSpace Defines the space between the dots and error bars of the plotted fitted models. Default
#'          is 0.3.
#' @param modelColors A vector with colors for representing the odds values (i.e. points and error bars)
#'          of the different fitted models. Thus, the length of this vector must be equal to
#'          the length of supplied fitted models, so each model is represented by its own color.
#'          You can use:
#'          \itemize{
#'            \item \code{"bw"} or \code{"black"} for only one colouring in almost black
#'            \item \code{"gray"}, \code{"grey"} or \code{"gs"} for a grayscale
#'            \item \code{"brewer"} for colours from the color brewer palette.
#'            }
#'          If \code{modelColors} is \code{"brewer"}, use the \code{colorPalette} parameter to specify a palette of the \url{http://colorbrewer2.org}
#'          Else specify your own color values as vector (e.g. \code{modelColors=c("#f00000", "#00ff00")}).
#' @param colorPalette If parameter \code{modelColors} is \code{brewer}, specify a color palette from the \url{http://colorbrewer2.org} here.
#'          All color brewer palettes supported by ggplot are accepted here.
#' @param axisLabelColor Colour of the tick labels at the axis (variable names, odds names).
#' @param valueLabelColor The colour of the odds values. These values are printed above the plots respectively beside the
#'          bar charts. default color is \code{"black"}.
#' @param valueLabelSize Size of the value labels. Drfault is 4. Recommended Values range from
#'          2 to 8
#' @param valueLabelAlpha The alpha level (transparancy) of the value labels. Default is 1, use
#'          any value from 0 to 1.
#' @param nsAlpha The alpha level (transparancy) of non significant predicors. Points and error bars
#'          are affected by this value and plotted with a slight transparancy. Default is 1.
#' @param usePShapes If \code{TRUE}, significant levels are distinguished by different point shapes and a related
#'          legend is plotted. Default is \code{FALSE}.
#' @param axisColor User defined color of axis border (y- and x-axis, in case the axes should have different colors than
#'          the diagram border).
#' @param borderColor User defined color of whole diagram border (panel border).
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param errorBarWidth The width of the error bar ends. Default is \code{0}
#' @param errorBarSize The size (thickness) of the error bars. Default is \code{0.5}
#' @param errorBarLineType The linetype of error bars. Default is \code{1} (solid line).
#' @param majorGridColor Specifies the color of the major grid lines of the diagram background.
#' @param minorGridColor Specifies the color of the minor grid lines of the diagram background.
#' @param hideGrid.x If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param hideGrid.y If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.
#' @param theme Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray 
#'          background with white grids.
#'          \itemize{
#'          \item Use \code{"bw"} for a white background with gray grids
#'          \item \code{"classic"} for a classic theme (black border, no grids)
#'          \item \code{"minimal"} for a minimalistic theme (no border,gray grids)
#'          \item \code{"none"} for no borders, grids and ticks or
#'          \item \code{"themr"} if you are using the \code{ggthemr} package
#'          }
#'          See \url{http://rpubs.com/sjPlot/custplot} for details and examples.
#' @param flipCoordinates If \code{TRUE} (default), predictors are plotted on the left y-axis and estimate
#'          values are plotted on the x-axis.
#' @param legendPos The position of the legend, if a legend is drawn. Use \code{"bottom"}, \code{"top"}, \code{"left"}
#'          or \code{"right"} to position the legend above, below, on the left or right side of the diagram. Right
#'          positioning is default.
#' @param legendSize The text size of the legend. Default is 1. Relative size, so recommended values are from 0.3 to
#'          2.5
#' @param legendBorderColor Color of the legend's border. Default is \code{"white"}, so no visible border is drawn.
#' @param legendBackColor Fill color of the legend's background. Default is \code{"white"}, so no visible background is drawn.
#' @param showIntercept If \code{TRUE}, the intercept of the fitted model is also plotted.
#'          Default is \code{FALSE}. Please note that due to exp-transformation of
#'          estimates, the intercept in some cases can not be calculated, thus the
#'          function call is interrupted and no plot printed.
#' @param showValueLabels Whether the beta and standardized beta values should be plotted 
#'          to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#'          
#' @examples
#' # prepare dummy variables for binary logistic regression
#' y1 <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
#' y2 <- ifelse(swiss$Infant.Mortality<median(swiss$Infant.Mortality), 0, 1)
#' y3 <- ifelse(swiss$Agriculture<median(swiss$Agriculture), 0, 1)
#' 
#' # Now fit the models. Note that all models share the same predictors
#' # and only differ in their dependent variable (y1, y2 and y3)
#' fitOR1 <- glm(y1 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#' fitOR2 <- glm(y2 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#' fitOR3 <- glm(y3 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#' 
#' # plot multiple models
#' sjp.glmm(fitOR1, fitOR2, fitOR3)
#' 
#' # plot multiple models with legend labels and point shapes instead of value  labels
#' sjp.glmm(fitOR1, fitOR2, fitOR3,
#'          labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
#'          showValueLabels=FALSE,
#'          showPValueLabels=FALSE,
#'          usePShapes=TRUE,
#'          nsAlpha=0.2)
#' 
#' @import ggplot2
#' @export
sjp.glmm <- function(..., 
                    title=NULL,
                    titleSize=1.3,
                    titleColor="black",
                    labelDependentVariables=NULL, 
                    legendDepVarTitle="Dependent Variables",
                    legendPValTitle="p-level",
                    stringModel="Model",
                    axisLabels.y=NULL, 
                    axisLabelSize=1.1,
                    axisLabelAngle.x=0,
                    axisLabelAngle.y=0, 
                    axisLabelColor="gray30", 
                    axisTitle.x="Odds Ratios",
                    axisTitleSize=1.2,
                    axisTitleColor=c("#444444"),
                    axisLimits=NULL,
                    breakTitleAt=50, 
                    breakLabelsAt=12,
                    breakLegendAt=20,
                    gridBreaksAt=0.5,
                    transformTicks=FALSE,
                    errorBarWidth=0,
                    errorBarSize=0.5,
                    errorBarLineType=1,
                    pointSize=3,
                    modelPlotSpace=0.4,
                    colorPalette="Paired",
                    modelColors=NULL,
                    valueLabelColor="black",
                    valueLabelSize=4,
                    valueLabelAlpha=1,
                    nsAlpha=1,
                    usePShapes=FALSE,
                    axisColor=NULL, 
                    borderColor=NULL, 
                    interceptLineType=2,
                    interceptLineColor="grey70",
                    majorGridColor=NULL,
                    minorGridColor=NULL,
                    hideGrid.x=FALSE,
                    hideGrid.y=FALSE,
                    theme=NULL,
                    flipCoordinates=TRUE,
                    legendPos="right",
                    legendSize=1,
                    legendBorderColor="white",
                    legendBackColor="white",
                    showIntercept=FALSE,
                    showAxisLabels.y=TRUE,
                    showTickMarks=TRUE,
                    showValueLabels=TRUE, 
                    labelDigits=2,
                    showPValueLabels=TRUE,
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # retrieve list of fitted models
  # --------------------------------------------------------
  input_list <- list(...)
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  # unlist axis labels (predictors)
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
  }
  # unlist labels of dependent variables (legend)
  if (!is.null(labelDependentVariables) && is.list(labelDependentVariables)) {
    labelDependentVariables <- unlistlabels(labelDependentVariables)
  }
  # ----------------------------
  # init final data frame
  # ----------------------------
  finalodds <- c()
  fitlength <- length(input_list)
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sju.wordwrap(axisTitle.x, breakTitleAt)
  }
  # check length of dependent variables
  if (!is.null(labelDependentVariables)) {
    labelDependentVariables <- sju.wordwrap(labelDependentVariables, breakLegendAt)
  }
  else {
    # else if we have no labels of dependent variables supplied, use a 
    # default string (Model) for legend
    labelDependentVariables <- c(sprintf("%s %i", stringModel, 1:fitlength))
  }
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)
  }
  # ----------------------------
  # iterate all fitted models
  # ----------------------------
  for (fitcnt in 1:fitlength) {
    # retrieve fitted model
    fit <- input_list[[fitcnt]]
    # ----------------------------
    # retrieve odds ratios (glm) 
    # ----------------------------
    # create data frame for ggplot
    odds <- data.frame(cbind(exp(coef(fit)), exp(confint(fit))))
    # ----------------------------
    # print p-values in bar charts
    # ----------------------------
    # retrieve sigificance level of independent variables (p-values)
    pv <- unname(coef(summary(fit))[,4])
    # for better readability, convert p-values to asterisks
    # with:
    # p < 0.001 = ***
    # p < 0.01 = **
    # p < 0.05 = *
    # retrieve odds ratios
    ov <- exp(coef(fit))
    # "ps" holds the p-value of the coefficients, including asterisks, as
    # string vector
    ps <- NULL
    # point shapes indicate different shapes for geom_point, according to
    # the p-level
    pointshapes <- NULL
    # palpha indicates whether a coefficient is significant or not.
    # non-significant values can be drawn with a lesser alpha-level
    # (i.e. are more transparent)
    palpha <- NULL
    for (i in 1:length(pv)) {
      ps[i] <- c("")
      pointshapes[i] <- 1
      palpha[i] <- "s"
    }
    # ----------------------------
    # copy OR-values into data column
    # ----------------------------
    if (showValueLabels) {
      for (i in 1:length(pv)) {
        ps[i] <- sprintf("%.*f", labelDigits, ov[i])
      }
    }
    # ----------------------------
    # copy p-values into data column
    # ----------------------------
    for (i in 1:length(pv)) {
      if (pv[i]>=0.05) {
        pointshapes[i] <- 1
        palpha[i] <- "ns"
      }
      else if (pv[i]>=0.01 && pv[i]<0.05) {
        if (showPValueLabels) {
          ps[i] <- paste(ps[i], "*")
        }
        pointshapes[i] <- 2
      }
      else if (pv[i]>=0.001 && pv[i]<0.01) {
        if (showPValueLabels) {
          ps[i] <- paste(ps[i], "**")
        }
        pointshapes[i] <- 3
      }
      else {
        if (showPValueLabels) {
          ps[i] <- paste(ps[i], "***")
        }
        pointshapes[i] <- 4
      }
    }  
    # ----------------------------
    # check if user defined labels have been supplied
    # if not, use variable names from data frame
    # ----------------------------
    if (is.null(axisLabels.y)) {
      axisLabels.y <- row.names(odds)
      #remove intercept from labels
      if (!showIntercept) {
        axisLabels.y <- axisLabels.y[-1]
      }
    }
    # ----------------------------
    # bind p-values to data frame
    # ----------------------------
    odds <- cbind(odds, ps, palpha, pointshapes, fitcnt)
    # set column names
    names(odds) <- c("OR", "lower", "upper", "p", "pa", "shape", "grp")
    # set x-position
    odds$xpos <- cbind(c(nrow(odds):1))
    odds$xpos <- as.factor(odds$xpos)
    #remove intercept from df
    if (!showIntercept) {
      odds <- odds[-1,]
    }
    # add data frame to final data frame
    finalodds <- rbind(finalodds, odds)
  }
  # convert to factor
  finalodds$xpos <- as.factor(finalodds$xpos)
  finalodds$grp <- as.factor(finalodds$grp)
  # convert to character
  finalodds$shape <- as.character(finalodds$shape)
  # reverse axislabel order, so predictors appear from top to bottom
  # as they appear in the console when typing "summary(fit)"
  axisLabels.y <- rev(axisLabels.y)
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user defined range
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    # we have confindence intervals displayed, so
    # the range corresponds to the boundaries given by
    # the CI's
    upper_lim <- (ceiling(10*max(finalodds$upper))) / 10
    lower_lim <- (floor(10*min(finalodds$lower))) / 10
    # if we show p value labels, increase upper
    # limit of x axis, so labels are plotted inside
    # diagram range
    if (showValueLabels || showPValueLabels) {
      upper_lim <- upper_lim + 0.1
    }
    # give warnings when auto-limits are very low/high
    if ((lower_lim < 0.1) || (upper_lim > 100)) {
      warning("Exp. coefficients and/or exp. confidence intervals may be out of printable bounds. Consider using \"axisLimits\" parameter!")
    }
  }
  else {
    # Here we have user defind axis range
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # --------------------------------------------------------
  # Define axis ticks, i.e. at which position we have grid
  # bars.
  # --------------------------------------------------------
  ticks<-c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  # since the odds are plotted on a log-scale, the grid bars'
  # distance shrinks with higher odds values. to provide a visual
  # proportional distance of the grid bars, we can apply the
  # exponential-function on the tick marks
  if (transformTicks) {
    ticks <- exp(ticks)-1
    ticks <- round(ticks[which(ticks<=upper_lim)],1)
  }
  # --------------------------------------------------------
  # define bar / line colors
  # --------------------------------------------------------
  # check whether modelColors is defined
  if (is.null(modelColors)) {
    # define default colours. we use a diverging palette here,
    # because the different models are not "sequential"
    barcols <- brewer_pal(type="div", palette="Dark2")(fitlength)
  }
  else {
    # if we have b/w colors, i.e. no differentiation between odds > 1 and < 1,
    # we simply set both colors for ORs lower and greater than 1 to the same color-value
    if (modelColors=="bw" || modelColors=="black") {
      barcols <- rep(c("#333333"), times=fitlength)
    }
    # grey-scale colors
    else if (modelColors=="gray" || modelColors=="grey" || modelColors=="gs") {
      barcols <- brewer_pal(palette="Greys")(fitlength)
    }
    else {
      # else, use user-colors
      barcols <- modelColors
    }
  }
  # check whether we have brewer color scale
  if (!is.null(modelColors) && modelColors=="brewer") {
    # remember to specify the "colorPalette" if you use "brewer" as "oddsColorss"
    scalecolors <- scale_colour_brewer(palette=colorPalette, labels=labelDependentVariables)
  }
  else {
    scalecolors <- scale_colour_manual(values=barcols, labels=labelDependentVariables)
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
  # Set up visibility oftick marks
  # --------------------------------------------------------
  if (!showTickMarks && !is.null(ggtheme)) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  # --------------------------------------------------------
  # body of plot
  # --------------------------------------------------------
  # The order of aesthetics matters in terms of ordering the error bars!
  # Using alpha-aes before colour would order error-bars according to
  # alpha-level instead of colour-aes.
  plotHeader <- ggplot(finalodds, aes(y=OR, x=xpos, colour=grp, alpha=pa))
  # --------------------------------------------------------
  # start with dot-plotting here
  # first check, whether user wants different shapes for
  # different p-levels
  # --------------------------------------------------------
  if (usePShapes) {
    plotHeader <- plotHeader +
      # set shape aesthetic. we have to repeat the other aesthestics as well,
      # because otherwise the order of point shapes differes from the order
      # of error bars.
      # The order of aesthetics matters in terms of ordering the error bars!
      # Using shape before colour would order points according to shapes instead
      # of colour-aes.
      geom_point(aes(y=OR, x=xpos, colour=grp, shape=shape), size=pointSize, position=position_dodge(-modelPlotSpace)) +
      # and use a shape scale, in order to have a legend
      scale_shape_manual(values=c(1,16,17,15), labels=c("n.s.", "*", "**", "***"))
  }
  else {
    plotHeader <- plotHeader +
      geom_point(size=pointSize, position=position_dodge(-modelPlotSpace))
  }
  # --------------------------------------------------------
  # continue with errorbars, p-value-label and intercept line
  # --------------------------------------------------------
  plotHeader <- plotHeader +
    # print confidence intervalls (error bars)
    geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(-modelPlotSpace), width=errorBarWidth, size=errorBarSize, linetype=errorBarLineType) +
    # print value labels and p-values
    geom_text(aes(label=p, y=upper), size=valueLabelSize, position=position_dodge(width=-modelPlotSpace), hjust=-0.1) +
    # Intercept-line
    geom_hline(yintercept=1, linetype=interceptLineType, color=interceptLineColor) +
    labs(title=title, x=NULL, y=axisTitle.x, shape=legendPValTitle, colour=legendDepVarTitle) +
    scale_x_discrete(labels=axisLabels.y) +
    # logarithmic scale for odds
    scale_y_log10(limits=c(lower_lim, upper_lim), breaks=ticks, labels=ticks) +
    # add colour legend to indicate different fitted models
    scalecolors +
    # use transparancy if requested, but hide legend
    scale_alpha_manual(values=c(nsAlpha,1.0), guide="none")
  # --------------------------------------------------------
  # apply theme
  # --------------------------------------------------------
  if (!is.null(ggtheme)) {
    plotHeader <- plotHeader +
      ggtheme +
      # --------------------------------------------------------
      # set axes text and label position etc.
      # --------------------------------------------------------
      theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
            axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor), 
            axis.text.x = element_text(angle=axisLabelAngle.x),
            axis.text.y = element_text(angle=axisLabelAngle.y),
            plot.title = element_text(size=rel(titleSize), colour=titleColor),
            legend.position = legendPos,
            legend.text = element_text(size=rel(legendSize)),
            legend.background = element_rect(colour=legendBorderColor, fill=legendBackColor))
  }
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (flipCoordinates)  {
    plotHeader <- plotHeader +
      coord_flip()
  }
  # --------------------------------------------------------
  # set border colors
  # --------------------------------------------------------
  if (!is.null(borderColor)) {
    if (!is.null(theme) && theme=="bw") {
      plotHeader <- plotHeader + 
        theme(panel.border = element_rect(colour=borderColor))
    }
    else {
      cat("\nParameter 'borderColor' can only be applied to 'bw' theme.\n")
    }
  }
  if (!is.null(axisColor)) {
    plotHeader <- plotHeader + 
      theme(axis.line = element_line(colour=axisColor))
  }
  if (!is.null(minorgrid)) {
    plotHeader <- plotHeader + 
      theme(panel.grid.minor = minorgrid)
  }
  if (!is.null(majorgrid)) {
    plotHeader <- plotHeader + 
      theme(panel.grid.major = majorgrid)
  }
  if (hideGrid.x) {
    plotHeader <- plotHeader + 
      theme(panel.grid.major.x = hidegrid,
            panel.grid.minor.x = hidegrid)
  }
  if (hideGrid.y) {
    plotHeader <- plotHeader + 
      theme(panel.grid.major.y = hidegrid,
            panel.grid.minor.y = hidegrid)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(plotHeader)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpglmm",
                       list(plot = plotHeader,
                            df = finalodds)))
}
