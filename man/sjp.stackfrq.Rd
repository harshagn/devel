% Generated by roxygen2 (4.0.2): do not edit by hand
\name{sjp.stackfrq}
\alias{sjp.stackfrq}
\title{Plot stacked proportional bars}
\usage{
sjp.stackfrq(items, legendLabels = NULL, orderBy = NULL, weightBy = NULL,
  weightByTitleString = NULL, hideLegend = FALSE, reverseOrder = TRUE,
  title = NULL, titleSize = 1.3, titleColor = "black",
  legendTitle = NULL, includeN = TRUE, axisLabels.y = NULL,
  axisLabelSize = 1.1, axisLabelAngle.x = 0, axisLabelColor = "gray30",
  valueLabelSize = 4, valueLabelColor = "black", breakTitleAt = 50,
  breakLabelsAt = 30, breakLegendTitleAt = 30, breakLegendLabelsAt = 28,
  gridBreaksAt = 0.2, expand.grid = FALSE, barWidth = 0.5,
  barColor = NULL, colorPalette = "GnBu", barAlpha = 1,
  borderColor = NULL, axisColor = NULL, barOutline = FALSE,
  barOutlineSize = 0.2, barOutlineColor = "black", majorGridColor = NULL,
  minorGridColor = NULL, hideGrid.x = FALSE, hideGrid.y = FALSE,
  axisTitle.x = NULL, axisTitle.y = NULL, axisTitleColor = "black",
  axisTitleSize = 1.3, theme = NULL, showTickMarks = FALSE,
  showValueLabels = TRUE, showPercentageAxis = TRUE,
  jitterValueLabels = FALSE, showItemLabels = TRUE,
  showSeparatorLine = FALSE, separatorLineColor = "grey80",
  separatorLineSize = 0.3, legendPos = "right", legendSize = 1,
  legendBorderColor = "white", legendBackColor = "white",
  flipCoordinates = TRUE, printPlot = TRUE)
}
\arguments{
\item{items}{A data frame with each column representing one likert-item.}

\item{legendLabels}{A list or vector of strings that indicate the likert-scale-categories and which
appear as legend text.}

\item{orderBy}{Indicates whether the \code{items} should be ordered by highest count of first or last category of \code{items}.
Use \code{"first"} to order ascending by lowest count of first category,
\code{"last"} to order ascending by lowest count of last category
or \code{NULL} (default) for no sorting.
In case you want to revers order (descending from highest count), use
\code{reverseOrder} parameter.}

\item{weightBy}{A weight factor that will be applied to weight all cases from \code{items}.}

\item{weightByTitleString}{If a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
cases are weighted. Use a string as parameter, e.g.: \code{weightByTitleString=" (weighted)"}.}

\item{hideLegend}{Indicates whether legend (guide) should be shown or not.}

\item{reverseOrder}{If \code{TRUE}, the item order on the x-axis is reversed.}

\item{title}{Title of the diagram, plotted above the whole diagram panel.}

\item{titleSize}{The size of the plot title. Default is 1.3.}

\item{titleColor}{The color of the plot title. Default is \code{"black"}.}

\item{legendTitle}{Title of the diagram's legend.}

\item{includeN}{If \code{TRUE} (default), the N of each item is included into axis labels.}

\item{axisLabels.y}{Labels for the y-axis (the labels of the \code{items}). These parameters must
be passed as list! Example: \code{axisLabels.y=list(c("Q1", "Q2", "Q3"))}
Axis labels will automatically be detected, when they have
a \code{"variable.lable"} attribute (see \code{\link{sji.setVariableLabels}}) for details).}

\item{axisLabelSize}{The size of category labels at the axes. Default is 1.1, recommended values range
between 0.5 and 3.0}

\item{axisLabelAngle.x}{Angle for axis-labels.}

\item{axisLabelColor}{User defined color for axis labels. If not specified, a default dark gray
color palette will be used for the labels.}

\item{valueLabelSize}{The size of value labels in the diagram. Default is 4, recommended values range
between 2 and 8}

\item{valueLabelColor}{The color of value labels in the diagram. Default is black.}

\item{breakTitleAt}{Wordwrap for diagram title. Determines how many chars of the title are displayed in
one line and when a line break is inserted into the title.}

\item{breakLabelsAt}{Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in
one line and when a line break is inserted.}

\item{breakLegendTitleAt}{Wordwrap for diagram legend title. Determines how many chars of the legend's title
are displayed in one line and when a line break is inserted.}

\item{breakLegendLabelsAt}{Wordwrap for diagram legend labels. Determines how many chars of the legend labels are
displayed in one line and when a line break is inserted.}

\item{gridBreaksAt}{Sets the breaks on the y axis, i.e. at every n'th position a major
grid is being printed. Valid values range from 0 to 1.}

\item{expand.grid}{If \code{TRUE} (default), the diagram has margins, i.e. the y-axis is not exceeded
to the diagram's boundaries.}

\item{barWidth}{Width of bars. Recommended values for this parameter are from 0.4 to 1.5}

\item{barColor}{User defined color for bars.
If not specified (\code{NULL}), a default blue color palette will be used
for the bar charts. You can use pre-defined color-sets that are independent from the amount of categories:
If barColor is \code{"brewer"}, use the \code{colorPalette} parameter to specify a palette of the color brewer
Else specify your own color values as vector (e.g. \code{barColor=c("darkred", "red", "green", "darkgreen")})}

\item{colorPalette}{If \code{barColor} is \code{"brewer"}, specify a color palette from the color brewer here. All color brewer
palettes supported by ggplot are accepted here.}

\item{barAlpha}{Specify the transparancy (alpha value) of bars.}

\item{borderColor}{User defined color of whole diagram border (panel border).}

\item{axisColor}{User defined color of axis border (y- and x-axis, in case the axes should have different colors than
the diagram border).}

\item{barOutline}{If \code{TRUE}, each bar gets a colored outline. Default is \code{FALSE}.}

\item{barOutlineSize}{The size of the bar outlines. Only applies if \code{barOutline} is \code{TRUE}.
Default is 0.2}

\item{barOutlineColor}{The color of the bar outline. Only applies, if \code{barOutline} is set to \code{TRUE}.}

\item{majorGridColor}{Specifies the color of the major grid lines of the diagram background.}

\item{minorGridColor}{Specifies the color of the minor grid lines of the diagram background.}

\item{hideGrid.x}{If \code{TRUE}, the x-axis-gridlines are hidden. Default if \code{FALSE}.}

\item{hideGrid.y}{If \code{TRUE}, the y-axis-gridlines are hidden. Default if \code{FALSE}.}

\item{axisTitle.x}{A label for the x axis. Useful when plotting histograms with metric scales where no category labels
are assigned to the x axis.}

\item{axisTitle.y}{A label for the y axis. Useful when plotting histograms with metric scales where no category labels
are assigned to the y axis.}

\item{axisTitleColor}{The color of the x and y axis labels. Refers to \code{axisTitle.x} and \code{axisTitle.y},
not to the tick mark or category labels.}

\item{axisTitleSize}{The size of the x and y axis labels. Refers to \code{axisTitle.x} and \code{axisTitle.y},
not to the tick mark or category labels.}

\item{theme}{Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray
background with white grids.
\itemize{
\item Use \code{"bw"} for a white background with gray grids
\item \code{"classic"} for a classic theme (black border, no grids)
\item \code{"minimal"} for a minimalistic theme (no border,gray grids)
\item \code{"none"} for no borders, grids and ticks or
\item \code{"themr"} if you are using the \code{ggthemr} package (in such cases, you may use the \code{ggthemr::swatch} function to retrieve theme-colors for the \code{barColor} parameter)
}
See \url{http://rpubs.com/sjPlot/custplot} for details and examples.}

\item{showTickMarks}{Whether tick marks of axes should be shown or not.}

\item{showValueLabels}{Whether counts and percentage values should be plotted to each bar.}

\item{showPercentageAxis}{If \code{TRUE} (default), the percentage values at the x-axis are shown.}

\item{jitterValueLabels}{If \code{TRUE}, the value labels on the bars will be "jittered", i.e. they have
alternating vertical positions to avoid overlapping of labels in case bars are
very short. Default is \code{FALSE}.}

\item{showItemLabels}{Whether x axis text (category names) should be shown or not.}

\item{showSeparatorLine}{If \code{TRUE}, a line is drawn to visually "separate" each bar in the diagram.}

\item{separatorLineColor}{The color of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}.}

\item{separatorLineSize}{The size of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}.}

\item{legendPos}{The position of the legend. Default is \code{"right"}. Use one of the following values:
\code{"right"}, \code{"left"}, \code{"bottom"}, \code{"top"}.}

\item{legendSize}{The size of the legend.}

\item{legendBorderColor}{The border color of the legend.}

\item{legendBackColor}{The background color of the legend.}

\item{flipCoordinates}{If \code{TRUE}, the x and y axis are swapped.}

\item{printPlot}{If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
want to plot any graphs. In either case, the ggplot-object will be returned as value.}
}
\value{
(Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
          was used for setting up the ggplot-object (\code{df}).
}
\description{
Plot items (variables) of a scale as stacked proportional bars. This
               function is useful when several items with identical scale/categoroies
               should be plotted to compare the distribution of answers.
}
\note{
Thanks to Forrest Stevens (\url{http://www.clas.ufl.edu/users/forrest/}) for bug fixes
}
\examples{
# -------------------------------
# random sample
# -------------------------------
# prepare data for 4-category likert scale, 5 items
likert_4 <- data.frame(as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.2,0.3,0.1,0.4))),
                       as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.5,0.25,0.15,0.1))),
                       as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.25,0.1,0.4,0.25))),
                       as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.1,0.4,0.4,0.1))),
                       as.factor(sample(1:4, 500, replace=TRUE, prob=c(0.35,0.25,0.15,0.25))))
# create labels
levels_4 <- list(c("Independent", "Slightly dependent", "Dependent", "Severely dependent"))

# create item labels
items <- list(c("Q1", "Q2", "Q3", "Q4", "Q5"))

# plot stacked frequencies of 5 (ordered) item-scales
sjp.stackfrq(likert_4, legendLabels=levels_4, axisLabels.y=items)


# -------------------------------
# Data from the EUROFAMCARE sample dataset
# -------------------------------
data(efc)

# recveive first item of COPE-index scale
start <- which(colnames(efc)=="c82cop1")

# recveive first item of COPE-index scale
end <- which(colnames(efc)=="c90cop9")

# retrieve variable and value labels
varlabs <- sji.getVariableLabels(efc)
vallabs <- sji.getValueLabels(efc)

# create value labels. We need just one variable of
# the COPE-index scale because they have all the same
# level / categorie / value labels
levels <- vallabs['c82cop1']

# create item labels
items <- list(varlabs[c(start:end)])

sjp.stackfrq(efc[,c(start:end)], legendLabels=levels,
             axisLabels.y=items, jitterValueLabels=TRUE)

# -------------------------------
# auto-detection of labels
# -------------------------------
efc <- sji.setVariableLabels(efc, varlabs)
sjp.stackfrq(efc[,c(start:end)])
}
\references{
\itemize{
             \item \url{http://rpubs.com/sjPlot/sjpstackfrq}
             \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
             }
}
\seealso{
\code{\link{sjp.likert}} \cr
         \code{\link{sjt.stackfrq}}
}

