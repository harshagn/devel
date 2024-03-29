% Generated by roxygen2 (4.0.2): do not edit by hand
\name{sjp.corr}
\alias{sjp.corr}
\title{Plot correlation matrix}
\usage{
sjp.corr(data, title = NULL, titleSize = 1.3, titleColor = "black",
  axisLabels = NULL, type = "circle", sortCorrelations = TRUE,
  decimals = 3, missingDeletion = "listwise", corMethod = "spearman",
  geomAlpha = 0.8, valueLabelColor = "black", valueLabelSize = 4.5,
  valueLabelAlpha = 1, circleSize = 15, outlineColor = "black",
  outlineSize = 1, axisColor = NULL, borderColor = NULL,
  axisLabelSize = 1.1, axisLabelColor = "gray30", axisLabelAngle.x = 0,
  axisLabelAngle.y = 0, breakTitleAt = 50, breakLabelsAt = 12,
  hideDiagCircle = TRUE, hideLegend = TRUE, legendTitle = NULL,
  showCorrelationValueLabels = TRUE, showCorrelationPValues = TRUE,
  pvaluesAsNumbers = FALSE, showTickMarks = FALSE, fillColor = NULL,
  majorGridColor = NULL, minorGridColor = NULL, theme = NULL,
  printPlot = TRUE)
}
\arguments{
\item{data}{A correlation object, built with the R-\code{\link{cor}}-function, or a data frame
which correlations should be calculated.}

\item{title}{Title of the diagram, plotted above the whole diagram panel.}

\item{titleSize}{The size of the plot title. Default is 1.3.}

\item{titleColor}{The color of the plot title. Default is \code{"black"}.}

\item{axisLabels}{Labels for the x- andy y-axis.
axisLabels are detected automatically if \code{data} is a data frame where each variable has
a \code{"variable.label"} attribute (see \code{\link{sji.setVariableLabels}}) for details).}

\item{type}{Indicates whether the geoms of correlation values should be plotted
as \code{"circle"} (default) or as \code{"tile"}.}

\item{sortCorrelations}{If \code{TRUE} (default), the axis labels are sorted
according to the correlation strength. If \code{FALSE}, axis labels
appear in order of how variables were included in the cor-computation or
data frame.}

\item{decimals}{Indicates how many decimal values after comma are printed when
the values labels are shown. Default is 3. Only applies when
\code{showCorrelationValueLabels} is \code{TRUE}.}

\item{missingDeletion}{Indicates how missing values are treated. May be either
\code{"listwise"} (default) or \code{"pairwise"}.}

\item{corMethod}{Indicates the correlation computation method. May be one of
\code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.}

\item{geomAlpha}{Specify the transparancy (alpha value) of geom objects (circles or tiles).
Default is 0.8.}

\item{valueLabelColor}{The color of the value labels (numbers) inside the diagram.
Default is \code{"black"}.}

\item{valueLabelSize}{The size of value labels in the diagram. Default is 4.5, recommended values range
between 2 and 8.}

\item{valueLabelAlpha}{Specify the transparancy (alpha value) of value labels.
Default is 1.}

\item{circleSize}{Specifies the circle size factor. The circle size depends on the correlation
value multiplicated with this factor. Default is 15.}

\item{outlineColor}{Defines the outline color of geoms (circles or tiles). Default is black.}

\item{outlineSize}{Defines the outline size of geoms (circles or tiles). Default is 1.}

\item{axisColor}{User defined color of axis border (y- and x-axis, in case the axes should have different colors than
the diagram border).}

\item{borderColor}{User defined color of whole diagram border (panel border).}

\item{axisLabelSize}{The size of variable labels at the axes. Default is 1.1, recommended values range
between 0.5 and 3.0.}

\item{axisLabelColor}{User defined color for axis labels. If not specified, a default dark gray
color palette will be used for the labels.}

\item{axisLabelAngle.x}{Angle for x-axis-labels.}

\item{axisLabelAngle.y}{Angle for y-axis-labels.}

\item{breakTitleAt}{Wordwrap for diagram title. Determines how many chars of the title are displayed in
one line and when a line break is inserted into the title. Default is 50.}

\item{breakLabelsAt}{Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in
one line and when a line break is inserted. Default is 12.}

\item{hideDiagCircle}{If \code{TRUE} (default), the geoms of the diagonal correlations
(self-correlations with value "1") are not plotted. Only applies if parameter
\code{type} is \code{"circle"}.}

\item{hideLegend}{Show or hide the legend. The legend indicates the strength of correlations
by gradient colour fill. Default is \code{TRUE}, hence the legend is hidden.}

\item{legendTitle}{The legend title, provided as string, e.g. \code{legendTitle=c("Strength of correlation")}.
Default is \code{NULL}, hence no legend title is used.}

\item{showCorrelationValueLabels}{Whether correlation values should be plotted to each geom}

\item{showCorrelationPValues}{Whether significance levels (p-values) of correlations should
be plotted to each geom.}

\item{pvaluesAsNumbers}{If \code{TRUE}, the significance levels (p-values) are printed as numbers.
if \code{FALSE} (default), asterisks are used.}

\item{showTickMarks}{Whether tick marks should be plotted or not. Default is \code{FALSE}.}

\item{fillColor}{A color palette for fillng the geoms. If not specified, the 5th diverging color palette
from the color brewer palettes (RdBu) is used, resulting in red colors for negative and blue colors
for positive correlations, that become lighter the weaker the correlations are. Use any
color palette that is suitbale for the \code{scale_fill_gradientn} parameter of ggplot2.}

\item{majorGridColor}{Specifies the color of the major grid lines of the diagram background.}

\item{minorGridColor}{Specifies the color of the minor grid lines of the diagram background.}

\item{theme}{Specifies the diagram's background theme. Default (parameter \code{NULL}) is a gray
background with white grids.
\itemize{
\item Use \code{"bw"} for a white background with gray grids
\item \code{"classic"} for a classic theme (black border, no grids)
\item \code{"minimal"} for a minimalistic theme (no border,gray grids)
\item \code{"none"} for no borders, grids and ticks or
\item \code{"themr"} if you are using the \code{ggthemr} package
}
See \url{http://rpubs.com/sjPlot/custplot} for details and examples.}

\item{printPlot}{If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
want to plot any graphs. In either case, the ggplot-object will be returned as value.}
}
\value{
(Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
          was used for setting up the ggplot-object (\code{df}) and the original correlation matrix
          (\code{corr.matrix}).
}
\description{
Plot correlations as ellipses or tiles. Required parameter is either
               a data frame or a computed \code{\link{cor}}-object. In case of ellipses, the
               ellipses size indicates the strength of the correlation. Furthermore,
               blue and red colors indicate positive or negative correlations, where
               stronger correlations are darkened.
}
\examples{
# create data frame with 5 random variables
df <- as.data.frame(cbind(rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10)))

# plot correlation matrix using circles
sjp.corr(df)

# plot correlation matrix using square tiles without diagram background
sjp.corr(df, type="tile", theme="none")


# -------------------------------
# Data from the EUROFAMCARE sample dataset
# -------------------------------
data(efc)

# retrieve variable and value labels
varlabs <- sji.getVariableLabels(efc)

# recveive first item of COPE-index scale
start <- which(colnames(efc)=="c83cop2")
# recveive last item of COPE-index scale
end <- which(colnames(efc)=="c88cop7")

# create data frame with COPE-index scale
df <- as.data.frame(efc[,c(start:end)])
colnames(df) <- varlabs[c(start:end)]

# we have high correlations here, because all items
# belong to one factor. See example from "sjp.pca".
sjp.corr(df, type="tile", theme="none", outlineColor="white", hideLegend=FALSE)

# -------------------------------
# auto-detection of labels
# -------------------------------
efc <- sji.setVariableLabels(efc, varlabs)
sjp.corr(efc[,c(start:end)])
}
\references{
\itemize{
             \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
             \item \url{http://strengejacke.wordpress.com/2013/04/18/examples-for-sjplotting-functions-including-correlations-and-proportional-tables-with-ggplot-rstats/}
            }
}
\seealso{
\code{\link{sjt.corr}}
}

