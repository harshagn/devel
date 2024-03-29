% Generated by roxygen2 (4.0.2): do not edit by hand
\name{sjc.grpdisc}
\alias{sjc.grpdisc}
\title{Compute a linear discriminant analysis on classified cluster groups}
\usage{
sjc.grpdisc(data, groups, groupcount, showTotalCorrect = TRUE,
  printPlot = TRUE)
}
\arguments{
\item{data}{The data frame containing all variables that should be used for the
check for goodness of classification of a cluster analysis.}

\item{groups}{The group classification of the cluster analysis that was returned
from the \code{\link{sjc.cluster}}-function.}

\item{groupcount}{The amount of groups (clusters) that should be used. Use
the \code{\link{sjc.elbow}}-function to determine the group-count depending
on the elbow-criterion.}

\item{showTotalCorrect}{If \code{TRUE} (default), a vertical line indicating the
overall goodness of classification is added to the plot, so one can see
whether a certain group is below or above the average classification goodness.}

\item{printPlot}{If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
         want to plot any graphs. In either case, the ggplot-object will be returned as value.}
}
\value{
(Invisibly) returns an object with
          \itemize{
           \item \code{data}: the used data frame for plotting,
           \item \code{plot}: the ggplot object,
           \item \code{accuracy}: a vector with the accuracy of classification for each group,
           \item \code{total.accuracy}: the total accuracy of group classification.
          }
}
\description{
Computes linear discriminant analysis on classified cluster groups.
               This function plots a bar graph indicating the goodness of classification
               for each group.
}
\examples{
# retrieve group classification from hierarchical cluster analysis
# on the mtcars data set (5 groups)
groups <- sjc.cluster(mtcars, 5)

# plot goodness of group classificatoin
sjc.grpdisc(mtcars, groups, 5)
}
\seealso{
\code{\link{sjc.dend}} \cr
         \code{\link{sjc.cluster}} \cr
         \code{\link{sjc.elbow}}
}

