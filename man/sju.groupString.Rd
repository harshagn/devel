% Generated by roxygen2 (4.0.2): do not edit by hand
\name{sju.groupString}
\alias{sju.groupString}
\title{Group near elements of string vectors}
\usage{
sju.groupString(strings, maxdist = 3, method = "lv", strict = FALSE,
  trim.whitespace = TRUE, remove.empty = TRUE, showProgressBar = FALSE)
}
\arguments{
\item{strings}{a character vector with string elements}

\item{maxdist}{the maximum distance between two string elements, which is allowed to treat two
elements as similar or equal.}

\item{method}{Method for distance calculation. The default is \code{"lv"}. See \code{stringdist} package for details.}

\item{strict}{if \code{TRUE}, value matching is more strictly. See examples for details.}

\item{trim.whitespace}{if \code{TRUE} (default), leading and trailing white spaces will
be removed from string values.}

\item{remove.empty}{if \code{TRUE} (default), empty string values will be removed from the
character vector \code{strings}.}

\item{showProgressBar}{If \code{TRUE}, the progress bar is displayed when computing the distance matrix.
         Default in \code{FALSE}, hence the bar is hidden.}
}
\value{
A character vector where similar string elements (values) are recoded into a new, single value.
}
\description{
This function groups elements of a string vector (character or string variable) according
               to the element's distance. The more similar two string elements are, the higher is the
               chance to be combined into a group.
}
\examples{
\dontrun{
oldstring <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
newstring <- sju.groupString(oldstring)
sjt.frq(data.frame(oldstring, newstring), removeStringVectors = FALSE, autoGroupStrings = FALSE)

newstring <- sju.groupString(oldstring, strict = TRUE)
sjt.frq(data.frame(oldstring, newstring), removeStringVectors = FALSE, autoGroupStrings = FALSE)}
}

