% Generated by roxygen2 (4.0.2): do not edit by hand
\name{sju.weight2}
\alias{sju.weight2}
\title{Weight a variable}
\usage{
sju.weight2(var, weights)
}
\arguments{
\item{var}{The (unweighted) variable}

\item{weights}{A vector with same length as \code{var}, which
         contains weight factors. Each value of \code{var} has a
         specific assigned weight in \code{weights}.}
}
\value{
The weighted \code{var}.
}
\description{
This function weights the variable \code{var} by
               a specific vector of \code{weights}. It's an
               alternative weight calculation to \code{\link{sju.weight}},
               where \code{\link{sju.weight}} usage is recommended.
               This function sums up all \code{weights} values of the associated
               categories of \code{var}, whereas the \code{\link{sju.weight}} function
               uses a \code{\link{xtabs}} formula to weight cases. Thus, this function
               may return a value with a different length than that from \code{var}.
}
\note{
The values of the returned vector are in sorted order, whereas the categories
       of the original \code{var} may be spread randomly. Hence, \code{var} can't be
       used, for instance, for further cross tabulation. In case you want to have
       weighted contingency tables or (grouped) box plots etc., use the \code{weightBy}
       parameter of most functions (like in \code{\link{sjt.xtab}} or \code{\link{sjp.grpfrq}}).
}
\examples{
v <- sample(1:4, 20, TRUE)
table(v)
w <- abs(rnorm(20))
table(sju.weight2(v,w))
}
\seealso{
\code{\link{sju.weight}}
}

