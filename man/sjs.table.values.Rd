% Generated by roxygen2 (4.0.2): do not edit by hand
\name{sjs.table.values}
\alias{sjs.table.values}
\title{Compute table's values}
\usage{
sjs.table.values(tab, digits = 2)
}
\arguments{
\item{tab}{A simple \code{\link{table}} or \code{\link{ftable}} of which cell, row and column percentages
as well as expected values are calculated. Tables of class \code{\link{xtabs}} and other will
be coerced to \code{\link{ftable}} objects.}

\item{digits}{The amount of digits for the table percentage values.}
}
\value{
(invisibly) returns a list with four tables:
        \enumerate{
         \item \code{cell} a table with cell percentages of \code{tab}
         \item \code{row} a table with row percentages of \code{tab}
         \item \code{col} a table with column percentages of \code{tab}
         \item \code{expected} a table with expected values of \code{tab}
        }
}
\description{
This function calculates a table's cell, row and column percentages as
               well as expected values and returns all results as lists of tables.
}
\examples{
tab <- table(sample(1:2, 30, TRUE), sample(1:3, 30, TRUE))
# show expected values
sjs.table.values(tab)$expected
# show cell percentages
sjs.table.values(tab)$cell
}
\seealso{
\code{\link{sjs.phi}} \cr
         \code{\link{sjs.cramer}}
}

