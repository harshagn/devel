% Generated by roxygen2 (4.0.2): do not edit by hand
\name{sjt.lm}
\alias{sjt.lm}
\title{Show linear regression as HTML table}
\usage{
sjt.lm(..., file = NULL, labelPredictors = NULL,
  labelDependentVariables = NULL, stringPredictors = "Predictors",
  stringDependentVariables = "Dependent Variables", stringModel = "Model",
  showHeaderStrings = FALSE, stringIntercept = "(Intercept)",
  stringObservations = "Observations", stringB = "B",
  stringSB = "std. Beta", stringCI = "CI", stringSE = "std. Error",
  stringP = "p", showConfInt = TRUE, showStdBeta = FALSE,
  showStdError = FALSE, digits.est = 2, digits.p = 3, digits.ci = 2,
  digits.se = 2, digits.sb = 2, digits.summary = 3,
  pvaluesAsNumbers = FALSE, boldpvalues = TRUE,
  separateConfColumn = FALSE, newLineConf = TRUE, showAbbrHeadline = TRUE,
  showR2 = TRUE, showFStat = FALSE, showAIC = FALSE, cellSpacing = 0.2,
  encoding = "UTF-8", CSS = NULL, useViewer = TRUE, no.output = FALSE)
}
\arguments{
\item{...}{One or more fitted lm-objects.}

\item{file}{The destination file, which will be in html-format. If no filepath is specified,
the file will be saved as temporary file and openend either in the RStudio View pane or
in the default web browser.}

\item{labelPredictors}{Labels of the predictor variables, provided as char vector.}

\item{labelDependentVariables}{Labels of the dependent variables of all fitted models
which have been used as first parameter(s), provided as char vector.}

\item{stringPredictors}{String constant used as headline for the predictor column.
Default is \code{"Predictors"}.}

\item{stringDependentVariables}{String constant used as headline for the
dependent variable columns. Default is \code{"Dependent Variables"}.}

\item{stringModel}{String constant used as headline for the model names in case no
labels for the dependent variables are provided (see labelDependentVariables).
Default is \code{"Model"}.}

\item{showHeaderStrings}{If \code{TRUE}, the header strings \code{stringPredictors}
and \code{stringDependentVariables} are shown. By default, they're hidden.}

\item{stringIntercept}{String constant used as headline for the Intercept row
default is \code{"Intercept"}.}

\item{stringObservations}{String constant used in the summary row for the count of observation
(cases). Default is \code{"Observations"}.}

\item{stringB}{String used for the column heading of beta coefficients. Default is \code{"B"}.}

\item{stringSB}{String used for the column heading of standardized beta coefficients. Default is \code{"std. Beta"}.}

\item{stringCI}{String used for the column heading of confidence interval values. Default is \code{"CI"}.}

\item{stringSE}{String used for the column heading of standard error values. Default is \code{"std. Error"}.}

\item{stringP}{String used for the column heading of p values. Default is \code{"p"}.}

\item{showConfInt}{If \code{TRUE} (default), the confidence intervall is also printed to the table. Use
\code{FALSE} to omit the CI in the table.}

\item{showStdBeta}{If \code{TRUE}, the standardized beta-coefficients are also printed.
Default is \code{FALSE}.}

\item{showStdError}{If \code{TRUE}, the standard errors are also printed.
Default is \code{FALSE}.}

\item{digits.est}{Amount of decimals for estimators.}

\item{digits.p}{Amount of decimals for p-values.}

\item{digits.ci}{Amount of decimals for confidence intervals.}

\item{digits.se}{Amount of decimals for standard error.}

\item{digits.sb}{Amount of decimals for standardized beta.}

\item{digits.summary}{Amount of decimals for values in model summary.}

\item{pvaluesAsNumbers}{If \code{TRUE}, p-values are shown as numbers. If \code{FALSE} (default),
p-values are indicated by asterisks.}

\item{boldpvalues}{If \code{TRUE} (default), significant p-values are shown bold faced.}

\item{separateConfColumn}{if \code{TRUE}, the CI values are shown in a separate table column.
Default is \code{FALSE}.}

\item{newLineConf}{If \code{TRUE} and \code{separateConfColumn} is \code{FALSE}, inserts a line break
between B and CI values. If \code{FALSE}, CI values are printed in the same
line with B values.}

\item{showAbbrHeadline}{If \code{TRUE} (default), the table data columns have a headline with
abbreviations for beta- and std. beta-values, confidence interval and p-values.}

\item{showR2}{If \code{TRUE} (default), the R2 and adjusted R2 values for each model are printed
in the model summary.}

\item{showFStat}{If \code{TRUE}, the F-statistics for each model is printed
in the model summary. Default is \code{FALSE}.}

\item{showAIC}{If \code{TRUE}, the AIC value for each model is printed
in the model summary. Default is \code{FALSE}.}

\item{cellSpacing}{The inner padding of table cells. By default, this value is 0.2 (measure is cm), which is
suitable for viewing the table. Decrease this value (0.05 to 0.1) if you want to import the table
into Office documents. This is a convenient parameter for the \code{CSS} parameter for changing
cell spacing, which would be: \code{CSS=list(css.thead="padding:0.2cm;", css.tzdata="padding:0.2cm;")}.}

\item{encoding}{The charset encoding used for variable and value labels. Default is \code{"UTF-8"}. Change
encoding if specific chars are not properly displayed (e.g.) German umlauts).}

\item{CSS}{A \code{\link{list}} with user-defined style-sheet-definitions, according to the official CSS syntax (see
\url{http://www.w3.org/Style/CSS/}). See return value \code{page.style} for details
of all style-sheet-classnames that are used in this function. Parameters for this list need:
\enumerate{
  \item the class-names with \code{"css."}-prefix as parameter name and
  \item each style-definition must end with a semicolon
}
You can add style information to the default styles by using a + (plus-sign) as
initial character for the parameter attributes. Examples:
\itemize{
  \item \code{css.table='border:2px solid red;'} for a solid 2-pixel table border in red.
  \item \code{css.summary='font-weight:bold;'} for a bold fontweight in the summary row.
  \item \code{css.lasttablerow='border-bottom: 1px dotted blue;'} for a blue dotted border of the last table row.
  \item \code{css.colnames='+color:green'} to add green color formatting to column names.
}
See further examples below and \url{http://rpubs.com/sjPlot/sjtbasics}.}

\item{useViewer}{If \code{TRUE}, the function tries to show the HTML table in the IDE's viewer pane. If
\code{FALSE} or no viewer available, the HTML table is opened in a web browser.}

\item{no.output}{If \code{TRUE}, the html-output is neither opened in a browser nor shown in
the viewer pane and not even saved to file. This option is useful when the html output
should be used in \code{knitr} documents. The html output can be accessed via the return
value.}
}
\value{
Invisibly returns a \code{\link{structure}} with
         \itemize{
           \item the web page style sheet (\code{page.style}),
           \item the web page content (\code{page.content}),
           \item the complete html-output (\code{output.complete}) and
           \item the html-table with inline-css for use with knitr (\code{knitr})
           }
           for further use.
}
\description{
Shows (multiple) fitted linear models (beta coefficients, std. beta values etc.)
               as HTML table, or saves them as file. The fitted lm's should have the same predictor variables and
               differ only in their response (dependent variable).
}
\note{
The HTML tables can either be saved as file and manually opened (specify parameter \code{file}) or
        they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
        or opened with the default web browser. Displaying resp. opening a temporary file is the
        default behaviour (i.e. \code{file=NULL}).
}
\examples{
# Now fit the models. Note that both models share the same predictors
# and only differ in their dependent variable
data(efc)

# fit first model
fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data=efc)
# fit second model
fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data=efc)

# create and open HTML-table in RStudio Viewer Pane or web browser
\dontrun{
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"))}

# show HTML-table, indicating p-values as numbers
\dontrun{
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
       showStdBeta=TRUE, pvaluesAsNumbers=TRUE)}

# create and open HTML-table in RStudio Viewer Pane or web browser,
# printing CI in a separate column
\dontrun{
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
       separateConfColumn=TRUE)}

# show HTML-table, indicating p-values as numbers
# and printing CI in a separate column
\dontrun{
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
       showStdBeta=TRUE, pvaluesAsNumbers=TRUE, separateConfColumn=TRUE)}

# ----------------------------------------------------------------
# connecting two html-tables
# ----------------------------------------------------------------
# fit two more models
fit3 <- lm(tot_sc_e ~ c160age + c12hour + c161sex + c172code, data=efc)
fit4 <- lm(e42dep ~ c160age + c12hour + c161sex + c172code, data=efc)
\dontrun{
# create and save first HTML-table
part1 <- sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
                labelPredictors=c("Carer's Age", "Hours of Care",
                                  "Carer's Sex", "Educational Status"))
# create and save second HTML-table
part2 <- sjt.lm(fit3, fit4, labelDependentVariables=c("Service Usage", "Elder's Dependency"),
                labelPredictors=c("Carer's Age", "Hours of Care",
                                  "Carer's Sex", "Educational Status"))
# browse temporary file
htmlFile <- tempfile(fileext=".html")
write(sprintf("<html><head>\%s</head><body>\%s<p></p>\%s</body></html>",
              part1$page.style, part1$page.content, part2$page.content),
              file=htmlFile)
viewer <- getOption("viewer")
if (!is.null(viewer)) viewer(htmlFile) else utils::browseURL(htmlFile)}

# ----------------------------------------------------------------
# User defined style sheet
# ----------------------------------------------------------------
\dontrun{
sjt.lm(fit1, fit2, labelDependentVariables=c("Barthel-Index", "Negative Impact"),
       labelPredictors=c("Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"),
       CSS=list(css.table="border: 2px solid;",
                css.tdata="border: 1px solid;",
                css.depvarhead="color:#003399;"))}
}
\references{
\itemize{
             \item \url{http://strengejacke.wordpress.com/sjplot-r-package/}
             \item \url{http://strengejacke.wordpress.com/2013/08/20/print-glm-output-to-html-table-rstats/}
             }
}
\seealso{
\code{\link{sjt.glm}} \cr
         \code{\link{sjp.lm}}
}

