% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmtest_tidiers.R
\name{tidy.coeftest}
\alias{tidy.coeftest}
\title{Tidying methods for coeftest objects}
\usage{
\method{tidy}{coeftest}(x, ...)
}
\arguments{
\item{x}{coeftest object}

\item{...}{extra arguments (not used)}
}
\value{
A \code{data.frame} with one row for each coefficient, with five columns:
  \item{term}{The term in the linear model being estimated and tested}
  \item{estimate}{The estimated coefficient}
  \item{std.error}{The standard error}
  \item{statistic}{test statistic}
  \item{p.value}{p-value}
}
\description{
This tidies the result of a coefficient test, from the \code{coeftest}
function in the \code{lmtest} package.
}
\examples{

if (require("lmtest", quietly = TRUE)) {
    data(Mandible)
    fm <- lm(length ~ age, data=Mandible, subset=(age <= 28))
    
    coeftest(fm)
    tidy(coeftest(fm))
}

}
