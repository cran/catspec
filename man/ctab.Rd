\name{ctab}
\alias{ctab}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Percentage tables}
\description{
  Prints one-way, two-way or multiway percentage tables
}
\usage{
ctab(..., digits = 2, type = c("n", "row", "column", "total"),
row.vars = NULL, col.vars = NULL, percentages = TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{\dots}{one or more factors or a table object of class \code{table}, \code{xtabs}, or
  \code{ftable}}
  \item{digits}{number of decimal places (default 2)}
  \item{type}{Row, column, total percentages or counts (\code{type=n})}
  \item{row.vars}{}
  \item{col.vars}{Same as \code{\link{ftable}}}
  \item{percentages}{If \code{FALSE}, proportions rather than percentages are printed}
}
\details{
  Simplifies the production of (multiway) percentage tables.
}
\value{
  For one-way and two-way tables, a \code{table} object is returned. For three-way and higher tables,
  an \code{ftable} object is returned.
}
\references{\url{http://www.xs4all.nl/~jhckx/}}
\author{John Hendrickx <\email{John\_Hendrickx@yahoo.com}>}
\note{
If the table contains three or more factors, the first is treated by default as the row variable, the
second as the column variable, the third and following as grouping variables. This can be overridden
using \code{row.vars} and/or \code{col.vars}. Note that  \code{\link[pkg:gregmisc]{CrossTable}} in
the \code{gregmisc} package also provides an easy method for producing percentage tables but only for
two-way tables.
}

\seealso{\code{\link{table}}, \code{\link{ftable}}, \code{\link{xtabs}}, \code{\link{prop.table}},
\code{\link[pkg:gregmisc]{CrossTable}}
}

\examples{
data(Titanic)
ctab(Titanic)
ctab(Titanic,type="r")
ctab(Titanic,row.vars=1:3,type="r")
ctab(Titanic,col.vars=c(2,4),type="r")
ctab(Titanic,row.vars=c(1,3),type="c")
ctab(Titanic,col.vars=c("Sex","Survived"),type="r")
ctab(Titanic,col.vars=c("Sex","Survived"),type="t")
}
\keyword{category}
