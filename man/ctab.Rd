\name{ctab}
\alias{ctab}
\alias{print.ctab}
\alias{summary.ctab}
\title{Percentage tables}
\description{
  Produces one-way, two-way or multi-way percentage tables
}
\usage{
ctab(\dots,dec.places=NULL,digits=NULL,type=NULL,style=NULL,row.vars=NULL,col.vars=NULL,percentages=NULL,addmargins=NULL)

\method{print}{ctab}(x, dec.places=x$dec.places, addmargins=x$addmargins, \dots)

\method{summary}{ctab}(object,\dots)
}
\arguments{
\item{\dots}{either \itemize{
	\item one or more factors,
	\item a class \code{\link{table}}
	\item a class \code{\link{ftable}} table object,
	\item or a class \code{ctab} table object
}}
\item{dec.places}{number of decimal places (default 2)}
\item{digits}{synonym for \code{dec.places}, for compatability with previous version}
\item{type}{Row, column, total percentages or counts (\code{type= n}). Multiple values may be specified as a character vector. Partial matchin is used.}
\item{style}{Applicable if more than one percentage type is specified. If \code{style=long}, percentages are printed underneath each other. If \code{style=wide}, the percentages are printed side by side}
\item{row.vars}{Same as \code{\link{ftable}}: \emph{\dQuote{a vector of integers giving the numbers of the variables, or a character vector giving the names of the variables to be used for the rows of the [] table}}}
\item{col.vars}{\emph{\dQuote{a vector of integers giving the numbers of the variables, or a character vector giving the names of the variables to be used for the columns of the [] table}}}
\item{percentages}{If \code{FALSE}, proportions rather than percentages are printed}
\item{addmargins}{Use \code{addmargins=TRUE} to add subtotals to the table}
\item{x}{is a tables object created by \code{ctab}}
\item{object}{is a tables object created by \code{ctab}}
}
\details{
Options have default NULL so attributes of a ctab object can be used as default.
In other cases, the following options will be used if nothing has been specified:
\preformatted{
ctab(\dots, dec.places=2, digits=dec.places, type=c("n", "row", "column", "total"),
style="long", row.vars=NULL, col.vars=NULL, percentages=TRUE, addmargins=FALSE)
}

\code{ctab} uses \code{\link{ftable}} and \code{\link{prop.table}} to produce one-way frequency tables, two-way crosstables, or multi-way percentage tables. More than one percentage type may be specified, in which case \dQuote{percentage type} is an unnamed dimension of the table. \code{row.vars} and \code{col.vars} can be used to control the layout of multi-way tables using the facilities of \code{ftable}. Subtotals can be added by specifiying \code{addmargins=TRUE}. 

\code{\link[gmodels]{CrossTable}} in the \code{gmodels} package also provides an easy method for producing percentage tables, but is restricted to two-way tables.

If \code{ctab} is specified with no further options and for more than one factor, the output is identical to that of \code{\link{ftable}}. If a single factor is specified, the default is to print the frequencies column-wise with the percentaqes next to them.
}
\value{
An object of class \dQuote{ctab}. \code{print.ctab} prints the table, \code{summary.ctab} passes the frequency table on to \code{\link{summary.table}}, which prints the number of cases, number of factors, and a chi-square test of independence.
\item{table}{A \code{class(table)} object containing the table counts. Used by \code{summary.ctab} and by \code{ctab} itself if a \code{ctab} object is used as input.}
\item{ctab}{A \code{class(ftable)} object containing the percentage types specified. This is printed by \code{print.ctab}.}
\item{row.vars}{The \code{row.vars} options as numeric vectors}
\item{col.vars}{The \code{col.vars} options as numeric vectors}
\item{dec.places}{The \code{dec.places} option}
\item{type}{The \code{type} option}
\item{style}{The \code{style} option}
\item{percentages}{The \code{percentages} option}
\item{addmargins}{The \code{addmargins} option}
}
\references{\url{http://home.wanadoo.nl/john.hendrickx/statres/}}
\author{John Hendrickx <\email{John_Hendrickx@yahoo.com}>}
\seealso{\code{\link{table}}, \code{\link{ftable}}, \code{\link{addmargins}}, \code{\link{prop.table}}, \code{\link{xtabs}}, \code{[gmodels]}\code{\link[gmodels]{CrossTable}}
}

\examples{
ftable(Titanic)
ctab(Titanic) # same output
ctab(Titanic,type="r")
ctab(Titanic,type=c("n","r"),addmargins=TRUE)
ctab(Titanic,type=c("n","c","t","r"),style="w")
mytab<-ftable(Titanic,row.vars=c(1,3),type="r")
mytab
ctab(mytab)
newtab<-ctab(mytab,type="r")
newtab
summary(newtab)

#second example using a data frame rather than table data
library(survival)
data(logan)
attach(logan)
class(logan) #"data.frame"
ctab(occupation)
ctab(occupation,addmargins=TRUE)
ctab(occupation,style="w",type="c")
ctab(occupation,style="l",type="n")
z<-ctab(occupation,addmargins=TRUE,style="l")
z
print(z,addmargins=FALSE,dec.places=5)
summary(z)

t<-ctab(focc,occupation,type=c("n","r","c"))
t
summary(t)
}
\keyword{category}

