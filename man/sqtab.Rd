\name{sqtab}
\alias{sqtab}
\alias{check.square}
\alias{fitmacro}
\alias{mob.cp}
\alias{mob.eqmain}
\alias{mob.qi}
\alias{mob.rc1}
\alias{mob.symint}
\alias{mob.unif}
%- Also NEED an `\alias' for EACH other topic documented here.

\title{sqtab: models for square tables}

\description{
  These functions are used to estimate loglinear models for square tables such as quasi-independence,
  quasi-symmetry.
}

\usage{
mob.qi(rowvar, colvar, constrained = FALSE, print.labels = FALSE)
mob.eqmain(rowvar, colvar, print.labels = FALSE)
mob.symint(rowvar, colvar, print.labels = FALSE)
mob.cp(rowvar,colvar)
mob.unif(rowvar, colvar)
mob.rc1(rowvar, colvar, equal = FALSE, print.labels = FALSE)
fitmacro(object)
check.square(rowvar, colvar, equal = TRUE)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{rowvar}{Factor representing the row variable}
  \item{colvar}{Factor representing the column variable}
  \item{print.labels}{If FALSE (default) then numeric values rather than factor values are printed for compact results}
  \item{equal}{(\code{mob.rc1}) If TRUE, a homogeneous row and column effects model 1 with equal scale values for the row and column variables is estimated. Otherwise, a (regular) RC1 model is estimated with different scale values for the row and column variables
  \cr
  (\code{check.square}) If TRUE, the row and column variables must have the same number of categories}
  \item{constrained}{(\code{mob.qi})If TRUE, a quasi-independence model-constrained is estimated with a single parameter for the diagonal cells}
  \item{object}{(\code{fitmacro}) An object of class \code{glm} for \code{family=poisson} and \code{link=log}}
}
\details{
  These functions are used to estimate loglinear models for square tables:
  \item{mob.qi}{Quasi-independence}
  \item{mob.eqmain}{Equal main effects (Hope's halfway model)}
  \item{mob.symint}{Symmetric interaction}
  \item{mob.cp}{Crossings-parameter model}
  \item{mob.unif}{Uniform association}
  \item{mob.rc1}{Row and columns model 1}
  \item{fitmacro}{Calculates \code{BIC} and \code{AIC} relative to a saturated loglinear model}
  \item{check.square}{Internal function to check if row and column variables are both factors with the same number of levels}

}
\value{
  \item{mob.qi}{A factor that will produce coefficients for the diagonal cells of a table, using off-
  diagonal cells as base category}
  \item{mob.eqmain}{A design matrix with equality contraints on the main effects}
  \item{mob.symint}{A design matrix for an interaction with equality contraints on coefficients on
  opposite sides of the diagonal}
  \item{mob.cp}{A set of vectors for a crossings-parameter model}
  \item{mob.unif}{A vector for a uniform association model}
  \item{mob.rc1}{A set of vectors for a row and columns model 1}
  \item{fitmacro}{Prints deviance, df, BIC, AIC, number of parameters and N}
  \item{check.square}{Stops function if either the row or column variable is not a factor or if the
  number of levels is unequal}
}
\references{
  Hout, Michael. (1983). \emph{Mobility Tables}. Sage Publication 07-031.

  Goodman, Leo A. (1984). \emph{The analysis of cross-classified data having ordered categories.} Cambridge, Mass.: Harvard University Press.
}
\author{John Hendrickx <\email{John\_Hendrickx@yahoo.com}>}

\seealso{\code{\link{glm}}, \code{\link{mclgen}}}

\examples{
# Examples of loglinear models for square tables,
# from Hout, M. (1983). "Mobility Tables". Sage Publication 07-031

# Table from page 11 of "Mobility Tables"
# Original source: Featherman D.L., R.M. Hauser. (1978) "Opportunity and Change."
# New York: Academic, page 49

data(FHtab)
attach(FHtab)

xtabs(Freq ~ .,FHtab)

# independence model
indep<-glm(Freq~OccFather+OccSon,family=poisson())
summary(indep)
fitmacro(indep)

wt <- (OccFather != OccSon)
qi0<-glm(Freq~OccFather+OccSon,weights=wt,family=poisson())
# A quasi-independence loglinear model, using structural zeros
# (page 23 of "Mobility Tables").
#  0  1  1  1  1   values of variable "wt"
#  1  0  1  1  1
#  1  1  0  1  1
#  1  1  1  0  1
#  1  1  1  1  0
qi0<-glm(Freq~OccFather+OccSon,weights=wt,family=poisson())
summary(qi0)
fitmacro(qi0)

# Quasi-independence using a "dummy factor" to create the design
# vectors for the diagonal cells (page 23).
#  1  0  0  0  0
#  0  2  0  0  0
#  0  0  3  0  0
#  0  0  0  4  0
#  0  0  0  0  5
glm.qi<-glm(Freq~OccFather+OccSon+mob.qi(OccFather,OccSon),family=poisson())
summary(glm.qi)
fitmacro(glm.qi)

# Quasi-independence constrained (QPM-C, page 31)
# Single immobility parameter
#  1  0  0  0  0
#  0  1  0  0  0
#  0  0  1  0  0
#  0  0  0  1  0
#  0  0  0  0  1
glm.q0<-glm(Freq~OccFather+OccSon+mob.qi(OccFather,OccSon,constrained=TRUE),family=poisson())
# slightly different results than Hout also found in Stata: L2=2567.658, q0=0.964
summary(glm.q0)
fitmacro(glm.q0)

# Quasi-symmetry using the symmetric cross-classification (page 23)
#  0  1  2  3  4   values of variable "sym"
#  1  0  5  6  7
#  2  5  0  8  9
#  3  6  8  0 10
#  4  7  9 10  0  */
glm.qsym<-glm(Freq~OccFather+OccSon+mob.symint(OccFather,OccSon),family=poisson())
summary(glm.qsym)
fitmacro(glm.qsym)

symmetry<-glm(Freq~mob.eqmain(OccFather,OccSon)+mob.symint(OccFather,OccSon),family=poisson())
summary(symmetry)
fitmacro(symmetry)

# Crossings parameter model (page 35)
#  0  v1 v1 v1 v1 |  0  0  v2 v2 v2 |  0  0  0  v3 v3 |  0  0  0  0  v4
#  v1 0  0  0  0  |  0  0  v2 v2 v2 |  0  0  0  v3 v3 |  0  0  0  0  v4
#  v1 0  0  0  0  |  v2 v2 0  0  0  |  0  0  0  v3 v3 |  0  0  0  0  v4
#  v1 0  0  0  0  |  v2 v2 0  0  0  |  v3 v3 v3 0  0  |  0  0  0  0  v4
#  v1 0  0  0  0  |  v2 v2 0  0  0  |  v3 v3 v3 0  0  |  v4 v4 v4 v4 0
glm.cp<-glm(Freq~OccFather+OccSon+mob.cp(OccFather,OccSon),family=poisson())
summary(glm.cp)
fitmacro(glm.cp)

# Uniform association model: linear by linear association (page 58)
glm.unif<-glm(Freq~OccFather+OccSon+mob.unif(OccFather,OccSon),family=poisson())
summary(glm.unif)
fitmacro(glm.unif)

# RC model 1 (unequal row and column effects, page 58)
# Fits a uniform association parameter and row and column effect
# parameters. Row and column effect parameters have the
# restriction that the first and last categories are zero.
glm.rc1<-glm(Freq~OccFather+OccSon+mob.rc1(OccFather,OccSon),family=poisson())
summary(glm.rc1)
fitmacro(glm.rc1)

# Homogeneous row and column effects model 1 (page 58)
# An equality restriction is placed on the row and column effects
glm.hrc1<-glm(Freq~OccFather+OccSon+mob.rc1(OccFather,OccSon,equal=TRUE),family=poisson())
# Results differ from those in Hout, replicated by other programs
summary(glm.hrc1)
fitmacro(glm.hrc1)
}

\keyword{multivariate}
