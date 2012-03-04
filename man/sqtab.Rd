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
These functions are used to estimate loglinear models for square tables such as quasi-independence, quasi-symmetry. Examples show how these models can be incorporated in a multinomial logistic model with covariates.
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
	\item{print.labels}{If FALSE (default) then numeric values rather
	than factor values are printed for compact results}
	\item{equal}{(\code{mob.rc1}) If TRUE, a homogeneous row and column effects model 1 with equal scale values for the row and column variables is estimated. Otherwise, a (regular) RC1 model is estimated with different scale values for the row and column variables
	\cr
	(\code{check.square}) If TRUE, the row and column variables must have the same number of categories}
	\item{constrained}{(\code{mob.qi})If TRUE, a quasi-independence model-constrained is estimated with a single parameter for the diagonal cells}
	\item{object}{(\code{fitmacro}) An object of class \code{glm} for \code{family=poisson} and \code{link=log} } }
\details{
These functions are used to estimate loglinear models for square tables:
\describe{
	\item{mob.qi}{Quasi-independence}
	\item{mob.eqmain}{Equal main effects (Hope's halfway model)}
	\item{mob.symint}{Symmetric interaction}
	\item{mob.cp}{Crossings-parameter model}
	\item{mob.unif}{Uniform association}
	\item{mob.rc1}{Row and columns model 1}
	\item{fitmacro}{Calculates \code{BIC} and \code{AIC} relative to a
	saturated loglinear model}
	\item{check.square}{Internal function to check if row and column variables are both factors with the same number of levels}
}
These functions create part of the design matrix for a loglinear model. With the exception of \code{mob.eqmain} the models are an interaction effect between the row and column variable in order to test for a certain pattern of association. As with most things in \R, these functions represent one way of doing things.

These restricted models can also be applied in a multinomial logistic regression model. In order to do this, the data have a \dQuote{long} shape. Rather than one record per subject, the dataset must have \emph{ncat} records per subject, where \emph{ncat} is the number of categories of the dependent variable in the multinomial logistic model. The dataset must also have variables indexing the choices and indicating the choice made by each subject. 

The documentation for \code{\link[survival]{clogit}} has an example that shows how the data can be restructured in this way. The \code{\link[mlogit]{mlogit.data}} function can also restructure the data as required. The models can be subsequently estimated using \code{\link[survival]{clogit}} or \code{\link[mlogit]{mlogit.data}}.
}
\value{
	\item{mob.qi}{A factor that will produce coefficients for the diagonal cells of a table, using off- diagonal cells as base category}
	\item{mob.eqmain}{A design matrix with equality contraints on the main effects}
	\item{mob.symint}{A design matrix for an interaction with equality contraints on coefficients on opposite sides of the diagonal}
	\item{mob.cp}{A set of vectors for a crossings-parameter model}
	\item{mob.unif}{A vector for a uniform association model}
	\item{mob.rc1}{A set of vectors for a row and columns model 1}
	\item{fitmacro}{Prints deviance, df, BIC, AIC, number of parameters and N}
	\item{check.square}{Stops function if either the row or column variable is not a factor or if the number of levels is unequal}
}

\references{
Agresti, Alan. (1990). \cite{Categorical data analysis}. New York: John Wiley & Sons.

Allison, Paul D. and Nicholas Christakis. (1994). Logit models for sets of ranked items. Pp. 199-228 in \cite{Peter V. Marsden (ed.), Sociological Methodology}. Oxford: Basil Blackwell.

Breen, Richard. (1994). Individual Level Models for Mobility Tables and Other Cross- Classifications. \cite{Sociological Methods & Research} 33: 147-173.

Goodman, Leo A. (1984). \cite{The analysis of cross-classified data having ordered categories.} Cambridge, Mass.: Harvard University Press.

Hendrickx, John. (2000). Special restrictions in multinomial logistic regression. \cite{Stata Technical Bulletin} 56: 18-26.

Hendrickx, John, Ganzeboom, Harry B.G. (1998). Occupational Status Attainment in the Netherlands, 1920-1990. A Multinomial Logistic Analysis. \cite{European Sociological Review} 14: 387-403.

Hout, Michael. (1983). \cite{Mobility Tables}. Sage Publication 07-031.

Logan, John A. (1983). A Multivariate Model for Mobility Tables. \cite{American Journal of Sociology 89}: 324-349.
}
\author{John Hendrickx <\email{John_Hendrickx@yahoo.com}>}

\seealso{\code{\link{glm}},
\code{[mlogit]}\code{\link[mlogit]{mlogit}},	
\code{[mlogit]}\code{\link[mlogit]{mlogit.data}},	
\code{[survival]}\code{\link[survival]{clogit}}, \code{[survival]}\code{\link[survival]{coxph}}, \code{[nnet]}\code{\link[nnet]{multinom}}
}
\examples{
# Examples of loglinear models for square tables,
# from Hout, M. (1983). "Mobility Tables". Sage Publication 07-031

# Table from page 11 of "Mobility Tables"
# Original source: Featherman D.L., R.M. Hauser. (1978) "Opportunity and Change."
# New York: Academic, page 49

Freq <- c(
1414,  521,  302,   643,   40,
 724,  524,  254,   703,   48,
 798,  648,  856,  1676,  108,
 756,  914,  771,  3325,  237,
 409,  357,  441,  1611, 1832)

OccFather<-gl(5,5,labels=c("Upper nonmanual","Lower nonmanual","Upper manual","Lower manual","Farm"))
OccSon<-gl(5,1,labels=c("Upper nonmanual","Lower nonmanual","Upper manual","Lower manual","Farm"))
FHtab <- data.frame(OccFather,OccSon,Freq)

xtabs(Freq~OccFather+OccSon,data=FHtab)

# independence model
indep<-glm(Freq~OccFather+OccSon,family=poisson(),data=FHtab)
summary(indep)
fitmacro(indep)

wt <- as.numeric(OccFather != OccSon)
qi0<-glm(Freq~OccFather+OccSon,weights=wt,family=poisson(),data=FHtab)
# A quasi-independence loglinear model, using structural zeros
# (page 23 of "Mobility Tables").
#  0  1  1  1  1   values of variable "wt"
#  1  0  1  1  1
#  1  1  0  1  1
#  1  1  1  0  1
#  1  1  1  1  0
qi0<-glm(Freq~OccFather+OccSon,weights=wt,family=poisson(),data=FHtab)
summary(qi0)
fitmacro(qi0)

# Quasi-independence using a "dummy factor" to create the design
# vectors for the diagonal cells (page 23).
#  1  0  0  0  0
#  0  2  0  0  0
#  0  0  3  0  0
#  0  0  0  4  0
#  0  0  0  0  5
glm.qi<-glm(Freq~OccFather+OccSon+mob.qi(OccFather,OccSon),family=poisson(),data=FHtab)
summary(glm.qi)
fitmacro(glm.qi)

# Quasi-independence without using the functions
# Factor labels prevent numeric comparisons, create numeric versions
# of the row and column variables
OccFather_n <- unclass(OccFather)
OccSon_n <- unclass(OccSon)
q1 <- ifelse(OccFather_n==OccSon_n & OccSon_n==1,1,0)
q2 <- ifelse(OccFather_n==OccSon_n & OccSon_n==2,1,0)
q3 <- ifelse(OccFather_n==OccSon_n & OccSon_n==3,1,0)
q4 <- ifelse(OccFather_n==OccSon_n & OccSon_n==4,1,0)
q5 <- ifelse(OccFather_n==OccSon_n & OccSon_n==5,1,0)
glm.qi2<-glm(Freq~OccFather+OccSon+q1+q2+q3+q4+q5,family=poisson(),data=FHtab)
summary(glm.qi2)
fitmacro(glm.qi2)

# Quasi-independence constrained (QPM-C, page 31)
# Single immobility parameter
#  1  0  0  0  0
#  0  1  0  0  0
#  0  0  1  0  0
#  0  0  0  1  0
#  0  0  0  0  1
glm.q0<-glm(Freq~OccFather+OccSon+mob.qi(OccFather,OccSon,constrained=TRUE),family=poisson(),data=FHtab)
# slightly different results than Hout also found in Stata: L2=2567.658, q0=0.964
summary(glm.q0)
fitmacro(glm.q0)

# Quasi-symmetry using the symmetric cross-classification (page 23)
#  0  1  2  3  4   values of variable "sym"
#  1  0  5  6  7
#  2  5  0  8  9
#  3  6  8  0 10
#  4  7  9 10  0  */
glm.qsym<-
glm(Freq~OccFather+OccSon+mob.symint(OccFather,OccSon),family=poisson(),data=FHtab)
summary(glm.qsym)
fitmacro(glm.qsym)

symmetry<-glm(Freq~mob.eqmain(OccFather,OccSon)
+mob.symint(OccFather,OccSon),family=poisson(),data=FHtab)
summary(symmetry)
fitmacro(symmetry)

# Crossings parameter model (page 35)
#  0  v1 v1 v1 v1 |  0  0  v2 v2 v2 |  0  0  0  v3 v3 |  0  0  0  0 v4
#  v1 0  0  0  0  |  0  0  v2 v2 v2 |  0  0  0  v3 v3 |  0  0  0  0 v4
#  v1 0  0  0  0  |  v2 v2 0  0  0  |  0  0  0  v3 v3 |  0  0  0  0 v4
#  v1 0  0  0  0  |  v2 v2 0  0  0  |  v3 v3 v3 0  0  |  0  0  0  0 v4
#  v1 0  0  0  0  |  v2 v2 0  0  0  |  v3 v3 v3 0  0  |  v4 v4 v4 v4 0
glm.cp<-glm(Freq~OccFather+OccSon+mob.cp(OccFather,OccSon),family=poisson(),data=FHtab)
summary(glm.cp)
fitmacro(glm.cp)

# Uniform association model: linear by linear association (page 58)
glm.unif<-glm(Freq~OccFather+OccSon+mob.unif(OccFather,OccSon),family=poisson(),data=FHtab)
summary(glm.unif)
fitmacro(glm.unif)

# RC model 1 (unequal row and column effects, page 58)
# Fits a uniform association parameter and row and column effect
# parameters. Row and column effect parameters have the
# restriction that the first and last categories are zero.
glm.rc1<-glm(Freq~OccFather+OccSon+mob.rc1(OccFather,OccSon),family=poisson(),data=FHtab)
summary(glm.rc1)
fitmacro(glm.rc1)

# Homogeneous row and column effects model 1 (page 58)
# An equality restriction is placed on the row and column effects
glm.hrc1<-glm(Freq~OccFather+OccSon+mob.rc1(OccFather,OccSon,equal=TRUE),family=poisson(),data=FHtab)
# Results differ from those in Hout, replicated by other programs
summary(glm.hrc1)
fitmacro(glm.hrc1)

#-------------------------------------------------------------------------------
# Examples on using these models in multinomial logistic regression
#-------------------------------------------------------------------------------
# Data from the 1972-78 GSS used by Logan (1983)
library(survival)
data(logan)

# Restructure the data in 'long' format using mlogit.data
library(mlogit)
pc <- mlogit.data(logan, shape = "wide", choice = "occupation")
head(pc,10)
# pc$alt indexes choices, pc$chid indexes respondents
# pc$occupation is a boolean variable that is TRUE where pc$alt corresponds
# with the actual choice made
class(logan$occupation) #"factor"
class(pc$alt) #"character"
# pc$alt needs to be transformed into a factor for use in the some models
# for square tables which assume ordered categories
pc$alt <- factor(pc$alt,levels=c("farm", "operatives", "craftsmen", "sales", "professional"))

# A regular multinomial logit model
m1<-mlogit(occupation ~ 0 | education+race, data=pc, reflevel = "farm")
summary(m1)

# Estimate a "quasi-uniform association" loglinear model for "focc" and "occupation"
# with "education" and "race" as covariates at the respondent level
# The quasi-uniform association model contains alternative-specific effects,
# education and race are individual specific effects
m2 <- mlogit(occupation ~ mob.qi(focc,alt)+mob.unif(focc,alt) | education+race,data=pc, reflevel = "farm")
summary(m2)

# Same model using survival::clogit
# First create a design matrix for the alternatives that drops the first category
occ.X<-model.matrix(~pc$alt)[,-1]
head(occ.X,10)
# The clogit output exceeds the default 80 characters
options(width=256)
# pc$chid which indexes respondents must be used in strata()
# Individual specific effecs are modelled as interactions between the 
# design matrix for pc$alt and the covariates 
cl.qu<-clogit(occupation~occ.X+occ.X:education+occ.X:race+
  mob.qi(focc,alt)+mob.unif(focc,alt)+strata(chid),data=pc)
summary(cl.qu)
}

\keyword{multivariate}

