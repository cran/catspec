\name{mclgen}

\alias{mclgen}

\title{Restructure a data-frame as a person-choice file}

\description{
\code{mclgen} restructures a data-frame into a \emph{person-choice}
file for estimation of a multinomial logit as a conditional logit model
}

\usage{
mclgen(datamat, catvar)
}
%- maybe also `usage' for other objects documented here.

\arguments{
  \item{datamat}{A data-frame to be transformed into a \emph{person-choice} file}
  \item{catvar}{A factor representing the response variable (i.e. the dependent variable in a multinomial logistic model)}
}

\details{
A multinomial logit model can be estimated using a program for conditional logit regression. This will produce the same coefficients and standard errors but allows greater flexibility for imposing restrictions on the dependent variable.

To estimate the multinomial logistic model as a conditional logit model, the data must be restructured as a \emph{person-choice} file. \code{mclgen} performs this operation such that:

\itemize{
  \item each record of the data-frame is duplicated \emph{ncat} times, where \emph{ncat} is the
  number of categories of the response variable
  \item A new variable \code{id} is created to index respondents. This variable is used as
  the stratifying variable in \code{clogit}
  \item A new variable \code{newy} is created to index response options for each respondent
  \item A new variable \code{depvar} is created which is equal to 1 for the record corresponding
  with the respondent's actual choice and is 0 otherwise
}

\code{depvar} is the dependent variable in \code{clogit}. The main effects of \code{catvar} correspond with the intercept term of a multinomial logit model, interactions of \code{catvar} with predictor variables correspond with the effects of these variables in a multinomial logit model.

Since \code{catvar} is now on the right-hand side of the model equation, restrictions can be imposed in the usual fashion. For example, by using \code{[MASS]}\code{\link[MASS]{contr.sdif}} in the MASS package for \code{catvar}, an adjacent logit model is obtained (Agresti 1990: 318). By adding the dummy variables for two categories of \code{catvar}, an equality constraint can be imposed on those categories. These equality constraints can then be imposed on the effects of some predictor variables but not others.

Another use is to include a mobility model in a multinomial logistic regression model. Mobility models are loglinear models for square tables. They lie in the space between a model of independence and a saturated model. This is accomplished by imposing restrictions on the interaction effect of the row and column variable. A number of these special models have been developed, see Hout (1983) or Goodman (1984) for an overview.

These loglinear mobility models can be seen as multinomial logistic regression models with special restrictions on the dependent variable. The nature of the restriction depends on the category of the predictor variable. In practise, mobility models can be included in an MCL model using the same specification as for a loglinear model. \R functions for several common mobility models can be found in \code{\link{sqtab}}.
}

\value{
  A data-frame is returned, restructured as a \emph{person-choice} file.
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

\url{http://www.xs4all.nl/~jhckx/}
}

\author{John Hendrickx \email{John\_Hendrickx@yahoo.com} }

\note{

The effects of predictor variables are modelled as interactions with \code{depvar} but the main effects of the predictor variables are not included in the model since these are constant within strata. This causes problems due to the way \R handles contrasts in interaction effects. If lower order effects of variables are not included in a model then contrasts are not used in interactions with these variables (see \code{\link{terms.object}} under the heading \code{factors}). For example, in the model \code{~Y+Y:X1+Y:X2}, contrasts will only be used for the main effect of \code{Y}. For the interaction effects \code{Y:X1} and \code{Y:X2}, dummies for all levels of \code{Y} will be used. The dummies for \code{Y} are linearly dependent within strata of the conditional logit model, therefore a warning issues that \emph{"X matrix deemed to be singular"} and the dummy for the last level of \code{Y} is dropped. So the model is estimated but \code{depvar} is not coded as intended.

The simplest workaround is to include the main effects of the predictor variables in the model, i.e. use \code{~Y*X1+Y*X2}. The main effects of \code{X1} and \code{X2} are constant within strata so a warning will issue that \emph{"X matrix deemed to be singular"} and these effects will be dropped from the model. This has no further consequences and \code{depvar} is coded as intended.

A second workaround can be to use \code{\link{model.matrix}} to create the dummies for \code{depvar} using the intended contrast. See the example below.

\code{\link[survival]{clogit}} does not accept weights. A workaround is to call \code{\link[survival]{coxph}} directly (see example below).

}

\seealso{\code{[survival]}\code{\link[survival]{clogit}}, \code{[survival]}\code{\link[survival]{coxph}}, \code{[nnet]}\code{\link[nnet]{multinom}}, \code{\link{sqtab}}
}

\examples{
## Example 1
# data from the Data from the 1972-78 GSS used by Logan (1983)
data(logan)

# create the "person-choice" file
pc<-mclgen(logan,occ)
summary(pc)
attach(pc)

library(survival)
# The following specification will work but R won't drop
# cl.lr<-clogit(depvar~occ+occ:educ+occ:black+strata(id),data=pc)
# However, R won't drop the first category of "occ"
# in the interaction effects. The last category will be omitted
# instead due to linear dependence within strata.
# Fix for the problem, create dummies manually for "occ"
occ.X<-model.matrix(~pc$occ)
occ.X<-occ.X[,attributes(occ.X)$assign==1]
cl.lr<-clogit(depvar~occ.X+occ.X:educ+occ.X:black+strata(id),data=pc)
summary(cl.lr)

# Estimate a "quasi-uniform association" loglinear model for "focc" and "occ"
# with "educ" and "black" as covariates at the respondent level
cl.qu<-clogit(depvar~occ.X+occ.X:educ+occ.X:black+
  mob.qi(focc,occ)+mob.unif(focc,occ)+strata(id),data=pc)
summary(cl.qu)

data(housing,package="MASS")
housing.prsch<-mclgen(housing,Sat)
library(survival)
# clogit doesn't support the weights argument at present
# a work-around is to call coxph directly
# coxph warns that X is singular, because the main
# effects of Infl, Type, and Cont are dropped
coxph.prsch<-coxph(Surv(rep(1, NROW(housing.prsch)), depvar) ~
  Sat+Sat*Infl+Sat*Type+Sat*Cont+strata(id),
  weights = housing.prsch$Freq, data = housing.prsch)
summary(coxph.prsch)

# the same model using multinomial logistic regression
library(nnet)
house.mult<- multinom(Sat ~ Infl + Type + Cont, weights = Freq,
                      data = housing)
summary(house.mult,correlation=FALSE)

# compare the coefficients
m1<-coef(coxph.prsch)
m1<-m1[!is.na(m1)]
dim(m1)<-c(2,7)
m2<-coef(house.mult)
m1
m2
m1-m2
max(abs(m1-m2))
mean(abs(m1-m2))
}

\keyword{multivariate}
