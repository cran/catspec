# function to restructure a data-frame as a "person-choice" file:
# "datamat" is the name of the data-frame
# "catvar" is the response factor,
# i.e. the dependent variable in a multinomial logistic model
# In the "person-choice" file, each record of "datamat" is duplicated
# "ncat" times, where "ncat" is the number of categories of "catvar")
# The variable "id" indexes respondents
# and is used as the stratifying variable in "clogit"
# The variable "newy" indexes response options for each respondent
# The variable "depvar" equals 1 for the record
# corresponding with the respondents actual choice
# and is 0 otherwise.
# "depvar" is the dependent variable in "clogit"
# Once "depvar" has been created, the variable "catvar" is redundant
# and it's contents can be replaced by "newy"
# In "clogit", the main effects of "catvar" will now correspond with the
# intercept term of a multinomial logit model, interactions of "catvar" with
# other independent variables will correspond with their effects
mclgen <- function (datamat,catvar) {
	stopifnot(is.data.frame(datamat))
	attach(datamat)
	stopifnot(is.factor(catvar))
	ncat <- nlevels(catvar)
	perschoice<-as.data.frame(rep(datamat,ncat))
	perschoice<-reshape(perschoice,direction="long",
		varying=lapply(names(datamat),rep,ncat),
		timevar="newy")
	perschoice<-perschoice[sort.list(perschoice$id),]
	dep<-parse(text=paste("perschoice$",substitute(catvar),sep=""))
	perschoice$depvar<-ifelse(as.numeric(eval(dep))==perschoice$newy,1,0)
	perschoice[[substitute(catvar)]]<-as.factor(perschoice$newy)
	perschoice[[substitute(catvar)]]<-factor(eval(dep),labels=levels(catvar))
	perschoice
}
