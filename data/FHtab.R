# Examples of loglinear models for square tables,
# from Hout, M. (1983). "Mobility Tables". Sage Publication 07-031

# Table from page 11 of "Mobility Tables"
# Original source: Featherman D.L., R.M. Hauser. (1978) "Opportunity and Change."
# New York: Academic, page 49

FHtab <- c(
1414,  521,  302,   643,   40,
 724,  524,  254,   703,   48,
 798,  648,  856,  1676,  108,
 756,  914,  771,  3325,  237,
 409,  357,  441,  1611, 1832)

dim(FHtab) <- c(5, 5)
FHtab<-t(FHtab)
dimnames(FHtab) <- list(OccFather=c("Upper nonmanual","Lower nonmanual","Upper manual","Lower manual","Farm"),
                        OccSon   =c("Upper nonmanual","Lower nonmanual","Upper manual","Lower manual","Farm"))
class(FHtab) <- "table"
FHtab<-as.data.frame(FHtab)
FHtab
