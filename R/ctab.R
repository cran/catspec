# ctab: oneway, twoway, multiway percentage tables
# first argument must consist of one or more factors
# or a table object (class table, xtabs, or ftable)
# digits: number of digits after the decimal (default 2)
# type: "n" for counts, "row", "column" or "total"
# for percentages (default "n")
# row.vars:
# col.vars: same usage as ftable, ignored for one- and
# two-way tables
# percentages: FALSE==> proportions are presented rather
# than percentages (default TRUE)

# comments to John Hendrickx <John_Hendrickx@yahoo.com>

ctab<-function(...,digits=2,
        type=c("n", "row", "column", "total"),
        row.vars=NULL, col.vars=NULL,
        percentages=TRUE) {
    if (attributes(...)$class=="factor") {
        # create a table if the arguments are factors
        tbl<-table(...)
    }
    else if ("table" %in% class(...) || class(...)=="ftable") {
        # the argument is a table object (table, xtabs, ftable)
        tbl<-eval(...)
    }
    else {
        stop("first argument must be either factors or a table object")
    }

    type<-match.arg(type)

    # one dimensional table,restrict choices to "n" and "total"
    if (length(dim(tbl))==1) {
        type<-ifelse(type=="n","n","total")
    }

    # if the object is an ftable, use the row.vars and col.vars
    # use numeric indices to avoid finding the omitted
    # the object must be converted to a table to get the dimensions right
    if (class(tbl)=="ftable") {
        nrowvar<-length(names(attr(tbl,"row.vars")))
        row.vars<-1:nrowvar
        col.vars<-(1:length(names(attr(tbl,"col.vars"))))+nrowvar
        tbl<-as.table(tbl)
    }

    # marginals to exclude assuming first factor is the row vaariable,
    # second factor is the column variable
    # is overridden by row.vars or col.vars
    mrg2drop<-0
    if (type=="column") {mrg2drop<-1}
    if (type=="row") {mrg2drop<-2}
    if (type=="total" && length(dim(tbl)) > 1) {mrg2drop<-c(1,2)}


    # use row.vars and col.vars to determine the
    # marginals to use when calculating percentages
    # start by translating names to variable positions
    nms<-names(dimnames(tbl))
    if (!is.null(row.vars) && !is.numeric(row.vars)) {
        row.vars<-order(match(nms,row.vars),na.last=NA)
    }
    if (!is.null(col.vars) && !is.numeric(col.vars)) {
        col.vars<-order(match(nms,col.vars),na.last=NA)
    }
    # calculate the other if only one is given
    if (!is.null(row.vars) && is.null(col.vars)) {
        col.vars<-(1:length(dim(tbl)))[-row.vars]
    }
    if (!is.null(col.vars) && is.null(row.vars)) {
        row.vars<-(1:length(dim(tbl)))[-col.vars]
    }
    # now determine the margin as the last element
    if (type=="row" && !is.null(col.vars)) {
        mrg2drop<-col.vars[length(col.vars)]
    }
    if (type=="column" && !is.null(row.vars)) {
        mrg2drop<-row.vars[length(row.vars)]
    }
    # if row.vars is given, col.vars has been determined
    if (type=="total" && !is.null(row.vars)) {
        mrg2drop<-c(col.vars[length(col.vars)],row.vars[length(row.vars)])
    }

    marg<-(1:length(dim(tbl)))[(-mrg2drop)]

    # create percentages
    if (type=="n") {
        digits<-0
    }
    else {
        tbl<-prop.table(tbl,marg)
        if (percentages) {tbl<-tbl*100}
    }


    # use ftable for more than 2 dimensions
    # (ftable doesn't work for 1 dimension,
    # and table is nicer for 2 dimensions IMHO
    if (length(dim(tbl))>2) {
        if (is.null(row.vars)) {
            # let the second variable be the column variable
            row.vars<-names(dimnames(tbl))[-2]
            # reverse the order, last variables are groups, first is row variable
            row.vars<-rev(row.vars)
        }
        tbl<-ftable(tbl,row.vars=row.vars,col.vars=col.vars)
    }

    # get the names of the column variable
    if (class(tbl)=="ftable") {
        nms<-attr(tbl,"col.vars")[[1]]
    }
    else if (length(dim(tbl))==1) {
        nms<-dimnames(tbl)[[1]]
    }
    else{
        nms<-dimnames(tbl)[[2]]
    }

    # present the (percentage) table
    wd<-max(nchar(nms),nchar(as.integer(tbl))+digits+1)
    tbl<-formatC(tbl,format="f",width=wd,digits=digits)
    tbl
}
