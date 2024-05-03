##
##  g l o p t . R
##
glopt <- function(fn, lb, ub, x0 = NULL, gr = NULL, 
                  incl = NULL, excl = NULL, control=NULL,...) {
    all_methods <-
        c("deoptim", "cppdeoptim", "deoptimr",          # **DE**
          "deopt", "simplede", "simpleea",              # **EA**
          "gensa", "ga", "genoud",                      # **GA**
          "pso", "psopt", "hydropso",                   # **PSO**
          "direct", "crs2lm", "isres", "stogo",         # **NLoptr**
          "cmaoptim", "cmaes", "cmaesr", "purecmaes",   # **CMA-ES**
          "malschains", "ceimopt",                      # **CE**
          "smco", "soma")                               # --others--

    if (is.null(incl))  incl <- all_methods
    if (!is.null(excl)) incl <- setdiff(incl, excl)
    nm<-length(incl)
    # make colnames
    dfpar <- matrix(NA, nrow=nm, ncol=length(lb))
    cnam<-c()
    for (i in 1:length(lb)){
        cnam<-c(cnam, paste("p",as.character(i),sep=''))
    }
    colnames(dfpar)<-cnam
    mthd <- c(); fminimum <- c(); thetime <- c();
    for (m in incl) {
        im <- which(incl == m)
        tm <- system.time(
            sol <- gloptim(fn, lb, ub, x0 = x0, method=m, gr=gr, 
                   control, ...)
        )
        mthd <- c(mthd, m); fminimum <- c(fminimum, sol$fmin)
        thetime <- c(thetime, unname(tm["elapsed"]))
        dfpar[im, ] <- sol$xmin
    }
    # Sort according to minimal value and return data frame
    o <- order(fminimum, na.last = TRUE)
    ans <- data.frame(method=mthd[o], fmin=fminimum[o], time=thetime[o])
    dfpar <- as.data.frame(dfpar)
    ans <- cbind(ans, dfpar)
}
