glopt <- function(fn, lb, ub, x0 = NULL, gr = NULL,
                  maxiter = NULL, popsize = NULL,
                  incl = NULL, excl = NULL) {
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

    mthd <- c(); fminimum <- c(); thetime = c()
    for (m in incl) {
        tm <- system.time(
            sol <- gloptim(fn, lb, ub, x0 = x0, method=m, gr=gr)
        )
        mthd <- c(mthd, m); fminimum <- c(fminimum, sol$fmin)
        thetime <- c(thetime, unname(tm["elapsed"]))
    }
    o <- order(fminimum)
    G <- data.frame(method=mthd[o], fmin=fminimum[o], time=thetime[o])
    G
}

gloptN <- function(fn, lb, ub, method, x0 = NULL,
                   maxiter = NULL, popsize = NULL,
                   N = 100) {
    fmin <- numeric(N)
    for (i in 1:N) {
        sol <- gloptim(fn, lb, ub, x0 = x0, method = method)
        fmin[i] <- sol$fmin
    }
    return(fmin)
}

n <- 5
lb <- rep(-1, n); ub <- rep(1, n)
G <- glopt(adagio::fnRastrigin, lb, ub,
           gr = adagio::grRastrigin)

fn <- gloptim::fnHald
lb <- rep(-1, 5); ub <- rep(1, 5)
G <- glopt(fn, lb, ub, excl=c("malschains"))
