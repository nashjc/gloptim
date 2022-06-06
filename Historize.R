##   Historize functions

### Definition
#
#  Add history storage to a function
#  histfun() will return the storage
#

historize <- function(fun) {
    local({
        H <- numeric()
        myFun <- function(x) {
            if(missing(x)) {
                return(H)
            } else if (is.null(x)) {
                H <<- numeric()
                return(invisible(NULL))
            } else {
                y <- fun(x)     # storing parameters is too slow
                H <<- c(H, y)   # rbind(H, c(x, y))
                return(y)
            }
        }
        return(myFun)
    })
}

Historize <- function(fun, len = 0) {
    local({
        H <- numeric()
        myFun <- function(x) {
            if(missing(x)) {
                return(H)
            } else if (is.null(x)) {
                H <<- numeric()
                return(invisible(NULL))
            } else {
                y <- fun(x)
                if (len == 0) {
                    H <<- c(H, y)
                } else {
                    if (length(x) != len)
                        stop("Incorrect parameter length.")
                    H <<- rbind(H, c(x, y))
                }
                return(y)
            }
        }
        return(myFun)
    })
}

### Example
#
#   We will use the Rastrigin function with DEoptimR
#
# FnRastrigin <- Historize(adagio::fnRastrigin)
# 
# lb <- rep(-5.2, 10); ub <- -lb
# system.time(
#   sol <- DEoptimR::JDEoptim(lb, ub, FnRastrigin)
# )                             # 50 times slower than w/o storing
# 
# H <- FnRastrigin()            # FnRastrigin(NULL)
# m <- nrow(H); n <- ncol(H)    # fvalues <- FnRastrigin w/ historize()
# fvalues <- H[, n]             # fvalues <- FnRastrigin()[, ncol(H)]
# 
# m1 <- fvalues[1]              # Is there a vectorized version?
# system.time(
#   for (i in 2:length(fvalues)) {
#       if (fvalues[i] < m1) { m1 <- fvalues[i]
#       } else { fvalues[i] <- m1 }
#   }
# )
# 
# plot(fvalues, type = 'l', col = "navy")
# grid()
# 