##
##  O P T B O X  Optimization Toolbox for R
##

library(pracma)

require(DEoptim)
deoptim <- function(fn, lb, ub, strategy = 3, maxiter = 1000, np = NA, ...)
{
	fun <- match.fun(fn)
	fn  <- function(x) fun(x, ...)
	S  <- DEoptim(fn, lower = lb, upper = ub,
	      	control = DEoptim.control(strategy = strategy,
	      	                          itermax = maxiter, 
	      	                          NP = np,
	      	                          trace = FALSE))$optim
	xmin   <- S$bestmem; fmin   <- S$bestval
	nfeval <- S$nfeval ; niter  <- S$iter
	list(xmin = xmin, fmin = fmin, nfeval = nfeval, niter = niter)
}


require(DEoptimR)
jdeoptim <- function(fn, lb, ub, maxiter = 1000, ...) {
    fun <- match.fun(fn)
	fn  <- function(x) fun(x, ...)
	S  <- JDEoptim(lb, ub, fn, maxiter = maxiter)
	xmin   <- S$par; fmin  <- S$value
	nfeval <- NA;    niter <- S$iter
	list(xmin = xmin, fmin = fmin, nfeval = nfeval, niter = niter)
}


require(GenSA)
gensa <- function(x0, fn, lb, ub, maxiter = 1000, ...,
                  smooth = FALSE, trace = FALSE)
{
    fun <- match.fun(fn)
	fn  <- function(x) fun(x, ...)
	S <- GenSA(par = x0, fn = fn, lower = lb, upper = ub,
	        control = list(maxit = maxiter, smooth = smooth, verbose = trace))
	list(xmin = S$par, fmin = S$value, nfeval = S$counts, niter = NULL)
}


require(Rmalschains)
malsch <- function( fn, lb, ub, maxiter = 10000, ...,
                    method = "cmaes", local = FALSE, tol = 1e-8,
                    popsize = min(100, 10*length(lb)), trace = FALSE)
{
    fun <- match.fun(fn)
    fn  <- function(x) fun(x, ...)
    S <- malschains(fn, lower = lb, upper = ub, maxEvals = maxiter, trace = trace,
            control = malschains.control(popsize = popsize, optimum = -Inf,
                ls = method, lsOnly = local,  # cmaes, sw, simple, ssw
                istep = 300, threshold = tol))
    list(xmin = S$sol, fmin = S$fitness)
}


require(pso)
psopt <- function( x0, fn, lb, ub, maxiter = 10000, ...,
                   trace = 0, scale = 1)
{
    fun <- match.fun(fn)
    fn  <- function(x) fun(x, ...)
    S <- psoptim(par = x0, fn = fn, lower = lb, upper = ub,
            control = list(trace = trace, fnscale = scale, maxit = maxiter))
    counts <- unname(S$counts)
    list(xmin = S$par, fmin = S$value,
         nfeval = counts[1], niter = counts[2], nrestarts = counts[3])
}


require(adagio)
cmaes <- function(x0, fn, lb, ub, sigma = 0.5, ...)
{
    fun <- match.fun(fn)
    fn  <- function(x) fun(x, ...)
    S <- pureCMAES( par = x0, fun = fn, lower = lb, upper = ub, ...,
                    sigma = sigma)
    return(S)  # list(xmin = S$xmin, fmin = S$fmin)
}

