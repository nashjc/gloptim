##
##  g l o p t i m . R
##

gloptim <- function(fn, lb, ub, x0 = NULL,  
        method = c("deoptim", "cppdeoptim", "deoptimr", 
                   "deopt", "simplede", "simpleea",     
                   "gensa", "ga", "genoud", 
                   "pso", "psopt", "hydropso", 
                   "direct", "crs2lm", "isres", "stogo", 
                   "cmaoptim", "cmaes", "cmaesr", "purecmaes",
                   "malschains", "ceimopt", "smco", "soma",
                   "ABC", "genalg"), 
                   control=list(), 
                   rand = FALSE, gr = NULL, ...) {
##        method = c("deoptim", "cppdeoptim", "deoptimr",         # **DE**
#                   "deopt", "simplede", "simpleea",             # **EA**
#                   "gensa", "ga", "genoud", # "rbga"            # **GA**
#                   "pso", "psopt", "hydropso", # "psoptim"      # **PSO**
#                   "direct", "crs2lm", "isres", "stogo",        # **NLoptr**
#                   "cmaoptim", "cmaes", "cmaesr", "purecmaes",  # **CMA-ES**
#                   "malschains", "ceimopt",                     # **CE**
#                   "smco", "soma"), # "tabusearch"              # --others--
##        rand = FALSE, type = NULL, g = NULL, gr = NULL,
##         control = list(), ...) {

    cat("rand="); print(rand)
    if (is.null(rand)) { rand <- FALSE }
    fn1 <- function(par) fn(par,...) # avoid dotarg clashes

#   probably don't need f
#    ## Checking input argument for being functions resp. numeric
#    fun = match.fun(fn)
#    f <- function(x) fun(x, ...)

    method = match.arg(method)
    stopifnot(is.numeric(lb), is.numeric(ub))
    d <- length(lb)
    if (length(ub) != d)
        stop("Lower and upper bound must be of the same length.")

    if (!is.null(x0)) {
        stopifnot(is.numeric(x0))
        if (length(x0) != d)
            stop("Argument 'x0' must have the same length as bounds.")
        if (any(x0 < lb) || any(x0 > ub))
            stop("Argument 'x0' not between lower and upper bound.")
    } else {
        if (rand) {
            x0 <- lb + runif(d) * (ub - lb)
        } else {
            x0 <- (ub + lb) / 2.0
        }
    }

    ## Handling of requested method and control options 
    cntrl <- list(trace = 0, 
                  popsize =  20*d,      # population size
                  maxiter = 200*d,      # max. no. of iterations
                  info    = FALSE       # shall info/trace be shown
    )
    for (nm in names(control)) {
        if (nm %in% names(cntrl)) {
            cntrl[nm] <- control[nm]
		} else
		    stop("Unknown name in control list: '", nm, "'.", call. = FALSE)
    }
    if (cntrl$trace > 0){cat("Global solver/method:", method, "\n")}

        ## Differential Evolution (DE) in packages DEoptim and DEoptimR
    if (method == "deoptim") {
        sol <- DEoptim::DEoptim(fn = fn1, lower = lb, upper = ub,
                                DEoptim::DEoptim.control(
                                trace = cntrl$info, itermax = cntrl$maxiter))
        return(list(xmin = sol$optim$bestmem, fmin = sol$optim$bestval,
                    niter = sol$optim$iter, nfeval = sol$optim$nfeval,
                    commment = ""))

    } else if (method == "cppdeoptim") {
        sol <- RcppDE::DEoptim(fn = fn1, lower = lb, upper = ub,
                                RcppDE::DEoptim.control(
                                trace = cntrl$info, itermax = cntrl$maxiter))
        return(list(xmin = sol$optim$bestmem, fmin = sol$optim$bestval,
                    niter = sol$optim$iter, nfeval = sol$optim$nfeval,
                    commment = ""))
        

    } else if (method == "deoptimr") {
        sol <- DEoptimR::JDEoptim(lower = lb, upper = ub,
                                  fn = fn1,
                                  maxiter = cntrl$maxiter)
        return(list(xmin = sol$par, fmin = sol$value,
                    niter = sol$iter, nfeval = NA,
                    comment = ""))

    } else if (method == "deopt") {
        sol <- NMOF::DEopt(OF = fn1,
                           algo = list(nP = cntrl$popsize, nG = cntrl$maxiter,
                                       min = lb, max = ub,
                                       printDetail = 2 * cntrl$info,
                                       printBar = FALSE))
        return(list(xmin = sol$xbest, fmin = sol$OFvalue,
                    niter = cntrl$maxiter, nfeval = NA,
                    comment = ""))

    } else if (method == "simplede") {
        sol <- adagio::simpleDE(fun = fn1, lower = lb, upper = ub,
                                N = cntrl$popsize, nmax = cntrl$maxiter,
                                log = cntrl$info)
        return(list(xmin = sol$xmin, fmin = sol$fmin))

    } else if (method == "simpleea") {
        sol <- adagio::simpleEA(fn = fn1, lower = lb, upper = ub,
                                # N = cntrl$popsize,
                                log = cntrl$info)
        return(list(xmin = sol$par, fmin = sol$val))
        
    ## Simulated Annealing (SA) in package GenSA
    } else if (method == "gensa") {
        sol <- GenSA::GenSA(fn=fn1, lower=lb, upper=ub,
                            control=list(maxit=cntrl$maxiter,
                                         smooth=FALSE,
                                         trace.mat=cntrl$info))
        return(list(xmin = sol$par, fmin = sol$value,
                    niter = cntrl$maxiter, feval = sol$counts))

    ## Genetic Algorithms (GA) in package GA
    } else if (method == "ga") {
        s <- -1 # ONLY minimize in gloptim
        fnf <- function(x) {s * fn1(x)}
# JN: 20240724 -- change fitness = fn to fitness = fnf or maximizes
        gasol <- GA::ga(type = "real-valued", fitness = fnf,
                      lower = lb, upper = ub, 
                      popSize = cntrl$popsize,
                      maxiter = cntrl$maxiter,
                      monitor = cntrl$info)
        # changed min to lower, max to upper (deprecated form replaced)
        gafmin<-as.numeric(gasol@fitnessValue)
        gaxmin<-gasol@solution
        if (dim(gaxmin)[1] > 1) gaxmin <- gaxmin[1,] # Use first only if multiple
        # 20241008 issue with multiple minima at same fn value
        ## gasol is ga-class object. (expletives deleted!!)
        return(list(xmin = gaxmin, fmin = s * gafmin))
    } else if (method == "genoud") {
        if (is.null(gr))
            gr <- function(x) rep(0, d)
        sol <- rgenoud::genoud(fn = fn1, nvars = d, gr = gr,
                               # pop.size = cntrl$popsize,
                               # max.generations = cntrl$maxiter,
                               Domains = cbind(lb, ub), boundary.enforcement = 1,
                               print.level = cntrl$trace)
        return(list(xmin = sol$par, fmin = sol$value))

    ## Particle Swarm Optimization (PSO) in packages pso, psoptim, and NMOF
    } else if (method == "pso") {
        if (is.null(x0)) x0 <- rep(NA, d)
        sol <- pso::psoptim(par=x0, fn = fn1,
                            lower = lb, upper = ub,
                            control=list(maxit=cntrl$maxiter))  # maxf=14*maxit
        return(list(xmin = sol$par, fmin = sol$value))

    } else if (method == "psopt") {
        sol <- NMOF::PSopt(OF = fn1,
                           algo = list(nP = cntrl$popsize, nG = cntrl$maxiter,
                                       min = lb, max = ub,
                                       printDetail = 0, printBar = FALSE))
        return(list(xmin = sol$xbest, fmin = sol$OFvalue))

## Package psoptim, recurring error:
## Error in plot.window(...) : need finite 'ylim' values
#     } else if (method == "psoptim") {
#         fnew <- function(x) {
#             if (is.vector(x)) {
#                 fval <- fn(x)
#             } else if (is.matrix(x)) {
#                 fval <- numeric(nrow(x))
#                 for (i in 1:nrow(x)) fval[i] <- fn(x[i, ])
#             }
#             return(fval)
#         }
#         sol <- psoptim::psoptim(FUN = fnew,
#                                 n = cntrl$popsize, max.loop = cntrl$maxiter,
#                                 xmin = lb, xmax = ub, vmax = rep(1, d),
#                                 anim = FALSE)
#         return(list(xmin = sol$sol, fmin = sol$val))

    } else if (method == "hydropso") {
        sol <- hydroPSO::hydroPSO(par = x0, fn = fn1,
                                  lower = lb, upper = ub,
                                  control = list(npart = cntrl$popsize,
                                                 maxit = cntrl$maxiter,
                                                 verbose = cntrl$info))
        return(list(xmin = sol$par, fmin = sol$value))

    ## Cross Entropy (CE) inspired methods in packages RCEIM and CEoptim
    } else if (method == "ceimopt") {
        sol <- RCEIM::ceimOpt(OptimFunction = fn1, nParams = d,
                              Ntot = cntrl$popsize, maxIter = cntrl$maxiter,
                              boundaries = cbind(lb, ub),
                              verbose = cntrl$info)
        xmin <- unname(sol$BestMember[1:d])
        fmin <- unname(sol$BestMember[d+1])
        return(list(xmin = xmin, fmin = fmin))

    ## Different stochastic solvers in package nloptr
    } else if (method == "direct") {
        sol <- nloptr::direct(fn = fn1, lower = lb, upper = ub,
                              control = list(maxeval = 10*cntrl$maxiter),
                              nl.info = cntrl$info)
        return(list(xmin = sol$par, fmin = sol$value))

    } else if (method == "crs2lm") {
        sol <- nloptr::crs2lm(x0 = x0, fn = fn,
                              lower = lb, upper = ub,
                              maxeval = 10*cntrl$maxiter, pop.size = cntrl$popsize,
                              nl.info = cntrl$info)
        return(list(xmin = sol$par, fmin = sol$value))

    } else if (method == "isres") {
        sol <- nloptr::isres(x0 = x0, fn = fn1,
                             lower = lb, upper = ub,
                             maxeval = 10*cntrl$maxiter, pop.size = cntrl$popsize,
                             xtol_rel = 1e-7, nl.info = cntrl$info)
        return(list(xmin = sol$par, fmin = sol$value))

    } else if (method == "stogo") {
        sol <- nloptr::stogo(x0 = x0, fn = fn1, gr = NULL,
                             lower = lb, upper = ub,
                             maxeval = cntrl$maxiter,
                             nl.info = cntrl$info)
        return(list(xmin = sol$par, fmin = sol$value))

    ## Covariance Matrix Adaptation Evolution Strategy(CMA-ES)
    ## in packages rCMA, parma, cmaes, adagio, and Rmalschains
    } else if (method == "cmaoptim") {
        cma_obj <- rCMA::cmaNew()
        rCMA::cmaInit(cma_obj, dimension=d)
        sol <- rCMA::cmaOptimDP(cma_obj, fitFunc = fn1,
                                verbose = 0)
        return(list(xmin = sol$bestX, fmin = sol$bestFitness))

    } else if (method == "cmaes") {
        cntrl_cmaes <- parma::cmaes.control()
        cntrl_cmaes$options$MaxIter <- cntrl$maxiter
        cntrl_cmaes$options$PopSize <- cntrl$popsize
        cntrl_cmaes$options$DispModulo <- 0
        cntrl_cmaes$options$DispFinal <- FALSE
        sol <- parma::cmaes(pars = x0, fun = fn,
                            lower = lb, upper = ub,
                            ctrl = cntrl_cmaes)
        return(list(xmin = sol$bestever$x, fmin = sol$bestever$f))

    } else if (method == "cmaesr") {
        fn_parset <- 
            ParamHelpers::makeNumericParamSet("R", len=d,
                                lower=lb, upper=ub)
        fn_fun <- 
            smoof::makeSingleObjectiveFunction("Function", fn = fn1,
                                        par.set = fn_parset)
        sol <- cmaesr::cmaes(fn_fun, start.point = x0,
                             monitor = NULL)
        return(list(xmin = sol$best.param, fmin = sol$best.fitness))

    } else if (method == "purecmaes") {
        sol <- adagio::pureCMAES(par = x0, fun = fn1,
                                 lower = lb, upper = ub,
                                 stopeval=5*d*cntrl$maxiter)
        return(list(xmin = sol$xmin, fmin = sol$fmin))

    } else if (method == "malschains") {
        sol <- Rmalschains::malschains(fn = fn1, dim = d,
                                       lower = lb, upper = ub,
                                       maxEvals = cntrl$popsize * cntrl$maxiter,
                                       verbosity = 0)
        return(list(xmin = sol$sol, fmin = sol$fitness))

    ## Monte Carlo Optimizer (MCO) in package smco
    } else if (method == "smco") {
        sol <- smco::smco(fn = fn1, gr = NULL, N = d, 
                    LB = lb, UB = ub,
                    maxiter = cntrl$maxiter, trc = cntrl$info)
        return(list(xmin = sol$par, fmin = sol$value))

    ## Self-Organizing Optimization (SOM) algorithm in package soma
    } else if (method == "soma") {
        sol <- soma::soma(costFunction = fn1,
                    bounds=list(min=lb, max=ub) )
        best <- sol$leader
        xmin <- (sol$population)[ ,best]
        return(list(xmin = xmin, fmin = (sol$cost)[best]) )
    ## Artificial Bee Colony
    # abc_optim(par, fn, ..., FoodNumber = 20, lb = rep(-Inf, length(par)),
    #  ub = rep(+Inf, length(par)), limit = 100, maxCycle = 1000,
    #  optiinteger = FALSE, criter = 50, parscale = rep(1, length(par)),
    #  fnscale = 1)
    } else if (method == "ABC") {
        sol <- ABCoptim::abc_optim(par = x0, fn = fn1, lb=lb, ub=ub)
        # 20241008 -- leave other parameters at default
        best <- sol$value
        xmin <- sol$par
        return(list(xmin = xmin, fmin = best) )
    } else if (method == "genalg") {
	sol <- genalg::rbga(lb, ub, popSize=200, iters=100,  # elitism=1,
           evalFunc=fn1)
        # 20241008 -- leave other parameters at default
        m = which.min(sol$evaluations)
        best = sol$population[m,]
        xmin = sol$evaluations[m] 
        return(list(xmin = xmin, fmin = best) )
    } else {
        stop("Argument '", method, "' has not (yet) been implemented.")
    }

} # EoF
