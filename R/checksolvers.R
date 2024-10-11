##
##  c h e c k s o l v e r s . R
##
##  Find if solver is installed and, if not, offer to install.

# https://stackoverflow.com/questions/15214411/see-if-a-variable-function-exists-in-a-package
function_exists <- function(package, funcname) {
  tryCatch({
    utils::getFromNamespace(funcname, package)
    TRUE
  }, error = function(...) { FALSE })
}

checksolvers <- function() {

allmethod = c(
    "deoptim",
    "cppdeoptim",
    "deoptimr", 
    "deopt", 
    "simplede", 
    "simpleea",  
    "gensa",
    "ga",
    "genoud", 
    "pso",
    "psopt",
    "hydropso", 
    "ceimopt",
    "direct",
    "crs2lm",
    "isres",
    "stogo",
    "cmaoptim",
    "cmaes",
    "cmaesr",
    "purecmaes",
    "malschains",
    "smco", 
    "soma",
    "ABC")
allpkg = c(
  "DEoptim",
  "RcppDE",
  "DEoptimR", 
  "NMOF", 
  "adagio", 
  "adagio",  
  "GenSA",
  "GA",
  "rgenoud", 
  "pso",
  "NMOF",
  "hydroPSO", 
  "RCEIM",
  "nloptr",
  "nloptr",
  "nloptr",
  "nloptr",
  "rCMA",
  "parma",
  "cmaesr",
  "adagio",
  "Rmalschains",
  "smco", 
  "soma",
  "ABCoptim")
truefnname = c(
  "DEoptim",  
  "DEoptim",
  "JDEoptim", 
  "DEopt", 
  "simpleDE", 
  "simpleEA",  
  "GenSA",
  "ga",
  "genoud", 
  "psoptim",
  "PSopt",
  "hydroPSO", 
  "ceimOpt",
  "direct",
  "crs2lm",
  "isres",
  "stogo",
  "cmaOptimDP",
  "cmaes",
  "cmaes",
  "pureCMAES",
  "malschains",
  "smco", 
  "soma",
  "abc_optim")
# Package origins
## Note fn BiocManager::available(), CRAN available.packages

  psrc<-c(
    "CRAN", ##   "DEoptim",
    "CRAN", ##   "RcppDE",
    "CRAN", ##   "DEoptimR", 
    "CRAN", ##   "NMOF", 
    "CRAN", ##   "adagio", 
    "CRAN", ##   "adagio",  
    "CRAN", ##   "GenSA",
    "CRAN", ##   "GA",
    "CRAN", ##   "rgenoud", 
    "CRAN", ##   "pso",
    "CRAN", ##   "NMOF",
    "https://github.com/hzambran/hydroPSO", ##   "hydroPSO", 
    "CRAN", ##   "RCEIM",
    "CRAN", ##   "nloptr",
    "CRAN", ##   "nloptr",
    "CRAN", ##   "nloptr",
    "CRAN", ##   "nloptr",
    "CRAN", ##   "rCMA",
    "CRAN", ##   "parma",
    "CRAN", ##   "cmaesr",
    "CRAN", ##   "adagio",
    "CRAN", ##   "Rmalschains",
    "https://github.com/nashjc/smco", ##     "smco", 
    "CRAN", ##     "soma",
    "CRAN") ##     "ABCoptim")
  nsolver<-length(allmethod)
  npkg <- length(allpkg)
  nfun <- length(truefnname)
  if ((nsolver != npkg) || (nfun != npkg)) {
    cat(nsolver," solvers, ",npkg," packages, ",nfun," functions\n")
    stop("Setup error in names of solvers, packages or functions")
  }
  fhere<-rep(FALSE,nsolver)
  for (i in 1:nsolver){
      cat(allmethod[i]," package source=",psrc[i],":\n")
      ishere<-function_exists(allpkg[i], truefnname[i])
      if (ishere){
          fhere[i]<-TRUE
          cat("is present as function ",truefnname[i]," from package ",
                   allpkg[i],"\n")
      } else {
          prompt <- paste("Install package ",allpkg[i],sep='')
          doinstall<-askYesNo(prompt, default=FALSE)
          if (doinstall){
            pksrc<-psrc[i]
            cat("Install source is:",pksrc,"\n")
            if(pksrc=="CRAN") {
              install.packages(allpkg[i])
              fhere[i]<-TRUE
            }
            else {
              cat("Installing from ",pksrc,"\n")
              devtools::install_github(pksrc)
              fhere[i]<-function_exists(allpkg[i], truefnname[i])
              if (! fhere[i]) stop("Install of ",pksrc," failed")
            }
          }          
      }  
  }
  if (all(fhere)==TRUE) cat("All methods available\n")
  newsol<-allmethod[fhere]
} # EoF

