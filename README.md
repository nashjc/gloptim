# gloptim

## Introduction

'gloptim' is intended as a wrapper for many global and stochastic optimization solvers in packages available on CRAN.

The following functions in CRAN packages are considered:

    "deoptim", "cppdeoptim", "deoptimr",          # Differential Evolution
    "deopt", "simplede", "simpleea",              # Evolutionary Algorithms
    "gensa", "ga", "genoud",                      # Genetic Algorithms
    "pso", "psopt", "hydropso",                   # Particle Swarm Optimization
    "direct", "crs2lm", "isres", "stogo",         # Stochastic optimizeres in 'NLoptr'
    "cmaoptim", "cmaes", "cmaesr", "purecmaes",   # CMA-ES -like procedures
    "malschains", "ceimopt",                      # CE
    "smco", "soma")                               # --others--


## Test functions

Our test functions are typical non-smooth functions, where gradient-based optimizers do not work well.

* Hald function


## First version

The first version of 'gloptim' got implemented in the source files `gloptim.R`, containing a set of local wrappers, and `glopt.R` for callinf several or all of these local wrappers and returning a dataframe with their results.

    source("gloptim")
    source("glopt.R")

Now we call, for example, apply several Differential Evolution minimizers and compare the results.


## Second version

This version developed in connection with the Google Summer of Code (GSoC) 2021 project 'StochOptim'.
