---
title: "gloptim examples"
author: "Hans W. Borchers"
date: "2022-05-05"
format:
  html:
    mainfont: "Trebuchet MS"  # Geneva
    fontsize: 1.0em
    toc: true
    toc-location: left
    theme: united
    keep-md: true
---



## Test function packages

The following report by Kate Mullen may be a good source of test functions for stochastic optimization:

Mullen, K. M. (2014). Continuous Global Optimization in R. Journal of Statistical Software, 60(6), 1--45. URL: <https://www.jstatsoft.org/index.php/jss/article/view/v060i06/792>

There is a CRAN package 'globalOptTests' that provides all test functions of this report as C functions and available in R. Table 1 shows a list of test functions, and Appendix A displays the results of applying different stochastic optimizers on these test functions as boxplots.

### ModLangerman function

The 10-parameter ModLangerman problem appears to be one of the most difficult problems for stochastic solvers, see the boxplot on page 32. Almost no stochastic solver does find the optimum when starting from a random initial point.

The Modlangerman function in 'globalOptTests' will be called in the following way:


::: {.cell}

```{.r .cell-code}
# install packages("globalOptTests")
library(globalOptTests)

# test function properties
fn_name = "Modlangerman"
dimen  = getProblemDimen(fn_name)    # d = 10
bounds = getDefaultBounds(fn_name)   # lower = 0; upper = 10
gloptm = getGlobalOpt(fn_name)       # glopt = -0.965

# test function definition
fn = function(x) goTest(fnName = fn_name, par = x)
```
:::


We apply a "Simulated Annealing" optimizer to it.


::: {.cell}

```{.r .cell-code}
system.time(
  sol <- GenSA::GenSA(par = rep(5, 10), fn = fn,
                     lower = bounds$lower, upper = bounds$upper,
                     control = list(maxit = 10000))
)
```

::: {.cell-output .cell-output-stdout}
```
   user  system elapsed 
 12.720   0.149  12.880 
```
:::

```{.r .cell-code}
sol$value; sol$par
```

::: {.cell-output .cell-output-stdout}
```
[1] -0.908
```
:::

::: {.cell-output .cell-output-stdout}
```
 [1] 2.196 0.415 5.649 6.979 9.510 9.166 6.304 6.054 9.377 1.426
```
:::
:::


GenSA provides a trace that can be plotted.


::: {.cell}

```{.r .cell-code}
min_values = sol$trace.mat[, 4]
plot(min_values, type = 'l', col = "blue",
     ylim = c(-1, 0), ylab = "current minimum",
     main = "ModLangerman with GenSA optimizer")
abline(h = gloptm, col = "darkred", lty = 2, lwd = 2)
grid()
```

::: {.cell-output-display}
![](examples_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::


The trajectory of "current minimum" values displays a kind of strange behavior of the ModLangerman function. The true minimum is by far not found.

The help page for "goTest" suggests to call a solver with, e.g., optim(par=rep(5,10), fn=goTest, fnName="Modlangerman"), but that will not work if the stochastic optimizer does not support the 'dot' notation.

Setting fn_name = "Rosenbrock" or fn_name = "Rastrigin" will call the Rosenbrock resp. Rastrigin function, for instance.

### The Hald function
