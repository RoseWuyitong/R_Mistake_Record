R log
================
Wu Yitong
2024-02-14

# ifelse

So in this case, `ifelse` can replace `if` + `else`.

``` r
ifelse(TRUE, 1, 0)
```

    ## [1] 1

``` r
if (TRUE) 1 else 0
```

    ## [1] 1

But for a list, `ifelse` can only return the first value.

``` r
ifelse(TRUE, c(1, 2, 3), c(5, 6, 7))
```

    ## [1] 1

``` r
if (TRUE) c(1, 2, 3) else c(5, 6, 7)
```

    ## [1] 1 2 3

# \[a:a+1\]

Be careful about the `[]`.

``` r
l <- c(0.1, 0.2, 0.3, 0.4, 0.5)

l[1:2 + 1] # == l[2:3]
```

    ## [1] 0.2 0.3

``` r
l[1 + 1:2] # == l[2:3]
```

    ## [1] 0.2 0.3

``` r
l[1 + 1:2 + 1] # == l[3:4]
```

    ## [1] 0.3 0.4

``` r
l[1:(2 + 1)] # == l[1:3]
```

    ## [1] 0.1 0.2 0.3

# match.call()

We saw lots of `match.call()` in many source code, including `glm`,
`rstanarm`, etc.

Here is an example from `glm`,

``` r
glm <- function(formula, family = gaussian, data, weights,
                subset, na.action, start = NULL,
                etastart, mustart, offset,
                control = list(...),
                model = TRUE, method = "glm.fit",
                x = FALSE, y = TRUE,
                contrasts = NULL, ...) {
  call <- match.call()

  ...

  mf <- match.call(expand.dots = FALSE)
  m <- match(c(
    "formula", "data", "subset", "weights", "na.action",
    "etastart", "mustart", "offset"
  ), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (identical(method, "model.frame")) return(mf)

  ...
```

and `rstanarm`.

``` r
stan_glm <-
  function(formula,
           family = gaussian(),
           data,
           weights,
           subset,
           na.action = NULL,
           offset = NULL,
           model = TRUE,
           x = FALSE,
           y = TRUE,
           contrasts = NULL,
           ...,
           prior = default_prior_coef(family),
           prior_intercept = default_prior_intercept(family),
           prior_aux = exponential(autoscale = TRUE),
           prior_PD = FALSE,
           algorithm = c("sampling", "optimizing", "meanfield", "fullrank"),
           mean_PPD = algorithm != "optimizing" && !prior_PD,
           adapt_delta = NULL,
           QR = FALSE,
           sparse = FALSE) {
    algorithm <- match.arg(algorithm)
    family <- validate_family(family)
    validate_glm_formula(formula)
    data <- validate_data(data, if_missing = environment(formula))

    call <- match.call(expand.dots = TRUE)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "subset", "weights", "na.action", "offset"),
      table = names(mf), nomatch = 0L
    )
    mf <- mf[c(1L, m)]
    mf$data <- data
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mf <- check_constant_vars(mf)
    mt <- attr(mf, "terms")
    Y <- array1D_check(model.response(mf, type = "any"))
    if (is.empty.model(mt)) {
      stop("No intercept or predictors specified.", call. = FALSE)
    }
    X <- model.matrix(mt, mf, contrasts)
    
    ...
```

This is definition of `match.call()`.

``` r
match.call(
  definition = sys.function(sys.parent()),
  call = sys.call(sys.parent()),
  expand.dots = TRUE,
  envir = parent.frame(2L)
)
```

Lets see what `match.call()` does.

``` r
func_1 <- function(a, b, c, ...) {
  return(match.call())
}

func_1(1, 2, 3)
```

    ## func_1(a = 1, b = 2, c = 3)

What if we have extra augments.

``` r
func_1(1, 2, 3, 4, 5)
```

    ## func_1(a = 1, b = 2, c = 3, 4, 5)

As you can see, `match.call()` can help us collect the augments passing
through the current function.

Lets use another function.

``` r
# multiply a and b only
multiply_inner <- function(a, b, c, ...) {
  if (is.null(c)) cat("value c is not available\n") 
  else cat("the value of c is:", c, "\n")
  return(a*b)
}
```

Put the similar `match.call` code from above into this outer function.

And lets see what changed during the running.

``` r
multiply_outer_1 <- function(a, b, c, d, ...) {
  cat("the value of d is:", d, "\n")
  
  ## expand.dot = TRUE => each extra value is a single value
  call <- match.call() # expand.dots = TRUE by defaut
  # [1] "call: multiply_outer"
  # [2] "call: 2"             
  # [3] "call: 3" 
  # [4] "call: 4"             
  # [5] "call: 5"   
  # [6] "call: 6"  
  # [6] "call: 7"  
  
  ## expand.dot = FALSE => all yje extra value will store as a whole one
  mf <- match.call(expand.dots = FALSE)
  # [1] "mf: multiply_outer"
  # [2] "mf: 2"             
  # [3] "mf: 3"   
  # [3] "mf: 4"   
  # [3] "mf: 5"   
  # [4] "mf: pairlist(6, 7)" 
  
  ## select augments a and b only
  m <- match(c("a", "b", "c"), names(mf), 0L)
  # [1] "m: 2" "m: 3" "m: 4"
  
  ## 1L is the current function
  mf <- mf[c(1L, m)] 
  # [1] "mf: multiply_outer" 
  # [2] "mf: 2"             
  # [3] "mf: 3"   
  # [3] "mf: 4"   
  
  ## change 1L into other function (here is multiply_inner)
  mf[[1L]] <- quote(multiply_inner)
  # [1] "mf: multiply_inner" 
  # [2] "mf: 2"             
  # [3] "mf: 3"  
  # [3] "mf: 4"  
  
  ## run the inner function we just assigned
  mf <- eval(mf, parent.frame())
  # [1] "mf: 6"
  
  return(mf)
}

multiply_outer_1(2,3,4,5,6,7)
```

    ## the value of d is: 5 
    ## the value of c is: 4

    ## [1] 6

So, you may ask, why we cannot just use…

``` r
multiply_outer_2 <- function(a, b, c, d, ...) {
  cat("the value of d is:", d, "\n")
  return(multiply_inner(a, b, c))
}

multiply_outer_2(2,3,4,5,6)
```

    ## the value of d is: 5 
    ## the value of c is: 4

    ## [1] 6

The answer is YES, in this case, using above code would suddenly
simplify the process.

But, what if you want more flexibility.

``` r
some_glm <- function(formula, model, ...) {
  
  call <- match.call()
  # call: customize_summary(formula = mpg ~ cyl, model = lm_model, method = "bootstrap", mean = TRUE, median = TRUE)
  
  args <- as.list(call)[-1]  # Remove the function name
  # args:
  # $formula
  # y ~ x1+x2
  # 
  # $model
  # lm_model
  # 
  # $method
  # [1] "bootstrap"
  # 
  # $mean
  # [1] TRUE
  # 
  # $median
  # [1] TRUE
  #
  # $sd
  # [1] FALSE

  ## Extract the method argument
  method_arg <- args$method
  # [1] method_arg: bootstrap
  
  ## Extract other relevant arguments
  statistics <- c("mean", "median", "sd")
  selected_stats <- intersect(names(args)[sapply(args, function(x) identical(x, TRUE))], statistics)

  return(selected_stats)
}

# Example usage
some_glm(y ~ x1+x2, lm_model, method = "bootstrap", mean = TRUE, median = TRUE, sd=FALSE)
```

    ## [1] "mean"   "median"

So you can play with the augments!
