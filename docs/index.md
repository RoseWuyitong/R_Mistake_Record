R log
================
Wu Yitong
2024-02-14

# ifelse

So in this case, `ifelse` can replace `if` with `else`.

``` r
cat("Using ifelse():", ifelse(TRUE, 1, 0))
```

    ## Using ifelse(): 1

``` r
cat("Using if () {} else {}:", if (TRUE) 1 else 0)
```

    ## Using if () {} else {}: 1

But for a list, `ifelse` can only return the first value.

``` r
cat("Using ifelse():", ifelse(TRUE, c(1, 2, 3), c(5, 6, 7)))
```

    ## Using ifelse(): 1

``` r
cat("Using if () {} else {}:", if (TRUE) c(1, 2, 3) else c(5, 6, 7))
```

    ## Using if () {} else {}: 1 2 3

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

``` r
func_1(1, 2, 3, 4, 5)
```

    ## func_1(a = 1, b = 2, c = 3, 4, 5)

What if .
