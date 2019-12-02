#'@export
MaximumLikelihoodEstimator <- function() {
    jestimator <- JuliaCall::julia_call('MaximumLikelihoodEstimator')
    structure(
        list(jestimator = jestimator),
        class = c('MaximumLikelihoodEstimator', 'Estimator', 'list')
    )
}

#'@export
PosteriorMean <- function(prior) {
    jestimator <- JuliaCall::julia_call('PosteriorMean', prior$jprior)
    structure(
        list(jestimator = jestimator),
        class = c('PosteriorMean', 'Estimator', 'list')
    )
}

#'@export
estimate <- function(estimator, x1, x2, design) {
    JuliaCall::julia_call("estimate.", estimator$jestimator, x1, x2, design$jdesign)
}

#' @export
bias <- function(p, estimator, design) {
    JuliaCall::julia_call("bias.", p, estimator$jestimator, design$jdesign)
}

#' @export
mean_squared_error <- function(p, estimator, design) {
    JuliaCall::julia_call("mean_squared_error.", p, estimator$jestimator, design$jdesign)
}
