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
PosteriorMeanPrecalculated <- function(prior, design) {
    jestimator <- JuliaCall::julia_call('PosteriorMeanPrecalculated',
        prior$jprior, design$jdesign
    )
    structure(
        list(jestimator = jestimator),
        class = c('PosteriorMeanPrecalculated', 'Estimator', 'list')
    )
}

#'@export
RaoBlackwellEstimator <- function() {
    jestimator <- JuliaCall::julia_call('RaoBlackwellEstimator')
    structure(
        list(
            jestimator = jestimator
        ),
        class = c('RaoBlackwellEstimator', 'Estimator', 'list')
    )
}



#'@export
CompatibleMLE <- function(design, lambda = 1.0, epsilon = 1e-8,
                          smoothmax_scale = 10.0, max_iter = 1e4L) {
    jestimator <- JuliaCall::julia_call('CompatibleMLE',
        design$jdesign, "\U003BB" = lambda, "\U003F5" = epsilon,
        smoothmax_scale = smoothmax_scale, max_iter = max_iter
    )
    structure(
        list(jestimator = jestimator),
        class = c('CompatibleMLE', 'Estimator', 'list')
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

#' @export
mean_absolute_error <- function(p, estimator, design) {
    JuliaCall::julia_call("mean_absolute_error.", p, estimator$jestimator, design$jdesign)
}
