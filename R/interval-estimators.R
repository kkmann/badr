#'@export
ClopperPearsonInterval <- function(ordering, design, alpha, epsilon = 1e-6) {
    epsilon_  <- epsilon
    jinterval <- JuliaCall::julia_call('ClopperPearsonInterval',
        ordering$jordering, design@jdesign, alpha, epsilon = epsilon_
    )
    structure(
        list(jinterval = jinterval),
        class = c('ClopperPearsonInterval', 'IntervalEstimator', 'list')
    )
}

#'@export
PosteriorCredibleInterval <- function(prior, design, alpha, epsilon = 1e-4) {
    epsilon_  <- epsilon
    jinterval <- JuliaCall::julia_call('PosteriorCredibleInterval',
        prior@jprior, design@jdesign, alpha, epsilon = epsilon_
    )
    structure(
        list(jinterval = jinterval),
        class = c('jinterval', 'IntervalEstimator', 'list')
    )
}

#' @export
get_bounds <- function(interval, x1, x2) {
    JuliaCall::julia_call('get_bounds', interval$jinterval, as.integer(x1), as.integer(x2))
}

#' @export
coverage_probability <- function(interval, p) {
    JuliaCall::julia_call('coverage_probability', interval$jinterval, p)
}

#' @export
mean_width <- function(interval, p) {
    JuliaCall::julia_call('mean_width.', interval$jinterval, p)
}