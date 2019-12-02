#'@export
PointMass <- function(p) {
    jprior <- JuliaCall::julia_call('PointMass', p)
    structure(list(jprior = jprior), class = c('PointMass', 'Prior', 'list'))
}

#'@export
Beta <- function(a, b, low = 0, high = 1) {
    jprior <- JuliaCall::julia_call('Beta', a, b, low = low, high = high)
    structure(list(jprior = jprior), class = c('Beta', 'Prior', 'list'))
}

#' @export
condition <- function(prior, low = 0, high = 1) {
    jprior <- JuliaCall::julia_call('condition', prior$jprior, low = low, high = high)
    structure(list(jprior = jprior), class = c(class(prior)[1], 'Prior', 'list'))
}

#' @export
weight <- function(weight, prior) {
    jprior <- JuliaCall::julia_call('*', weight, prior$jprior)
    structure(list(jprior = jprior), class = c('BetaMixture', 'Prior', 'list'))
}

#' @export
add <- function(prior1, prior2) {
    jprior <- JuliaCall::julia_call('+', prior1$jprior, prior2$jprior)
    structure(list(jprior = jprior), class = c('BetaMixture', 'Prior', 'list'))
}
