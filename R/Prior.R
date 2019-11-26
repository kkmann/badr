Beta <- function(a, b, low = 0, high = 1) {
    jprior <- JuliaCall::julia_call('Beta', a, b, low = low, high = high)
    structure(list(jprior = jprior), class = c('Beta', 'Prior', 'list'))
}

params <- function(prior) UseMethod('params')
params.Beta <- function(prior) {
    params  <- c('a', 'b', 'low', 'high')
    values <- lapply(params, function(x) JuliaCall::field(prior$jprior, x))
    names(values) <- params
    values
}
