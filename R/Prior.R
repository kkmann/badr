#'@export
Beta <- function(a, b, low = 0, high = 1) {
    jprior <- JuliaCall::julia_call('Beta', a, b, low = low, high = high)
    structure(list(jprior = jprior), class = c('Beta', 'Prior', 'list'))
}
