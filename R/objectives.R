#'@export
minimise_expected_sample_size <- function(prior) {
    structure(list(
            jobjective = JuliaCall::julia_call("minimise_expected_sample_size", prior$jprior)
        ),
        class = c('ExpectedSampleSize', 'Objective', 'list')
    )
}

#' @export
minimise_maximal_sample_size <- function(lambda, prior) {
    structure(list(
            jobjective = JuliaCall::julia_call("MiniMaxSampleSize", lambda, prior$jprior)
        ),
        class = c('MiniMaxSampleSize', 'Objective', 'list')
    )
}

#' @export
maximise_utility <- function(prior, lambda_tp, lambda_fp, mcr) {
    structure(list(
        jobjective = JuliaCall::julia_call("ExpectedUtility", prior$jprior, lambda_tp, lambda_fp, mcr)
    ),
    class = c('ExpectedUtility', 'Objective', 'list')
    )
}