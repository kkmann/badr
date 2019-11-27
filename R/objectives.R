#'@export
minimise_expected_sample_size <- function(prior) {
    structure(list(
            jobjective = JuliaCall::julia_call("minimise_expected_sample_size", prior$jprior)
        ),
        class = c('ExpectedSampleSize', 'Objective', 'list')
    )
}

