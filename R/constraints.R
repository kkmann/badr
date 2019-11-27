#'@export
maximal_type_one_error_rate <- function(p0, alpha, k = 2L) {
    structure(list(
            jcnstr = JuliaCall::julia_call("maximal_type_one_error_rate", p0, alpha, k = k)
        ), class = c('MaximalTypeOneErrorRateConstraint', 'TypeOneErrorRateConstraint', 'list')
    )
}

#'@export
minimal_expected_power <- function(prior, mrv, threshold,
                                   conditional_threshold = .5,
                                   power_curtail = .999) {
    structure(list(
            jcnstr = JuliaCall::julia_call("minimal_expected_power", prior$jprior, mrv, threshold, conditional_threshold = conditional_threshold, power_curtail = power_curtail)
        ), class = c('ExpectedPowerConstraint', 'PowerConstraint', 'list')
    )
}

