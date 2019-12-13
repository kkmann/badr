#' @export
EstimatorOrdering <- function(estimator, orientation = "superiority") {
    jordering <- JuliaCall::julia_call('EstimatorOrdering',
        estimator$jestimator,
        orientation = JuliaCall::julia_eval(sprintf(":%s", orientation), "Julia")
    )
    structure(
        list(jordering = jordering),
        class = c('EstimatorOrdering', 'Ordering', 'list')
    )
}

#' @export
p_value <- function(x1, x2, p0, ordering, design) {
    JuliaCall::julia_call('p_value.',
        as.integer(x1), as.integer(x2), p0, ordering$jordering, design$jdesign
    )
}

#' @export
PValue <- function(estimator, design, p0, orientation = 'superiority') {
    jpvalue <- JuliaCall::julia_call('PValue',
        estimator$jestimator, design$jdesign, p0,
        orientation = JuliaCall::julia_eval(sprintf(":%s", orientation), "Julia")
    )
    structure(
        list(jpvalue = jpvalue),
        class = c('PValue', 'list')
    )
}

#'@export
get_p <- function(pval, x1, x2) {
    JuliaCall::julia_call("evaluate.", pval$jpvalue, as.integer(x1), as.integer(x2))
}
