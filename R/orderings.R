#' @export
EstimatorOrdering <- function(estmator) {
    jordering <- JuliaCall::julia_call('EstimstorOrdering', estimtor$jestimator)
    structure(
        list(jordering = jordering),
        class = c('EstimatorOrdering', 'Ordering', 'list')
    )
}

#' @export
p_value <- function(x1, x2, p0, ordering, design, orientation = "superiority") {
    JuliaCall::julia_call('p_value.', x1, x2, p0, ordering$jordering,
                          design$jdesign,
                          JuliaCall::julia_eval(
                              sprintf(":%s", orientation), "Julia"
                                )
                          )
}