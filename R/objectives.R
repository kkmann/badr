setClass('Objective')

#' Objective functions for optimal two-stage designs
#'
#'
#' @seealso \code{\link{Design}}, \link{constraints}, \link{scores}, \code{\link{Prior}}
#'
#' @name objectives


setClass('MiniMaxSampleSize', list(jobjective = 'ANY'), contains = 'Objective')

#' @rdname objectives
#'
#' @param lambda minimax sample size alone does not always lead to a unique
#' minimiser. Instead a combination of \code{(1 - lambda)} times the maximal
#' sample size and \code{lambda} times the expected sample size under the
#' given \code{prior} is minimised.
#'
#' @param prior \code{\link{Prior}} object
#'
#' @export
minimise_maximal_sample_size <- function(lambda, prior) {
    new('MiniMaxSampleSize', jobjective = JuliaCall::julia_call('MiniMaxSampleSize',
        lambda, prior@jprior
    ))
}

setClass('ExpectedScoreObjective', list(jobjective = 'ANY'), contains = 'Objective')

#' @rdname objectives
#'
#' @param score \code{Score} object to be minimised/maximised
#'
#' @export
minimise <- function(score) {
    new('ExpectedScoreObjective', jobjective = JuliaCall::julia_call('minimise', score@jscore))
}

#' @rdname objectives
#'
#' @export
maximise <- function(score) {
    new('ExpectedScoreObjective', jobjective = JuliaCall::julia_call('maximise', score@jscore))
}
