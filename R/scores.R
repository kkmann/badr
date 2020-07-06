#' Scores
#'
#' Score objects allows the computation of conditional and unconditional
#' performance measures for \code{\link{Design}} objects.
#' The \code{evaluate} generic function and its methods allow to compute
#' scores conditional on observed (partial) stage one results or stage
#' two results or completely unconditional.
#'
#' @seealso \code{\link{SampleSize}}, \code{\link{Power}}
#'
#' @name scores

setClass('Score')

#' @rdname scores
#'
#' @param score \code{Score} object to evaluate
#' @param design \code{\link{Design}} object to evaluate score on
#'
#' @export
setGeneric('evaluate', function(score, design, ...) standardGeneric('evaluate'))

#' @rdname scores
#'
#' @param x1 completely observed number of stage one responses under design; if given,
#' thew score is evaluated conditional on \code{x1}
#' @param partial partial observations \code{c(x, n)} in stage two (if \code{x1} is given) or stage
#' one (if \code{x1} is not given)
#'
setMethod('evaluate', c('Score', 'Design'), function(score, design, x1 = NULL, partial = c(0, 0)) {
    partial <- JuliaCall::julia_call('Tuple', as.integer(partial))
    if (is.null(x1)) {
        JuliaCall::julia_call('evaluate', score@jscore, design@jdesign, partial_stage_one = partial)
    } else {
        JuliaCall::julia_call('evaluate', score@jscore, design@jdesign, as.integer(x1), partial_stage_two = partial )
    }
})

setClass('SampleSize', list(jscore = 'ANY'), contains = 'Score')

#' Sample-size score
#'
#' this \code{\link{scores}} measures the expected sample size of a design
#' under a specified \code{\link{Prior}}.
#'
#' @param prior \code{\link{Prior}} object
#'
#' @seealso \code{\link{Prior}}, \code{\link{scores}}
#'
#' @examples
#' \donttest{
#'   load_julia_package()
#'   design <- Design(c(0, 25, 20, 11, 0, 0, 0), c(Inf, 10, 8, 6, -Inf, -Inf, -Inf))
#'   ess    <- SampleSize(Beta(5, 7))
#'   # unconditional expected sample size
#'   evaluate(ess, design)
#'   # conditional sample size given X1 = 1 ( = 6 + 25 = 31)
#'   evaluate(ess, design, x1 = 1)
#'   # expected sample size given 2 responses out of 3 observations (in stage one)
#'   evaluate(ess, design, partial = c(2, 3))
#' }
#' @export
SampleSize <- function(prior) {
    new('SampleSize', jscore = JuliaCall::julia_call('SampleSize', prior@jprior) )
}

setClass('Power', list(jscore = 'ANY'), contains = 'Score')

#' (Expected) power score
#'
#' expected power averages the power curve over a \code{\link{Prior}} conditional on
#' p >= pmcr where pmcr is the minimal clinically relevant response probability.
#' For a \code{PointMass} prior, expected power reduces to power at the respective
#' probability atom.
#'
#' @param prior \code{\link{Prior}} object
#' @param pmcr minimal clinically relevant response rate
#'
#' @seealso \code{\link{Prior}}, \code{\link{scores}}
#'
#' @examples
#' \donttest{
#'   load_julia_package()
#'   design <- Design(c(0, 25, 20, 11, 0, 0, 0), c(Inf, 10, 8, 6, -Inf, -Inf, -Inf))
#'   power  <- Power(Beta(5, 7) >= 0.3)
#'   # unconditional expected power
#'   evaluate(power, design)
#'   # expected power given 2 out of 4 responses in stage one
#'   evaluate(power, design, partial = c(2, 4))
#'   # expected power given 1 stage-one response
#'   evaluate(power, design, x1 = 1)
#'   # expected power given 1 stage one response and 8 out of 12 stage two responses
#'   evaluate(power, design, x1 = 1, partial = c(8, 12))
#'
#'   utility <- 80*Power(Beta(5, 7) >= 0.3) - SampleSize(Beta(5, 7))
#'   evaluate(utility, design)
#' }
#' @export
Power <- function(prior, bounds = NULL) {
    bounds <- if (is.null(bounds)) JuliaCall::julia_call('bounds', prior@jprior) else bounds
    new('Power', jscore = JuliaCall::julia_call('Power', prior@jprior, bounds = JuliaCall::julia_call('Tuple', bounds)))
}

setClass('CompositeScore', list(jscore = 'ANY'), contains = 'Score')

setMethod('*', c('numeric', 'Score'), function(e1, e2) {
    new('CompositeScore', jscore = JuliaCall::julia_call('*', e1, e2@jscore) )
})

setMethod('+', c('Score', 'Score'), function(e1, e2) {
    new('CompositeScore', jscore = JuliaCall::julia_call('+', e1@jscore, e2@jscore) )
})

setMethod('-', c('Score', 'Score'), function(e1, e2) {
    new('CompositeScore', jscore = JuliaCall::julia_call('-', e1@jscore, e2@jscore) )
})
