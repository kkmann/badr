#' Power and type-one-error-rate constraints
#'
#'
#' @seealso \code{\link{scores}}, \code{\link{objectives}}
#'
#' @name constraints

setClass('Constraint')

setClass('PowerConstraint', list(jconstraint = 'ANY'), contains = 'Constraint')

setClass('TypeOneErrorRateConstraint', list(jconstraint = 'ANY'), contains = 'Constraint')

#' @rdname constraints
#'
#' @param score either a score of type \code{Power} or \code{TypeOneErrorRate},
#' see \code{\link{Score}}.
#' @param threshold lower (Power) or upper (type one error rate) threshold for
#' the constraint
#' @param conditional vector of length two giving the corridor for conditional
#' power/type one error; defaults to \code{c(.5, .99)} for power and
#' \code{c(.001, .99)} for type one error rate. Set to c(0, 1)
#' to deactivate completely.
#'
#' @include scores.R
#'
#' @examples
#' \donttest{
#'   load_julia_package()
#'   # maximal type one error rate constraint
#'   toer_cnstr  <- Power(Beta(5, 7) %|% 0.2) <= 0.05
#'   # power constraint with conditional power (given x1) restricted to 25%-100%
#'   power_cnstr <- (Power(Beta(5, 7) >=  0.3) >= 0.8) %>%
#'     conditional(c(0.25, 1.00))
#' }
setMethod('>=', c('Power', 'numeric'), function(e1, e2) {
    new( 'PowerConstraint', jconstraint = JuliaCall::julia_call('>=', e1@jscore, e2) )
})

setMethod('<=', c('Power', 'numeric'), function(e1, e2) {
    new( 'TypeOneErrorRateConstraint', jconstraint = JuliaCall::julia_call('<=', e1@jscore, e2) )
})

#' @export
conditional <- function(cnstr, bounds = c(0, 1)) {
    new( class(cnstr), jconstraint = JuliaCall::julia_call('conditional',
            cnstr@jconstraint,
            JuliaCall::julia_call('Tuple', bounds)
        )
    )
}
