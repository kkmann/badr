#' Prior distributions for response probability
#'
#' \code{badr} supports \code{PointMass}, \code{Beta}, \code{BetaMixture}, and
#' \code{JeffreysPrior} distributions for the reponse probability.
#' The probability density function and cumulative probability functions are
#' available via \code{density} and \code{cdf} methods.
#' The mean of a distribution can quickly be accessed via \code{mean}.
#'
#' @seealso \code{\link{condition}} for restricting the support of a prior,
#' \code{\link{updating}} for Bayesian posterior updates
#'
#' @name Prior
NULL

setClass('Prior')

#' Conditioning of prior distributions
#'
#' \code{\link{Prior}} objects can be conditioned on lower or upper boundaries,
#' intervals, or single values to restrict their support.
#' Various shorthand notations are supported (see examples).
#'
#' @param prior \code{\link{Prior}} distribution object representing a prior on
#' the unit interval [0,1]
#' @param low [optional] lower boundary to condition on
#' @param high [optional] upper boundary to condition on
#'
#' @seealso \code{\link{Prior}}, \code{\link{updating}} for Bayesian updating
#'
#' @export
setGeneric('condition', function(prior, low = 0, high = 1) standardGeneric('condition'))

#' @rdname condition
#'
#' @examples
#' \donttest{
#'   condition(Beta(2, 3), low = 0.2)
#'   condition(Beta(2, 3), high = 0.9)
#'   condition(Beta(2, 3), low = 0.2, high = 0.9)
#'
#'   Beta(5, 7) %|% c(0.2, 0.7) # shorthand for conditioning on interval
#'   Beta(5, 7) %|% 0.5 # condition on single point
#'
#'   Beta(5, 7) <= 0.5 # = condition(Beta(5, 7), high = 0.5)
#'   0.2 <= Beta(5, 7) # = condition(Beta(5, 7), low = 0.2)
#' }
setMethod('condition', c('Prior'), function(prior, low = 0, high = 1) {
    new(as.character(class(prior)),
        jprior = JuliaCall::julia_call('condition', prior@jprior, low = low, high = high)
    )
})

#' @rdname condition
#'
#' @export
setGeneric('%|%', function(prior, interval) standardGeneric('%|%'))

#' @rdname condition
setMethod('%|%', c('Prior', 'numeric'), function(prior, interval) {
    new(as.character(class(prior)),
        jprior = JuliaCall::julia_call('|', prior@jprior, interval)
    )
})

setMethod('<=', c('Prior', 'numeric'), function(e1, e2) condition(e1, low = 0.0, high = e2))
setMethod('<=', c('numeric', 'Prior'), function(e1, e2) condition(e2, low = e1, high = 1.0))
setMethod('>=', c('numeric', 'Prior'), function(e1, e2) condition(e2, low = 0.0, high = e1))
setMethod('>=', c('Prior', 'numeric'), function(e1, e2) condition(e1, low = e2, high = 1.0))

#' @param x \code{Prior} distribution object (\code{density})
#'
#' @examples
#' \donttest{
#'   density(Beta(1, 1), seq(0, 1, .1)) == 1 # uniform distribution on [0, 1]
#' }
#' @rdname Prior
#'
#' @export
setMethod('density', c('Prior'), function(x, p) JuliaCall::julia_call('pdf.', p, x@jprior) )

#' @param prior \code{Prior} distribution object
#'
#' @rdname Prior
#'
#' @export
setGeneric('cdf', function(prior, p, ...) standardGeneric('cdf'))

#' @examples
#' \donttest{
#'   cdf(PointMass(1/3), c(0.3, 1/3)) == c(0, 1)
#' }
#' @rdname Prior
#'
#' @export
setMethod('cdf', c('Prior', 'numeric'), function(prior, p) JuliaCall::julia_call('cdf.', p, prior@jprior) )

#' @examples
#' \donttest{
#'   mean(Beta(5, 7)) == 5/(5 + 7)
#' }
#' @rdname Prior
#'
#' @export
setMethod('mean', c('Prior'), function(x) JuliaCall::julia_call('bad.mean', x@jprior))



setClass('PointMass', slots = list(jprior = 'ANY'), contains = 'Prior')

#' @param p probability atom, i.e. the response probability equals \code{p}
#' almost surely.
#'
#' @examples
#' \donttest{
#'   PointMass(0.4)
#' }
#' @rdname Prior
#'
#' @export
PointMass <- function(p) new('PointMass', jprior = JuliaCall::julia_call('PointMass', p))


#' Bayesian updating of prior distributions
#'
#' All prior distributions described in \code{\link{Prior}} can be updated with
#' binomial observations (\code{x} out of \code{n} responses).
#'
#' @param object \code{\link{Prior}} distribution object to update
#' @param x number of responses out of \code{n} individuals
#' @param n number of individuals out of which \code{x} had a response
#'
#' @seealso \code{\link{Prior}}, \code{\link{condition}}
#'
#' @aliases update
#' @name updating
NULL


#' @rdname updating
#'
#' @examples
#' update(PointMass(.4), 3, 10) # point mass distributions are invariant under updating
#'
#' @export
setMethod('update', c('PointMass'), function(object, x, n) object)



setClass("Beta", slots = c(jprior = "ANY"), contains = "Prior")

#' @param a Beta distribution parameter
#' @param b Beta distribution paramter
#'
#' @examples
#' \donttest{
#'   Beta(5, 7)
#'   1/3*Beta(5, 7) + 2/3*Beta(1,1) # create a BetaMixture distribution
#'  }
#' @rdname Prior
#'
#' @export
Beta <- function(a, b) new('Beta', jprior = JuliaCall::julia_call('Beta', a, b))

#' @rdname updating
#'
#' @examples
#' update(Beta(5, 7), 3, 10) # same as Beta(8, 14)
#'
#' @export
setMethod('update', c('Beta'), function(object, x, n) new('Beta',
    jprior = JuliaCall::julia_call('update', object@jprior, as.integer(x), as.integer(n))
))



setClass("BetaMixture", slots = c(jprior = "ANY"), contains = 'Prior')

#' @rdname updating
#'
#' @examples
#' update(1/3*Beta(5, 7) + 2/3*Beta(1,1), 3, 10) # update mixtures
#'
#' @export
setMethod('update', c('BetaMixture'), function(object, x, n) new('BetaMixture',
    jprior = JuliaCall::julia_call('update', object@jprior, as.integer(x), as.integer(n))
))

setMethod("+", c("BetaMixture", "BetaMixture"), function(e1, e2) {
    new("BetaMixture", jprior = JuliaCall::julia_call('+', e1@jprior, e2@jprior))
})
setMethod("+", c("Beta", "BetaMixture"), function(e1, e2) {
    new("BetaMixture", jprior = JuliaCall::julia_call('+', 1*e1@jprior, e2@jprior))
})
setMethod("+", c("BetaMixture", "Beta"), function(e1, e2) {
    new("BetaMixture", jprior = JuliaCall::julia_call('+', e1@jprior, 1*e2@jprior))
})
setMethod("*", c("numeric", "BetaMixture"), function(e1, e2) {
    new("BetaMixture", jprior = JuliaCall::julia_call('*', e1, e2@jprior))
})
setMethod("*", c("numeric", "Beta"), function(e1, e2) {
    new("BetaMixture", jprior = JuliaCall::julia_call('*', e1, e2@jprior))
})




setClass("GenericDistribution", slots = c(jprior = "ANY"), contains = 'Prior')

#' @rdname updating
#'
#' @export
setMethod('update', c('GenericDistribution'), function(object, x, n) new('GenericDistribution',
    jprior = JuliaCall::julia_call('update', object@jprior, as.integer(x), as.integer(n))
))



setClass("JeffreysPrior", slots = c(jprior = "ANY"), contains = 'Prior')

#' @rdname Prior
#'
#' @param design \code{\link{Design}} object
#'
#' @examples
#' \donttest{
#'   design <- Design(c(0, 30, 25, 0), c(Inf, 10, 7, -Inf))
#'   JeffreysPrior(design)
#' }
#' @export
JeffreysPrior <- function(design) new('JeffreysPrior',
    jprior = JuliaCall::julia_call('JeffreysPrior', design@jdesign)
)

#' @rdname updating
#'
#' @examples
#' \donttest{
#'   design    <- Design(c(0, 30, 25, 0), c(Inf, 10, 7, -Inf))
#'   prior     <- JeffreysPrior(design)
#'   posterior <- update(prior, 3, 10) # results in a GenericDistribution object (no analytical update)
#'   update(posterior, 2, 5) # the generic posterior of a Jeffreys prior can also be updated again
#' }
#' @export
setMethod('update', c('JeffreysPrior'), function(object, x, n) new('GenericDistribution',
    jprior = JuliaCall::julia_call('update', object@jprior, as.integer(x), as.integer(n))
))
