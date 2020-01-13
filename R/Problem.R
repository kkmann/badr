#' Optimise a trial design
#'
#' [todo]
#'
#' @seealso \link{scores}, \link{constraints}, \link{objectives}, \link{Design}
#'
#' @name optimise-design

setClass('Problem', list(jproblem = 'ANY', label = 'character') )

#' @rdname optimise-design
#'
#' @param objective object of class \code{Objective} (see \link{objectives})
#' @param toer type one error rate constraint (see \link{constraints})
#' @param power power constraint (see \link{constraints})
#' @param label label for the problem
#' @param unimodal force unimodal solution (can be time consuming!)
#'
#' @export
Problem <- function(objective, toer, power, label = '', unimodal = FALSE, ...) {
    new('Problem',  jproblem = JuliaCall::julia_call('Problem',
        objective@jobjective, toer@jconstraint, power@jconstraint,
        unimodal = unimodal, ...),
        label = label
    )
}

#' @rdname optimise-design
#'
#' @param problem object of class \code{Problem}
#' @param verbosity verbosity level of optimisation procedure
#' @param timelimit maximum time spent in ILP solver (in seconds)
#' @param ... optional arguments
#'
#' @examples
#' \donttest{
#'   prior   <- Beta(5, 7)
#'   problem <- Problem(
#'       minimise(SampleSize(prior)),
#'       Power(prior %|% 0.2) <= 0.05,
#'       Power(prior  >= 0.3) >= 0.80
#'   )
#'   design <- optimise_design(problem)
#' }
#' @export
optimise_design <- function(problem, verbosity = 3, timelimit = 180, ...) {
    new('Design', jdesign = JuliaCall::julia_call('optimise',
            problem@jproblem, verbosity = as.integer(verbosity),
            timelimit = as.integer(timelimit), ...
        ),
        label = problem@label
    )
}

#' Adapt an existing design
#'
#' @param design asd
#' @param prior ad
#' @param partial asd
#' @param verbosity asd
#' @param timelimit maximum time spent in ILP solver (in seconds)
#' @param ... optional arguments
#'
#' @examples
#' \donttest{
#'   prior   <- Beta(5, 7)
#'   problem <- Problem(
#'       minimise(SampleSize(prior)),
#'       Power(prior %|% 0.2) <= 0.05,
#'       Power(prior  >= 0.3) >= 0.80
#'   )
#'   design <- optimise_design(problem)
#' }
#' @export
adapt <- function(design, prior, partial, verbosity = 3, timelimit = 180, ...) {
    partial <- JuliaCall::julia_call('Tuple', as.integer(partial))
    JuliaCall::julia_call('adapt', design@jdesign, prior@jprior,
                          partial, verbosity = as.integer(verbosity),
                          timelimit = as.integer(timelimit),
                          ...)
}
