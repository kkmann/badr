#'@export
Problem <- function(objective, toer, power, label = '') {
    structure(list(
            jproblem = JuliaCall::julia_call("Problem", objective$jobjective, toer$jcnstr, power$jcnstr),
            label = label
        ), class = c('Problem', 'list')
    )
}

#'@export
optimise         <- function(problem, ...) UseMethod('optimise', problem)

#'@export
optimise.Problem <- function(problem, verbosity = 3, timelimit = 600, ...) {
    Design(
        JuliaCall::julia_call("optimise", problem$jproblem, verbosity = verbosity, timelimit = timelimit),
        label = problem$label
    )
}

