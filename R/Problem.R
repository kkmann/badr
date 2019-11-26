Problem <- function(objective, toer, power) {
    structure(list(
            jproblem = JuliaCall::julia_call("Problem", objective$jobjective, toer$jcnstr, power$jcnstr)
        ), class = c('Problem', 'list')
    )
}


optimise         <- function(problem, ...) UseMethod('problem')
optimise.Problem <- function(problem, verbosity = 3, timelimit = 300) {
    Design(
        JuliaCall::julia_call("optimise", problem$jproblem, verbosity = verbosity, timelimit = timelimit)
    )
}
