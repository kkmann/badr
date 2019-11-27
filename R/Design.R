#'@export
Design <- function(jdesign) structure(list(jdesign = jdesign), class = c('Design', 'list'))

#'@export
n1        <- function(design) UseMethod('n1')
#'@export
n1.Design <- function(design) JuliaCall::julia_call('n1', design$jdesign)

#'@export
n2        <- function(design, x1) UseMethod('n2')
#'@export
n2.Design <- function(design, x1) {
    if (any(x1 != round(x1))) stop('x1 must be integer')
    JuliaCall::julia_call('n2.', design$jdesign, as.integer(x1))
}
#'@export
n        <- function(design, x1) UseMethod('n')
#'@export
n.Design <- function(design, x1) {
    if (any(x1 != round(x1))) stop('x1 must be integer')
    JuliaCall::julia_call('n.', design$jdesign, as.integer(x1))
}

#'@export
c2        <- function(design, x1) UseMethod('c2')
#'@export
c2.Design <- function(design, x1) {
    if (any(x1 != round(x1))) stop('x1 must be integer')
    JuliaCall::julia_call('c2.', design$jdesign, as.integer(x1))
}

#' Convert Design-object to tibble
#'
#' @importFrom tibble as_tibble
#' @export
as_tibble.Design <- function(design) {
    tibble::tibble(
        n1      = rep(n1(design), n1(design) + 1),
        x1      = seq(0, n1(design)),
        `x1/n1` = x1/n1,
        n2      = n2(design, x1),
        c2      = c2(design, x1),
        n       = n(design, x1)
    )
}

