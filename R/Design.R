Design <- function(jdesign) structure(list(jdesign = jdesign), class = c('Design', 'list'))

n1        <- function(design) UseMethod('n1')
n1.Design <- function(design) JuliaCall::julia_call('n1', design$jdesign)

n2        <- function(design, x1) UseMethod('n2')
n2.Design <- function(design, x1) {
    if (any(x1 != round(x1))) stop('x1 must be integer')
    JuliaCall::julia_call('n2.', design$jdesign, as.integer(x1))
}

n        <- function(design, x1) UseMethod('n')
n.Design <- function(design, x1) {
    if (any(x1 != round(x1))) stop('x1 must be integer')
    JuliaCall::julia_call('n.', design$jdesign, as.integer(x1))
}

c2        <- function(design, x1) UseMethod('c2')
c2.Design <- function(design, x1) {
    if (any(x1 != round(x1))) stop('x1 must be integer')
    JuliaCall::julia_call('c2.', design$jdesign, as.integer(x1))
}