#'@export
Design <- function(jdesign, label = '') {
    structure(list(
            jdesign = jdesign,
            label   = label
        ),
        class = c('Design', 'list')
    )
}

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

#'@export
early_futility        <- function(design) UseMethod('early_futility')
#'@export
early_futility.Design <- function(design) {
    JuliaCall::julia_call('early_futility', design$jdesign)
}

#'@export
early_efficacy        <- function(design) UseMethod('early_efficacy')
#'@export
early_efficacy.Design <- function(design) {
    JuliaCall::julia_call('early_efficacy', design$jdesign)
}

#' Convert Design-object to tibble
#'
#' @importFrom tibble as_tibble
#' @export
as_tibble.Design <- function(design) {
    tibble::tibble(
        label   = design$label,
        n1      = rep(n1(design), n1(design) + 1),
        x1      = seq(0, n1(design)),
        `x1/n1` = x1/n1,
        n2      = n2(design, x1),
        c2      = c2(design, x1),
        n       = n(design, x1),
        c       = c2 + x1
    )
}

#' @export
plot.Design <- function(design) {
    tbl_plot <- as_tibble(design) %>%
        select(`x1/n1`, n2, c2) %>%
        group_by(`x1/n1`) %>%
        nest() %>%
        mutate(
            x1 = map(data, ~tibble(x1 = 0:n1(design))),
            x2 = map(data, ~tibble(
                x2     = 0:(.$n2),
                reject = x2 > (.$c2)
            )
            )
        ) %>%
        unnest(c(x1, data)) %>%
        unnest(x2) %>%
        ungroup() %>%
        transmute(
            `x1/n1`,
            x = x1 + x2,
            reject
        )
    breaks_x <- c(0, early_futility(design)/n1(design), early_efficacy(design)/n1(design), 1)
    labels_x <- map2_chr(
        breaks_x,
        c(0, early_futility(design), early_efficacy(design), n1(design)),
        ~sprintf("%4.2f (%i)", .x, .y)
    )
    breaks_y <- unique(c(seq(0, max(n(design, seq(0, n1(design)))), by = 10), n1(design)))
    ggplot(tbl_plot) +
        aes(x = `x1/n1`, y = x) +
        geom_tile(aes(fill = reject), width = .4/n1(design), height = .75) +
        scale_x_continuous("stage-one success rate (absolute count)", breaks = breaks_x, labels = labels_x) +
        scale_y_continuous("overall sample size", breaks = breaks_y) +
        scale_fill_manual('', breaks = c(FALSE, TRUE), labels = c('accept', 'reject'),
                          values = c('darkgray', 'black')) +
        theme_bw() +
        theme(
            panel.grid.major.x   = element_blank(),
            panel.grid.minor.x   = element_blank(),
            legend.position      = c(.99, .99),
            legend.justification = c(1, 1),
            legend.direction     = 'horizontal'
        )
}
