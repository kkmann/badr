#'@export
Design <- function(jdesign, label = '') {
    structure(list(
            jdesign = jdesign,
            label   = label
        ),
        class = c('Design', 'list')
    )
}

#' @export
as_Design <- function(n2, c2, label = '') {
    Design(JuliaCall::julia_call('Design', n2, c2), label = label)
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


#' @export
pmf <- function(design, x1, x2, p) {
    JuliaCall::julia_call('pdf.', as.integer(x1), as.integer(x2), design$jdesign, p)
}

#'@export
power <- function(design, p, ...) {
    JuliaCall::julia_call('power.', design$jdesign, p)
}

#'@export
expected_sample_size <- function(design, p, ...) {
    JuliaCall::julia_call('expected_sample_size.', design$jdesign, p)
}


#'@export
sample_space <- function(design) {
    res           <- JuliaCall::julia_call('sample_space', design$jdesign)
    colnames(res) <- c('x1', 'x2')
    return(as_tibble(res))
}


#'@export
reject <- function(x1, x2, design) {
    JuliaCall::julia_call('reject_null.', as.integer(x1), as.integer(x2), design$jdesign)
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




#' Plot a design
#'
#' @importFrom ggrepel geom_text_repel
#' @importFrom cowplot plot_grid
#' @export
plot.Design <- function(design, tbl_power_annotations = NULL, ...) {
    tbl_plot  <- get_tbl_plot(design)
    breaks_y <- unique(c(seq(0, max(n(design, seq(0, n1(design))))*1.075, by = 10), n1(design)))
    p1 <- ggplot(tbl_plot) +
        aes(x1, x) +
        geom_hline(yintercept = n1(design)) +
        geom_tile(aes(fill = reject), width = .45, height = .75) +
        geom_segment(x = -.45/2, xend = .45/2, y = n1(design), yend = n1(design),
                     color = ifelse(c2(design, 0) >= 0, 'darkgray', 'black')) +
        geom_segment(x = 0, xend = 0, y = n1(design) - .75/2, yend = n1(design) + .75/2,
                     color = ifelse(c2(design, 0) >= 0, 'darkgray', 'black')) +
        scale_x_continuous("stage-one responses", limits = c(0, NA)) +
        scale_y_continuous("overall sample size", breaks = breaks_y, limits = c(0, max(n(design, seq(0, n1(design))))*1.05)) +
        scale_fill_manual('', breaks = c(FALSE, TRUE), labels = c('not reject', 'reject'),
                          values = c('darkgray', 'black')) +
        theme_bw() +
        theme(
            panel.grid.major.x   = element_blank(),
            panel.grid.minor.x   = element_blank(),
            panel.grid.minor.y   = element_blank(),
            legend.position      = c(.99, .99),
            legend.justification = c(1, 1),
            legend.direction     = 'horizontal',
            legend.key.size      = grid::unit(.5, 'lines')
        )
    tbl_plot <- tibble(
        p     = seq(0, 1, by = .001),
        power = power(design, p)
    )
    p_breaks <- seq(0, 1, by = .2)
    if (inherits(tbl_power_annotations, 'tbl_df')) {
        pp <- tbl_power_annotations$p
        for (i in 1:length(p_breaks)) {
            if (any(abs(pp - p_breaks[i]) < .1)) p_breaks[i] <- NA_real_
        }
        p_breaks <- unique(c(p_breaks[complete.cases(p_breaks)], tbl_power_annotations$p))
    }
    p_breaks <- round(p_breaks, 2)
    p2 <- ggplot(tbl_plot) +
        aes(p, power) +
        geom_line() +
        scale_y_continuous("power", breaks = seq(0, 1, .1)) +
        scale_x_continuous("p", breaks = p_breaks, expand = c(0, 0)) +
        theme_bw() +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank()
        )
    if (inherits(tbl_power_annotations, 'tbl_df')) {
        tbl_power_annotations <- tbl_power_annotations %>%
            mutate(
                power = power(design, p),
                label = sprintf("%.1f%% (%s)", 100*power, label)
            )
        p2 <- p2 +
            geom_vline(aes(xintercept = p), color = 'lightgray', size = .5,
                              data = tbl_power_annotations) +
            ggrepel::geom_text_repel(
                aes(label = label), nudge_x = .25, nudge_y = .01, size = 3.5,
                segment.color = 'darkgray',
                xlim = c(0, 1), ylim = c(0, 1), seed = 42, max.iter = 10000,
                min.segment.length = Inf,
                data = tbl_power_annotations
            ) +
            geom_point(data = tbl_power_annotations)
    }
    cowplot::plot_grid(p1, p2, nrow = 1)
}
