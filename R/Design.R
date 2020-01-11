#' Generic two-stage designs for single-arm trials with binary endpoint
#'
#' [todo]
#'
#' @name Design

setClass('Design', list(jdesign = 'ANY', label = 'character'))

#' @rdname Design
#'
#' @param n2 numeric vector with stage-two sample sizes for x1 = 0, 1, ..., n1
#' @param c2 numeric vectot with stage-two decision boundary for x1 = 0, 1, ..., n1;
#' the design rejects H0: p <= p0 iff X2 > c_2(X1); use +/- Inf for early futility/efficacy (n2 = 0).
#' @param label an optional label to use for pretty plotting etc.
#'
#' @export
Design <- function(n2, c2, label = '') {
    new('Design',
        jdesign = JuliaCall::julia_call('Design', as.integer(n2), c2),
        label   = label
    )
}

#' @rdname Design
#'
#' @param design a \code{Design} object
#' @param x1 stage-one number of results
#'
#' @export
sample_size <- function(design, x1 = NULL) {
    if (!is(design, 'Design')) stop(sprintf("design must be of class badr::Design, is %s", class(design)))
    if (!is.null(x1)) {
        return( JuliaCall::julia_call('n.', design@jdesign, as.integer(x1)) )
    } else {
        return( JuliaCall::julia_call('n1', design@jdesign) )
    }
}

#' @rdname Design
#'
#' @export
critical_value <- function(design, x1) {
    return( JuliaCall::julia_call('c2.', design@jdesign, as.integer(x1) ) )
}

#' @rdname Design
#'
#' @export
futility_region <- function(design) {
    JuliaCall::julia_call('futility_region', design$jdesign)
}

#' @rdname Design
#'
#' @export
efficacy_region <- function(design) {
    JuliaCall::julia_call('efficacy_region', design$jdesign)
}

#' @rdname Design
#'
#' @export
continuation_region <- function(design) {
    JuliaCall::julia_call('continuation_region', design$jdesign)
}

#' @rdname Design
#'
#' @param x2 stage-two number of responses
#' @param p response probability
#'
#' @export
pmf <- function(design, x1, x2, p) {
    JuliaCall::julia_call('pmf.', as.integer(x2), sample_size(design, x1), p) *
    JuliaCall::julia_call('pmf.', as.integer(x1), sample_size(design), p)
}

#' @rdname Design
#'
#' @param x2 stage-two number of responses
#' @param p response probability
#'
#' @export
probability_to_reject <- function(design, p, x1 = NULL) {
    n1 <- sample_size(design)
    if (is.null(x1)) {
        n2 <- sample_size(design, 0:n1) - n1
        return(sum(
            dbinom(0:n1, size = n1, prob = p) *
            (1 - pbinom(critical_value(design, 0:n1), size = n2, prob = p))
        ))
    } else {
        n2 <- sample_size(design, x1) - n1
        return( 1 - pbinom(critical_value(design, x1), size = n2, prob = p) )
    }
}
probability_to_reject <- Vectorize(probability_to_reject, vectorize.args = c('p', 'x1'))

#' @rdname Design
#'
#'@export
expected_sample_size <- function(design, p) {
    n1 <- sample_size(design)
    return( sum( dbinom(0:n1, size = n1, prob = p) * sample_size(design, 0:n1) ) )
}
expected_sample_size <- Vectorize(expected_sample_size, vectorize.args = c('p'))


#' @rdname Design
#'
#'@export
sample_space <- function(design) {
    res           <- JuliaCall::julia_call('sample_space', design$jdesign)
    colnames(res) <- c('x1', 'x2')
    return(as_tibble(res))
}

#' @rdname Design
#'
#' @export
reject <- function(x1, x2, design) {
    JuliaCall::julia_call('reject.', as.integer(x1), as.integer(x2), design$jdesign)
}


#' @rdname Design
#'
#' @importFrom tibble as_tibble
#' @export
as_tibble.Design <- function(design) {
    tibble::tibble(
        label   = design@label,
        n1      = rep(sample_size(design), sample_size(design) + 1),
        x1      = seq(0, sample_size(design)),
        `x1/n1` = x1/n1,
        n2      = sample_size(design, x1) - n1,
        c2      = critical_value(design, x1),
        n       = sample_size(design, x1),
        c       = c2 + x1
    )
}

#' @rdname Design
#'
#' @importFrom ggrepel geom_text_repel
#' @importFrom cowplot plot_grid
#' @export
plot.Design <- function(design, tbl_power_annotations = NULL, ...) {
    tbl_plot <- get_tbl_plot(design)
    n1       <- sample_size(design)
    p1 <- ggplot(tbl_plot) +
        aes(x1, x) +
        geom_tile(aes(fill = reject), width = .45, height = .75) +
        geom_tile(fill = "white", width = .25, height = .3,
            data = filter(tbl_plot, x > n1)
        ) +
        geom_segment(x = -.45/2, xend = .45/2, y = n1, yend = n1,
            color = ifelse(critical_value(design, 0) >= 0, 'darkgray', 'black')
        ) +
        geom_segment(x = 0, xend = 0, y = n1 - .75/2, yend = n1 + .75/2,
            color = ifelse(critical_value(design, 0) >= 0, 'darkgray', 'black')
        ) +
        annotate('text', x = 0, y = 1.075*max(sample_size(design, 0:n1)),
                 label = sprintf("stage-one sample size: %i", n1),
                 hjust = 0, size = 3.5) +
        scale_x_continuous("stage-one responses", limits = c(0, NA)) +
        scale_y_continuous("",
            limits = c(0, 1.075*max(sample_size(design, 0:n1)) )
        ) +
        scale_fill_manual('', breaks = c(FALSE, TRUE), aesthetics = c('fill', 'color'),
            labels = c('not reject', 'reject'), values = c('darkgray', 'black')
        ) +
        theme_bw() +
        theme(
            panel.grid.major.x   = element_blank(),
            panel.grid.minor.x   = element_blank(),
            panel.grid.minor.y   = element_blank(),
            legend.position      = c(.99, .99),
            legend.justification = c(1, 1),
            legend.direction     = 'horizontal',
            legend.key.size      = grid::unit(.5, 'lines'),
            plot.margin          = margin(.1, .1, .1, .1, 'lines')
        )
    tbl_plot <- tibble(
        p     = seq(0, 1, by = .001),
        power = probability_to_reject(design, p)
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
        scale_x_continuous("response probability", breaks = p_breaks, expand = c(0, 0)) +
        theme_bw() +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.margin        = margin(.1, .5, .1, .1, 'lines')
        )
    if (inherits(tbl_power_annotations, 'tbl_df')) {
        tbl_power_annotations <- tbl_power_annotations %>%
            mutate(
                power = probability_to_reject(design, p),
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
