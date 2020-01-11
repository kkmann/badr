#' @rdname Design
#'
#' @param ... one ore more \code{Design} objects to plot
#' @param min.segment.length passed on to ggrepel
#' @param tbl_power_annotations a \code{\link{tibble}} with annotation data for
#' plotted in the expected sample size and power plots.
#'
#' @importFrom cowplot get_legend
#' @include Design.R
#' @export
plot_designs <- function(..., tbl_power_annotations = NULL, min.segment.length = Inf) {
    designs <- list(...)
    if (is.null(names(designs))) names(designs) <- map_chr(designs, ~.@label)
    i <- 1
    for (j in 1:length(designs)) {
        if (names(designs)[j] == '') {
            str <- ifelse(designs[[j]]@label != '',
                          designs[[j]]@label,
                          sprintf("design_%i", i)
                )
            names(designs)[j] <- str
            i <- i + 1
        }
    }
    tbl_plot <- map(
            seq(1, length(designs)),
            function(i) get_tbl_plot(designs[[i]]) %>%
                mutate(
                    design_name = names(designs)[i],
                    n1          = sample_size(designs[[i]])
                )
        ) %>%
        bind_rows()
    tbl_zeros <- tibble(
            design      = designs,
            design_name = names(designs)
        ) %>%
        mutate(
            data = map(design, ~tibble(
                    x      = c(0, -.45/2),
                    xend   = c(0,  .45/2),
                    y      = c(sample_size(.) - .75/2, sample_size(.)),
                    yend   = c(sample_size(.) + .75/2, sample_size(.)),
                    reject = ifelse(critical_value(., 0) >= 0, FALSE, TRUE)
                )
            )
        ) %>%
        filter(map_lgl(design, ~sample_size(., 0) == sample_size(.) )) %>%
        unnest(data) %>%
        select(-design)
    tbl_n1 <- tibble(
            design      = designs,
            design_name = names(designs)
        ) %>%
        mutate(
            n1 = map_dbl(design, sample_size)
        ) %>%
        select(-design)
    p1 <- ggplot(tbl_plot) +
        aes(x = x1, y = x) +
        geom_tile(aes(fill = reject), width = .45, height = .75) +
        geom_tile(fill = "white", width = .25, height = .3,
                  data = filter(tbl_plot, x > n1)
        ) +
        geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = reject),
                     data = tbl_zeros) +
        geom_text(aes(label = sprintf("stage-one sample size: %i", n1)),
                  x = 0, y = 1, data = tbl_n1, size = 2, hjust = 0) +
        scale_x_continuous("stage-one responses") +
        scale_y_continuous("", breaks = seq(0, max(tbl_plot$x), by = 10)) +
        scale_fill_manual('', breaks = c(FALSE, TRUE), labels = c('not reject', 'reject'),
                          values = c('darkgray', 'black'), aesthetics =  c('fill', 'color')) +
        facet_wrap(~design_name, nrow = 1, scales = 'free_x') +
        theme_bw() +
        theme(
            panel.grid.major.x   = element_blank(),
            panel.grid.minor.x   = element_blank(),
            legend.position      = 'top',
            legend.direction     = 'horizontal',
            legend.key.size      = grid::unit(.5, 'lines')
        )
    legend1 <- cowplot::get_legend(p1)
    tbl_plot <- map(
            seq(1, length(designs)),
            function(i) tibble(
                design_name = names(designs)[i],
                p           = seq(0, 1, by = .001),
                power       = probability_to_reject(designs[[i]], p),
                ess         = expected_sample_size(designs[[i]], p)
            )
        ) %>%
        bind_rows()
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
        geom_line(aes(linetype = design_name), color = 'darkgray', size = .5) +
        scale_y_continuous("power", breaks = seq(0, 1, .1)) +
        scale_x_continuous("response probability", breaks = p_breaks, expand = c(0, 0)) +
        scale_linetype_discrete("") +
        theme_bw() +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.direction   = 'horizontal',
            plot.margin        = margin(.1, .5, .1, .1, 'lines')
        )
    if (inherits(tbl_power_annotations, 'tbl_df')) {
        tbl_power_annotations <- tbl_power_annotations %>%
            mutate(
                data = map(
                    p,
                    ~tibble(
                        design_name = names(designs),
                        power       = map_dbl(designs, function(x) probability_to_reject(x, .)),
                        ess         = map_dbl(designs, function(x) expected_sample_size(x, .))
                    )
                )
            ) %>%
            unnest(data) %>%
            mutate(
                label_power = sprintf("%.2f%% (%s)", 100*power, design_name),
                label_ess   = sprintf("%.2f (%s)", ess, design_name)
            )
        p2 <- p2 +
            geom_vline(aes(xintercept = p), color = 'lightgray', size = .5,
                       data = tbl_power_annotations) +
            ggrepel::geom_text_repel(
                aes(label = label_power), nudge_x = .1, nudge_y = .01, size = 2,
                segment.color = 'darkgray', seed = 42, max.iter = 10000,
                min.segment.length = min.segment.length,
                # xlim = c(0, 1), ylim = c(0, 1),
                data = tbl_power_annotations
            ) +
            geom_point(data = tbl_power_annotations, size = .5)
    }
    p3 <- ggplot(tbl_plot) +
        aes(p, ess) +
        geom_line(aes(linetype = design_name), color = 'darkgray', size = .5) +
        scale_y_continuous("expected sample size", limits = c(0, NA)) +
        scale_x_continuous("response probability", breaks = p_breaks, expand = c(0, 0)) +
        scale_linetype_discrete("") +
        theme_bw() +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position    = 'none',
            plot.margin        = margin(.1, .5, .1, .1, 'lines')
        )
    if (inherits(tbl_power_annotations, 'tbl_df')) {
        p3 <- p3 +
            geom_vline(aes(xintercept = p), color = 'lightgray', size = .5,
                       data = tbl_power_annotations) +
            ggrepel::geom_text_repel(
                aes(label = label_ess), nudge_x = .1, nudge_y = -1, size = 2,
                segment.color = 'darkgray', seed = 42, max.iter = 10000,
                min.segment.length = min.segment.length,
                # xlim = c(0, 1),
                data = tbl_power_annotations
            ) +
            geom_point(data = tbl_power_annotations, size = .5)
    }
    legend2 <- cowplot::get_legend(p2)
    cowplot::plot_grid(
        p1 + theme(legend.position = 'none'),
        cowplot::plot_grid(legend1, legend2),
        cowplot::plot_grid(p2 + theme(legend.position = 'none'), p3),
        ncol = 1,
        rel_heights = c(1.15, .15, 1)
    )
}
