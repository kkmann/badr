fix_design_names <- function(...) {
    designs <- list(...)
    # figure out design labels
    if (is.null(names(designs))) names(designs) <- purrr::map_chr(designs, ~.@label)
    # fix missing labels
    i <- 1; for (j in 1:length(designs)) {
        if (names(designs)[j] == "") {
            defaultlabel <- ifelse(designs[[j]]@label != "", designs[[j]]@label, sprintf("design %i", i) )
            names(designs)[j] <- defaultlabel
            i                 <- i + 1
        }
    }
    return(designs)
}

fix_p_breaks <- function(annotations, threshold = .15) {
    p_breaks <- seq(0, 1, by = .2)
    if (!is.null(annotations)) {
        pp <- annotations
        for (i in 1:length(p_breaks)) {
            if (any(abs(pp - p_breaks[i]) < threshold)) p_breaks[i] <- NA_real_
        }
        p_breaks <- unique(c(p_breaks[complete.cases(p_breaks)], annotations))
    }
    round(p_breaks, 2)
}

#' @rdname Design
#'
#' @param textsize size of c2/n2 annotation in design plot
#' @param ystep tick marks step in design plot y axis
#' @param yexpandlower factor for (lower) y axis expansion in design plot,
#' @param skip_listing character vector with design labels for which to not
#' include the c2/n2 listings
#'
#' @include Design.R
#' @export
plot_designs <- function(..., textsize = 1.75, ystep = 10, yexpandlower = .1,
                         skip_listing = NULL) {

    designs    <- fix_design_names(...)

    .dummy <<- map(designs,  as_tibble) %>% bind_rows()

    plt <- ggplot(.dummy) +
        aes(x1) +
        geom_hline(aes(yintercept = n1), linetype = 'dotted') +
        geom_segment(aes(xend = x1, y = 0, yend = n1 + ifelse(is.finite(c2), c2, 0)), color = 'gray') +
        geom_segment(aes(xend = x1, y = pmax(n1, pmin(n, n1 + c2 + 1)), yend = n)) +
        geom_segment(aes(xend = x1, y = 0, yend = ifelse(c2 < 0, n1, 0))) +
        geom_text(aes(
                label = sprintf("%s\n%s",
                    ifelse(label %in% skip_listing, " ", sprintf("%.0f", c2)),
                    ifelse(label %in% skip_listing, " ", sprintf("%i", n2))
                ),
                y = -1.5
            ),
            size = textsize, lineheight = .8, vjust = 1) +
        scale_y_continuous("", breaks = seq(0, 1000, by = ystep),
                           expand = expansion(mult = c(yexpandlower, .05))) +
        scale_x_continuous(expression(x[1]), breaks = seq(0, 1000, by = 5)) +
        facet_grid(1 ~ label, scales = 'free_x', space = "free_x",
                   labeller = label_bquote(
                       cols = .(label) ~ (n[1] == .(.dummy[.dummy$label == label, ]$n1[[1]]))
                   )
        ) +
        theme_bw() +
        theme(
            panel.grid = element_blank(),
            plot.margin        = margin(.1, .75, .1, .1, 'lines'),
            strip.background.y = element_blank(),
            strip.text.y       = element_blank()
        )

    return(plt)

}



#' @rdname Design
#'
#' @param ... one more more \code{Design} objects to plot
#'
#' @include Design.R
#' @export
plot_power <- function(...,  annotations = NULL, min.segment.length = Inf,
                       annotation_text_size = 1.5) {

    designs  <- fix_design_names(...)
    tbl_plot <- map(
            names(designs),
            function(name) tibble(
                design_name = name,
                p           = seq(0, 1, by = .001),
                power       = probability_to_reject(designs[[name]], p)
            )
        ) %>%
        bind_rows()
    p_breaks <- fix_p_breaks(annotations, threshold = .15)
    plt <- ggplot(tbl_plot) +
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
            plot.margin        = margin(.1, .75, .1, .75, 'lines'),
            legend.position    = 'top'
        )
    if (!is.null(annotations)) {
        tbl_annotations <- tibble(
            p    = annotations,
            data = map(
                    annotations,
                    ~tibble(
                        design_name = names(designs),
                        power       = map_dbl(designs, function(x) probability_to_reject(x, .))
                    )
                )
            ) %>%
            unnest(data) %>%
            mutate(
                label = sprintf("%.2f%% (%s)", 100*power, design_name)
            )
        plt <- plt +
            geom_vline(aes(xintercept = p), color = 'lightgray', size = .5,
                       data = tbl_annotations) +
            ggrepel::geom_text_repel(
                aes(label = label), nudge_x = .1, nudge_y = .01, size = annotation_text_size,
                segment.color = 'darkgray', seed = 42, max.iter = 10000,
                min.segment.length = min.segment.length,
                data = tbl_annotations
            ) +
            geom_point(data = tbl_annotations, size = .5)
    }
    return(plt)
}



#' @rdname Design
#'
#' @include Design.R
#' @export
plot_ess <- function(...,  annotations = NULL, min.segment.length = Inf,
                     annotation_text_size = 2) {

    designs  <- fix_design_names(...)
    tbl_plot <- map(
            names(designs),
            function(name) tibble(
                design_name = name,
                p           = seq(0, 1, by = .001),
                ess         = expected_sample_size(designs[[name]], p)
            )
        ) %>%
        bind_rows()
    p_breaks <- fix_p_breaks(annotations, threshold = .15)
    plt <- ggplot(tbl_plot) +
        aes(p, ess) +
        geom_line(aes(linetype = design_name), color = 'darkgray', size = .5) +
        scale_y_continuous("ESS", limits = c(0, NA_real_)) +
        scale_x_continuous("response probability", breaks = p_breaks, expand = c(0, 0)) +
        scale_linetype_discrete("") +
        theme_bw() +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.direction   = 'horizontal',
            plot.margin        = margin(.1, .75, .1, .75, 'lines'),
            legend.position    = 'top'
        )
    if (!is.null(annotations)) {
        tbl_annotations <- tibble(
            p    = annotations,
            data = map(
                annotations,
                ~tibble(
                    design_name = names(designs),
                    ess         = map_dbl(designs, function(x) expected_sample_size(x, .))
                )
            )
        ) %>%
            unnest(data) %>%
            mutate(
                label = sprintf("%.2f (%s)", ess, design_name)
            )
        plt <- plt +
            geom_vline(aes(xintercept = p), color = 'lightgray', size = .5,
                       data = tbl_annotations) +
            ggrepel::geom_text_repel(
                aes(label = label), nudge_x = .1, nudge_y = .01, size = annotation_text_size,
                segment.color = 'darkgray', seed = 42, max.iter = 10000,
                min.segment.length = min.segment.length,
                data = tbl_annotations
            ) +
            geom_point(data = tbl_annotations, size = .5)
    }
    return(plt)
}



#' @rdname Design
#'
#' @param ... one more more \code{Design} objects to plot
#' @param annotations vector of response prbabilities to annotate
#' @param min.segment.length minimal lenght of arrow to annotations
#' @param annotation_text_size annotation text size
#' @param args_design_plot list of arguments for the design plot
#' @param args_power_plot list of arguments for the power plot
#' @param args_ess_plotlist of arguments for the expected sample size plot
#'
#' @importFrom cowplot get_legend
#' @include Design.R
#' @export
plot_compare_designs <- function(...,
        annotations = NULL, min.segment.length = Inf,
        annotation_text_size = 2,
        args_design_plot = list(textsize = 1.75, ystep = 10),
        args_power_plot  = list(
            annotations = annotations,
            min.segment.length = min.segment.length,
            annotation_text_size = annotation_text_size
        ),
        args_ess_plot    = list(
            annotations = annotations,
            min.segment.length = min.segment.length,
            annotation_text_size = annotation_text_size
        )
    ) {

    designs <- list(...)

    plt_designs <- do.call(plot_designs, args = c(designs, args_design_plot))
    plt_power   <- do.call(plot_power, args = c(designs, args_power_plot))
    plt_ess     <- do.call(plot_ess, args = c(designs, args_ess_plot))

    cowplot::plot_grid(
        plt_designs + theme(legend.position = 'none'),
        cowplot::get_legend(plt_power),
        cowplot::plot_grid(
            plt_power + theme(legend.position = 'none'),
            plt_ess + theme(legend.position = 'none')
        ),
        ncol = 1,
        rel_heights = c(1, .15, 1)
    )

}

