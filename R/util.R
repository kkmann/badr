get_tbl_plot <- function(design) {
    as_tibble(design) %>%
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
}
