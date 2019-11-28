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

#' @export
binomial_test <- function(alpha, beta, p0, p1) {
    z_1_a   <- qnorm(1 - alpha)
    z_1_b   <- qnorm(1 - beta)
    napprox <- p1*(1 - p1)*((z_1_a + z_1_b) / (p1 - p0))^2
    nn      <- as.integer(ceiling(napprox))
    f       <- function(n) {
        candidates <- which((1 - pbinom(seq(0, n), size = n, prob = p0) ) > alpha)
        ifelse(length(candidates) == 0, n, tail(candidates, 1))
    }
    cc      <- f(nn)
    tibble(
        n     = seq(0, nn),
        c     = map_int(n, f),
        power = 1 - pbinom(c, size = n, prob = p1),
        toer  = 1 - pbinom(c, size = n, prob = p0)
    ) %>%
    filter(power >= 1 - beta, toer <= alpha) %>%
    arrange(n) %>%
    {
        return(list(n = pull(., n)[1], c = pull(., c)[1]))
    }
}