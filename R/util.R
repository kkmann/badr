get_tbl_plot <- function(design) {
    tibble(
        x1 = 0:n1(design),
        n1 = n1(design)
    )
    as_tibble(design) %>%
        select(x1, n1, n2, c2) %>%
        group_by(x1) %>%
        nest() %>%
        mutate(
            tmp = map2(x1, data, function(rate, d)
                expand_grid(xx1 = 0:d$n1, x2 = 0:d$n2) %>%
                mutate(reject = x2 > d$c2) %>%
                filter((xx1 >= d$n1 - x1 + 1) | (x1 == 0 & d$n2 > 0),
                       !(x1 == 0 & xx1 > 0), !(x1 == 0 & x2 == 0) )
            )
        ) %>%
        unnest(tmp) %>% unnest(data) %>%
        ungroup() %>%
        transmute(
            x1,
            x       = ifelse(x1 == 0, n1 + x2, xx1 + x2),
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