alpha <- .05; beta <- .2
p0 <- .2; p1 <- .4

binomial_test(alpha, beta, p0, p1) %>%
    {sample_size <<- .$n; critical_value <<- .$c}

dsgn_binom <- as_Design(
    rep(0, sample_size + 1),
    c(rep(Inf, critical_value + 1), rep(-Inf, sample_size - critical_value)),
    label = 'binomial'
)

dsgn_simon <- as_Design(
    c(rep(0, 4), rep(30, 10)),
    c(rep(Inf, 4), rep(12, 10) - seq(4, 13)),
    label = "Simon's"
)

plot(dsgn_binom)

plot(dsgn_simon)

plot_designs(dsgn_binom, dsgn_simon, tbl_power_annotations = tibble(
    p     = c(p0, p1),
    label = c('null', 'MRV')
))
