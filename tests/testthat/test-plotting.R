alpha <- .05
beta  <- .2
pnull <- .2
palt  <- .4

binomial_test(alpha, beta, pnull, palt) %>%
    {nn <<- .$n; cc <<- .$c}

dsgn_binom <- Design(
    rep(0, nn + 1),
    c(rep(Inf, cc + 1), rep(-Inf, nn - cc)),
    label = 'binomial'
)

dsgn_simon <- Design(
    c(rep(0, 4), rep(30, 10)),
    c(rep(Inf, 4), rep(12, 10) - seq(4, 13)),
    label = "Simon's"
)

plot(dsgn_simon)

plot(dsgn_binom)

plot_designs(dsgn_binom, dsgn_simon, tbl_power_annotations = tibble(
    p     = c(pnull, palt),
    label = c('null', 'MRV')
))
