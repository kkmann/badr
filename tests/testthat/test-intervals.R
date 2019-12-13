prior <- Beta(5, 7)
p0    <- .2
alpha <- .05; beta  <- .2

design <- Problem(
        minimise_expected_sample_size(prior),
        maximal_type_one_error_rate(p0, alpha),
        minimal_expected_power(prior, p0 + .1, 1 - beta)
    ) %>%
    optimise()

print(design$jdesign)

XX <- sample_space(design)


tmp = PosteriorCredibleInterval(prior, design, alpha)

mean_width(tmp, .5)
coverage_probability(tmp, .5)
get_bounds(tmp, c(0, 1), c(0,0))
