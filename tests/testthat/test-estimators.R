prior <- Beta(5, 7)
p0    <- .2
mrv   <- .3
alpha <- .05
beta  <- .2

design <- Problem(
        minimise_expected_sample_size(prior),
        maximal_type_one_error_rate(p0, alpha),
        minimal_expected_power(prior, mrv, 1 - beta)
    ) %>%
    optimise()

pme <- PosteriorMean(prior)
mle <- MaximumLikelihoodEstimator()

plot(bias(seq(0, 1, .01), pme, design))
plot(sqrt(mean_squared_error(seq(0, 1, .01), pme, design)))
plot(bias(seq(0, 1, .01), mle, design))
plot(sqrt(mean_squared_error(seq(0, 1, .01), mle, design)))
