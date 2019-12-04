prior <- Beta(5, 7)
p0    <- .2
alpha <- .05; beta  <- .2

design <- Problem(
        minimise_expected_sample_size(prior),
        maximal_type_one_error_rate(p0, alpha),
        minimal_expected_power(prior, p0 + .1, 1 - beta)
    ) %>%
    optimise()

XX <- sample_space(design)

estimators <- list(
    mle  = MaximumLikelihoodEstimator(),
    pme1 = PosteriorMean(prior),
    pme2 = PosteriorMean(JeffreysPrior(design)),
    rbe  = RaoBlackwellEstimator(),
    cme  = CompatibleMLE(design)
)

estimate(estimators$rbe, XX[,1], XX[,2], design)

tbl_performance <- expand_grid(
        estimator = names(estimators),
        p         = seq(0, 1, .01)
    ) %>%
    mutate(
        data = map2(p, estimator,
                ~tibble(
                    bias = bias(.x, estimators[[.y]], design),
                    RMSE = sqrt(mean_squared_error(.x, estimators[[.y]], design)),
                    MAE  = mean_absolute_error(.x, estimators[[.y]], design)
                )
            )
    ) %>%
    unnest(data) %>%
    pivot_longer(c(bias, RMSE, MAE), names_to = 'quantity')

ggplot(tbl_performance) +
    aes(p, value) +
    geom_line(aes(color = estimator)) +
    coord_fixed() +
    facet_wrap(~quantity)

