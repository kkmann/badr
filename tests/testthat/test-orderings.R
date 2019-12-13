prior <- PointMass(.4)
p0    <- .2
alpha <- .05; beta  <- .2

design <- Problem(
        minimise_expected_sample_size(PointMass(.2)),
        maximal_type_one_error_rate(p0, alpha),
        minimal_expected_power(prior, p0, 1 - beta)
    ) %>%
    optimise()

print(design$jdesign)

XX <- sample_space(design)

tbl_estimators <- tibble(
        mle = list(MaximumLikelihoodEstimator()),
        pme = list(PosteriorMeanPrecalculated(JeffreysPrior(design), design)),
        rbe = list(RaoBlackwellEstimator()),
        cme = list(CompatibleMLE(design))
    ) %>%
    pivot_longer(
        everything(), values_to = 'estimator'
    ) %>%
    mutate(
        pvalue = map(estimator, ~PValue(., design, p0, orientation = 'superiority'))
    )

tbl_compatibility <- XX %>%
    mutate(
        name      = list(tbl_estimators$name),
        estimator = list(tbl_estimators$estimator),
        pvalue    = list(tbl_estimators$pvalue)
    ) %>%
    unnest(c(name, estimator, pvalue)) %>%
    mutate(
        estimate = pmap( list(x1, x2, estimator), ~estimate(..3, ..1, ..2, design) ),
        p_value  = pmap( list(x1, x2, pvalue), ~get_p(..3, ..1, ..2) ),
        reject   = map2(x1, x2, ~reject(..1, ..2, design))
    ) %>%
    unnest(c(estimate, p_value, reject))

ggplot(tbl_compatibility) +
    aes(x1, x2) +
    geom_tile(aes(fill = p_value), color = 'gray', width = 1, height = 1) +
    geom_point(shape = 4, size = .5,
        data = filter(tbl_compatibility, p_value > alpha, reject)) +
    geom_point(shape = 1, size = .5,
        data = filter(tbl_compatibility, p_value <= alpha, !reject)) +
    scale_fill_gradient('', low = 'white', high = 'black', limits = c(0, NA)) +
    facet_wrap(~name) +
    theme_bw() +
    theme(
        panel.grid = element_blank()
    )
