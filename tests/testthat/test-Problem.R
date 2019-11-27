prior <- Beta(3, 4)
p0    <- .2
mrv   <- .3
alpha <- .05
beta  <- .2

problem <- Problem(
    minimise_expected_sample_size(prior),
    maximal_type_one_error_rate(p0, alpha),
    minimal_expected_power(prior, mrv, 1 - beta)
)

design1 <- optimise(problem)

as_tibble(design1)

power(design1, seq(0, 1, by = .01))

plot(design1,
     tbl_power_annotations = tibble(
         p     = c(p0, mrv, .55),
         label = c('null', 'MRV', '???')
     )
)

prior <- Beta(5, 6)

problem <- Problem(
    minimise_expected_sample_size(prior),
    maximal_type_one_error_rate(p0, alpha),
    minimal_expected_power(prior, mrv, 1 - beta)
)

design2 <- optimise(problem)

plot_designs(a = design1, b = design2)
