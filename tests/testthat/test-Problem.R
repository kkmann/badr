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

design <- optimise(problem)

as_tibble(design)



plot(design)
