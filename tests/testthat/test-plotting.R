load_julia_package()

alpha <- .05
beta  <- .2
pnull <- .2
palt  <- .4

design_simon <- Design(
    c(rep(0, 4), rep(30, 10)),
    c(rep(Inf, 4), rep(12, 10) - seq(4, 13)),
    label = "A"
)
design_simon2 <- Design(
    c(rep(0, 4), rep(30, 10)),
    c(rep(Inf, 4), rep(12, 10) - seq(4, 13)),
    label = "B"
)
design_simon3 <- Design(
    c(rep(0, 4), rep(30, 15)),
    c(rep(Inf, 4), rep(12, 15) - seq(4, 18)),
    label = "C"
)

plot_compare_designs(design_simon, annotations = c(.245, .5))


plot_designs(design_simon, design_simon2, design_simon3)

plot_compare_designs(design_simon, design_simon2, design_simon3,
    annotations = c(.245, .5),
    args_design_plot = list(
        ystep = 5, textsize = 2, yexpandlower = .1, skip_listing = "A"
    )
)
