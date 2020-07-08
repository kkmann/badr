#' badr
#'
#' [todo]
#'
#' @import ggplot2 dplyr tidyr purrr
#' @importFrom magrittr %>%
#' @importFrom methods is new
#' @importFrom stats complete.cases pbinom qnorm
#'
#' @export %>%
#'
#' @docType package
#' @name badr
NULL

#' @export
load_julia_package <- function(verbose = FALSE, useRCall = FALSE, ...){
    tryCatch({
        JuliaCall::julia_setup(..., verbose = verbose, useRCall = useRCall)
        JuliaCall::julia_command(sprintf(
            'import Pkg; Pkg.activate("%s"); Pkg.instantiate(); using bad',
            system.file("julia_environment", package = "badr")
        ))
    }, error = function(e) {
        packageStartupMessage("Could not load julia package 'bad', make sure julia is found and 'bad' is installed. Try running 'badr::load_julia_package()' with optional arguments for 'JuliaCall::julia_setup()' to install.")
    })
}
