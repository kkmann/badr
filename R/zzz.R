#' badr
#'
#' [todo]
#'
#' @import tidyverse
#'
#' @docType package
#' @name badr
NULL

#' @export
load_julia_package <- function(libname, pkgname){
    tryCatch({
        JuliaCall::julia_command('using bad')
        packageStartupMessage("Loaded julia package 'bad' successfully.")
    }, error = function(e) {
        packageStartupMessage("Could not load julia package 'bad', make sure julia is found and 'bad' is installed. Try running 'badr::setup_julia_package()' with optional arguments for 'JuliaCall::julia_setup()' to install.")
    })
}