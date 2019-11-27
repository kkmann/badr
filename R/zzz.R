#' badr
#'
#' [todo]
#'
#' @import tidyverse
#'
#' @docType package
#' @name badr
NULL

.onLoad <- function(libname, pkgname){
    tryCatch({
        JuliaCall::julia_command('using bad')
        message("Loaded julia package 'bad' successfully.")
    }, error = function(e) {
        print(e)
        message("Could not load julia package 'bad', make sure julia is found and 'bad' is installed. Try running 'badr::setup_julia()' with optional arguments for 'JuliaCall::julia_setup()' to install.")
    })
}