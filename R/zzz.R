.onLoad <- function(libname, pkgname){
    tryCatch({
        JuliaCall::julia_command('using bad')
    }, error = function(e) {
        cat(e)
        stop("Could not load julia package 'bad', make sure julia is found and 'bad' is installed. Try running 'badr::setup_julia()' with optional arguments for 'JuliaCall::julia_setup()' to install.")
    })
}