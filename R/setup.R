#'@export
setup_julia_package <- function(...) {
    JuliaCall::julia_setup(...)
    JuliaCall::julia_install_package('https://github.com/kkmann/bad.jl')
}
