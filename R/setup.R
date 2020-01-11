#'@export
setup <- function(...) {
    JuliaCall::julia_setup(...)
    JuliaCall::julia_install_package('https://github.com/kkmann/bad.jl')
    JuliaCall::julia_update_package('bad')
    JuliaCall::julia_command('using bad')
}
