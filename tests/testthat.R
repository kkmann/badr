library(testthat)
library(badr)

setup_julia_package()
load_julia_package()
test_check("badr")
