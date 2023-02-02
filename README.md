# cmatools

###### A collection of utility functions used in our R projects

Packages are the fundamental units of reproducible R code. They include reusable
R functions, the documentation that describes how to use them, and sample data.
You can find a thorough discussion of this topic in:
**R packages (2e)** by *Hadley Wickham* and *Jenny Bryan*, available online at:
https://r-pkgs.org/


## Basic workflow:

1. Make sure `devtools`, `roxygen2`, `testthat`, and `knitr` packages are
already installed, otherwise install them running:
`install.packages(c("devtools", "roxygen2", "testthat", "knitr"))`.

1. Write/copy your function(s) into a R file in `.../cmatools/R/`.

1. Remember to include the `@export` tag in the `roxygen` comments of all the
functions you want to make visible when `cmatools` package will be loaded.

1. It’s important to note that `library()` should **NEVER** be used inside a
package. Use instead the `::` notation to explicitly refer specific namespaces.

1. Set the project directory as the working directory in RStudio.

1. For each dependency, use `usethis::use_package("package_name", min_version = TRUE)`
to add `package_name` to `Imports` field in `DESCRIPTION`.

1. Run `devtools::document()` to convert roxygen comments into proper R
documentation and (re)generate `NAMESPACE` (based on `@export` tags).

1. Possibly update tests in `.../cmatools/tests/testthat/test-data_exploration.R`.

1. Run `devtools::check()` (Remember that all `@examples` will be used as tests
in this phase. To prevent some of them from being run put them inside a
`\dontrun{}` section).

1. Use `devtools::load_all()` to load the package from the local directory.

1. Regardless of your wd, use `devtools::install_github("CMA-Lab/cmatools")` to
install (or update) the package from GitHub, and load it as usual through
`library(cmatools)`.
