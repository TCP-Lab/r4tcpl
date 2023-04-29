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

1. Write/copy your function(s) into a .R file within `.../cmatools/R/`.

1. Remember to include the `@export` tag in the `roxygen` comments of all the
functions you want to make visible when `cmatools` package will be loaded. When
building the package, the corresponding `export()` statement will be
automatically generated inside `NAMESPACE` file.

1. It’s important to note that `library()` should **NEVER** be used inside a
package. Use instead the `::` notation to explicitly refer specific namespaces.

1. For each dependency (i.e., wherever you used the syntax
`<package_name>::<object>`) remember to add `package_name` to `Imports` field in
`DESCRIPTION` by typing `usethis::use_package("package_name", min_version=TRUE)`
from the project directory `.../cmatools/` (i.e., set it as the working
directory in RStudio).

1. Unlike *User Libraries*, *System Libraries* don't need to be declared as
dependencies (since they are expected to be always installed by default in every
R distribution). However, whenever some function from a System Library other than
*The R Base Package* (`base`) is used it is recommended to import it or the
entire library in the package namespace by adding the `roxygen` tags `@importFrom`
or `@import`, respectively. When building the package, the corresponding
`import()` statement will be automatically generated inside `NAMESPACE` file.

1. Add example data sets to the package running `usethis::use_data(<my_pkg_data>)`
from the project directory. This command will save the data contained in the R
variable `<my_pkg_data>` to the folder `.../cmatools/data/` as a binary (.rda)
representation (Store one R object in each .rda file). Finally, also remind to
document the data by editing the file `.../cmatools/R/data.R`

1. From the project directory, run `devtools::document()` to convert roxygen
comments into proper R documentation and (re)generate `NAMESPACE` (based on
`@export` tags).

1. Possibly update tests in `.../cmatools/tests/testthat/test-data_exploration.R`.

1. Possibly update the package version in the `DESCRIPTION` file

1. From the project directory, run `devtools::check()` to build the package
(remember that all `@examples` will be used as tests in this phase; to prevent
some of them from being run put them inside a `\dontrun{}` section).

1. Now you can `git add`, `commit`, and `push`.

1. Use `devtools::load_all()` to load the package from the local directory.

1. Regardless of your wd, use `devtools::install_github("CMA-Lab/cmatools")` to
install (or update) the package from GitHub, and load it as usual through
`library(cmatools)`.
