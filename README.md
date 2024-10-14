# r4tcpl

###### A collection of utility functions used in our R projects
Packages are the fundamental units of reproducible __R__ code. They include
reusable __R__ functions, the documentation that describes how to use them, and
sample data. You can find a thorough discussion of this topic in:
**R packages (2e)** by *Hadley Wickham* and *Jenny Bryan*, available online at:
https://r-pkgs.org/


## Building dependencies
This is what you need to _build_ the package, not the packages that __r4tcpl__
will need to run properly. Those are listed further down (see __Package
dependencies__ section).

1. If you are working on __Windows__, install the
[__RTools__](https://cran.r-project.org/bin/windows/Rtools/) version appropriate
for the __R__ release you are using.

1. Make sure `devtools`, `roxygen2`, `testthat`, and `knitr` packages are
already installed, otherwise install them running:
`install.packages(c("roxygen2", "testthat", "knitr", "devtools"))`.


## Basic workflow to add new functions and data
1. Write/copy your function(s) into a .R file within `.../r4tcpl/R/`.

1. Remember to include the `@export` tag in the `roxygen` comments of all the
functions you want to make visible when `r4tcpl` package will be loaded. When
building the package, the corresponding `export()` statement will be
automatically generated inside `NAMESPACE` file.

1. It’s important to note that `library()` should **NEVER** be used inside a
package. Use instead the `::` notation to explicitly refer specific namespaces.

1. For each dependency (i.e., wherever you used the syntax
`<package_name>::<object>`) remember to add `package_name` to `Imports` field
within the `DESCRIPTION` file by typing
`usethis::use_package("package_name", min_version=TRUE)` from the project
directory `.../r4tcpl/` (set it as the working directory in __RStudio__).

1. Unlike *User Libraries*, *System Libraries* don't need to be declared as
dependencies (since they are expected to be always installed by default in every
__R__ distribution). However, whenever some function from a System Library other
than *The R Base Package* (`base`) is used it is recommended to import it or the
entire library in the package namespace by adding the `roxygen` tags
`@importFrom` or `@import`, respectively. When building the package, the
corresponding `import()` statement will be automatically generated inside
`NAMESPACE` file.

1. Add example data sets to the package running `usethis::use_data(<my_pkg_data>)`
from the project directory. This command will save the data contained in the R
variable `<my_pkg_data>` to the folder `.../r4tcpl/data/` as a binary `.rda`
representation (storing one R object in each .rda file).

1. Remember to provide a complete documentation for both functions (by roxygen2
heading) and data (by editing the file `.../r4tcpl/R/data.R`), otherwise you
will get a warning when building the package.

1. From the project directory, run `devtools::document()` to convert roxygen
comments into proper __R__ documentation and (re)generate `NAMESPACE` (based on
`@export` tags). Then you can use `pkgload::dev_help('<function_name>')` to have
a quick preview of the `<function_name>.Rd` file.

1. Possibly update tests in `.../r4tcpl/tests/testthat/test-data_exploration.R`.

1. Possibly update the package version in the `DESCRIPTION` file

1. From the project directory, run `devtools::check()` to build the package
(remember that all `@examples` will be used as tests in this phase; to prevent
some of them from being run put them inside a `\dontrun{}` section).

1. Use `devtools::load_all()` to load the package from the local directory and
possibly test the new features.

1. When you are happy, you can `git add`, `commit`, and `push`.

1. Regardless of your wd, use `devtools::install_github("TCP-Lab/r4tcpl")` to
install (or update) the package from __GitHub__, and load it as usual through
`library(r4tcpl)`.


## Package dependencies
The following packages need to be preinstalled before running
`devtools::install_github("TCP-Lab/r4tcpl")`.
- GEOquery
- ggplot2
- svDialogs
- tools
- dplyr
- stringr
- AnnotationDbi
- VennDiagram
- futile.logger
- org.Hs.eg.db
- mclust


## How to rename the package
1. Update the package name field in the `DESCRIPTION` file.
1. Update all package names in the `README` file.
1. Update the `.RProj` name.
1. Update the `.Rbuildignore` file.
1. Within the folders `.../r4tcpl/R/` and `.../r4tcpl/tests/` look for mentions
of the old package name within all .R files and update them.
1. `git add`, `commit`, and `push` these changes to the old repository.
1. Delete the local repository folder.
1. Go to GitHub and use the repository _Settings_ to rename it.
1. `git clone` locally the renamed repository.
1. From RStudio, run `devtools::document()` to update documentation (i.e., Rd
files).
1. Run `devtools::check()` to build the renamed package.
1. If everything went fine, `git add`, `commit`, and `push` some changes to the
new repository as a final test.
1. Finally, remove your old package from __R__ by `remove.packages("oldName")`...
1. ...and install the new one `devtools::install_github("TCP-Lab/newName")`

## Troubleshooting
If after `devtools::install_github("TCP-Lab/r4tcpl")` you get this error
```
Using GitHub PAT from the git credential store.
Error: Failed to install 'r4tcpl' from GitHub:
  HTTP error 401.
  Bad credentials
```
you've got to find where the token is stored by running
```
Sys.getenv("GITHUB_TOKEN")
Sys.getenv("GITHUB_PAT")
gitcreds::gitcreds_get()
gitcreds::gitcreds_get()$password
```
and then remove it through, e.g., `gitcreds::gitcreds_delete()`, as per
[this thread](https://github.com/r-lib/remotes/issues/797).


