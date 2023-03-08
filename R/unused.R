
# From: R packages (2e) by Hadley Wickham and Jenny Bryan
#
# 12.5.1.1 How to not use a package in Imports
# Sometimes you have a package listed in Imports, but you don’t actually use it
# inside your package or, at least, R doesn’t think you use it. That leads to a
# NOTE from R CMD check:
#  
#  * checking dependencies in R code ... NOTE
#  Namespace in Imports field not imported from: ‘<some_package>’
#  All declared Imports should be used.
#
# This can happen if your only use of <some_package> is a call like
# <some_package>::<some_function>() in the default for a function argument in
# your package. This also comes up if you want to list an indirect dependency in
# Imports, so you can state a minimum version for it. The tidyverse meta-package
# has this problem on a large scale, since it exists mostly to install a bundle
# of packages at specific versions.
#
# How can you get rid of this NOTE?
#
# Our recommendation is to put a namespace-qualified reference (not a call) to
# an object in <some_package> in some file below R/, such as a .R file
# associated with package-wide setup.

ignore_unused_imports <- function() {
  stringr::str_length
  dplyr::mutate
  org.Hs.eg.db::org.Hs.eg.db
}