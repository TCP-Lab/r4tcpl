#' General Test Variable (Animals, Ages, and Owners)
#' 
#' A list containing the most common basic data type you can find in R. Useful
#' for interactive trials and testing.
#'
#' @format ## `x`
#' A list containing 12 named objects of different types.
#' \describe{
#'   \item{animal}{Character vector with no duplicated entries}
#'   \item{owner}{Character vector with duplicated entries}
#'   \item{named_animal}{Named character vector (with duplicated names)}
#'   \item{a_age}{Numeric vector with no duplicated entries}
#'   \item{o_age}{Numeric vector with duplicated entries}
#'   \item{named_ages}{Named numeric vector (with duplicated names)}
#'   \item{fur}{Logical vector}
#'   \item{sex}{A factor with 3 levels}
#'   \item{whos}{Character matrix}
#'   \item{ages}{Numeric matrix}
#'   \item{named_ages}{Numeric matrix with (duplicated) row and column names}
#'   \item{all_data}{A data frame containing all the previous objects}
#' }
#' @source Created by me (FeA.R). See comments in .../cmatools/R/data.R to
#'         recreate or modify the dataset.
"x"
## Source code for x
# animal <- c("dog", "cat", "mouse", "fish", "hamster")
# owner <- c("Mike", "Tom", "Mike", "James", "Julia")
# named_animal <- animal
# names(named_animal) <- owner
# a_age <- c(6, 12, 2, NA, 1)
# o_age <- c(17, 30, 17, 5, 25)
# named_age <- o_age
# names(named_age) <- owner
# fur <- c(T, T, F, F, T)
# sex <- factor(c("male", "female", "male", "hermaphrodite", "female"))
# whos <- matrix(c(animal, owner), nrow = 5, ncol = 2)
# ages <- matrix(c(a_age, o_age), nrow = 5, ncol = 2)
# named_ages <- ages
# row.names(named_ages) <- owner
# colnames(named_ages) <- c("animal", "owner")
# all_data = data.frame(a_age, sex, fur, owner, o_age,
#                       row.names = c("dog", "cat", "mouse", "fish", "hamster"))
# x <- list(animal = animal,
#           owner = owner,
#           named_animal = named_animal,
#           a_age = a_age,
#           o_age = o_age,
#           named_age = named_age,
#           fur = fur,
#           sex = sex,
#           whos = whos,
#           ages = ages,
#           named_ages = named_ages,
#           all_data = all_data)



#' Anti-TNFalpha-induced differential gene expression in AR patients.
#' 
#' DEG list from my Rheumatoid Arthritis (AR) microarray study (references...)
#' This subset contains the expression values for each sample taking part in the
#' Etanercept (anti-TNFalpha) vs Methotrexate (MTX) contrast (n = 5 and n = 6,
#' respectively). Row names are Agilent probe_IDs. Additional columns (1 to 3)
#' contains some gene annotation, namely: GeneSymbol, GeneName, and Category
#' membership.
#' Row-wise, only statistically significant genes are included in this dataset.
#'
#' @format ## `DEGs`
#' A data frame with 436 rows and 14 columns:
#' \describe{
#'   \item{GeneSymbol}{HGNC Official Gene Symbol}
#'   \item{GeneName}{Official Gene Name}
#'   \item{Category}{Manual curated membership to one of the following
#'    categories: NA, "Inflammation", "Immune Response", "Metallopeptidase"}
#'   \item{Anti-TNFa_1 : Anti-TNFa_5}{anti-TNFalpha-treated patients}
#'   \item{MTX_1 : MTX_6}{Methotrexate-treated patients}
#' }
#' @source <https://www.referenzaweb>
"DEGs"



#' Titanium surface ISO 25178 descriptors
#' 
#' ISO 25178: Geometrical Product Specifications (GPS) – Surface texture: areal.
#'
#' @format ## `ISO`
#' A data frame with 8 rows and 11 columns:
#' \describe{
#'   \item{Sa}{Arithmetical mean height of the surface}
#'   \item{Sku}{Kurtosis of height distribution}
#'   \item{Sp}{Maximum height of peaks}
#'   \item{Sq}{Root mean square height of the surface}
#'   \item{Ssk}{Skewness of height distribution}
#'   \item{Sv}{Maximum height of valleys}
#'   \item{Sz}{	Maximum height of the surface}
#'   \item{Sdq}{Root mean square gradient of the surface}
#'   \item{Sdr}{Developed area ratio}
#'   \item{Sal}{Fastest decay auto-correlation rate}
#'   \item{Str}{Texture aspect ratio of the surface}
#' }
#' @source <https://en.wikipedia.org/wiki/ISO_25178>
"ISO"



