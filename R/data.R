#' The Nanto Warriors Data Set
#' 
#' This data set, inspired by the six Nanto warriors of the epic
#' **Hokuto no Ken** (aka *Fist of the North Star*) saga, is provided in the
#' form of a named list containing some of the most common basic data types you
#' can find in R. It is very useful for educational purposes, interactive
#' trials, and general testing.
#' 
#' @format ## `nanto`
#' A named list containing 12 named objects of different types.
#' \describe{
#'   \item{warrior}{Character vector with no duplicated entries}
#'   \item{hair_color}{Character vector with duplicated entries}
#'   \item{named_warrior}{Named character vector (with duplicated names)}
#'   \item{power}{Numeric (float) vector with no duplicated entries}
#'   \item{w_age}{Numeric (integer) vector with duplicated entries and NAs}
#'   \item{named_pwr}{Named numeric vector}
#'   \item{killed_by_Ken}{Logical vector}
#'   \item{sex}{A factor with 3 levels}
#'   \item{whos}{Character matrix}
#'   \item{scores}{Numeric matrix}
#'   \item{named_scores}{Numeric matrix with row and column names}
#'   \item{all_data}{A data frame containing all the previous objects}
#' }
#' @source <https://en.wikipedia.org/wiki/List_of_Fist_of_the_North_Star_characters>
"nanto"
## Source code to modify or recreate `nanto` data set
# warrior <- c("Shin", "Rei", "Yuda", "Shu", "Souther", "Yuria")
# hair_color <- c("blonde", "blue", "red", "blue", "blonde", "variable")
# named_warrior <- warrior
# names(named_warrior) <- hair_color
# power_score <- c(5.3, 7.7, 3.6, 7.8, 8.7, 10.0)
# w_age <- c(20, 19, 19, NA, NA, 22)
# named_pwr <- power_score
# names(named_pwr) <- warrior
# killed_by_Ken <- c(T, F, F, F, T, F)
# sex <- factor(c("male", "male", "hermaphrodite", "male", "male", "female"))
# whos <- matrix(c(warrior, hair_color), nrow = 6, ncol = 2)
# scores <- matrix(c(w_age, power_score), nrow = 6, ncol = 2)
# named_scores <- scores
# row.names(named_scores) <- warrior
# colnames(named_scores) <- c("age", "power_score")
# all_data <- data.frame(power_score, w_age, sex, hair_color, killed_by_Ken,
#                        row.names = warrior)
# nanto <- list(warrior = warrior,
#               hair_color = hair_color,
#               named_warrior = named_warrior,
#               power_score = power_score,
#               w_age = w_age,
#               named_pwr = named_pwr,
#               killed_by_Ken = killed_by_Ken,
#               sex = sex,
#               whos = whos,
#               scores = scores,
#               named_scores = named_scores,
#               all_data = all_data)



#' Anti-TNFalpha-induced transcription in AR patients
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
#' ISO 25178: Geometrical Product Specifications (GPS) -- Surface texture: areal.
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



