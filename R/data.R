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
#'   \item{Anti-TNFa_1, _2, _3, _4, _5}{anti-TNFalpha-treated patients}
#'   \item{MTX_1, _2, _3, _4, _5, _6}{Methotrexate-treated patients}
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



