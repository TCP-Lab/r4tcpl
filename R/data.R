#' The Nanto Warriors Data Set
#' 
#' This data set, inspired by the six Nanto warriors of the epic
#' **Hokuto no Ken** (aka *Fist of the North Star*) saga, is provided in the
#' form of a named list containing some of the most common basic data types you
#' can find in R. It is very useful for educational purposes, interactive
#' trials, and general testing.
#' 
#' @format ## `nanto`
#' A named list containing 12 objects of different types:
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
#' @author FeA.R
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



#' Anti-TNFalpha-induced transcription in AR patients - Expression
#' 
#' Expression values for the 436 Differentially Expressed Genes (DEGs) resulting
#' from my Rheumatoid Arthritis (AR) microarray study (PMID: ..., DOI: ...,
#' GEO accession "GSE224330").
#' Agilent-072363 SurePrint G3 Human GE v3 8x60K chips Chip were hybridized with
#' RNA of monocytes taken from bDMARD-treated AR-patients and compared with the
#' transcriptional profile of healthy and Methotrexate (MTX)-treated samples.
#' Raw florescence intensities were background-subtracted and interarray-
#' normalized to obtain the expression data matrix. The expression data set
#' stored in this R variable is a subset of the original one, in that it only
#' contains the expression values of the statistically significant genes
#' resulting from the sole comparison of Etanercept (anti-TNFalpha) with MTX-
#' treated patients (RankProduct-based differential expression analysis; see
#' `cmatools::DEGs_stat` data set). Accordingly, only samples taking part in
#' such a contrast (n = 5 for Etanercept and n = 6 for MTX) have been included.
#' Additional columns from 1 to 3 contain some gene annotation, namely:
#' GeneSymbol, GeneName, and Category membership. Row names are Agilent
#' probe_IDs.
#' 
#' @format ## `DEGs_expr`
#' A data frame with 436 rows and 14 columns:
#' \describe{
#'   \item{GeneSymbol}{HGNC Official Gene Symbol}
#'   \item{GeneName}{Official Gene Name}
#'   \item{Category}{Manual curated membership to one of the following
#'    categories: NA, "Inflammation", "Immune Response", "Metallopeptidase"}
#'   \item{Anti-TNFa_1 : Anti-TNFa_5}{anti-TNFalpha-treated patients}
#'   \item{MTX_1 : MTX_6}{Methotrexate-treated patients}
#' }
#' @author FeA.R
#' @source PMID: ..., DOI: ..., GEO accession "GSE224330"
"DEGs_expr"



#' Anti-TNFalpha-induced transcription in AR patients - Statistics
#' 
#' Descriptive (logFC) and inferential (*p*-value) statistics for the same 436
#' significant genes already used to build the `DEGs_expr` data set and
#' resulting from the Differential Expression Analysis (DEA) of the
#' transcriptomics data from my Rheumatoid Arthritis (AR) microarray study
#' (PMID: ..., DOI: ..., GEO accession "GSE224330").
#' Agilent-072363 SurePrint G3 Human GE v3 8x60K chips Chip were hybridized with
#' RNA of monocytes taken from bDMARD-treated AR-patients and compared with the
#' transcriptional profile of healthy and Methotrexate (MTX)-treated samples.
#' Raw florescence intensities were background-subtracted and interarray-
#' normalized to obtain the expression data matrix. DEA among the different
#' experimental groups were conducted using a RankProduct-based statistical
#' analysis. All genes with an adjusted *p*-value < 0.05 and with a |FC| > 1.5
#' were deemed as statistically significant. The data set stored in this R
#' variable is a subset of the original one, in that it only contains the
#' statistics of the Differentially Expressed Genes (DEGs) resulting from the
#' sole comparison of Etanercept (anti-TNFalpha) with MTX-treated patients
#' (n = 5 and n = 6, respectively). Additionally, first two columns contain some
#' gene annotation, namely GeneSymbol and GeneName. Row names are Agilent
#' probe_IDs.
#' 
#' @format ## `DEGs_stat`
#' A data frame with 436 rows and 5 columns:
#' \describe{
#'   \item{GENE_SYMBOL}{HGNC Official Gene Symbol}
#'   \item{GENE_NAME}{Official Gene Name}
#'   \item{logFC}{log_2 Fold Changes of gene expression in anti-TNFalpha
#'                compared to MTX-treated patients}
#'   \item{p_value}{RankProduct-based (unadjusted) *p*-values}
#'   \item{adj.p_value}{Benjamini-Hochberg adjusted *p*-values}
#' }
#' @author FeA.R
#' @source PMID: ..., DOI: ..., GEO accession "GSE224330"
"DEGs_stat"



#' Titanium surface descriptors (ISO 25178)
#' 
#' This data set contains the experimentally measured values of eleven ISO 25178
#' (Geometrical Product Specifications, GPS, Surface texture: areal) descriptors
#' evaluated for eight different types of titanium disks. These data, together
#' with the related profiles of cell adhesion and protein adsorption, have been
#' presented, analyzed, and discussed in the following paper: PMID: ...,
#' DOI: ...,
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
#' @author FeA.R
#' @source PMID: ..., DOI: ..., <https://en.wikipedia.org/wiki/ISO_25178>
"ISO"



#' The Transportome Gene Set - TGS
#' 
#' This is a comprehensive list of the gene symbols representing the entire
#' human transportome, defined as the set of all the Ion Channels and
#' Transporters (ICTs) that can be found across the human cell membrane. Gene
#' Symbols are organized in 5 character vectors as described below.
#' 
#' @format ## `TGS`
#' A named list containing 5 character vectors:
#' \describe{
#'   \item{pseudo}{All human ICTs, including pseudogenes}
#'   \item{whole}{All human ICTs, without pseudogenes (a subset of `pseudo`)}
#'   \item{ICs}{All human Ion Channels (a subset of `whole`)}
#'   \item{Ca_ICs}{All human Calcium-Permeable Ion Channels (a subset of `ICs`)}
#'   \item{trans}{All human transporters, including solute carriers (SLCs), ABC
#'                transporters, and ATPase pumps (a subset of `whole` with null
#'                overlap with `ICs`).}
#' }
#' @author FeA.R
#' @source HGNC DB at <https://www.genenames.org/> and
#'         IUPHAR/BPS DB at <https://www.guidetopharmacology.org/>
"TGS"


