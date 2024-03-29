% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{DEGs_expr}
\alias{DEGs_expr}
\title{Anti-TNFalpha-induced transcription in AR patients - Expression}
\format{
\subsection{\code{DEGs_expr}}{

A data frame with 436 rows and 14 columns:
\describe{
\item{\code{GeneSymbol}}{HGNC Official Gene Symbol}
\item{\code{GeneName}}{Official Gene Name}
\item{\code{Category}}{Manual curated membership to one of the following
categories: NA, "Inflammation", "Immune Response", and
(Matrix)"Metallopeptidase"}
\item{\code{Anti-TNFa_1 : Anti-TNFa_5}}{anti-TNFalpha-treated patients}
\item{\code{MTX_1 : MTX_6}}{Methotrexate-treated patients}
}
}
}
\source{
PMID: 36877690, DOI: 10.1371/journal.pone.0282564,
GEO accession "GSE224330".
}
\usage{
DEGs_expr
}
\description{
Expression values for the 436 Differentially Expressed Genes (DEGs) resulting
from our 2017-2023 Rheumatoid Arthritis (AR) microarray study (PMID: 36877690,
DOI: 10.1371/journal.pone.0282564, GEO accession "GSE224330").
Agilent-072363 SurePrint G3 Human GE v3 8x60K chips Chip were hybridized with
RNA of monocytes taken from bDMARD-treated AR-patients and compared with the
transcriptional profile of healthy and Methotrexate (MTX)-treated samples.
Raw florescence intensities were background-subtracted and interarray-
normalized to obtain the expression data matrix. The expression data set
stored in this R variable is a subset of the original one, in that it only
contains the expression values of the statistically significant genes
resulting from the sole comparison of Etanercept (anti-TNFalpha) with MTX-
treated patients (RankProduct-based differential expression analysis; see
\code{r4tcpl::DEGs_stat} data set). Accordingly, only samples taking part in such
a contrast (n = 5 for Etanercept and n = 6 for MTX) have been included.
Additional columns from 1 to 3 contain some gene annotation, namely:
GeneSymbol, GeneName, and Category membership. Row names are Agilent
probe_IDs.
}
\author{
FeA.R
}
\keyword{datasets}
