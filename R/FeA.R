# Header Info ------------------------------------------------------------------
# 
# cmatools - A collection of utility functions used in our R projects
#
# by //--FeA.R--//
# ------------------------------------------------------------------------------



#' Show Data as matrix and return its dimensions
#' 
#' @description A custom version of the classical `head()` that prints the upper
#'              leftmost corner of a data set, also showing row names and
#'              controlling for possible out-of-bounds exceptions. Compared to
#'              `head()`, `show_data()` **always displays data by columns**,
#'              even in the case of vectors (i.e., one-dimensional arrays).
#'              Also, `show_data` prints and returns the dimensions of the data
#'              set, together with a custom heading label.
#' 
#' @param dataset Data frame, matrix, or vector to print.
#' @param name Explanatory name to print in the heading (useful when logging).
#' @param rows Maximum number of rows to display.
#' @param cols Maximum number of columns to display.
#' 
#' @returns A vector of the form `c(rows,cols)` containing `dataset` dimensions.
#' @export
#' 
#' @examples
#' \dontrun{
#' x <- data.frame(var1 = c(1, 2, 3, 4, 5), var2 = c(6, 7, 8, 9, 10))
#' show_data(x, "an example dataset", rows = 3)}
#' @author FeA.R
show_data <- function(dataset, name = NULL, rows = 10, cols = 5)
{
  # Whenever possible, 'dataset' is converted to a data frame to be passed to
  # dim() function and displayed column-wise preserving the type specificity of
  # each column. Notably, duplicated names are allowed in both named vectors and
  # matrices, but not in data frames. So, when converting to data frame, names
  # of named vectors are discarded (or better "reassigned to integers) if and
  # only if duplicates are present, while possible duplicated row names from
  # matrices are disambiguated by progressively appending the suffixes '.1',
  # '.2', '.3'... to the original row names!! For all these reasons, matrices
  # and named vectors with duplicated names will be converted to matrices
  # instead of data frames before being printed, in order to always preserve
  # original row names. Note that all these conversions only concern the printed
  # copy of 'dataset', while the type of the original data is unaffected.
  if ((is.vector(dataset) & sum(duplicated(names(dataset))) > 0) |
      (is.matrix(dataset) & sum(duplicated(row.names(dataset))) > 0)) { 
    # If 'dataset' is a named vector with duplicated names or a matrix with
    # duplicated row names, convert it to a matrix...
    dataset <- as.matrix(dataset)
  } else {
    # ...otherwise convert it to a data frame.
    dataset <- as.data.frame(dataset)
  }
  d <- dim(dataset)
  cat("\nDataset", name, "dimensions:", d[1], "x", d[2], "\n\n")
  
  rows <- min(d[1], rows)
  cols <- min(d[2], cols)
  
  # 'print' because automatic printing is turned off in loops (and functions)
  # NOTE: if you only return a data frame subset of one column, R will drop the
  # names by default. To avoid this use the drop=FALSE option.
  print(dataset[1:rows, 1:cols, drop = FALSE])
  
  return(d)
}



#' Title here
#' 
#' @description This function searches the vector passed as input for duplicated
#'              entries and the positions they fill in the vector. Unlike
#'              `base::duplicated()` that only detects duplicated entries
#'              *after* their first occurrence, this function looks at *all*
#'              non-unique values.
#'
#' @param vec A vector to be scanned for duplicates.
#' 
#' @returns A list of the duplicated elements found in `vec`. Each element of
#'          this list is in turn a list of two elements: an integer indicating
#'          the times the element is repeated and a vector of integers
#'          representing its positions within `vec`.
#' @export
#' 
#' @author FeA.R
duplication_report <- function(vec)
{
  if(!is.vector(vec)) {
    stop("The input argument is not a vector")
  }
  vec <- na.omit(vec)
  
  dups_indx <- which(duplicated(vec))
  dups <- unique(vec[dups_indx])
  lapply(dups, function(dups){
    srch <- vec == dups
    list(repeated = sum(srch), is_in = which(srch))
  }) -> report
  names(report) <- dups
  
  return(report)
}



#' Title here
#' 
#' @description A function that tells how many *different non-unique* elements
#'              there are in a given vector. Unlike `base::duplicated()` that
#'              detects duplicated entries *after* their first occurrence, this
#'              function looks at *all* non-unique values.
#'              
#' @param vec A vector to be scanned for duplicates.
#' 
#' @returns A vector of two elements, namely the number of different non-unique
#'          elements, and the global number of non-unique entries in `vec`.
#' @export
#' 
#' @author FeA.R
howMany_dnues <- function(vec)
{
  if(!is.vector(vec)) {
    stop("The input argument is not a vector")
  }
  vec <- na.omit(vec)
  
  report <- duplication_report(vec)
  a <- length(report)
  b <- sum(sapply(report, function(repo){repo[[1]]}))
  
  return(c(a,b))
  # Interpretation:
  # 'length(dup)' different entries are repeated over 'sum(idx)' vector slots,
  # corresponding to 'sum(idx) - length(dup) == sum(duplicated(vec))' duplicated
  # entries.
}



#' Title here
#' 
#' @description An alternative (faster?) implementation of `howMany_dnues()`,
#'              independent of `duplication_report()`.
#'              A function that tells how many *different non-unique* elements
#'              there are in a given vector. Unlike `base::duplicated()` that
#'              detects duplicated entries *after* their first occurrence, this
#'              function looks at *all* non-unique values.
#'              
#' @param vec A vector to be scanned for duplicates.
#' 
#' @returns A vector of two elements, namely the number of different non-unique
#'          elements, and the global number of non-unique entries in `vec`.
#' @export
#' 
#' @author FeA.R
howMany_dnues2 <- function(vec)
{
  if(!is.vector(vec)) {
    stop("The input argument is not a vector")
  }
  vec <- na.omit(vec)
  
  idx <- duplicated(vec) | duplicated(vec, fromLast = TRUE)
  dup <- unique(vec[idx])
  
  return(c(length(dup), sum(idx)))
  # Interpretation:
  # 'length(dup)' different entries are repeated over 'sum(idx)' vector slots,
  # corresponding to 'sum(idx) - length(dup) == sum(duplicated(vec))' duplicated
  # entries.
}



#' Title here
#' 
#' @description R-equivalent to the MATLAB `repmat()` function. Just specify the
#'              matrix X and how many times you want it replicated row- (m) and
#'              column- (n) wise.
#'              
#' @param X A vector, matrix, or data frame with source array data.
#' @param m An integer specifying the number of row-wise repetitions.
#' @param n An integer specifying the number of column-wise repetitions.
#' 
#' @returns A new matrix resulting from the m-by-n juxtaposition of the starting
#'          matrix `X`.
#' @export
#' 
#' @author FeA.R
repmat <- function(X, m, n)
{
  # Always convert to matrix first
  X <- as.matrix(X)
  cName_save <- colnames(X)
  rName_save <- rownames(X)
  
  mx <- dim(X)[1]
  nx <- dim(X)[2]
  
  # Reminder:
  # `matrix(x,y,z)` means: fill by columns (or by row if specified) a y-by-z
  # matrix using numbers from x interpreted as a vector (always the sequence of
  # its columns) and using the recycling rule.
  mat <- matrix(t(matrix(X, mx, nx*n)), mx*m, nx*n, byrow = TRUE)
  colnames(mat) <- rep(cName_save, n)
  rownames(mat) <- rep(rName_save, m)
  
  return(mat)
}



#' Title here
#' 
#' @description A function that extracts the text from the help pages of a R
#'              package or function. Adapted from MrFlick's `help_text()`
#'              function, originally posted on stackoverflow (13 Jul 2018; see
#'              References section).
#'              
#' @param meth_or_pkg The *quoted* name of the method or package of interest.
#' @param pkg Name of the package to look into for documentation.
#' 
#' @returns A character vector containing the rows from the help page. Use
#'          `help_as_text("seq") |> cat(sep = "\n")` to print.
#' @export
#' 
#' @references https://stackoverflow.com/questions/51330090/how-to-get-text-data-from-help-pages-in-r
#' 
#' @author MrFlick, FeA.R
help_as_text <- function(meth_or_pkg, pkg = NULL)
{
  file <- help(meth_or_pkg, package = (pkg))
  
  if (length(file) > 1) {
    warning("\nMore than one ", meth_or_pkg, " method or package exist.\n",
        "The first one of the alphabetically ordered list will be selected.\n",
        "However, you can disambiguate using the `pkg` parameter.")
    file <- file[1]
  }
  
  path <- dirname(file)
  dirpath <- dirname(path)
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  
  print(dirpath)
  rd <- tools:::fetchRdDB(RdDB, basename(file))
  capture.output(tools::Rd2txt(rd, out = "",
                               options = list(underline_titles = FALSE)))
}



#' Title here
#' 
#' @description To align strings in console as if using MS-Word tab stop
#'              feature. It allows to control tab stop positions filling the
#'              given string with white spaces to reach a fixed width.
#' 
#' @param word The string to be included in the tabular space.
#' @param sp Spacing parameter indicating tab stop position (i.e., the total
#'           width of the final string returned by the function).
#'
#' @returns A string consisting of the `word` followed by a variable number of
#'          white spaces.
#' @export
#' 
#' @examples
#' \dontrun{
#' for (animal in c("dog", "sheep", "worm", "lioness", "monkey")) {
#'    cat(" -", tab(animal), ":: looking for a", animal, "...found!\n")}}
#' @author FeA.R
tab <- function(word = "", sp = 7)
{
  if (sp < nchar(word)) {
    warning("The word to include exceeds tab stop... can't align properly!")
    entry <- word
  } else {
    entry <- paste0(word,
                    paste(rep(" ", sp-nchar(word)), collapse = ""))
  }
  return(entry)
}



#' Title here
#' 
#' @description A wrapper for the Hypergeometric test function. Beside
#'              *p*-values, `hgt()` also computes and returns other enrichment
#'              statistics like the expected value and fold enrichment.
#' 
#' @param k Number of hits in the experimental set.  --OR--> Intersection
#' @param n Size of the experimental set.            --OR--> Set A
#' @param K Number of possible hits in the universe. --OR--> Set B
#' @param N Size of the universe.                    --OR--> Background
#'
#' @returns A 1-by-5 data frame containing enrichment statistics. Many
#'          single-row data frames can be easily stacked using `rbind()`.
#' @export
#'          
#' @author FeA.R
hgt <- function(k, n, K, N = 1e4)
{
  pval <- phyper(k-1, K, N-K, n, lower.tail = FALSE) # p-value
  expect <- n*(K/N) # Expected value
  stdev <- sqrt(n*(K/N)*((N-K)/N)*((N-n)/(N-1))) # Standard Deviation
  FE <- k/(n*(K/N)) # Fold Enrichment
  
  stats <- data.frame(Hits = k,
                      Expected_Value = expect,
                      SD = stdev,
                      Fold_Enrichment = FE,
                      p.value = pval)
  return(stats)
}



#' Title here
#' 
#' @description A minimal graphical interface to retrieve the name of a suitable
#'              annotation database (a `.db` R-package or a `GPL` GEO platform
#'              record) starting from the selection of the name of a microarray
#'              chip model. To be used only in interactive mode.
#'
#' @param filt A string used to subset the list of platform annotations among
#'             which to choose (i.e., a `grep` filter working on array names).
#'             It can target array manufacturer ("Affymetrix", "Agilent"),
#'             or the source of annotation ("Biocondutor", "GPL" for GEO).
#'
#' @returns The name of the database corresponding to the platform chosen by the
#'          user (to be used with `create.annot()` function).
#' @export
#' 
#' @examples
#' \dontrun{
#' array_platform_selector() |> array_create_annot(platform) |> head()}
#' @author FeA.R
array_platform_selector <- function(filt = "All")
{
  # Name of the Bioconductor db package or GPL GEO platform record
  db_BCorGPL <- c("hgu133a",
                  "hgu133b",
                  "hgu133plus2",
                  "hugene10stprobeset",
                  "hugene10sttranscriptcluster",
                  "",
                  "hgug4112a",
                  "GPL6480",
                  "HsAgilentDesign026652",
                  "GPL22763",
                  "GPL19072")
  
  # Array full-length name or `title` value from @header slot of a GPL object
  # NOTE_1: GEOquery::getGEO() function needs FTP
  # NOTE_2: Only Agilent 'Probe Name Versions' (NO 'Gene Symbol Versions') GPLs
  #         have been included in this list. 
  long_names <- c("Affymetrix Human Genome U133 A Set - from Bioconductor",
                  "Affymetrix Human Genome U133 B Set - from Bioconductor",
                  "Affymetrix Human Genome U133 Plus 2.0 Array - from Bioconductor",
                  "Affymetrix GeneChip Human Gene 1.0 ST Array - Exon Level - from Bioconductor",
                  "Affymetrix GeneChip Human Gene 1.0 ST Array - Gene Level - from Bioconductor",
                  "---",
                  "Agilent-014850 Whole Human Genome Microarray 4x44K G4112F - from Bioconductor",
                  "Agilent-014850 Whole Human Genome Microarray 4x44K G4112F - from GPL6480",
                  "Agilent-026652 Whole Human Genome Microarray 4x44K v2 G4845A - from Bioconductor",
                  "Agilent-039714 LincRNA SurePrint G3 Human GE 8x60K Microarray PVD 028004 - from GPL22763",
                  "Agilent-052909 CBC_lncRNAmRNA_V3 - from GPL19072")
  
  if (filt != "All") {
    filt_index <- grep(filt, long_names, ignore.case = TRUE)
    long_names_sub <- long_names[filt_index]
    db_BCorGPL_sub <- db_BCorGPL[filt_index]
  } else {
    long_names_sub <- long_names
    db_BCorGPL_sub <- db_BCorGPL
  }
  
  platform_index <- menu(long_names_sub, title = "Choose platform annotation",
                         graphics = TRUE)
  
  if (platform_index == 0) {
    cat("\nNo platform selected!\n\n")
  } else {
    cat(paste("\nSelected platform:\n", long_names_sub[platform_index], "\n\n"))
    
    return(db_BCorGPL_sub[platform_index])
  }
}



#' Title here
#' 
#' @description Create the annotation data frame starting from a database name
#'              as returned by the `array_platform_selector()`. This function
#'              implements a minimal graphical interface allowing the user to
#'              select the number and the type of features to be used as
#'              annotation.
#' 
#' @param platform Affymetrix/Agilent platform annotation database.
#' @param collapsing Boolean flag to choose whether to collapse by unique Probe
#'                   ID in the case of annotation packages from Bioconductor
#'                   (GPL records from GEO are already collapsed).
#' 
#' @returns A data frame containing for each probe of the platform an number of
#'          features selected by the user.
#' @export
#'
#' @examples
#' \dontrun{
#' annot <- array_create_annot("hgu133plus2", collapsing = TRUE)
#' missing_report(annot)
#' d <- show_data(annot)}
#' @author FeA.R
array_create_annot <- function(platform, collapsing = FALSE)
{
  # Download the annotation matrix from GEO and subset by columns of interest  
  if (grepl("GPL", platform)) {
    
    # Download annotation, extract the `dataTable`, print some information
    GEO_GPL <- GEOquery::getGEO(platform)
    annot_full <- GEOquery::Table(GEO_GPL)
    cat("\nLoaded annotation: ", platform,
        " (Last Update: ", GEO_GPL@header$last_update_date, ")", sep = "")
    
    # Retrieve all the columns of the database for subsequent feature selection
    cols <- colnames(annot_full)
    feats <- svDialogs::dlg_list(choice = cols,
                                 preselect = c("ENSEMBL_ID",  # Ensembl
                                               "GENE",        # NCBI Entrez Gene
                                               "GENE_SYMBOL", # HGNC Gene Symbol
                                               "GENE_NAME"),  # Gene Name
                                 multiple = TRUE,
                                 title = "Select multiple features")$res
    
    # Always use Probe IDs as keys (GPL matrices are already collapsed by unique
    # Probe IDs)
    ids <- annot_full[,1] # "Probe ID" feature should always be the first column
    cat("\n", length(ids), " unique Probe_IDs retrieved from ",
        platform," GEO platform record\n\n", sep = "")
    
    # Subset (including Probe IDs) and return
    annot <- annot_full[,c(cols[1],feats)]
    return(annot)
    
  } else {
    
    # Load Annotation Database from Bioconductor and retrieve columns of interest
    annot_db <- paste0(platform, ".db")
    # library() converts its argument into a string unless you specify the
    # option character.only = TRUE
    library(annot_db, character.only = TRUE)
    library(AnnotationDbi)
    # Print some information
    cat("\nLoaded annotation: ", annot_db,
        " (ver.: ", toString(packageVersion(annot_db)), ")",
        " [date: ", toString(packageDate(annot_db)), "]", sep = "")
    
    # The evaluated expression of the (unquoted) annot_db (e.g., hgu133a.db)
    evaluating_db <- eval(parse(text = annot_db))
    
    # Retrieve all the columns of the data base, except "PROBEID" that will be
    # used as key and so it must be unique
    cols <- columns(evaluating_db)
    cols <- cols[! cols %in% "PROBEID"]
  
    # Feature selection
    feats <- svDialogs::dlg_list(choice = cols,
                                 preselect = c("ENSEMBL",
                                               "ENTREZID",
                                               "SYMBOL",
                                               "GENENAME"),
                                 multiple = TRUE,
                                 title = "Select multiple features")$res
    
    # Always use Probe IDs as keys (although they cannot be row names since, in
    # general, they will not be unique after feature retrieval)
    ids <- keys(evaluating_db, keytype = "PROBEID")
    cat("\n", length(ids), " unique Probe_IDs retrieved from ",
        annot_db,"\n", sep = "")
    annot_long <- select(evaluating_db,
                         keys = ids,
                         columns = feats,
                         keytype = "PROBEID")
    cat("1:many mapping resulted in a ",
        dim(annot_long)[1], " x ", dim(annot_long)[2],
        " annotation data frame", sep = "")
    
    if (collapsing) {
      del = " /// " # Affymetrix-style delimiter
      # Always use Probe IDs as keys
      annot_collapse <- data.frame(PROBEID = keys(evaluating_db,
                                                  keytype = "PROBEID"))
      for (feat in feats) {
        annot_collapse <- merge(annot_collapse,
                                aggregate(annot_long[feat],
                                          by = annot_long["PROBEID"],
                                          function(...){paste(unique(...),
                                                              collapse = del)}),
                                by.x = "PROBEID", by.y = "PROBEID", all.x = TRUE)
      }
      cat("\n...now reduced to ", dim(annot_collapse)[1], " x ",
          dim(annot_collapse)[2], " after collapsing on Probe_IDs\n\n", sep = "")
      return(annot_collapse)
      
    } else {
      cat("\n\n")
      return(annot_long)
    }
  }
}



#' Title here
#' 
#' @description This function takes a data frame, searches its columns for many
#'              common missing-value placeholders, and finally prints a report
#'              report of the number and the relative amount of missing values
#'              detected column-wise. In particular, it searches for `NA`,
#'              `"NA"`, and patterns of whitespaces (`\s`), hyphens (`-`),
#'              slashes (`/`), including empty fields and a possible
#'              user-defined sequence.
#' 
#' @param dataFrame Data frame or matrix to be scanned for NAs.
#' @param naSymb A string containing the user-defined sequence for NAs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' array_platform_selector() |> array_create_annot() |> missing_report()}
#' @author FeA.R
missing_report <- function(dataFrame, naSymb = "")
{
  cols <- colnames(dataFrame)
  missing_data <- matrix(0, nrow = 2, ncol = length(cols),
                         dimnames = list(c("Not Mapped","%"), cols))
  
  # Search for NAs, "NA"s and patterns of whitespaces (\s), hyphens (-),
  # slashes (/), including empty fields (*) and a user-defined sequence
  for (feat in cols) {
    missing_data[1,feat] <- sum(
      is.na(dataFrame[,feat]) |
        dataFrame[,feat] == "NA" |
        grepl("^(\\s|-|/)*$", dataFrame[,feat]) |
        dataFrame[,feat] == naSymb)
    missing_data[2,feat] <- round((missing_data[1,feat]/dim(dataFrame)[1])*1e2,
                                  digits = 2)
  }
  cat("\n")
  print(missing_data)
  cat("\n")
}











#'--- old ---
#' @description ppend annotation to genes and sort (do nothing if
#'              do.the.job == FALSE). This is a GATTACA legacy piece of code.
#' 
#' @param gene.stat The table of genes, usually a DEG summary-statistic
#'                  top-table (or an expression matrix).
#' @param ann The matrix containing the annotation data.
#' @param do.the.job FALSE to skip the appending task by global settings,
#'                   without the need for an external IF.
#' @param sort.by The name or index of the column used to sort the final dataset
#'
#' @returns The annotated and sorted data frame passed as input.
#' 
#' @author FeA.R
appendAnnotation = function(gene.stat, ann,
                            do.the.job = getOption("append.annot"),
                            sort.by = 1)
{
  # Check argument values
  if (is.null(do.the.job)) {
    do.the.job = TRUE
    cat("\nWARNING: \'append.annot\' option defaulted to TRUE\n\n")
  }
  
  if (do.the.job) {
    
    # 'merge' function to merge two matrix-like objects horizontally and cast to
    # data frame (right outer join).
    # NOTE: both gene.stat and ann are supposed to have the Probe_IDs as rownames
    
    # To merge two data frames horizontally by one or more common key variables:
    #  - inner join (default): Return only the rows that have matching keys in both
    #     the tables (~ intersection)
    #  - outer join (all = T): Return all rows from both the tables, joining the
    #     records that have matching (~ union)
    #  - left outer (all.x = T): Return all rows from the left table, and any rows
    #     with matching keys from the right table
    #  - right outer (all.y = T): Return all rows from the right table, and any rows
    #     with matching keys from the left table
    #  - cross join (by = NULL): Return the Cartesian product
    
    joined = merge(ann, gene.stat,
                   by.x = "row.names", by.y = "row.names", all.y = TRUE)
    rownames(joined) = joined[,1]
    gene.stat = joined[,-1]
    
    # Re-sort the data frame by the content of 'sort.by' column ('sort.by' can
    # be either a number or a column name).
    gene.stat = gene.stat[order(gene.stat[,sort.by]),]
  }
  return(gene.stat)
}



#'--- old ---
#' @description Return basics descriptive statistics of a single gene, by group
#'              label. This is a GATTACA legacy piece of code.
#' 
#' @param gene Numeric vector or single-row data frame from gene expression
#'             matrix.
#' @param gr Group names.
#' @param des Experimental design (full design mode vector).
#' @param prec Decimal precision.
#'
#' @returns A data frame containing the statistics of interest for each gene of
#'          `gene`.
#'
#' @author FeA.R
descStat1G = function(gene, gr, des, prec = 4)
{
  # Define a new empty data frame
  stat.frame = data.frame(GROUP = character(),
                          n = integer(),
                          MEAN = double(),
                          VAR = double(),
                          SD = double(),
                          SEM = double(),
                          stringsAsFactors = FALSE)
  
  for (i in 1:length(gr)) {
    
    n.gene = as.numeric(gene[des == i]) # Downcast to numeric vector
    
    stat.frame[i,1] = gr[i]
    stat.frame[i,2] = sum(des == i)
    stat.frame[i,3] = round(mean(n.gene), digits = prec)
    stat.frame[i,4] = round(var(n.gene), digits = prec)
    stat.frame[i,5] = round(sd(n.gene), digits = prec)
    stat.frame[i,6] = round(sd(n.gene)/sqrt(sum(des == i)), digits = prec) # SEM
  }
  return(stat.frame)
}



#'--- old ---
#' @description Plot single gene comparison chart. This is a GATTACA legacy
#'              piece of code.
#' 
#' @param exp.mat Expression matrix (as data frame).
#' @param gr Group names.
#' @param des Experimental design (full design mode vector).
#' @param gois Genes of interest by probe (char vector).
#' @param chart.type "BP" (Box Plot), "BC" (Bar Chart), or "MS" (Mean & SEM).
#' @param ann Optional annotation data frame.
#'
#' @author FeA.R
singleGeneView = function(exp.mat, gr, des, gois, chart.type = "BP", ann = NULL)
{
  geo = switch(chart.type,
               "BP" = "point",
               "BC" = "bar",
               "MS" = "crossbar")
  
  for (i in 1:length(gois)) {
    
    var.expr = as.numeric(exp.mat[gois[i],]) # Downcast to vector
    var.groups = gr[des]
    sgex = data.frame(var.expr, var.groups) # Single Gene Expression Data Frame
    sgs = descStat1G(exp.mat[gois[i],], gr, des, 6) # Single Gene Summary Data Frame
    
    if (is.null(ann)) {
      gene.symb = ""
    } else {
      gene.symb = paste(ann[gois[i], grepl("Symbol", colnames(ann))], " - ", sep = "")
    }
    
    if (chart.type == "BP") {
      
      print( # NOTICE: When in a for loop, you have to explicitly print your resulting ggplot object
        ggplot(data = sgex, aes(var.groups, var.expr)) +
          theme_bw(base_size = 15, base_rect_size = 1.5) +
          xlab("Group") + # In the following functions, when data=NULL (default), the data is inherited from ggplot()
          ylab("log2 Expression") +
          ggtitle(label = "Box Plot with Jitter", subtitle = paste(gene.symb, "Probe ID: ", gois[i], sep = "")) +
          geom_boxplot(width = 0.5, size = 0.5, notch = TRUE, outlier.shape = NA) +
          stat_summary(fun = "mean", geom = geo, color = "red3", size = 2) +
          geom_jitter(position = position_jitter(width = 0.1, height = 0, seed = 123), size = 1.5))
      
    } else if (chart.type == "BC" | chart.type == "MS") {
      
      print(
        ggplot(data = sgex, aes(var.groups, var.expr)) +
          theme_bw(base_size = 15, base_rect_size = 1.5) +
          xlab("Group") +
          ylab("log2 Expression") +
          ggtitle(label = "Mean & SEM Plot with Jitter", subtitle = paste(gene.symb, "Probe ID: ", gois[i], sep = "")) +
          stat_summary(fun = "mean", geom = geo, color = "black", size = 0.5, width = 0.2) +
          # Recommended alternative for bar charts in ggplot2:
          #geom_bar(data = sgs, aes(GROUP, MEAN), stat = "identity", color = "black", size = 0.5, width = 0.2) +
          geom_errorbar(data = sgs, aes(GROUP, MEAN, ymin = MEAN - SEM, ymax = MEAN + SEM), size = 1, width = 0.1) + 
          geom_jitter(position = position_jitter(width = 0.1, height = 0, seed = 123), size = 1.5))
      
    } else {
      
      cat("\n")
      stop("Invalid chart.type!\n\n")
    }
    
    printPlots(paste("SingleGene Plot - ", chart.type, " - ", gois[i], sep = ""))
  }
}


