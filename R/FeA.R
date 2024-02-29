# Header Info ------------------------------------------------------------------
# 
# r4tcpl - A collection of utility functions used in our R projects
#
# by //--FeA.R--//
#



#' Let Me See (Data as Matrix)
#' @export
#' 
#' @description A custom version of the classical `head()` that prints the upper
#'              leftmost corner of a data set, also showing row names and
#'              controlling for possible out-of-bounds exceptions. Compared to
#'              `head()`, `lms()` **displays 1D vectors by columns**, unquotes
#'              strings in character vectors and matrices, and prints the
#'              dimensions and the type (`class()`) of the data set, along with
#'              a custom heading label. In addition, `lms()` features a better
#'              list management.
#' 
#' @param data2see Data frame, matrix, factor, or vector to print.
#' @param rows Maximum number of rows to display.
#' @param cols Maximum number of columns to display.
#' @param name Explanatory name to print in the heading (useful when logging).
#' 
#' @examples
#' # Let me see The Nanto Warriors Data Set
#' lms(nanto)
#' 
#' # Compare `head()` behavior when applied to a list of objects
#' head(nanto)
#' 
#' # Compare `lms()` and `head()` behavior when applied to different data types
#' for (i in 1:length(nanto)) {
#'   cat("\nhead()\n")
#'   print(head(nanto[[i]]))
#'   cat("\nlms()")
#'   lms(nanto[[i]])
#'   cat("\n------------------------------------------------------\n")
#' }
#' @author FeA.R
lms <- function(data2see, rows = 10, cols = 5, name = NULL)
{
  stored_class <- class(data2see)
  
  # NOTE 1: is.list() would have been TRUE also for data.frames!
  # NOTE 2: deparse(substitute()) is used to get the variable name as a string
  if (stored_class[1] == "list") {
    cat(paste0("\n", deparse(substitute(data2see)),
               " is a list of ", length(data2see), " objects:\n"))
    for (i in 1:length(data2see)) {
      # Recursive call
      lms(data2see[[i]], rows = rows, cols = cols, name = names(data2see)[i])
    }
  } else {
    if (is.vector(data2see)) {
      suffix_class <- "vector"
      stored_names <- names(data2see)
      data2see <- matrix(data2see, ncol = 1)
      row.names(data2see) <- stored_names
    } else if (is.factor(data2see)) {
      suffix_class <- c("[", levels(data2see), "]")
      data2see <- matrix(data2see, ncol = 1)
    } else { # it is a matrix or a data frame
      suffix_class <- NULL
    }
    
    d <- dim(data2see)
    if (is.null(name)) {
      cat("\nObject dimensions:", d[1], "x", d[2])
    } else {
      cat(paste0("\n\'", name, "\' dimensions: ", d[1], " x ", d[2]))
    }
    cat("\nObject class:", stored_class, suffix_class, "\n\n")
    
    rows <- min(d[1], rows)
    cols <- min(d[2], cols)
    
    # 'print' because automatic printing is turned off in loops (and functions)
    # NOTE: if you only return a data frame subset of one column, R will drop
    #       names by default. To avoid this behavior use the option drop=FALSE.
    print(data2see[1:rows, 1:cols, drop = FALSE], quote = FALSE)
  }
}



#' Get filename without extension
#' @export
#' @import tools
#' 
#' @description Just like `basename()`, this wrapper of
#'              `tools::file_path_sans_ext()` removes the path leading to the
#'              file. However, unlike the native `basename()`, any file
#'              extension is also removed.
#'              
#' @param file_name A string containing the file name to be *basenamed*.
#' 
#' @returns A string containing the pure file name, without trailing path nor
#'          extension.
#'
#' @examples
#' # Comparison with the native 'basename'
#' basename("/path/to/the/file.ext")
#' basename2("/path/to/the/file.ext")
#' @author FeA.R
basename2 <- function(file_name){file_path_sans_ext(basename(file_name))}



#' Remove substring
#' @export
#' 
#' @description A simple wrapper for `gsub()` to remove **all** the substrings
#'              that match a given regex pattern. Notably, `rmv()` is compliant
#'              with the native pipe operator.
#'              
#' @param x The input string.
#' @param rgx The regex pattern to be found and removed.
#' 
#' @examples
#' # Make them singular
#' "children, cars, and watches" |> rmv("e?s|ren")
#' @author FeA.R
rmv <- function(x, rgx){gsub(rgx, "", x)}



#' Duplication Report
#' @export
#' @import stats
#' 
#' @description This function searches the vector passed as input for duplicated
#'              entries and the positions they fill in the vector. Unlike
#'              `base::duplicated()` that only detects duplicated entries
#'              *after* their first occurrence, this function looks at *all*
#'              non-unique values. `NA`s are ignored by default.
#'
#' @param vec A vector to be scanned for duplicates.
#' 
#' @returns A list of the duplicated elements found in `vec`. Each element of
#'          this list is in turn a list of two elements: an integer indicating
#'          the times the element is repeated and a vector of integers
#'          representing its positions within `vec`.
#' 
#' @examples
#' # Locate duplicated entries within the `Category` column of `DEGs_expr` dataset
#' dup_report(DEGs_expr$Category)
#' @author FeA.R
dup_report <- function(vec)
{
  if(!is.vector(vec)) {
    stop("The input argument is not a vector")
  }
  
  dups_indx <- which(duplicated(vec))
  dups <- unique(vec[dups_indx])
  dups <- as.vector(na.omit(dups))
  lapply(dups, function(dups){
    srch <- vec == dups
    list(repetitions = sum(na.omit(srch)), found_in = which(srch))
  }) -> report
  names(report) <- dups
  
  return(report)
}



#' Different Non-Unique Elements
#' @export
#' @import stats
#' 
#' @description A function that tells how many *different non-unique* elements
#'              there are in a given vector. Unlike `base::duplicated()` that
#'              detects duplicated entries *after* their first occurrence, this
#'              function looks at *all* non-unique values. `NA`s are ignored by
#'              default.
#'              
#' @param vec A vector to be scanned for duplicates.
#' 
#' @returns A numeric vector of two elements, namely the number of different
#'          non-unique elements, and the global number of non-unique entries in
#'          `vec`.
#' 
#' @examples
#' # How many different non-unique Gene Symbols are there in DEGs_expr data set? 
#' dnues(DEGs_expr$GeneSymbol)
#' @author FeA.R
dnues <- function(vec)
{
  if(!is.vector(vec)) {
    stop("The input argument is not a vector")
  }
  vec <- as.vector(na.omit(vec))
  
  report <- dup_report(vec)
  a <- length(report)
  if (a > 0) {
    b <- sum(sapply(report, function(repo){repo[[1]]}))
  } else {
    b <- 0
  }
  
  return(c(a,b))
  # Interpretation:
  # `a` different entries are found to be repeated over `b` slots of the input
  # vector, corresponding to `b - a == sum(duplicated(vec))` duplicated entries
  # (in the absence of NAs).
}



#' Different Non-Unique Elements (Alternative)
#' @export
#' @import stats
#' 
#' @description An alternative and faster implementation of `dnues()`,
#'              independent of `dup_report()` function. A function that tells
#'              how many *different non-unique* elements there are in a given
#'              vector. Unlike `base::duplicated()` that detects duplicated
#'              entries *after* their first occurrence, this function looks at
#'              *all* non-unique values. `NA`s are ignored by default.
#'              
#' @param vec A vector to be scanned for duplicates.
#' 
#' @returns A numeric vector of two elements, namely the number of different
#'          non-unique elements, and the global number of non-unique entries in
#'          `vec`.
#' 
#' @examples
#' # How many different non-unique Gene Symbols are there in DEGs_expr data set? 
#' dnues2(DEGs_expr$GeneSymbol)
#' @author FeA.R
dnues2 <- function(vec)
{
  if(!is.vector(vec)) {
    stop("The input argument is not a vector")
  }
  vec <- na.omit(vec)
  
  idx <- duplicated(vec) | duplicated(vec, fromLast = TRUE)
  dup <- unique(vec[idx])
  
  return(c(length(dup), sum(idx)))
  # Interpretation:
  # 'length(dup)' different entries are found to be repeated over 'sum(idx)'
  # slots of the input vector, corresponding to 'sum(idx) - length(dup) ==
  # sum(duplicated(vec))' duplicated entries (in the absence of NAs).
}



#' Replicate Matrix-like Objects
#' @export
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
#'
#' @examples
#' # Replicate some scores of the Nanto warriors
#' repmat(nanto$named_scores,2,3)
#' @references \url{https://stackoverflow.com/questions/19590541/r-duplicate-a-matrix-several-times-and-then-bind-by-rows-together}
#' @author Lucas Fortini, FeA.R
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



#' Rotate a matrix or data frame
#' @export
#' 
#' @description This function takes in a matrix or a data frame and rotates it
#'              a user-defined number of times.
#' 
#' @param mat A matrix, or data frame. Row- and column-names are handled.
#' @param turns Number of 90° clockwise rotations. Use negative integers for
#'              counterclockwise rotations.
#' 
#' @details R modulus definition is consistent with a "floored division"
#'          approach, so that `((a %/% b)*b + a %% b) == a`. For this reason,
#'          taking the modulus 4 directly implements a counterclockwise rotation
#'          when the operand is negative!
#'          
#' @returns A rotated matrix. If `mat` is a data frame, it will be converted to
#'          matrix during transposition.
#' 
#' @examples
#' # Clockwise rotation
#' rotate(nanto$scores)
#' # Counterclockwise rotation
#' rotate(nanto$scores, -3)
#' @author FeA.R
rotate <- function(mat, turns = 1)
{
  turns <- turns %% 4 # Take modulus 4
  if (turns) {
    for (i in 1:turns) {
      mat <- t(apply(mat, 2, rev))
    }
  }
  return(mat)
}



#' Help Pages as Text
#' @export
#' @import utils
#' 
#' @description A function that extracts the text from the help pages of a R
#'              package or function. Adapted from MrFlick's `help_text()`
#'              function, originally posted on stackoverflow (13 Jul 2018; see
#'              References section).
#'              
#' @param meth_or_pkg The *quoted* name of the method or package of interest.
#' @param pkg Name of the package to look into for documentation.
#' 
#' @returns A character vector containing the rows from the help page.
#' 
#' @examples
#' # Print `seq()` help to console
#' help_as_text("seq") |> cat(sep = "\n")
#' 
#' # Get author's name:
#' hh <- help_as_text("lms")
#' gsub("\\s{2,}", "", hh[grep("^Author", hh) + 2])
#' @references \url{https://stackoverflow.com/questions/51330090/how-to-get-text-data-from-help-pages-in-r}
#' @author MrFlick, FeA.R
help_as_text <- function(meth_or_pkg, pkg = NULL)
{
  # The following function is the mere copy-and-paste of `tools:::fetchRdDB`
  # with the addition of the only line
  #  `%notin%` <- Negate(`%in%`)
  # to define the %notin% operator used therein.
  # This function is used to replace the `fetchRdDB()` unexported function from
  # `tools` package and avoid annoying warnings when running `devtools::check()`.
  tools_fetchRdDB <- function (filebase, key = NULL) 
  {
    fun <- function(db) {
      
      `%notin%` <- Negate(`%in%`)
      
      vals <- db$vals
      vars <- db$vars
      datafile <- db$datafile
      compressed <- db$compressed
      envhook <- db$envhook
      fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]], 
                                             datafile, compressed, envhook)
      if (length(key)) {
        if (key %notin% vars) 
          stop(gettextf("No help on %s found in RdDB %s", 
                        sQuote(key), sQuote(filebase)), domain = NA)
        fetch(key)
      }
      else {
        res <- lapply(vars, fetch)
        names(res) <- vars
        res
      }
    }
    res <- lazyLoadDBexec(filebase, fun)
    if (length(key)) 
      res
    else invisible(res)
  }
  
  # Here it starts the actual `help_as_text()` function...
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
  
  #rd <- tools:::fetchRdDB(RdDB, basename(file)) # Replaced by tools_fetchRdDB()
  rd <- tools_fetchRdDB(RdDB, basename(file))
  capture.output(tools::Rd2txt(rd, out = "",
                               options = list(underline_titles = FALSE)))
}



#' Tab Stop
#' @export
#' 
#' @description To align strings in console as if using MS-Word tab stops
#'              It allows to control tab stop positions padding the given
#'              string with white spaces to reach a fixed width.
#' 
#' @param word The string to be included in the tabular space.
#' @param sp Spacing parameter indicating tab stop position (i.e., the total
#'           width of the final string returned by the function).
#'
#' @returns A string consisting of the `word` followed by a variable number of
#'          white spaces.
#' 
#' @examples
#' # Print Nanto warriors' hair color
#' for (warrior in nanto$warrior) {
#'   cat(" -", tab(warrior), ":: has",
#'       tab(nanto$all_data[warrior, "hair_color"], 8), "hair\n")
#' }
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



#' Hypergeometric Test
#' @export
#' @import stats
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
#' @details The default value for the size of the universe (or background) set
#'          is `N=2e4`, very close to the current estimate of the number of
#'          human protein-coding genes, as annotated in `org.Hs.eg.db` (see
#'          example below).
#'
#' @returns A 1-by-5 data frame containing enrichment statistics. Many
#'          single-row data frames can be easily stacked using `rbind()`.
#'          
#' @examples
#' # Annotation packages
#' library(AnnotationDbi)
#' library(org.Hs.eg.db) # Human
#' 
#' # Make a genome-wide data frame that associates the gene-type to each Entrez
#' # ID, then count only the "protein-coding" entries
#' x <- select(org.Hs.eg.db,
#'             keys = keys(org.Hs.eg.db),
#'             columns = c("ENTREZID", "GENETYPE"),
#'             keytype = "ENTREZID")
#' N <- sum(x$GENETYPE == "protein-coding") # 20,598 - EGSOURCEDATE: 2022-Sep12
#' 
#' # Matrix MetalloPeptidases (MMPs) in a DEG list
#' k <- sum(na.omit(DEGs_expr$Category) == "Metallopeptidase")
#' 
#' # Number of DEGs
#' n <- dim(DEGs_expr)[1]
#' 
#' # Total number of MMPs in Humans
#' K <- 24 # source https://en.wikipedia.org/wiki/Matrix_metalloproteinase
#' 
#' # Hypergeometric Test with the `N=2e4` conventional background
#' hgt(k, n, K)
#' 
#' # Hypergeometric Test with the actual background for human
#' hgt(k, n, K, N)
#' @author FeA.R
hgt <- function(k, n, K, N = 2e4)
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



#' Overlap Analysis
#' @export
#' @import grid
#' 
#' @description Given **two** vectors of symbols, this function finds the
#'              elements that exist in both sets and computes the statistical
#'              significance of the overlap between them. Optionally, it plots
#'              a Venn diagram representation.
#' 
#' @param set_A A character or numeric (1D) vector.
#' @param set_B Another character or numeric (1D) vector.
#' @param N Size of the universe (or background set). see `hgt()` function.
#' @param venn Boolean. Set it to `FALSE` to suppress Venn plotting.
#' @param lab Labels for the Venn. A character vector of two elements, that
#'            defaults to variable names.
#' @param titles custom titles and subtitles for the Venn diagram. A character
#'               vector of two elements. 
#'
#' @returns A list made up of a data frame named `ORA` (containing the results
#'          of the OverRepresentation Analysis), and three vectors named
#'          `intersection`, `diff_AB`, and `diff_BA`, featuring the common
#'          elements, the elements that exist only in set A, and those that
#'          exist only in set B, respectively.
#'
#' @examples
#' # Find how many (and which) Ion Channels there are within a given DEG list
#' # and if that gene set is enriched (over-represented) or not:
#' x <- venny(TGS$ICs, DEGs_stat$GENE_SYMBOL,
#'            lab = c("Ion Channels", "DEGs"),
#'            titles = c("Transportome Analysis", "Ion Channels"))
#' 
#' # Have an overview of the results
#' lms(x)
#' 
#' # ICs within DEG list
#' x$intersection
#' 
#' # Is the gene set of ion channels enriched?
#' x$ORA$p.value # Nope
#' 
#' # Show all DEGs except ion channels
#' x$diff_BA
#' @author FeA.R
venny <- function(set_A, set_B, N = 2e4,
                  venn = TRUE,
                  lab = c(deparse(substitute(set_A)),
                          deparse(substitute(set_B))),
                  titles = c("Venn diagram", "by r4tcpl"))
{
  # Check arguments
  if (!is.vector(set_A) | !is.vector(set_B)) {
    stop("At least one input set is not a vector")
  }
  
  # Set operations
  intersection <- intersect(set_A, set_B)
  diff_AB <- setdiff(set_A, set_B)
  diff_BA <- setdiff(set_B, set_A)
  
  # Compute p-values through hypergeometric distribution
  ORA <- hgt(length(intersection),
             length(set_A), length(set_B), N)
  
  # Output list
  set_stat <- list(ORA = ORA,
                   intersection = intersection,
                   diff_AB = diff_AB,
                   diff_BA = diff_BA)
  
  if (venn) {
    # To suppress 'venn.diagram()' log messages with priority lower than "ERROR"
    futile.logger::flog.threshold(futile.logger::ERROR,
                                  name = "VennDiagramLogger")
    # Create the Venn diagram
    venn.plot <- VennDiagram::venn.diagram(
      x = list(set_A, set_B),
      force.unique = TRUE,   # Remove duplicates
      na = "remove",         # Remove NAs
      
      # Output features
      filename = NULL,        # Print plot just on screen
      disable.logging = TRUE, # Disable log file output and print to console
      
      # Title and Subtitle
      main = titles[1], 
      main.cex = 2,
      main.fontface = "bold",
      main.fontfamily = "sans",
      sub = titles[2],
      sub.fontfamily = "sans",
      
      # Circles
      lwd = 2,
      lty = 1, # Set lty="blank" to remove borders
      fill = c("mediumpurple3", "#0073C2FF"),
      alpha = 0.5,
      #col = c("#401050ff", "#353560ff"),
      rotation.degree = 30,
      scaled = FALSE,
      
      # Numbers
      cex = 2,
      fontface = "bold",
      fontfamily = "sans",
      
      # Set names (labels)
      category.names = lab,
      cat.cex = 2,
      cat.fontface = "bold",
      cat.default.pos = "outer",
      cat.pos = c(-20, -20),
      cat.dist = c(0.05, 0.05),
      cat.fontfamily = "sans")
    
    # Create a new canvas and draw the Venn
    grid::grid.newpage()
    grid::grid.draw(venn.plot)
  }

  return(set_stat)
}



#' Basic Descriptive Statistics
#' @export
#' @import stats
#' 
#' @description Use this function to get basic descriptive statistics of many
#'              experimental groups from a single one-dimensional numeric vector
#'              according to a user-defined experimental design. This function
#'              is a generalization of the legacy `descStat1G()` function
#'              implemented in GATTACA for single-gene inspection.
#' 
#' @param vals One-dimensional numeric vector or data frame.
#' @param design Experimental design: a numeric or character vector that
#'               associates each element of `vals` to a symbol, based on the
#'               experimental group the element belongs to.
#' @param prec Output decimal precision.
#'
#' @returns A data frame containing the statistics of interest (i.e., sample
#'          size, arithmetic mean, median, IQR, variance, standard deviation,
#'          and SEM) for each group defined in `design`.
#' 
#' @examples
#' # Get descriptive statistics of gene expression
#' gene <- DEGs_expr["A_33_P3307955", -c(1:3)]
#' dsgn <- c(rep("Anti-TNFa",5), rep("MTX",6))
#' descriptives(gene, dsgn, 4)
#' @author FeA.R
descriptives <- function(vals, design = rep(1,length(vals)), prec = 3)
{
  # Check design length
  if (length(design) != length(vals)) {
    stop("Bad design size!")
  }
  # Values-group association
  if (is.numeric(design)) {
    design <- paste0("Group_", design)
  }
  
  # Experimental groups
  grps <- unique(design)
  m <- length(grps)
  
  # Prepare a new empty data frame
  stat_frame <- data.frame(Group = grps,
                           n = integer(m),
                           Mean = double(m),
                           Median = double(m),
                           IqR = double(m),
                           Var = double(m),
                           SD = double(m),
                           SEM = double(m),
                           stringsAsFactors = FALSE)
  row.names(stat_frame) <- grps
  
  # Fill the data frame with the stats of interest
  # 
  # You can alternatively use by() for a non-loopy implementation:
  # dim(vals) <- c(length(vals),1) # Force `vals` to column shape
  # vals_grps <- data.frame(vals, design)
  # stat_frame[,1]<-round(as.matrix(by(vals_grps$vals, vals_grps$design, mean)),
  #                         digits = prec)
  # stat_frame[,2]<-round(as.matrix(by(vals_grps$vals, vals_grps$design, median)),
  #                         digits = prec)
  #                 [...]
  for (i in 1:m) {
    sub_vals <- as.numeric(vals[which(design == grps[i])]) # Downcast to vector
    stat_frame[i,2] <- length(sub_vals) # Sample size
    stat_frame[i,3] <- round(mean(sub_vals), digits = prec)
    stat_frame[i,4] <- round(median(sub_vals), digits = prec)
    stat_frame[i,5] <- round(IQR(sub_vals), digits = prec)
    stat_frame[i,6] <- round(var(sub_vals), digits = prec)
    stat_frame[i,7] <- round(sd(sub_vals), digits = prec)
    stat_frame[i,8] <- round(sd(sub_vals)/sqrt(stat_frame$n[i]),
                             digits = prec) # SEM
  }
  return(stat_frame)
}



#' Standard Plots for Categorical Data
#' @export
#' 
#' @description Use this function to plot a continuous response variable as a
#'              function of a categorical explanatory one. Data need to be
#'              passed as a single one-dimensional numeric vector together with
#'              a user-defined experimental design. This function is a
#'              generalization of the legacy `singleGeneView()` function
#'              implemented in GATTACA for single-gene inspection.
#' 
#' @param vals One-dimensional numeric vector or data frame.
#' @param design Experimental design: a numeric or character vector that
#'               associates each element of `vals` to a symbol, based on the
#'               experimental group the element belongs to.
#' @param chart_type A string among the following: "BP" (Box Plot), "VP"
#'                   (Violin Plot), "BC" (Bar Chart), or "MS" (Mean & SEM).
#' 
#' @examples
#' # Get a graphical representation of gene differential expression
#' gene <- DEGs_expr["A_33_P3307955", -c(1:3)]
#' dsgn <- c(rep("Anti-TNFa",5), rep("MTX",6))
#' for (type in c("BP", "VP", "BC", "MS")) {
#'   quick_chart(gene, dsgn, type)
#' }
#' @author FeA.R
quick_chart <- function(vals, design, chart_type = "BP")
{
  # Check design length
  if (length(design) != length(vals)) {
    stop("Bad design size!")
  }
  
  # Colors
  line_color <- "gray17"
  point_color <- "steelblue4"
  fill_col <- "slategray4"
  err_color <- "gray17"
  
  # Values-group association
  if (is.numeric(design)) {
    design <- paste0("Group_", design)
  }
  
  # Descriptive statistics
  desc <- descriptives(vals = vals, design = design)
  
  # Common ggplot terms
  gg_base <- ggplot2::ggplot(data = data.frame(vals = as.numeric(vals), design),
                             mapping = ggplot2::aes(x = design, y = vals)) +
    ggplot2::theme_bw(base_size = 15, base_rect_size = 1.5) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 14)) +
    ggplot2::xlab("Groups") + ggplot2::ylab("Response variable")
  # geom_jitter() is a convenient shortcut for geom_point(position = "jitter")
  gg_jitter <- ggplot2::geom_jitter(size = 2, color = point_color,
                                    position = ggplot2::position_jitter(
                                      width = 0.1, height = 0, seed = 123))
  gg_errors <- ggplot2::geom_errorbar(data = desc,
                                      mapping = ggplot2::aes(
                                        desc$Group,
                                        desc$Mean,
                                        ymin = desc$Mean - desc$SEM,
                                        ymax = desc$Mean + desc$SEM),
                                      linewidth = 1.1, width = 0.2,
                                      color = err_color)
  # Now plot!
  # NOTE: ggplot objects Within a for loop need to be printed explicitly 
  if (chart_type == "BP") {
    print(
      gg_base +
        ggplot2::geom_boxplot(color = line_color, fill = fill_col, alpha = 0.6,
                              linewidth = 1, width = 0.5,
                              notch = TRUE, outlier.shape = NA) +
        ggplot2::stat_summary(fun = "mean", geom = "point",
                              color = line_color, shape = "cross",
                              size = 3, stroke = 2) +
        gg_jitter +
        ggplot2::ggtitle(label = "Box Plot with Notch and Jitter")
    )
  } else if (chart_type == "BC") {
    print(
      gg_base +
        ggplot2::geom_bar(data = desc,
                          mapping = ggplot2::aes(desc$Group, desc$Mean),
                          stat = "identity",
                          color = line_color, fill = fill_col, alpha = 0.6,
                          linewidth = 1, width = 0.5) +
        gg_errors +
        gg_jitter +
        ggplot2::ggtitle(label = "Bar Chart with Jitter")
    )
  } else if (chart_type == "MS") {
    print(
      gg_base +
        ggplot2::stat_summary(fun = "mean", geom = "crossbar",
                              color = line_color,
                              linewidth = 0.5, width = 0.5) +
        gg_errors +
        gg_jitter +
        ggplot2::ggtitle(label = "Mean & SEM Plot with Jitter")
    )
  } else if (chart_type == "VP") {
    print(
      gg_base +
        ggplot2::geom_violin(color = line_color, fill = fill_col, alpha = 0.6,
                             linewidth = 1, width = 0.5) +
        gg_jitter +
        ggplot2::ggtitle(label = "Violin Plot with Jitter")
    )
  } else {
    cat("\n")
    stop("Invalid chart_type!\n\n")
  }
}



#' Save plot to a file in multiple formats 
#' @export
#' @import grDevices utils
#' 
#' @description This function saves a graphical output to `figure_Folder`
#'              sub-directory in both raster (PNG) and vectorial (PDF) formats.
#'              Automatically makes the output folder if not already there.
#'
#' @param plotfun A callback function that prints a plot to an open device.
#' @param ratio The plot aspect ratio, as a single numeric input.
#' @param figure_Name Name of the output file (without extension).
#' @param figure_Folder Name of the saving (sub)folder.
#' @param png_out Boolean. Set it to `FALSE` to suppress PNG graphics device.
#' @param pdf_out Boolean. Set it to `FALSE` to suppress PDF graphics device.
#' 
#' @details This implementation of `savePlots()` takes a _function_ as its first
#'          argument, and not a plot object (i.e., a device number) as some
#'          plotting facilities (notably the default one, `savePlot()`) that
#'          cannot print plot objects conveniently.
#'          
#' @examples
#' \dontrun{
#' # Get a numeric-only expression matrix (as a data frame)
#' numeric_df <- DEGs_expr[,4:14]
#'
#' # Save box plots of per-sample distributions of expression levels
#' savePlots(
#'   \(){boxplot(numeric_df)},
#'   figure_Name = "Boxplot",
#'   figure_Folder = ".")
#' 
#' # Calculate PCA on samples
#' metadata <- data.frame(row.names = colnames(numeric_df))
#' metadata$Group <- c(rep("AntiTNF",5), rep("MTX",6))
#' pcaOut <- PCAtools::pca(numeric_df, metadata = metadata)
#' 
#' # Print the biplot
#' savePlots(
#'   \(){print(PCAtools::biplot(pcaOut, colby = "Group"))},
#'   figure_Name = "BiPlot",
#'   figure_Folder = ".")
#' }
#' @author FeA.R, Hedmad
savePlots <- function(plotfun, ratio = 16/9, figure_Name, figure_Folder,
                      png_out = TRUE, pdf_out = TRUE)
{
  if (png_out || pdf_out) {
    fullName <- file.path(figure_Folder, figure_Name)
    if (!file.exists(figure_Folder)) {
      dir.create(figure_Folder, recursive = TRUE)
    }
  } else {
    stop("No graphics device selected!")
  }
  
  if (png_out) {
    # Width and height are in pixels.
    w_px <- 1024
    png(filename = paste0(fullName, ".png"), width = w_px, height = w_px/ratio)
    plotfun()
    invisible(capture.output(dev.off())) # Suppress automatic output to console.
  }
  if (pdf_out) {
    # Width and height are in inches.
    w_in <- 14
    pdf(file = paste0(fullName, ".pdf"), width = w_in, height = w_in/ratio)
    plotfun()
    invisible(capture.output(dev.off()))
  }
}



#' Microarray Platform Selector
#' @export
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
#'          user (to be used with `array_create_annot()` function).
#' 
#' @examples
#' \dontrun{
#' array_platform_selector() |> array_create_annot() |> lms(10,10)
#' }
#' @author FeA.R
array_platform_selector <- function(filt = ".*")
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
                  "GPL13497",
                  "GPL22763",
                  "GPL19072",
                  "GPL21185",
                  "GPL10787")
  
  # Array full-length name or `title` value from @header slot of a GPL object
  # NOTE_1: GEOquery::getGEO() function needs a working FTP connection.
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
                  "Agilent-026652 Whole Human Genome Microarray 4x44K v2 G4845A - from GPL13497",
                  "Agilent-039714 LincRNA SurePrint G3 Human GE 8x60K Microarray PVD 028004 - from GPL22763",
                  "Agilent-052909 CBC_lncRNAmRNA_V3 - from GPL19072",
                  "Agilent-072363 SurePrint G3 Human GE v3 8x60K Microarray 039494 - from GPL21185",
                  "Agilent-028005 SurePrint G3 Mouse GE 8x60K Microarray G4852A - from GPL10787")
  
  # Filter according to `filt` argument (regex ".*" stand for "take all")
  filt_index <- grep(filt, long_names, ignore.case = TRUE)
  long_names_sub <- long_names[filt_index]
  db_BCorGPL_sub <- db_BCorGPL[filt_index]
  
  platform_index <- utils::menu(long_names_sub,
                                title = "Choose platform annotation",
                                graphics = TRUE)
  
  if (platform_index == 0) {
    selected_db <- ""
  } else {
    selected_long <- long_names_sub[platform_index]
    selected_db <- db_BCorGPL_sub[platform_index]
  }
  
  if (selected_db == "") {
    cat("\nNo platform selected!\n\n")
  } else {
    cat(paste("\nSelected platform:\n", selected_long, "\n\n"))
    
    return(selected_db)
  }
}



#' Microarray Annotation Retriever
#' @export
#' @import utils
#' 
#' @description This function retrieves gene annotation for a given microarray
#'              platform and returns them as a data frame. As input, it requires
#'              a database name as returned by `array_platform_selector()`.
#'              This function uses \pkg{svDialogs} package to implement a
#'              minimal graphical interface allowing the user to select the
#'              number and the type of features to be used as annotation.
#'              GPL-based annotation are retrieved from GEO using
#'              `GEOquery::getGEO()` function that needs a working FTP
#'              connection.
#' 
#' @param platform Affymetrix/Agilent platform annotation database as returned
#'                 by the `array_platform_selector()` function.
#' @param collapsing Boolean flag to choose whether to collapse by unique Probe
#'                   ID in the case of annotation packages from Bioconductor
#'                   (GPL records from GEO are already collapsed).
#' 
#' @returns A data frame containing for each probe of the platform a number of
#'          features as selected by the user.
#'
#' @examples
#' \dontrun{
#' annot <- array_create_annot("hgu133plus2", collapsing = TRUE)
#' missing_report(annot)
#' lms(annot)
#' }
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
    
    # Load Annotation db from Bioconductor and retrieve the columns of interest
    annot_db <- paste0(platform, ".db")
    if(!requireNamespace(annot_db, quietly = TRUE)) {
      # Don't want to make all annotation packages mandatory dependencies for
      # r4tcpl package!
      stop(paste0(annot_db," package is not installed.",
                  "\nRun `BiocManager::install(\"", annot_db,
                  "\")` to proceed."))
    }
    
    # Print some information
    cat("\nLoaded annotation: ", annot_db,
        " (ver.: ", toString(packageVersion(annot_db)), ")",
        " [date: ", toString(packageDate(annot_db)), "]", sep = "")
    
    # The evaluated expression of the (unquoted) annot_db
    # e.g., hgu133a.db::hgu133a.db
    evaluating_db <- eval(parse(text = paste0(annot_db, "::", annot_db)))
    
    # Retrieve all the columns of the data base, except "PROBEID" that will be
    # included by default as key
    cols <- AnnotationDbi::columns(evaluating_db)
    cols <- cols[! cols %in% "PROBEID"]
    
    # Feature selection
    feats <- svDialogs::dlg_list(choice = cols,
                                 preselect = c("ENSEMBL",
                                               "ENTREZID",
                                               "SYMBOL",
                                               "GENENAME"),
                                 multiple = TRUE,
                                 title = "Select multiple features")$res
    
    # Always use Probe IDs as keys (although they cannot be used as row names
    # since, in general, they will not be unique after feature retrieval)
    ids <- AnnotationDbi::keys(evaluating_db, keytype = "PROBEID")
    cat("\n", length(ids), " unique Probe_IDs retrieved from ",
        annot_db,"\n", sep = "")
    annot_long <- AnnotationDbi::select(evaluating_db,
                                        keys = ids,
                                        columns = feats,
                                        keytype = "PROBEID")
    cat("1:many mapping resulted in a ",
        dim(annot_long)[1], " x ", dim(annot_long)[2],
        " annotation data frame", sep = "")
    
    if (collapsing) {
      del = " /// " # Affymetrix-style delimiter
      # Always use Probe IDs as keys
      annot_collapse <- data.frame(PROBEID = ids)
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
# A quick reminder about `merge()` usage:
#
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



#' Count Missing Values in a Matrix-like Object
#' @export
#' 
#' @description This function takes a data frame, searches its columns for many
#'              common missing-value placeholders, and finally prints a report
#'              of the absolute number and the relative amount of missing values
#'              detected column-wise. In particular, it searches for `NA`,
#'              `"NA"`, and patterns of whitespaces (`\s`), hyphens (`-`),
#'              slashes (`/`), including empty fields and a possible
#'              user-defined sequence.
#' 
#' @param dataFrame Data frame or matrix to be scanned for NAs.
#' @param naSymb A string containing the user-defined sequence for NAs.
#'
#' @examples
#' \dontrun{
#' array_platform_selector() |> array_create_annot() |> missing_report()
#' }
#' @author FeA.R
missing_report <- function(dataFrame, naSymb = "")
{
  cols <- colnames(dataFrame)
  missing_data <- matrix(0, nrow = 2, ncol = length(cols),
                         dimnames = list(c("Not Mapped", "%"), cols))
  
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



#' GMM-based subpopulation separation
#' @export
#' @import stats
#' 
#' @description This is a wrapper for the `Mclust` function (from the
#'              \pkg{mclust} package) which uses Gaussian Mixture Models (GMMs)
#'              to fit data distribution. GMMs represent a general approach to
#'              data clustering and subpopulations detection. If applied to gene
#'              expression data (from either microarray or RNA-Seq experiments),
#'              GMMs can be used to separate expressed from unexpressed genes.
#'              To this aim, the `GMM_divide` function returns the values of the
#'              individual probability-weighted Gaussian components of the
#'              mixture and the boundary points suitable for subpopulation
#'              separation. Expected input data are **unbinned** expression
#'              values (not their probability density function), usually
#'              log2-transformed. **NOTE:** when analyzing RNA-Seq ***counts***,
#'              it is always better to remove 0s first (e.g.,
#'              `GMM_divide(counts[counts != 0], G = 3)`).
#' 
#' @param vec One-dimensional numeric vector or data frame.
#' @param G Integer number of Gaussian components to be used in the mixture.
#'
#' @details The decision boundaries are computed from the intersection of two
#'          GMM components at a time, meaning that two probability-weighted
#'          Gaussian functions with unequal variances need to be equated for
#'          each possible combination \eqn{\left(i,j\right)} of two components
#'          of the mixture, namely
#'          \deqn{
#'            \begin{gathered}
#'              \frac{P_i}{\sqrt{2\pi}\sigma_i}
#'                e^{-\left(\mu_i-x\right)^2/2\sigma_{i}^{2}}=
#'                \frac{P_j}{\sqrt{2\pi}\sigma_j}
#'                e^{-\left(\mu_j-x\right)^2/2\sigma_{j}^{2}}\\[20pt]
#'              e^{\left(\mu_j-x\right)^2/2\sigma_{j}^{2}-
#'                \left(\mu_i-x\right)^2/2\sigma_{i}^{2}}=
#'                \frac{P_j\sigma_i}{P_i\sigma_j}\\[20pt]
#'              \frac{\sigma_{i}^{2}\left(\mu_j-x\right)^2-
#'                \sigma_{j}^{2}\left(\mu_i-x\right)^2}
#'                {\sigma_{j}^{2}\sigma_{i}^{2}}=
#'                2\ln{\frac{P_j\sigma_i}{P_i\sigma_j}}\\[20pt]
#'              x^2\left(\sigma_{i}^{2}-\sigma_{j}^{2}\right)-
#'                2x\left(\sigma_{i}^{2}\mu_j-\sigma_{j}^{2}\mu_i\right)+
#'                \sigma_{i}^{2}\mu_{j}^{2}-
#'                \sigma_{j}^{2}\mu_{i}^{2}=
#'                2\sigma_{i}^{2}\sigma_{j}^{2}
#'                \ln{\frac{P_j\sigma_i}{P_i\sigma_j}}\\[20pt]
#'              x_{1,2}=\frac{-B/2\ \pm\ \sqrt{\left(B/2\right)^2-AC}}
#'                {A},\ \ \ \text{with}\\[20pt]
#'              \left\{
#'                \begin{aligned}
#'                  A    & = \sigma_{i}^{2}-\sigma_{j}^{2}\\
#'                  -B/2 & = \sigma_{i}^{2}\mu_j-\sigma_{j}^{2}\mu_i\\
#'                  C    & = \sigma_{i}^{2}\mu_{j}^{2}-
#'                           \sigma_{j}^{2}\mu_{i}^{2}-
#'                           2\sigma_{i}^{2}\sigma_{j}^{2}
#'                           \ln{\frac{P_j\sigma_i}{P_i\sigma_j}}
#'                \end{aligned}
#'              \right.
#'            \end{gathered}
#'          }
#'          The last equations show that it is possible to have one single root
#'          (i.e., just one intersection point) only when
#'          \eqn{\sigma_{i}=\sigma_{j}} (but \eqn{\mu_i\neq\mu_j}), in which
#'          case the boundary is found at
#'          \deqn{
#'            \begin{gathered}
#'              x=\frac{\mu_i+\mu_j}{2}+
#'                \frac{\sigma^2}{\mu_i-\mu_j}\ln\frac{P_j}{P_i}
#'            \end{gathered}
#'          }
#'          On the contrary, if \eqn{\sigma_{i}\neq\sigma_{j}} solutions are
#'          always two (although, in principle, they can be real-valued, complex
#'          conjugates, or a double root).
#'          Provided that the solutions are real-valued, `GMM_divide` function
#'          returns *the highest intersection point* as single decision
#'          boundary, namely
#'          \deqn{
#'            \begin{aligned}
#'              \argmax_{x\in\left\{x_{1},x_{2}\right\}}
#'                \frac{P_k}{\sqrt{2\pi}\sigma_k}
#'                e^{-\left(\mu_k-x\right)^2/2\sigma_{k}^{2}}\ \ \ \ \ 
#'                \text{with}\ \ k=i\ \vee\ k=j
#'            \end{aligned}
#'          }
#'          In general, the number of *combinations* of two
#'          elements from a given set of `G` elements (i.e., the length of
#'          `boundary` output vector) is
#'          \deqn{
#'            \begin{aligned}
#'              \binom{G}{2}=\frac{G!}{2!(G-2)!}=\frac{G\left(G-1\right)}{2}
#'                =\sum_{k=1}^{G-1}k=T_{G-1}
#'            \end{aligned}
#'          }
#'          where \eqn{T_{n}} denotes the n-th
#'          \href{https://en.wikipedia.org/wiki/Triangular_number}{triangular
#'          number}.
#'          
#' @returns A named list with the following elements:
#' \describe{
#'   \item{`fit`}{An object of class 'Mclust' providing the GMM estimation.}
#'   \item{`x`}{A 1000-point numeric vector providing the x-values used
#'              to evaluate the GMM components. Useful for plotting.}
#'   \item{`components`}{A 1000-by-`G` data frame featuring the single
#'                       probability-weighted Gaussian components evaluated in
#'                       `x`. Useful for plotting.}
#'   \item{`boundary`}{A named numeric vector containing the \eqn{T_{G-1}}
#'                     decision boundaries computed as intersection between the
#'                     elements of all the possible *2*-combinations of Gaussian
#'                     components.}
#' }
#' 
#' @examples
#' # Single sample and group analysis
#' 
#' log_expression <- as.data.frame(DEGs_expr$`Anti-TNFa_4`)
#' log_expression[,2] <- rowMeans(DEGs_expr[,grep("MTX", colnames(DEGs_expr))])
#' colnames(log_expression) <- c("Single_Sample", "Whole_Group")
#' 
#' for (condition in colnames(log_expression)) {
#'   plot(density(log_expression[,condition]),
#'        main = paste0(condition, " - Kernel Density Plot"))
#'   gmm <- GMM_divide(log_expression[,condition], G = 3)
#'   for (i in 1:gmm$fit$G) {
#'     lines(gmm$x, gmm$components[,i], col = "blue")
#'   }
#'   lines(gmm$x, rowSums(gmm$components), col = "red", lty = 2)
#'   for (i in 1:gmm$fit$G) {
#'     lines(c(gmm$boundary[i], gmm$boundary[i]), c(0, 1), lty = 2)
#'   }
#' }
#' @references \url{https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html}
#' @author FeA.R
GMM_divide <- function(vec, G = 2)
{
  # To fix mclust package issue 'could not find function "mclustBIC"'
  mclustBIC <- mclust::mclustBIC
  
  # Mclust V (univariate, unequal variance) model with G components
  fit <- mclust::Mclust(vec, G = G, modelNames = "V")
  print(summary(fit))
  
  # Prepare x-domain
  x_high <- ceiling(max(vec))
  x_low <- floor(min(vec)) - 1
  x <- seq(x_low, x_high, length.out = 1e3)
  
  # GMM components
  components <- data.frame(x) # Just to set the right row dimension
  p <- fit$parameters$pro
  mu <- fit$parameters$mean
  s2 <- fit$parameters$variance$sigmasq
  for (i in 1:G) {
    components[,i] <- p[i] * dnorm(x, mu[i], sqrt(s2[i]))
    colnames(components)[i] <- paste0("comp_", i)
  }
  
  # Subset parameters for boundary computation (do all the combinations)
  boundary <- vector(mode = "numeric")
  for (i in 1:(G-1)) {
    for (j in (i+1):G) {
      A <- s2[i] - s2[j]
      B <- 2*(s2[j]*mu[i] - s2[i]*mu[j])
      C <- s2[i]*mu[j]^2 - s2[j]*mu[i]^2 -
        2*s2[i]*s2[j]*log((p[j]*sqrt(s2[i]))/(p[i]*sqrt(s2[j])))
      
      delta <- sqrt((B/2)^2 - A*C) # actually the square root of a quarter delta
      roots <- c((-B/2 - delta)/A, (-B/2 + delta)/A)
      index <- which.max(dnorm(roots, mu[i], sqrt(s2[i])))
      boundary[length(boundary)+1] <- roots[index]
      names(boundary)[length(boundary)] <- paste(i, j, sep = "_")
    }
  }
  
  return(list(fit = fit, x = x, components = components, boundary = boundary))
}



#' FPKM to TPM
#' @export
#' 
#' @description A function to convert normalized RNA-Seq counts from FPKM (or
#'              RPKM) to TPM scale. **NOTE:** to be properly converted, the
#'              input FPKM counts need to be linearly scaled
#'              (**NOT log-transformed!**)
#' 
#' @param fpkm A data frame containing unlogged normalized counts in FPKM units.
#'             Only numeric columns are allowed.
#'
#' @returns A data frame containing unlogged normalized counts in TPM units.
#'
#' @author FeA.R
fpkm2tpm <- function(fpkm) {
  tpm <- apply(fpkm, 2, \(x){x/sum(as.numeric(x))*(10^6)}) |> as.data.frame()
  return(tpm)
}



#' Kernel Density Plot for RNA-Seq Counts
#' @export
#' @import stats graphics
#' 
#' @description Make a Kernel Density Plot from RNA-Seq data counts.
#'              `count_density()` iterates `stats::density()` function over all
#'              the columns of the `count_table` input data and produce a plot
#'              with many density curves superimposed. In addition, it
#'              implements a switch to remove zero counts (that usually prevent
#'              a clear inspection of the data) and the possibility to set the 
#'              lower and upper bounds for both x- and y-axes (which is useful
#'              for zooming in the region of interest).
#' 
#' @param count_table A data frame containing gene expression counts. Only
#'                    numeric columns are allowed.
#' @param remove_zeros A Boolean flag. If set to `TRUE` (the default) zeros
#'                     are removed from count table beforehand.
#' @param xlim A two-element numeric vector specifying the visualization limits
#'             for the x-axis. If set to `NULL` (the default), `plot()` default
#'             values are used.
#' @param ylim A two-element numeric vector specifying the visualization limits
#'             for the y-axis. If set to `NULL` (the default), `plot()` default
#'             values are used.
#' @param titles Custom titles and subtitles for the Density Plot. A character
#'               vector of two elements.
#' @param col Custom color for density curves.
#' 
#' @author FeA.R
count_density <- function(count_table, remove_zeros = TRUE,
                          xlim = NULL, ylim = NULL,
                          titles = c("Kernel Density Plot", ""),
                          col = "black") {
  if (remove_zeros) {
    cutoff <- 0
  } else {
    cutoff <- -1
  }
  
  count_table <- as.data.frame(count_table)
  d <- density(count_table[count_table[,1] > cutoff, 1])
  plot(d, xlim = xlim, ylim = ylim,
       main = titles[1], sub = titles[2], col = col)
  
  m <- dim(count_table)[2]
  if (m > 1) {
    for (i in 2:m) {
      d <- density(count_table[count_table[, i] > cutoff, i])
      lines(d$x, d$y, col = col)
    }
  }
}


