
printPlots = function(figureName, folderPrefix = getOption("scriptName"),
                      PNG = getOption("save.PNG.plot"),
                      PDF = getOption("save.PDF.plot"))
{
  flag = FALSE # A dummy flag to insert a couple of 'new lines' in case of WARNINGs

  # Check argument values
  # NOTE: considering that getOption("...") returns NULL for undefined arguments,
  #       IFs are evaluated only when:
  #       the corresponding global option is not defined
  #         AND
  #       no argument is passed runtime
  if (is.null(PNG)) {
    PNG = TRUE
    flag = TRUE
    cat("\nWARNING: \'save.PNG.plot\' option defaulted to TRUE")
  }
  if (is.null(PDF)) {
    PDF = TRUE
    flag = TRUE
    cat("\nWARNING: \'save.PDF.plot\' option defaulted to TRUE")
  }
  if (is.null(folderPrefix)) {
    figSubFolder = "Figures"
  } else {
    figSubFolder = paste(folderPrefix, "Figures", sep = " ")
  }

  fullName = file.path(figSubFolder, figureName, fsep = .Platform$file.sep)

  if (!file.exists(figSubFolder) && (PNG || PDF)) {
    dir.create(figSubFolder)
    flag = TRUE
    cat("\nNew folder '", figSubFolder, "' has been created in the current WD",
        sep = "")
  }
  if (PNG) { # invisible(capture.output()) to suppress automatic output to console
    invisible(capture.output(
      dev.print(device = png, filename = paste0(fullName, ".png"),
                width = 820, height = 600)))
  }
  if (PDF) {
    invisible(capture.output(
      dev.print(device = pdf, paste0(fullName, ".pdf"))))
  }
  if (flag) {
    cat("\n\n")
  }
}
