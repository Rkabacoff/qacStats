#' @title Data Audit
#' @description Generates an HTML report describing a dataset
#' @details
#' \code{audit} generates a semi-customizable HTML report describing a dataset
#' with information on types of variables, bivariate analysis, and possible issues.
#' Portions can be included or excluded with the Boolean variables.
#' @param data a data frame
#' @param Basic include Basic analysis?
#' @param Quantitative include Histograms of each Quantitative Variable?
#' @param Categorical include Bar Charts of Categorical Variables?
#' @param Bivariate include Bivariate charts?
#' @param Issues include Potential Issues?
#' @param summaryStats summaryStats passed to skimr function in the form
#' \code{list(numeric=list(mean=mean), character=list(len= length), factor=list(levels=levels))}
#' @param gg use ggplot2 for graphing?
#' @import ggplot2
#' @import skimr
#' @import knitr
#' @import e1071
#' @import dplyr
#' @import rlang
#' @import rmarkdown
#' @export
#' @return path to HTML file
#' @examples
#' # from qacData
#' data(tv17, package="qacData")
#' audit(tv17)
#' audit(tv17, Basic=FALSE, Categorical=FALSE,
#' gg=FALSE, summaryStats=list(numeric=list(mean=mean)))


audit <- function(data, Basic = TRUE, Quantitative = TRUE, Categorical = TRUE,
                  Issues = TRUE, Bivariate = FALSE,
                  summaryStats = NULL, gg = TRUE) {
  if (is.tbl(data)) {
    class(data) <- "data.frame"
  }
  if(!is.data.frame(data)) data <- as.data.frame(data)
  data <- deparse(substitute(data))

  # read template file and modify
  report <- readLines(system.file("DataAuditTemplateR.txt", package = "qacStat"))
  # report <- readLines("DataAuditTemplateR.txt"))
  report <- gsub("xxxdata", data, report, fixed = TRUE)
  report <- gsub("xxxgg", gg, report, fixed = TRUE)
  report <- gsub("BASIC", Basic, report, fixed = TRUE)
  report <- gsub("QUANTITATIVE", Quantitative, report, fixed = TRUE)
  report <- gsub("CATEGORICAL", Categorical, report, fixed = TRUE)
  report <- gsub("ISSUES", Issues, report, fixed = TRUE)
  report <- gsub("BIVARIATE", Bivariate, report, fixed = TRUE)
  if (!is.null(summaryStats)) {
    #report <- gsub("SUMMARYSTATS", summaryStats, report, fixed = TRUE)
  }

  # output template and render
  tf <- tempfile(fileext = ".Rmd")
  to <- tempfile(fileext = ".html")
  writeLines(report, tf) # Writes to the temporary file
  library(rmarkdown)
  render(input = tf, output_format = "html_document", output_file = to)
  viewer <- getOption("viewer")
  viewer(to)
  invisible(to) # returns the path silently
}
