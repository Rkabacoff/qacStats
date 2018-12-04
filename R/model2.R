#' @title Breast Cancer GLM Example
#'
#' @description
#' \code{plotROCS} Another model Example for plotROCS function.
#'
#' @details
#' A model example copied from Professor Robert Kabacoff, QAC385:Advanced R.
#' @import caret
#' @import regclass
#'
#' @export
#' @examples
#' @author Elizaveta Kravchenko <ekravchenko@@wesleyan.edu>

model2 <- function() {
  loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
  ds  <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
  url <- paste(loc, ds, sep="")

  breast <- read.table(url, sep=",", header=FALSE, na.strings="?")
  names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                     "shapeUniformity", "maginalAdhesion",
                     "singleEpithelialCellSize", "bareNuclei",
                     "blandChromatin", "normalNucleoli", "mitosis", "class")

  df <- breast[-1]
  df$class <- factor(df$class, levels=c(2,4),
                     labels=c("benign", "malignant"))

  index <- createDataPartition(breast$class, p=0.7, list=FALSE)
  train1 <- df[index,]
  test <- df[-index,]
  fit <- glm(class ~ ., data=train1, family=binomial)
  return(fit)
}
