#' @title PIMA GLM Example
#'
#' @description
#' \code{plotROCS} Model Example for plotROCS function.
#'
#' @details
#' A model example copied from Professor Robert Kabacoff, QAC385:Advanced R.
#'
#' @import caret
#' @import regclass
#'
#' @export
#' @examples
#' @author Elizaveta Kravchenko <ekravchenko@@wesleyan.edu>

model1 <- function() {
  library(regclass)
  data("PIMA")
  index <- createDataPartition(PIMA$Diabetes, p=0.7, list=FALSE)
  train <- PIMA[index,]
  test <- PIMA[-index,]
  fit.glm <- glm(Diabetes ~ ., data=train, family = binomial)
  return(fit.glm)
}
