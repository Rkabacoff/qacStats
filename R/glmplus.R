#' @title Enhanced Logistic Regression
#'
#' @description
#' \code{glmplus} generates a glmplus model
#'
#' @details
#' This function outputs a glm model with an additional class glmplus
#'
#' @param ... glm object for evaluation
#' @export
#'
#' @return a list with 2 elements:
#' \item{glm_model}{a glmplus model}
#'
#' @author Lex Liu <rliu01@@wesleyan.edu>
#'
#' @examples
#' data(mtcars)
#' glm_model <- glmplus(vs ~ cyl + mpg + hp, data=mtcars, family=binomial)
#' summary(glm_model)
#' plot(glm_model, "hp", "cyl")
#' plot(glm_model, "hp")
#'
glmplus <- function(formula, family="binomial", data) {
    glm_model <- glm(formula=formula, family=family, data=data)
    class(glm_model) <- c("glmplus", class(glm_model))
    glm_model
}
