#' @title Enhanced Summary for Logistic Model
#'
#' @description
#' \code{summary.glmplus} an enhanced summary for logistic models
#'
#' @details
#' This function provides odds ratio and confidence interval in addition to summary results of a glm object.
#'
#' @param glm_model a logistic model
#' @export
#'
#' @return Enhanced summary for a logistic regression
#' \item{glm_summary}{an enhanced logisitc regression summary}
#'
#' @author Lex Liu <rliu01@@wesleyan.edu>
#'
#' @examples
#' data(mtcars)
#' glm_model <- glmplus(vs ~ cyl + mpg + hp, data=mtcars, family=binomial)
#' summary(glm_model)
#'
summary.glmplus <- function(glm_model) {
    class(glm_model) <- class(glm_model)[class(glm_model) != "glmplus"]
    glm_summary <- summary(glm_model)

    coef <- glm_summary$coefficients
    odd <- as.matrix(exp(coef[,1]))
    colnames(odd) <- c("Odds Ratio")

    cofint <- exp(confint(glm_model))
    colnames(cofint) <- c("[95% Conf.", "Interval]")

    glm_summary$coefficients <- cbind(odd, coef, cofint)

    glm_summary
}
