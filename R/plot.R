#' @title Visualization for Logistic Regression
#'
#' @description
#' \code{helper} ogistic regression visualization
#'
#' @details
#' Visualization for logistic regression
#' @import ggplot2
#' @import visreg
#' @export
#'
#' @param glm_model a logistic model
#' @param variable whose relationship with the response variable is investigated
#' @param by a third variable
#'
#'
#' @return NULL
#'
#' @author Lex Liu <rliu01@@wesleyan.edu>
#'
#' @examples
#' data(mtcars)
#' glm_model <- glmplus(vs ~ cyl + mpg + hp, data=mtcars, family=binomial)
#' plot(glm_model, "hp", "cyl")
#' plot(glm_model, "hp")

plot.glmplus <- function (glm_model, variable, by) {
    library(ggplot2)
    library(visreg)
    class(glm_model) <- c("glm", "lm")
    y <- as.character(glm_model$formula[[2]])
    vars <- strsplit(as.character(glm_model$formula)[3], " ")[[1]]
    varlist <- vars[vars != "+"]
    print("The return value is a ggplot object, so you can make modifcation to it.")
    if (missing(by)) {
        return(visreg(glm_model, variable,
                      gg = TRUE,
                      scale="response") +
            labs(y = paste0("Prob(", y, ")"),
                 x = variable,
                 title = paste("Relationship of", y, "and", variable),
                 subtitle = paste("Controlling for", paste(varlist[varlist!=variable], collapse=", "))))
    } else {
        return(visreg(glm_model, variable,
                      gg = TRUE,
                      by = by,
                      scale="response") +
                   labs(y = paste0("Prob(", y, ")"),
                        x = variable,
                        title = paste("Relationship of", y, "and", variable),
                        subtitle = paste("Controlling for", paste(varlist[varlist!=variable & varlist!=by], collapse=", "))))
    }
}
