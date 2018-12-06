#' @title Multi Reg
#' @description Improved summary.lm() function for multiple regression.
#' @details
#' \code{multi_reg} depicts the results from a multiple regression model 
#' through a stargazer table and outputs a correlation matrix describing
#' the correlation of the model's predictors. This function enhances the 
#' print.summary.lm() output by including a 95% confidence interval for 
#' each coefficient as well as stars to depict the significance level of 
#' each predictor. If the user specifies, the function will also provide
#' an excel file depicting the multiple regression model results.
#' @param formula formula for a linear model
#' @param data data frame
#' @param results if \code{TRUE}, outputs stargazer table with model results
#' @param type type of output prouduced by the results parameter
#' @param out filename to save stargazer table
#' @param export_table if \code{TRUE}, saves the multiple regression results to an excel file
#' @param file_name name of excel table file containing regression results
#' @param cor_matrix if \code{TRUE}, outputs a correlation matrix with all of the predictors of the multiple regression model
#' @param hist_of_resid if \code{TRUE}, outputs a histogram of the model's residuals
#' @import ggplot2
#' @import stargazer
#' @import corrplot
#' @import dplyr
#' @import xlsx
#' @import knitr
#' @export
#' @return a list with 11 elements in summary.lm function
#' @author Shane Ross <saross@@wesleyan.edu>
#' @examples
#' data(mtcars)
#' regression <- lm(mpg ~ wt + cyl + hp, data = mtcars)
#' multi_reg(regression, mtcars, results = TRUE,
#'           type = "text", out = "my_table", export_table = TRUE,
#'           file_name = "my_table.xlsx", cor_matrix = TRUE,
#'           hist_of_resid = TRUE)

multi_reg <- function(formula, data, results = TRUE,
                      type= "text", out = NULL,
                      export_table = FALSE,
                      file_name = NULL,
                      cor_matrix = FALSE,
                      hist_of_resid = FALSE) {
  
  regression <- lm(formula = formula, data = data)
  
  if (results == TRUE) {
    stargazer(regression, type = "text",
              title = "Regression Results",
              single.row = TRUE,
              ci = TRUE, ci.level = .95,
              out = out)
  }
  
  summary.lm <- summary(regression)
  
  F <- summary.lm$fstatistic[[1]]
  dfn <- summary.lm$fstatistic[[2]]
  dfd <- summary.lm$fstatistic[[3]]
  p <- pf(F, dfn, dfd,lower.tail=FALSE)
  p.formated <-  format.pval(p)
  
  if (p < .001) {
    p_reported = "p < .001"
  } else if (p < .01) {
    p_reported = "p < .01"
  } else if (p < .05) {
    p_reported = "p < .05"
  } else {
    p_reported = "p > .05"
  }
  
  mcp <- summary.lm$coefficients
  mcp_table <- as.data.frame(mcp)
  mcp_table <- round(mcp_table, digits = 3)
  names(mcp_table)[4] <- "p"
  mcp_table$sig <- ""
  
  
  add_stars <- function(x){
    if (x <= .01) {
      sig = "***"
    } else if (x <= .05) {
      sig = "**"
    } else if (x <= .1) {
      sig = "*"
    } else {
      sig =""
    }
    return(sig)
  }
  
  add_stars <- Vectorize(add_stars)
  
  b <- as.data.frame(confint(regression, level = .95))
  b <- round(b, 3)
  b[3] <- paste("(", b[[1]], ", ", b[[2]], ")", sep = "")
  b <- b[3]
  colnames(b) <- "95% confidence interval"
  
  mcp_table$sig <- add_stars(mcp_table$p)
  
  mcp_table <-cbind(mcp_table, b)
  col_order <- c("Estimate", "95% confidence interval", "Std. Error",
                 "t value", "p", "sig")
  mcp_table <- mcp_table[, col_order]
  
  if (export_table == TRUE) {
    write.xlsx(mcp_table, file = file_name)
  }
  
  if (cor_matrix == TRUE) {
  
  summary.lm[["coefficients"]] <- mcp_table
  
  a <- rownames(mcp_table)[-1]
  
  df <- data[, a]
  
  m <- cor(df)
  
  correlation_test <- function(d_mat, ...) {
    d_mat <- as.matrix(d_mat)
    n <- ncol(d_mat)
    p_mat <- matrix(NA, n, n)
    diag(p_mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        temp <- cor.test(d_mat[, i], d_mat[, j], ...)
        p_mat[i, j] <- p_mat[j, i] <- temp$p.value
      }
    }
    colnames(p_mat) <- rownames(p_mat) <- colnames(d_mat)
    p_mat
  }
  
  p_mat <- correlation_test(df)
  
  colors <- colorRampPalette(c("#E495A5", "#ABB065", "#39BEB1", "#ACA4E2"))
  corr <- corrplot(m, method="color", col=colors(100),  
                   type="upper", order="hclust", 
                   addCoef.col = "black",
                   tl.col="black", tl.srt=60,
                   p.mat = p_mat, sig.level = 0.05, insig = "blank", 
                   diag=FALSE 
  )
  }
  
  gg <-  ggplot(data=regression, aes(.resid)) + 
    geom_histogram(aes(y = ..density..), 
                   breaks = seq(-3, 5, by = 1),
                   col="red", 
                   fill="green", 
                   alpha = .2) + 
    geom_density(col = "red") +
    labs(title="Histogram for Residuals") +
    labs(x="Residuals", y="Count") +
    theme_classic()
  
  if (hist_of_resid == TRUE) {
    print(gg)
}
  return(invisible(summary.lm))
}
