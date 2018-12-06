#' @title Multi Plot
#' @description Improved plot.lm() function for multiple regression
#' @details
#' \code{multi_plot} outputs five diagnostic plots assessing the validity
#' of a mulitple regression. These diagnostic plots include: a Residuals Versus Fitted plot,
#' a Normal Quantile-Quantile plot, a Scale-Location versus Residuals plot,
#' a Residuals versus Leverage plot, and a Residuals versus Order plot.
#' @param lm linear model
#' @param data data frame
#' @param time_var variable depicting order observations were collected
#' @param rvf_plot if \code{TRUE}, outputs Residuals versus Fitted plot
#' @param qq_plot if \code{TRUE}, outputs Normal Quantile-Quantile plot
#' @param scl_loc if \code{TRUE}, outputs Scale-Location plot
#' @param resvlev_plot If \code{TRUE}, outputs Residuals versus Leverage plot
#' @param rvo_plot If \code{TRUE}, outputs Residuals versus Order plot
#' @import ggplot2
#' @import broom
#' @import knitr
#' @export
#' @return NULL
#' @author Shane Ross <saross@@wesleyan.edu>
#' @examples
#' data(mtcars)
#' regression <- lm(mpg ~ wt + cyl + hp, data = mtcars)
#' multi_plot(regression, mtcars, qsec, rvf_plot = TRUE,
#'           qq_plot = TRUE, scloc_plot = TRUE, resvlev_plot = TRUE)

multi_plot <- function(lm, data, time_var = NULL,
                       rvf_plot = TRUE, qq_plot = TRUE, scloc_plot = TRUE,
                       resvlev_plot = TRUE, rvo_plot = TRUE) {
  
  time_var <- deparse(substitute(time_var))
  
  if (rvf_plot == TRUE) {
    rvf_plot <- function(lm) {
      
      if (class(lm) != "lm") {
        stop("lm must be a linear model")
      }
      
      a <- order(abs(lm[["residuals"]]), decreasing = TRUE)[1:5]
      highest <- ifelse(lm[["residuals"]] %in% lm[["residuals"]][a], labels(lm[["residuals"]]), "")
      
      
      p1<-ggplot(lm, aes(.fitted, .resid))+geom_jitter() +
        stat_smooth(se = TRUE, method="loess", alpha = .2) +
        geom_hline(yintercept=0, col="red", linetype="dashed") +
        geom_text(aes(label = highest), check_overlap = FALSE, nudge_y = .2) +
        xlab("Fitted values")+ylab("Residuals")+
        ggtitle("Residuals vs Fitted")+theme_classic()
      print(p1)
    }
  }
  if (qq_plot == TRUE) {
    
    if (class(lm) != "lm") {
      stop("lm must be a linear model")
    }
    
    x <- rstudent(lm)
    x <- na.omit(x)
    ord <- order(x)
    n <- length(x)
    P <- ppoints(length(x))
    df <- data.frame(ord.x = x[ord], z = qnorm(P))
    
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- qnorm(c(0.25, 0.75))
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
    
    
    zz <- qnorm(1 - (1 - .95)/2)
    SE <- (coef[2]/dnorm(df$z)) * sqrt(P * (1 - P)/n)
    fit.value <- coef[1] + coef[2] * df$z
    df$upper <- fit.value + zz * SE
    df$lower <- fit.value - zz * SE
    
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, names(x)[ord],"")
    
    
    p <- ggplot(df, aes(x=z, y=ord.x)) +
      geom_point() + 
      geom_abline(intercept = coef[1], slope = coef[2]) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
      labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
      theme_classic()
    
    p <- p + geom_text(aes(label = label))
    print(p)
  }
  if (scloc_plot == TRUE) {
    
    if (class(lm) != "lm") {
      stop("lm must be a linear model")
    }
    
    sqrt_standard_resid <- sqrt(abs(rstandard(lm)))
    lm[["fitted.values"]]
    
    b <- order(abs(sqrt_standard_resid), decreasing = TRUE)[1:5]
    highest <- ifelse(sqrt_standard_resid %in% sqrt_standard_resid[b], labels(sqrt_standard_resid), "")
    
    p<-ggplot(lm, aes(lm[["fitted.values"]], sqrt_standard_resid))+geom_jitter(na.rm=TRUE)
    p<-p+stat_smooth(method="loess", na.rm = TRUE, alpha = .2)+xlab("Fitted Value")
    p <- p+geom_text(aes(label = highest), nudge_y = .05, nudge_x = .1)
    p<-p+ylab(expression(sqrt("|Standardized residuals|")))
    p<-p+ggtitle("Scale-Location")+theme_classic()
    print(p)
  }
  if (resvlev_plot == TRUE) {
    if (class(lm) != "lm") {
      stop("lm must be a linear model")
    }
    
    sqrt_standard_resid <- sqrt(abs(rstandard(lm)))
    
    p5<-ggplot(lm, aes(.hat, sqrt_standard_resid))+geom_point(aes(size=.cooksd), na.rm=TRUE) +
      stat_smooth(method="loess", na.rm=TRUE) +
      xlab("Leverage")+ylab("Standardized Residuals") +
      ggtitle("Residuals vs Leverage") +
      scale_size_continuous("Cook's Distance", range=c(1,5)) +
      theme_classic() + 
      theme(legend.position="bottom")
    print(p5)
  }
  if (!is.null(data[[time_var]]) & rvo_plot == TRUE) {
    
    if (class(lm) != "lm") {
      stop("lm must be a linear model")
    }
    
    if(class(data) != "data.frame") {
      stop("df must be a data frame")
    }
    
    if(class(data[[time_var]]) != "numeric") {
      stop("time variable must be numeric")
    }
    
    if (length(data[[time_var]]) == 0) {
      stop("time variable must be a variable in data frame")
    }
    
    p7 <- ggplot(lm, aes(y = .resid, x = data[[time_var]])) + geom_jitter() +
      stat_smooth(method = "loess") +
      geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
      xlab("Observation Order") + ylab("Residuals") +
      ggtitle("Residuals vs Order") +
      theme_classic()
    print(p7)
  }
  return(NULL)
}
