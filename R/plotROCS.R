#' @title Create ROC plots for multiple models
#'
#' @description
#' \code{plotROCS} creates ROC plots for one or more statistical model
#'
#' @details
#' A function for quick ROC plots of one or more model.
#'
#' @param ... models to be supplied to the function. Ie., `model`, `fit.glm`, etc.
#' @param colorsList option for the color of lines to be plotted, in order of models listed. Default `c("red", "blue", "magenta", "lightgreen")`.
#' @param colorPoints option for the color of optimal cutoff point(s), if plotted; if opCP=`FALSE`, results in no change.
#' @param cutoffs option to plot the cutoff values. Default `FALSE`.
#' @param cutoffSpec details on the method to plot the cutoff. Uses format seq(0,1,by=.5) where 0 is the "from" value, 1 is the "to" value, and "by" is the interval.
#' @param opCP option to include the optimal cutoff point(s) when plotting. Default `TRUE`.
#'
#' @export
#' @author Elizaveta Kravchenko <ekravchenko@@wesleyan.edu>

plotROCS <- function(..., colorsList= c("red", "blue", "magenta", "lightgreen"),
                     colorPoints = "red", cutoffs=F, cutoffSpec=NULL, opCP=T){
  warning("The data sets of your models must be unique.")
  x<-quos(...)
  useList <- vector("list", length(x))
  for(i in 1:length(x)){
    test<- x[[i]][2]
    if (as.character(test) %in% ls(envir = .GlobalEnv)) {
      useList[[i]] <- get(as.character(test), envir = .GlobalEnv)
      test2<-((useList[[i]]$call))
      if(class(test2)!="call") {
        stop("The inputs of this function must be models.")
      }
      names(useList)[i] <- as.character(test)
    } else {
      i
    }
  }
  getProb(useList, colorsList, colorPoints, cutoffs, cutoffSpec,opCP)
}
