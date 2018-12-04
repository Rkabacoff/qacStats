#' @title getProbs
#'
#' @description
#' \code{getProbs} hidden function creating the actual ROC plots and table output with relevant model values.
#'
#' @details
#' A function to help process the models supplied to plotROCS function and plot (potentially multiple) ROC curves, and related model calculations.
#'
#' @param list1 models to be supplied to the function. Ie., `model`, `fit.glm`, etc.
#' @param colorsList option for the color of lines to be plotted, in order of models listed. Call taken from `plotROCS`.
#' @param colorPoints option for the color of optimal cutoff point(s), if plotted.  Call taken from `plotROCS`.
#' @param cutoffs option to plot the cutoff values.  Call taken from `plotROCS`.
#' @param cutoffSpec details on the method to plot the cutoff. Uses format seq(0,1,by=.5) where 0 is the "from" value, 1 is the "to" value, and "by" is the interval. Call taken from `plotROCS`.
#' @param opCP option to include the optimal cutoff point(s) when plotting. Call taken from `plotROCS`.
#'
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @import caret
#' @import regclass
#' @import ROCR
#' @import ROCR
#' @export
#' @author Elizaveta Kravchenko <ekravchenko@@wesleyan.edu>

getProb <- function(list1, colorsList, colorPoints, cutoffs, cutoffSpec, opCP) {
  x<-1
  for(i in 1:length(list1)){
    model <- (list1[i][[1]])
    data1 <- eval(model$call$data, environment(formula(model)))
    predVar <- as.character(as.formula(model)[[2]])
    use <- data1[predVar]
    labels1 <- levels(use[[1]])
    # inside the model is the data set you used
    data1$prob <- predict(model, data1, type="response")
    data1$pred <- factor(data1$prob > .5,
                         levels=c(FALSE,TRUE),
                         labels=labels1)

    cm<-confusionMatrix(data1$pred, use[[1]], positive=labels1[2])

    pred <- prediction(data1$prob, use)
    perf <- performance(pred, "tpr", "fpr")

    l<-length(list1)
    ifelse(x==1,output<-tibble(name=character(l),
                               optimalCP=numeric(l),
                               sensitivity=numeric(l),
                               specificity=numeric(l),
                               accuracy=numeric(l),
                               areaUnderCurve=numeric(l)),
           x<-x)

    title <- (names(list1)[x])
    ### cut point
    cost = performance(pred, "cost")
    k <- which.min(cost@y.values[[1]])
    cut <- pred@cutoffs[[1]][k]
    ### sensitivity
    sens <- pred@tp[[1]][k] / pred@n.pos[[1]]
    ### specificity
    spec <- pred@tn[[1]][k] / pred@n.neg[[1]]
    ### Accuracy
    acc <- cm$overall['Accuracy']
    ### area under curve
    perf.auc <- performance(pred, "auc")
    # to print out the area itself
    useAUC <- perf.auc@y.values
    output[x,] <- c(name=title, optimalCP=cut, sensitivity=sens, specificity=spec, accuracy=acc, areaUnderCurve=useAUC)
    ifelse(x==length(list1), print(output), x<-x)
    #--------------------------------------------------------------------------------------------------

    if(typeof(cutoffSpec)=="double"){
      cutoffsUse=cutoffSpec
    }else if(cutoffs==T){
      cutoffsUse=seq(0, 1,by=.05)
    } else {
      cutoffsUse=NULL
    }

    ifelse(x>1, par(new=TRUE), x<-x)
    plot(perf,
         main="ROC Curve with Cutoffs",
         colorize=FALSE,
         col = colorsList[x],
         print.cutoffs.at=cutoffsUse,
         text.adj=c(1.5, 1),
         text.cex=.6,
         xlab="1 - Specificity (False Positive Rate)",
         ylab="Sensitivity (True Positive Rate)")
    cost = performance(pred, "cost")
    k <- which.min(cost@y.values[[1]])
    cut <- pred@cutoffs[[1]][k]
    sens <- pred@tp[[1]][k] / pred@n.pos[[1]]
    spec <- pred@tn[[1]][k] / pred@n.neg[[1]]
    if(opCP) {points(1-spec, sens, col=colorPoints, pch=19)} else {x<-x}
    x <- x+1
  }

  legend("bottomright", legend=names(list1)[1:length(list1)],
         col=colorsList[1:x], lty = 1, cex=0.8)
  abline(a=0, b= 1)
  abline(h=seq(0, 1, by=.1), lty=3, col="lightgrey")
  abline(v=seq(0, 1, by=.1), lty=3, col="lightgrey")
}
