##
## FPR_TPR
FPR_TPR <- function(prediction, actual){
  
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <- data.frame( FPR = FP / (FP + TN), TPR = TP / (TP + FN), ACC = (TP+TN)/(TP+TN+FP+FN), TP = TP, TN=TN, FP=FP, FN=FN )
  
return (result)
}

