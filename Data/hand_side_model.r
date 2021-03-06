## Model for distinguishing left and right hand.side


#test <- data.frame("Side"=(fm.X.reduced[["FM.Mean.2.4.X"]]) - (rect.X.reduced[["R.Mean.2.4.X"]] + mass.X.reduced[["Mass.Mean.2.4.X"]] + circle.X.reduced[["C.Mean.2.4.X"]]) )
#test <- abs(X.models.norm[["Circle.Center"]] - X.models.norm[["Feature.Mean"]]) * 
# test <- log(2 + X.models.norm[["Feature.Mean"]] - X.models.norm[["Circle.Center"]]) 
#test <- (pmax(X.models.norm[["Circle.Center"]], X.models.norm[["Mass.Center"]], X.models.norm[["Rect.Center"]], na.rm=FALSE) - (X.models.norm[["Feature.Mean"]]))
#plot(test, col=hand.side[["actual.side"]])
#plot(test~obj.or.tar.norm$X, col=hand.side[["actual.side"]])
#plot(combined.data[["min_rect.angle.2"]]~obj.or.tar.norm[["X"]], col=hand.side[["actual.side"]])
#plot(combined.data[["min_rect.size.height.1"]]~obj.or.tar.norm[["X"]], col=hand.side[["actual.side"]])
#plot(combined.data[["min_rect.size.width.1"]]~obj.or.tar.norm[["X"]], col=hand.side[["actual.side"]])
#test <- abs(combined.data[["min_rect.angle.1"]]) + X.models.norm[["Feature.Mean"]]


find_side <- function(angle, FM.X, ...) {
  y <- 2
  y[angle < 120 - 100*FM.X ] <- 1
  y[angle < 90 - 100*FM.X ] <- 2
  y[angle < 45 - 100*FM.X] <- 1
  y
}

remove_zero_angle <- function(angle, ...) {
  retVal <- abs(angle)
  retVal[angle==0] <- NA
  retVal
}

### Define some colors
leftCol = "#ff000090"
rightCol = "#0000ff90"
sidePointSize = 1.0
leftSidePchType = 10
righSidePchType = 12
sepCol = "ORANGE"
#angles <- remove_zero_angle(combined.data[["min_rect.angle.1"]])
plot(angles~X.models.norm[["Feature.Mean"]], col=c(leftCol, rightCol)[hand.side[["actual.side"]]], pch=c(leftSidePchType,righSidePchType)[hand.side[["actual.side"]]], cex=sidePointSize)
abline(45, -100, col=sepCol)
abline(90,-100, col=sepCol)
abline(120, -100, col=sepCol)

hand.side[["calculated.side"]] <- find_side(angles, X.models.norm[["Feature.Mean"]])
#number of successfull prediction
sum(hand.side[["calculated.side"]] == hand.side[["actual.side"]], na.rm=TRUE)
#number of incorrect prediction
sum(hand.side[["calculated.side"]] != hand.side[["actual.side"]], na.rm=TRUE)