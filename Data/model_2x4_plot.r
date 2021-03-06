## Main model plot consisting of a 2X4 matrix of regression plots one for each model against X and Y position

### Define some colors
bg1 = "#00000010"
bg2 = "#00000010"
leftCol = "#ff000060"
rightCol = "#0000ff60"
pointSize = 1
pchType = 16
modelFontCol = "midnightblue"

#layout(matrix(c(1:8), nrow=2, byrow=TRUE), heights=list(top.padding=-30, bottom.padding=-30), mar=c(0,0,0,0))
par(mfrow=c(2,4)) 
#par(byrow=TRUE)

############################
## Y ~ MC
par(mar = c(0, 5, 0, 0))

plot(obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Mass.Center"]], xlab="", ylab=list("Y Center Coordinate", font=6, cex=1.5), col=bg1, pch=pchType,cex=pointSize)
abline(Y.lm.MC, col="black", lwd=3)
points(Y.models.right$Y ~ Y.models.right$MC, col=rightCol, pch=pchType, cex=pointSize)
points(Y.models.left$Y ~ Y.models.left$MC, col=leftCol, pch=pchType, cex=pointSize)
abline(Y.lm.MC.left, col=leftCol, lwd=1)
abline(Y.lm.MC.right, col=rightCol, lwd=1)

## Y ~ CC
par(mar = c(0, 0, 0, 0)) 
plot(obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Circle.Center"]], xlab="", ylab="", col=bg2, pch=pchType,cex=pointSize)
abline(Y.lm.CC, col="black", lwd=3)
points(Y.models.right$Y ~ Y.models.right$CC, col=rightCol, pch=pchType, cex=pointSize)
points(Y.models.left$Y ~ Y.models.left$CC, col=leftCol, pch=pchType, cex=pointSize)
abline(Y.lm.CC.left, col=leftCol, lwd=1)
abline(Y.lm.CC.right, col=rightCol, lwd=1)

## Y ~ RC
plot(obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Rect.Center"]], xlab="", ylab="", col=bg1, pch=pchType,cex=pointSize)
abline(Y.lm.RC, col="black", lwd=3)
points(Y.models.right$Y ~ Y.models.right$RC, col=rightCol, pch=pchType, cex=pointSize)
points(Y.models.left$Y ~ Y.models.left$RC, col=leftCol, pch=pchType, cex=pointSize)
abline(Y.lm.RC.left, col=leftCol, lwd=1)
abline(Y.lm.RC.right, col=rightCol, lwd=1)

## Y ~ FM
plot(obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Feature.Mean"]], xlab="", ylab="", col=bg2, pch=pchType,cex=pointSize)
abline(Y.lm.FM, col="black", lwd=3)
points(Y.models.right$Y ~ Y.models.right$FM, col=rightCol, pch=pchType, cex=pointSize)
points(Y.models.left$Y ~ Y.models.left$FM, col=leftCol, pch=pchType, cex=pointSize)
abline(Y.lm.FM.left, col=leftCol, lwd=1)
abline(Y.lm.FM.right, col=rightCol, lwd=1)

#############################
## X ~ MC
par(mar = c(5, 5, 0, 0)) 
plot(X.models.norm.action[["obj.or.tar.norm.X"]] ~ X.models.norm.action[["X.models.norm...Mass.Center..."]], xlab=list("Mass Center (MC)", font=6, col=modelFontCol, cex=2), ylab=list("X Center Coordinate", font=6, cex=1.5), col=bg1, pch=pchType,cex=pointSize)
points(X.models.right$X ~ X.models.right$MC, col=rightCol, pch=pchType, cex=pointSize)
points(X.models.left$X ~ X.models.left$MC, col=leftCol, pch=pchType, cex=pointSize)
abline(X.lm.MC.left, col=leftCol, lwd=3)
abline(X.lm.MC.right, col=rightCol, lwd=3)

## X ~ CC
par(mar = c(5, 0, 0, 0)) 
plot(X.models.norm.action[["obj.or.tar.norm.X"]] ~ X.models.norm.action[["X.models.norm...Circle.Center..."]], xlab=list("Circle Center (CC)", font=6, col=modelFontCol, cex=2), ylab="", col=bg2, pch=pchType,cex=pointSize)
points(X.models.right$X ~ X.models.right$CC, col=rightCol, pch=pchType, cex=pointSize)
points(X.models.left$X ~ X.models.left$CC, col=leftCol, pch=pchType, cex=pointSize)
abline(X.lm.CC.left, col=leftCol, lwd=3)
abline(X.lm.CC.right, col=rightCol, lwd=3)

## X ~ RC
plot(X.models.norm.action[["obj.or.tar.norm.X"]] ~ X.models.norm.action[["X.models.norm...Rect.Center..."]], xlab=list("Rectangle Center (RC)", font=6, col=modelFontCol, cex=2), ylab="", col=bg1, pch=pchType,cex=pointSize)
points(X.models.right$X ~ X.models.right$RC, col=rightCol, pch=pchType, cex=pointSize)
points(X.models.left$X ~ X.models.left$RC, col=leftCol, pch=pchType, cex=pointSize)
abline(X.lm.RC.left, col=leftCol, lwd=3)
abline(X.lm.RC.right, col=rightCol, lwd=3)

## X ~ FM
plot(X.models.norm.action[["obj.or.tar.norm.X"]] ~ X.models.norm.action[["X.models.norm...Feature.Mean..."]], xlab=list("Feature Mean (FM)", font=6, col=modelFontCol, cex=2), ylab="", col=bg2, pch=pchType,cex=pointSize)
points(X.models.right$X ~ X.models.right$FM, col=rightCol, pch=pchType, cex=pointSize)
points(X.models.left$X ~ X.models.left$FM, col=leftCol, pch=pchType, cex=pointSize)
abline(X.lm.FM.left, col=leftCol, lwd=3)
abline(X.lm.FM.right, col=rightCol, lwd=3)


#mtext("Linear Regression Models for Predicting X and Y Corodinates of Center of Action", outer=TRUE, line=1)
