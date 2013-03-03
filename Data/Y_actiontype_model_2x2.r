## Effect of action type plot consisting of a 2X2 matrix of regression plots one for each model against Y position
## seperated by action type

### Define some colors
bgGrabCol = "#ff000020"
bgReleaseCol = "#0000ff20"
grabCol = "#ff000080"
releaseCol = "#0000ff80"
pointSize = 0.5
pchType = 16
modelFontCol = "midnightblue"

#layout(matrix(c(1:8), nrow=2, byrow=TRUE), heights=list(top.padding=-30, bottom.padding=-30), mar=c(0,0,0,0))
par(mfrow=c(2,2)) 
#par(byrow=TRUE)

############################
## Y ~ MC
par(mar = c(0, 0, 0, 0))

plot(Y.models.release$Y ~ Y.models.release$MC, col=bgReleaseCol, pch=pchType, cex=pointSize)
points(Y.models.grab$Y ~ Y.models.grab$MC, col=bgGrabCol, pch=pchType, cex=pointSize)
abline(Y.lm.MC, col="black", lwd=1)
abline(Y.lm.MC.grab, col=grabCol, lwd=2)
abline(Y.lm.MC.release, col=releaseCol, lwd=2)

## Y ~ CC
par(mar = c(0, 0, 0, 0)) 
plot(Y.models.release$Y ~ Y.models.release$CC, col=bgReleaseCol, pch=pchType, cex=pointSize)
points(Y.models.grab$Y ~ Y.models.grab$CC, col=bgGrabCol, pch=pchType, cex=pointSize)
abline(Y.lm.CC, col="black", lwd=1)
abline(Y.lm.CC.grab, col=grabCol, lwd=2)
abline(Y.lm.CC.release, col=releaseCol, lwd=2)

## Y ~ RC
plot(Y.models.release$Y ~ Y.models.release$RC, col=bgReleaseCol, pch=pchType, cex=pointSize)
points(Y.models.grab$Y ~ Y.models.grab$RC, col=bgGrabCol, pch=pchType, cex=pointSize)
abline(Y.lm.RC, col="black", lwd=1)
abline(Y.lm.RC.grab, col=grabCol, lwd=2)
abline(Y.lm.RC.release, col=releaseCol, lwd=2)

## Y ~ FM
plot(Y.models.release$Y ~ Y.models.release$FM, col=bgReleaseCol, pch=pchType, cex=pointSize)
points(Y.models.grab$Y ~ Y.models.grab$FM, col=bgGrabCol, pch=pchType, cex=pointSize)
abline(Y.lm.FM, col="black", lwd=1)
abline(Y.lm.FM.grab, col=grabCol, lwd=2)
abline(Y.lm.FM.release, col=releaseCol, lwd=2)
