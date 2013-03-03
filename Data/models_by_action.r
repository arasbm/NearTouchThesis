## Testing if regression models are different based on action type (grab and release)
##

library(car)
library(lattice)
X.models.norm.action <- data.frame(obj.or.tar.norm$X, X.models.norm[["Mass.Center"]], X.models.norm[["Rect.Center"]], X.models.norm[["Feature.Mean"]], X.models.norm[["Circle.Center"]], action.type)
plot(X.models.norm.action[["X.models.norm...Circle.Center..."]]~X.models.norm.action[["obj.or.tar.norm.X"]], col=X.models.norm.action[["action.type"]])

X.circle.action.lm <- lm(X.models.norm.action[["X.models.norm...Circle.Center..."]] ~ X.models.norm.action[["obj.or.tar.norm.X"]] + X.models.norm.action[["action.type"]])

xyplot(X.models.norm.action[["X.models.norm...Circle.Center..."]] ~ X.models.norm.action[["obj.or.tar.norm.X"]] | X.models.norm.action[["action.type"]],
  panel = function(x, y, ...)
  {
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, ...)
  }
)
# X.models.grab <- subset(X.models.norm.action, X.models.norm.action[["action.type"]]=="GRAB")
# X.models.release <- subset(X.models.norm.action, X.models.norm.action[["action.type"]]=="RELEASE")
# X.grab.lm <- lm(
#   cbind(X.models.grab[["X.models.norm...Circle.Center..."]], 
# 	X.models.grab[["X.models.norm...Feature.Mean..."]], 
# 	X.models.grab[["X.models.norm...Mass.Center..."]], 
# 	X.models.grab[["X.models.norm...Rect.Center..."]]) ~ X.models.grab[["obj.or.tar.norm.X"]])
# X.release.lm <- lm(X.models.release[["X.models.norm...Circle.Center..."]] ~ X.models.release[["obj.or.tar.norm.X"]])
# ## plot the two regression lines
#abline(X.grab.lm, col="YELLOW")
#abline(X.release.lm, col="GREEN")
