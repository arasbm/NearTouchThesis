####################################################################
## Constructing Linear Regression Models                          ##
####################################################################

## Modeling Horizontal Distance
## X position data is bimodal
library(mixtools)
hrz.dist = X.norm.dist[["Circle.Center"]] 
mixmdl = normalmixEM(hrz.dist)
plot(mixmdl, which=2)
lines(density(hrz.dist), lty=2, lwd=2)


test <- glm(X.models.norm[["Circle.Center"]]~obj.or.tar.norm[["X"]])

####################################################################
## Modeling Vertical Position (Y)

library(relaimpo)
plot(obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Rect.Center"]])

Y.combined.lm <- lm (obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Rect.Center"]] + Y.models.norm[["Mass.Center"]] + Y.models.norm[["Feature.Mean"]] + Y.models.norm[["Circle.Center"]])
Y.lm.RC <- lm (obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Rect.Center"]])
Y.lm.MC <- lm (obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Mass.Center"]])
Y.lm.FM <- lm (obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Feature.Mean"]])
Y.lm.CC <- lm (obj.or.tar.norm[["Y"]] ~ Y.models.norm[["Circle.Center"]])

summary(Y.combined.lm)
coefficients(Y.combined.lm)
calc.relimp(Y.lm)

test <- princomp(Y.models.norm, scores=TRUE, cor=TRUE)

plot(Y.models.norm[["Circle.Center"]]~obj.or.tar.norm[["Y"]])

#################################################################
## Horizontal models (X)

## Mass Center
plot(obj.or.tar.norm$X~X.models.norm$Mass.Center)
abline(lm(obj.or.tar.norm$X~X.models.norm[["Feature.Mean"]]), col="PINK")
abline(lm(obj.or.tar.norm$X~X.models.norm$Mass.Center), col="GREEN")
abline(test.model1, col="PINK")

## Circle Center
plot(obj.or.tar.norm$X~X.models.norm$Circle.Center)
abline(lm(obj.or.tar.norm$X~X.models.norm$Circle.Center), col="RED")

## Feature Mean
plot(obj.or.tar.norm$X~X.models.norm[["Feature.Mean"]])
abline(lm(obj.or.tar.norm$X~X.models.norm[["Feature.Mean"]]), col="RED")

## Rect Center
plot(obj.or.tar.norm$X~X.models.norm$Rect.Center)
abline(lm(obj.or.tar.norm$X~X.models.norm[["Feature.Mean"]]), col="RED")
abline(lm(obj.or.tar.norm$X~X.models.norm$Rect.Center), col="GREEN")

max(mass.clean.X[["Mass.X1"]])

####################################
## split X data by hand side
X.models.norm.side <- data.frame(
  "X"=obj.or.tar.norm$X, 
  "MC"=X.models.norm$Mass.Center, 
  "CC"=X.models.norm[["Circle.Center"]], 
  "RC"= X.models.norm[["Rect.Center"]],
  "FM"=X.models.norm[["Feature.Mean"]],
  "AT"=combined.data[["action_type"]], 
  "HS"=hand.side[["actual.side"]]
   )
X.models.left <- subset(X.models.norm.side, X.models.norm.side[["HS"]]==1)
X.models.right <- subset(X.models.norm.side, X.models.norm.side[["HS"]]==2)
## CC
X.lm.CC.left = lm(X.models.left[["X"]]~X.models.left[["CC"]])
X.lm.CC.right = lm(X.models.right[["X"]]~X.models.right[["CC"]])
## MC
X.lm.MC.left = lm(X.models.left[["X"]]~X.models.left[["MC"]])
X.lm.MC.right = lm(X.models.right[["X"]]~X.models.right[["MC"]])
## RC
X.lm.RC.left = lm(X.models.left[["X"]]~X.models.left[["RC"]])
X.lm.RC.right = lm(X.models.right[["X"]]~X.models.right[["RC"]])
## FM
X.lm.FM.left = lm(X.models.left[["X"]]~X.models.left[["FM"]])
X.lm.FM.right = lm(X.models.right[["X"]]~X.models.right[["FM"]])

#####################################
## Furthur split X data by action type (grab | release) 
X.models.left.grab <- subset(X.models.left, X.models.left[["AT"]]=="grab") 
X.models.left.release <- subset(X.models.left, X.models.left[["AT"]]=="release") 

X.models.right.grab <- subset(X.models.right, X.models.right[["AT"]]=="grab") 
X.models.right.release <- subset(X.models.right, X.models.right[["AT"]]=="release")

## CC
X.lm.CC.left.grab = lm(X.models.left.grab[["X"]]~X.models.left.grab[["CC"]])
X.lm.CC.left.release = lm(X.models.left.release[["X"]]~X.models.left.release[["CC"]])
X.lm.CC.right.grab = lm(X.models.right.grab[["X"]]~X.models.right.grab[["CC"]])
X.lm.CC.right.release = lm(X.models.right.release[["X"]]~X.models.right.release[["CC"]])

##MC
X.lm.MC.left.grab = lm(X.models.left.grab[["X"]]~X.models.left.grab[["MC"]])
X.lm.MC.left.release = lm(X.models.left.release[["X"]]~X.models.left.release[["MC"]])
X.lm.MC.right.grab = lm(X.models.right.grab[["X"]]~X.models.right.grab[["MC"]])
X.lm.MC.right.release = lm(X.models.right.release[["X"]]~X.models.right.release[["MC"]])

##RC
X.lm.RC.left.grab = lm(X.models.left.grab[["X"]]~X.models.left.grab[["RC"]])
X.lm.RC.left.release = lm(X.models.left.release[["X"]]~X.models.left.release[["RC"]])
X.lm.RC.right.grab = lm(X.models.right.grab[["X"]]~X.models.right.grab[["RC"]])
X.lm.RC.right.release = lm(X.models.right.release[["X"]]~X.models.right.release[["RC"]])

##FM
X.lm.FM.left.grab = lm(X.models.left.grab[["X"]]~X.models.left.grab[["FM"]])
X.lm.FM.left.release = lm(X.models.left.release[["X"]]~X.models.left.release[["FM"]])
X.lm.FM.right.grab = lm(X.models.right.grab[["X"]]~X.models.right.grab[["FM"]])
X.lm.FM.right.release = lm(X.models.right.release[["X"]]~X.models.right.release[["FM"]])

####################################
## split Y data by hand side
Y.models.norm.side <- data.frame(
  "Y"=obj.or.tar.norm[["Y"]], 
  "MC"=Y.models.norm[["Mass.Center"]], 
  "CC"=Y.models.norm[["Circle.Center"]], 
  "RC"= Y.models.norm[["Rect.Center"]],
  "FM"=Y.models.norm[["Feature.Mean"]],
  "AT"=combined.data[["action_type"]],
  "HS"=hand.side[["actual.side"]]
)
Y.models.left <- subset(Y.models.norm.side, Y.models.norm.side[["HS"]]==1)
Y.models.right <- subset(Y.models.norm.side, Y.models.norm.side[["HS"]]==2)
## CC
Y.lm.CC.left = lm(Y.models.left[["Y"]]~Y.models.left[["CC"]])
Y.lm.CC.right = lm(Y.models.right[["Y"]]~Y.models.right[["CC"]])
## MC
Y.lm.MC.left = lm(Y.models.left[["Y"]]~Y.models.left[["MC"]])
Y.lm.MC.right = lm(Y.models.right[["Y"]]~Y.models.right[["MC"]])
## RC
Y.lm.RC.left = lm(Y.models.left[["Y"]]~Y.models.left[["RC"]])
Y.lm.RC.right = lm(Y.models.right[["Y"]]~Y.models.right[["RC"]])
## FM
Y.lm.FM.left = lm(Y.models.left[["Y"]]~Y.models.left[["FM"]])
Y.lm.FM.right = lm(Y.models.right[["Y"]]~Y.models.right[["FM"]])

####################################
## split Y by action type
Y.models.action <- data.frame(
  "Y"=obj.or.tar.norm[["Y"]], 
  "MC"=Y.models.norm[["Mass.Center"]], 
  "CC"=Y.models.norm[["Circle.Center"]], 
  "RC"=Y.models.norm[["Rect.Center"]],
  "FM"=Y.models.norm[["Feature.Mean"]], 
  "AT"=combined.data[["action_type"]],
  "HS"=hand.side[["actual.side"]]
)
Y.models.grab <- subset(Y.models.action, Y.models.action[["AT"]]=="grab")
Y.models.release <- subset(Y.models.action, Y.models.action[["AT"]]=="release")
## CC
Y.lm.CC.grab = lm(Y.models.grab[["Y"]]~Y.models.grab[["CC"]])
Y.lm.CC.release = lm(Y.models.release[["Y"]]~Y.models.release[["CC"]])
## MC
Y.lm.MC.grab = lm(Y.models.grab[["Y"]]~Y.models.grab[["MC"]])
Y.lm.MC.release = lm(Y.models.release[["Y"]]~Y.models.release[["MC"]])
## RC
Y.lm.RC.grab = lm(Y.models.grab[["Y"]]~Y.models.grab[["RC"]])
Y.lm.RC.release = lm(Y.models.release[["Y"]]~Y.models.release[["RC"]])
## FM
Y.lm.FM.grab = lm(Y.models.grab[["Y"]]~Y.models.grab[["FM"]])
Y.lm.FM.release = lm(Y.models.release[["Y"]]~Y.models.release[["FM"]])