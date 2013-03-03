##############################################################
## 	Preperation and Cleaning the Data                   ##
##############################################################

##############################################################
## Important meta data:
## tracker resolution: 668x376
## active region: top 56 left 31 width 556 height 314
## Screen resolution: 1920x1080
## experiment app resolution: 1600x900
## aspect ration of both resolutions = 1.77
## size: [20 500]
##############################################################

local({
## Prepare
#store source data into combined.data variable  

## Remove outliers and zeros
remove_outliers <- function(x, na.rm = TRUE, ...) {

  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y[x == 0] <- NA
  y
}

#############################################################
## initialize and clean data
library(plyr)
data.clean <- numcolwise(remove_outliers)(combined.data)

mass.clean.X = data.frame ("Actual X"=c(remove_outliers(combined.data$Object.or.Target.X)), 
  "Mass X1"=c(remove_outliers(combined.data$mass.center.x)), "Mass X2"=c(remove_outliers(combined.data$mass.center.x.1)), 
  "Mass X3"=c(remove_outliers(combined.data$mass.center.x.2)), "Mass X4"=c(remove_outliers(combined.data$mass.center.x.3)), 
  "Mass X5"=c(remove_outliers(combined.data$mass.center.x.4)), "Mass X6"=c(remove_outliers(combined.data$mass.center.x.5)), 
  "Mass X7"=c(remove_outliers(combined.data$mass.center.x.6)), "Mass X8"=c(remove_outliers(combined.data$mass.center.x.7)), 
  "Mass X9"=c(remove_outliers(combined.data$mass.center.x.8)), "Mass X10"=c(remove_outliers(combined.data$mass.center.x.9)), 
  "Mass X11"=c(remove_outliers(combined.data$mass.center.x.10)), "Mass X12"=c(remove_outliers(combined.data$mass.center.x.11)))

mass.clean.Y = data.frame ("Actual Y"=c(remove_outliers(combined.data$Object.or.Target.Y)), 
  "Mass Y1"=c(remove_outliers(combined.data$mass.center.y)), "Mass Y2"=c(remove_outliers(combined.data$mass.center.y.1)), 
  "Mass Y3"=c(remove_outliers(combined.data$mass.center.y.2)), "Mass Y4"=c(remove_outliers(combined.data$mass.center.y.3)), 
  "Mass Y5"=c(remove_outliers(combined.data$mass.center.y.4)), "Mass Y6"=c(remove_outliers(combined.data$mass.center.y.5)), 
  "Mass Y7"=c(remove_outliers(combined.data$mass.center.y.6)), "Mass Y8"=c(remove_outliers(combined.data$mass.center.y.7)), 
  "Mass Y9"=c(remove_outliers(combined.data$mass.center.y.8)), "Mass Y10"=c(remove_outliers(combined.data$mass.center.y.9)), 
  "Mass Y11"=c(remove_outliers(combined.data$mass.center.y.10)), "Mass Y12"=c(remove_outliers(combined.data$mass.center.y.11)))

mass.diff.X = data.frame (
  "Mass X2"=c(remove_outliers(combined.data$Object.or.Target.X) / 1600 - remove_outliers(combined.data$mass.center.x.1) / 600), 
  "Mass X3"=c(remove_outliers(combined.data$Object.or.Target.X) / 1600 - remove_outliers(combined.data$mass.center.x.2) / 600), 
  "Mass X4"=c(remove_outliers(combined.data$Object.or.Target.X) / 1600 - remove_outliers(combined.data$mass.center.x.3) / 600))

circle.clean.X = data.frame ("Actual X"=c(remove_outliers(combined.data$Object.or.Target.X)), 
  "Circle X1"=c(remove_outliers(combined.data[["min_circle.center.x"]])), 
  "Circle X2"=c(remove_outliers(combined.data[["min_circle.cente
r.x.1"]])),
  "Circle X3"=c(remove_outliers(combined.data[["min_circle.center.x.2"]])), 
  "Circle X4"=c(remove_outliers(combined.data[["min_circle.center.x.3"]])),
  "Circle X5"=c(remove_outliers(combined.data[["min_circle.center.x.4"]])), 
  "Circle X6"=c(remove_outliers(combined.data[["min_circle.center.x.5"]])),
  "Circle X7"=c(remove_outliers(combined.data[["min_circle.center.x.6"]])), 
  "Circle X8"=c(remove_outliers(combined.data[["min_circle.center.x.7"]])), 
  "Circle X9"=c(remove_outliers(combined.data[["min_circle.center.x.8"]])), 
  "Circle X10"=c(remove_outliers(combined.data[["min_circle.center.x.9"]])),
  "Circle X11"=c(remove_outliers(combined.data[["min_circle.center.x.10"]])), 
  "Circle X12"=c(remove_outliers(combined.data[["min_circle.center.x.11"]])))

circle.clean.Y = data.frame ("Actual Y"=c(data.clean[["Object.or.Target.Y"]]), 
  "Circle Y1"=c(data.clean[["min_circle.center.y"]]), 
  "Circle Y2"=c(data.clean[["min_circle.center.y.1"]]),
  "Circle Y3"=c(data.clean[["min_circle.center.y.2"]]), 
  "Circle Y4"=c(data.clean[["min_circle.center.y.3"]]),
  "Circle Y5"=c(data.clean[["min_circle.center.y.4"]]), 
  "Circle Y6"=c(data.clean[["min_circle.center.y.5"]]),
  "Circle Y7"=c(data.clean[["min_circle.center.y.6"]]), 
  "Circle Y8"=c(data.clean[["min_circle.center.y.7"]]), 
  "Circle Y9"=c(data.clean[["min_circle.center.y.8"]]), 
  "Circle Y10"=c(data.clean[["min_circle.center.y.9"]]),
  "Circle Y11"=c(data.clean[["min_circle.center.y.10"]]), 
  "Circle Y12"=c(data.clean[["min_circle.center.y.11"]]))

## Minimum Enclosing Circle Center
circle.Y.reduced = data.frame ("Actual Y"=c(data.clean[["Object.or.Target.Y"]]), 
  "Circle Y2"=c(data.clean[["min_circle.center.y.1"]]),
  "Circle Y3"=c(data.clean[["min_circle.center.y.2"]]), 
  "Circle Y4"=c(data.clean[["min_circle.center.y.3"]]),
  "C.Med.2-4.Y"=apply(data.clean[c("min_circle.center.y.1", "min_circle.center.y.2", "min_circle.center.y.3")], 1, median),
  "C.Mean.2-4.Y"=apply(data.clean[c("min_circle.center.y.1", "min_circle.center.y.2", "min_circle.center.y.3")], 1, mean))

circle.X.reduced = data.frame("Actual X"=c(data.clean[["Object.or.Target.X"]]),
  "Circle X2"=c(data.clean[["min_circle.center.x.1"]]),
  "Circle X3"=c(data.clean[["min_circle.center.x.2"]]),
  "Circle X4"=c(data.clean[["min_circle.center.x.3"]]),
  "C.Med.2-4.X"=apply(data.clean[c("min_circle.center.x.1", "min_circle.center.x.2", "min_circle.center.x.3")], 1, median),
  "C.Mean.2-4.X"=apply(data.clean[c("min_circle.center.x.1", "min_circle.center.x.2", "min_circle.center.x.3")], 1, mean))

## Feature Mean Position
fm.X.reduced = data.frame("Actual X"=c(data.clean[["Object.or.Target.X"]]),
  "FM X2"=c(data.clean[["feature_mean.x.1"]]),
  "FM X3"=c(data.clean[["feature_mean.x.2"]]),
  "FM X4"=c(data.clean[["feature_mean.x.3"]]),
  "FM.Med.2-4.X"=apply(data.clean[c("feature_mean.x.1", "feature_mean.x.2", "feature_mean.x.3")], 1, median),
  "FM.Mean.2-4.X"=apply(data.clean[c("feature_mean.x.1", "feature_mean.x.2", "feature_mean.x.3")], 1, mean)
)

fm.Y.reduced = data.frame("Actual Y"=c(data.clean[["Object.or.Target.Y"]]),
  "FM Y2"=c(data.clean[["feature_mean.y.1"]]),
  "FM Y3"=c(data.clean[["feature_mean.y.2"]]),
  "FM Y4"=c(data.clean[["feature_mean.y.3"]]),
  "FM.Med.2-4.Y"=apply(data.clean[c("feature_mean.y.1", "feature_mean.y.2", "feature_mean.y.3")], 1, median),
  "FM.Mean.2-4.Y"=apply(data.clean[c("feature_mean.y.1", "feature_mean.y.2", "feature_mean.y.3")], 1, mean)
)

## Center of Mass Position
mass.X.reduced = data.frame("Actual X"=c(data.clean[["Object.or.Target.X"]]),
  "Mass X2"=c(data.clean[["mass.center.x.1"]]),
  "Mass X3"=c(data.clean[["mass.center.x.2"]]),
  "Mass X4"=c(data.clean[["mass.center.x.3"]]),
  "Mass.Med.2-4.X"=apply(data.clean[c("mass.center.x.1", "mass.center.x.2", "mass.center.x.3")], 1, median),
  "Mass.Mean.2-4.X"=apply(data.clean[c("mass.center.x.1", "mass.center.x.2", "mass.center.x.3")], 1, mean)
)

mass.Y.reduced = data.frame("Actual Y"=c(data.clean[["Object.or.Target.Y"]]),
  "Mass Y2"=c(data.clean[["mass.center.y.1"]]),
  "Mass Y3"=c(data.clean[["mass.center.y.2"]]),
  "Mass Y4"=c(data.clean[["mass.center.y.3"]]),
  "Mass.Med.2-4.Y"=apply(data.clean[c("mass.center.y.1", "mass.center.y.2", "mass.center.y.3")], 1, median),
  "Mass.Mean.2-4.Y"=apply(data.clean[c("mass.center.y.1", "mass.center.y.2", "mass.center.y.3")], 1, mean)
)

## Minimum Enclosing Rectangle Center
rect.Y.reduced = data.frame ("Actual Y"=c(data.clean[["Object.or.Target.Y"]]), 
  "Rect Y2"=c(data.clean[["min_rect.center.x.1"]]),
  "Rect Y3"=c(data.clean[["min_rect.center.y.2"]]), 
  "Rect Y4"=c(data.clean[["min_rect.center.y.3"]]),
  "R.Med.2-4.Y"=apply(data.clean[c("min_rect.center.y.1", "min_rect.center.y.2", "min_rect.center.y.3")], 1, median),
  "R.Mean.2-4.Y"=apply(data.clean[c("min_rect.center.y.1", "min_rect.center.y.2", "min_rect.center.y.3")], 1, mean)
)

rect.X.reduced = data.frame("Actual X"=c(data.clean[["Object.or.Target.X"]]),
  "Rect X2"=c(data.clean[["min_rect.center.x.1"]]),
  "Rect X3"=c(data.clean[["min_rect.center.x.2"]]),
  "Rect X4"=c(data.clean[["min_rect.center.x.3"]]),
  "R.Med.2-4.X"=apply(data.clean[c("min_rect.center.x.1", "min_rect.center.x.2", "min_rect.center.x.3")], 1, median),
  "R.Mean.2-4.X"=apply(data.clean[c("min_rect.center.x.1", "min_rect.center.x.2", "min_rect.center.x.3")], 1, mean)
)

feat.stddev = data.frame("Object Size"=c(remove_outliers(combined.data[["grab.or.release.obj.size"]])),
  "Std 1"=c(remove_outliers(combined.data[["feature_mean.x"]])),
  "Std 2"=c(remove_outliers(combined.data[["feature_mean.x.1"]])),
  "Std 3"=c(remove_outliers(combined.data[["feature_mean.x.2"]])),
  "Std 4"=c(remove_outliers(combined.data[["feature_mean.x.3"]])),
  "Std 5"=c(remove_outliers(combined.data[["feature_mean.x.4"]])),
  "Std 6"=c(remove_outliers(combined.data[["feature_mean.x.5"]])),
  "Std 7"=c(remove_outliers(combined.data[["feature_mean.x.6"]])),
  "Std 8"=c(remove_outliers(combined.data[["feature_mean.x.7"]])),
  "Std 9"=c(remove_outliers(combined.data[["feature_mean.x.8"]])),
  "Std 10"=c(remove_outliers(combined.data[["feature_mean.x.9"]])),
  "Std 11"=c(remove_outliers(combined.data[["feature_mean.x.10"]])),
  "Std 12"=c(remove_outliers(combined.data[["feature_mean.x.11"]])))

## Hand Side
hand.side = data.frame(
  "participant"=c(data.clean[["participant_number"]]),
  "record"=c(data.clean[["record_number"]])
)
for(i in 7852:7860) {
  hand.side[["actual.side"]][i] = 1;
}

## Object or Target normalized position
obj.or.tar.norm = data.frame(
  "X"=c((data.clean[["Object.or.Target.X"]] - 160) / 1600),
  "Y"=c(1 - (data.clean[["Object.or.Target.Y"]] - 90) / 900)
)

## Normalized X differences using selected models
X.models.norm <- data.frame(
  "Feature Mean"	= c((fm.X.reduced[["FM.Mean.2.4.X"]] - 56) / 668),
  "Mass Center" 	= c((mass.X.reduced[["Mass.Mean.2.4.X"]] - 56) / 668),
  "Circle Center" 	= c((circle.X.reduced[["C.Mean.2.4.X"]] - 56) / 668),
  "Rect Center" 	= c((rect.X.reduced[["R.Mean.2.4.X"]] - 56) / 668)
)

## Normalized Y models
Y.models.norm <- data.frame(
  "Feature Mean"=c((fm.Y.reduced[["FM.Mean.2.4.Y"]] - 31) / 376),
  "Mass Center"=c((mass.Y.reduced[["Mass.Mean.2.4.Y"]] - 31) / 376),
  "Circle Center"=c((circle.Y.reduced[["C.Mean.2.4.Y"]] - 31) / 376),
  "Rect Center"=c((rect.Y.reduced[["R.Mean.2.4.Y"]] - 31) / 376)
)

## normalized horizontal distance
X.norm.dist <- data.frame(
  "Feature Mean" = c(X.models.norm[["Feature.Mean"]] - obj.or.tar.norm[["X"]]),
  "Mass Center" = c(X.models.norm[["Mass.Center"]] - obj.or.tar.norm[["X"]]),
  "Circle Center" = c(X.models.norm[["Circle.Center"]] - obj.or.tar.norm[["X"]]),
  "Rect Center" = c(X.models.norm[["Rect.Center"]] - obj.or.tar.norm[["X"]])
)

qqnorm(mass.clean[["c.remove_outliers.combined.data.mass.center.x.."]], main="Normal Q-Q Plot of mass center X for frame 1", xlab="Theoretical Quantiles of mass center X", ylab="Sample Quantiles of mass center X", col="green")
qqline(mass.clean[["c.remove_outliers.combined.data.mass.center.x.."]], col="red")
