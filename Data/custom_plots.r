library(lattice) 

## Density plot of circle horizontal distance by participant
participants <- factor(c(data.clean[["participant_number"]]), levels=c(1:26), labels=c(1:26))
densityplot(~X.reduced.diff[["Circle.Center"]]|participants,
   main="Density Plot of Circle Center X Distance by Participant",
   xlab="Circle Center Horizontal Distance to Actual Center",
   layout=c(7,4))


## Density plot of circle vertical distance by participant
#TODO

## Density plot of circle horizontal distance by action type
action.type <- factor(c(combined.data[["action_type.1"]]), levels=c(1,2), labels=c("GRAB","RELEASE"))
densityplot(~X.reduced.diff[["Circle.Center"]]|action.type,
  main="Density plot of circle horizontal distance by action type",
  xlab="Circle Center Horizontal Distance to Actual Center")

## Boxplot of Circle X for each combination of Size and Action Type
size.factor <- cut(data.clean[["grab.or.release.obj.size"]], 5, labels=c("< 100 px", "100 to 200 px", "200 to 300 px", "300 to 400 px", "400 to 500 px"))
bwplot(action.type~X.reduced.diff[["Circle.Center"]]|size.factor,
   ylab="action type", xlab="object width",
   main="Circle Center Horizontal Distance for each Combination of Size and Action Type",
   layout=(c(2,3)))

