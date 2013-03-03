######################################################################
## Analysis of co-variance to compare Linear Regression Models      ##
######################################################################

library(lattice)

#### Does hand side have an effect on the X coordinate of each of the four models? Yes for all four:
#RC For Rectangle Center
X.RC.side.aov.interaction <- aov(X~RC*HS, data=X.models.norm.side)
X.RC.side.aov.no.interaction <- aov(X~RC+HS, data=X.models.norm.side)
anova(X.RC.side.aov.interaction, X.RC.side.aov.no.interaction)

#CC
X.CC.side.aov.interaction <- aov(X~CC*HS, data=X.models.norm.side)
X.CC.side.aov.no.interaction <- aov(X~CC+HS, data=X.models.norm.side)
anova(X.CC.side.aov.interaction, X.CC.side.aov.no.interaction)

#MC
X.MC.side.aov.interaction <- aov(X~MC*HS, data=X.models.norm.side)
X.MC.side.aov.no.interaction <- aov(X~MC+HS, data=X.models.norm.side)
anova(X.MC.side.aov.interaction, X.MC.side.aov.no.interaction)

#FM
X.FM.side.aov.interaction <- aov(X~FM*HS, data=X.models.norm.side)
X.FM.side.aov.no.interaction <- aov(X~FM+HS, data=X.models.norm.side)
anova(X.FM.side.aov.interaction, X.FM.side.aov.no.interaction)

#### Does action type (grab or release) have an effect on the model?

## First looking at Y models:
#RC
Y.RC.actiontype.interaction <- aov(Y~RC*AT, data=Y.models.action)
Y.RC.actiontype.no.interaction <- aov(Y~RC+AT, data=Y.models.action)

#CC: Circle Center
Y.CC.actiontype.interaction <- aov(Y~CC*AT, data=Y.models.action)
Y.CC.actiontype.no.interaction <- aov(Y~CC+AT, data=Y.models.action)

#MC
Y.MC.actiontype.interaction <- aov(Y~MC*AT, data=Y.models.action)
Y.MC.actiontype.no.interaction <- aov(Y~MC+AT, data=Y.models.action)

#FM
Y.FM.actiontype.interaction <- aov(Y~FM*AT, data=Y.models.action)
Y.FM.actiontype.no.interaction <- aov(Y~FM+AT, data=Y.models.action)
plot(Y~FM, col=AT, data=Y.models.action)
abline(Y.lm.FM.grab, col="RED")
abline(Y.lm.FM.release, col="BLUE")

#Plots:
xyplot(Y ~ CC | AT, data = Y.models.action,
  panel = function(x, y, ...)
  {
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, ...)
  }
)

## Second looking at X models split by hand side
## LEFT HAND
#RC
X.RC.left.actiontype.interaction <- aov(X~RC*AT, data=X.models.left)
X.RC.left.actiontype.no.interaction <- aov(X~RC+AT, data=X.models.left)

#CC
X.CC.left.actiontype.interaction <- aov(X~CC*AT, data=X.models.left)
X.CC.left.actiontype.no.interaction <- aov(X~CC+AT, data=X.models.left)

#MC
X.MC.left.actiontype.interaction <- aov(X~MC*AT, data=X.models.left)
X.MC.left.actiontype.no.interaction <- aov(X~MC+AT, data=X.models.left)

#FM
X.FM.left.actiontype.interaction <- aov(X~FM*AT, data=X.models.left)
X.FM.left.actiontype.no.interaction <- aov(X~FM+AT, data=X.models.left)

## RIGHT HAND
#RC
X.RC.right.actiontype.interaction <- aov(X~RC*AT, data=X.models.right)
X.RC.right.actiontype.no.interaction <- aov(X~RC+AT, data=X.models.right)

#CC
X.CC.right.actiontype.interaction <- aov(X~CC*AT, data=X.models.right)
X.CC.right.actiontype.no.interaction <- aov(X~CC+AT, data=X.models.right)

#MC
X.MC.right.actiontype.interaction <- aov(X~MC*AT, data=X.models.right)
X.MC.right.actiontype.no.interaction <- aov(X~MC+AT, data=X.models.right)

#FM
X.FM.right.actiontype.interaction <- aov(X~FM*AT, data=X.models.right)
X.FM.right.actiontype.no.interaction <- aov(X~FM+AT, data=X.models.right)
