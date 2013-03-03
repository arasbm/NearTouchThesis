## to create a 2X2 layout we can do this before running 4 plot()
par(mfrow=c(2,2))

## Layout for automatically creating four important plots for data
layout(matrix(c(1,2,3,4), nrow=2, byrow=T))
plot(mydata)