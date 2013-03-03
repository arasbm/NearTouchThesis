local({
## Prepare
library(lattice)

## Compute
objects <- list (substitute (data[["combined.data...mass.center.x.9..."]]), substitute (data[["combined.data...mass.center.x.8..."]]), substitute (data[["combined.data...mass.center.x.7..."]]), 
substitute (data[["combined.data...mass.center.x.6..."]]), substitute (data[["combined.data...mass.center.x.5..."]]), substitute (data[["combined.data...mass.center.x.4..."]]), substitute (data[["combined.data...mass.center.x.3..."]]), 
substitute (data[["combined.data...mass.center.x.2..."]]), substitute (data[["combined.data...mass.center.x.11..."]]), substitute (data[["combined.data...mass.center.x.10..."]]), substitute (data[["combined.data...mass.center.x.1..."]]), 
substitute (data[["combined.data...mass.center.x..."]]), substitute (data[["combined.data...Object.or.Target.X..."]]))

# cor requires all objects to be inside the same data.frame.
# Here we construct such a temporary frame from the input variables
data <- data.frame (lapply (objects, function (x) eval (x, envir=globalenv ())))

# calculate correlation matrix
result <- cor (data, use="pairwise.complete.obs", method="pearson")
# calculate matrix of probabilities
result.p <- matrix (nrow = length (data), ncol = length (data))
for (i in 1:length (data)) {
	for (j in i:length (data)) {
		if (i != j) {
			t <- cor.test (data[[i]], data[[j]], method="pearson")
			result.p[i, j] <- t$p.value
			result.p[j, i] <- sum (complete.cases (data[[i]], data[[j]]))
		}
	}
}
## Print result
rk.header ("Correlation Matrix", parameters=list ("Method", "pearson", "Exclusion", "pairwise.complete.obs"))

result <- data.frame (I (sapply (objects, FUN=function (x) rk.get.description (x, is.substitute=TRUE))), as.data.frame (result))
# rk.results (result, titles=c ('Coefficient', sapply (objects, rk.get.short.name)))
rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "rgb")
levelplot(result, main="array correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))


result.p <- data.frame (I (sapply (objects, FUN=function (x) rk.get.description (x, is.substitute=TRUE))), as.data.frame (result.p))
# rk.results (result.p, titles=c ('n \\ p', sapply (objects, rk.get.short.name)))
})

##another corrolation matrix using heatmap
##  View the correlation matrix.

old.digits = getOption( "digits" )
options( digits = 2 )
cor( mass.clean, method = "spearman" )
options( digits = old.digits )

##  Use image() to create the color plot of the correlation matrix.

get( getOption( "device" ) )()

image(
    z    = cor( x = mass.clean, method = "spearman" ),
    axes = FALSE,
    zlim = c( -1.0, 1.0 ) )

axis(
    side     = 1,
    labels   = names( mass.clean ),
    at       = seq( 0, 1, length = length( names( mass.clean ) ) ),
    cex.axis = 0.8 )

axis(
    side     = 2,
    labels   = names( mass.clean ),
    at       = seq( 0, 1, length = length( names( mass.clean ) ) ),
    cex.axis = 0.8 )

box()

#### Third heatmap corrolation matrix method ####
##  Start a default device.

get( getOption( "device" ) )()

##  Create the colors. The blue colors are designed to be lighter than pure
##  blue by adding equal amounts of red and green.

red <- c(
        seq( from = 1.0,  to = 0.0, by = -0.05 ),
        seq( from = 0.05, to = 0.5, length = 20 ) )

green <- red

blue <- c(
        rep( x = 0.0, times = 20 ),
        seq( from = 0.0, to = 1.0, by = 0.05 ) )

colors <- rgb( red = red, green = green, blue = blue )

##  Create the Spearman correlation matrix.

corr <- cor( x = mass.clean, method = "pearson" )

##  Reverse the columns of the matrix so it will be drawn correctly.

n = ncol( corr )
corr2 <- corr[ , n:1 ]

##  Create the image.

image(
    z    = corr2,
    axes = FALSE,
    col  = colors,
    zlim = c( -1.0, 1.0 ) )

##  Add labels for the y axis.

axis(
    side     = 2,
    labels   = colnames( corr2 ),
    at       = seq( 0, 1, length = length( rownames( corr2 ) ) ),
    cex.axis = 0.8,
    las      = 2)

##  Add labels for the x axis, but along the top.

axis(
    side     = 3,
    labels   = rownames( corr2 ),
    at       = seq( 0, 1, length = length( colnames( corr2 ) ) ),
    cex.axis = 0.8,
    las      = 2 )

#### USING PLOT.COR
##  Load the data.
library( package = ISwR )
data( mass.clean )
##  Use plot.cor() to create a color plot of the correlation matrix.
library( package = sma )
##  Open a default device.
##  Adjust the character size to be more readable for the quartz() device for
##  Mac OS X.
get( getOption( "device" ) )()
par( cex = 1.2 )
##  Create the plot.
plot.cor(
    x      = cor( mass.clean, method = "spearman" ),
    new    = FALSE,
    labels = names( mass.clean ),
    zlim   = c( -1.0, 1.0 ) )

############################################################
## Draw a corrolation matrix graph
cor.graph <- function(x) {
	panel.cor <- function(x, y, digits=3, cex.cor, use="pairwise.complete.obs", method="pearson", scale=TRUE) {
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(0, 1, 0, 1))
		r <- abs(cor(x, y, use=use, method=method))
		txt <- format(c(r, 0.123456789), digits=digits)[1]
		if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
	
		test <- cor.test(x,y, use=use, method=method)
		Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
				cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
				symbols = c("***", "**", "*", ".", " "))

		if(scale) text(0.5, 0.5, txt, cex = cex * r)
		else text(0.5, 0.5, txt, cex = cex)
		text(.8, .8, Signif, cex=cex, col=2)
	}

	pairs(x, lower.panel=panel.smooth, upper.panel=panel.cor)
}

################################################
## Scatterplot Matrices from the glus Package 
library(gclus)
dta <- mass.clean # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5, main="Variables Ordered and Colored by Correlation" )

################################################
### Scatterplot Matrices from the car Package
library(car)
attach(mass.clean)
scatterplotMatrix(~mass.clean[["c.remove_outliers.combined.data.mass.center.x.."]]+mass.clean[["c.remove_outliers.combined.data.mass.center.x.1.."]]+mass.clean[["c.remove_outliers.combined.data.mass.center.x.3.."]]+
mass.clean[["c.remove_outliers.combined.data.Object.or.Target.X.."]]+mass.clean[["c.remove_outliers.combined.data.mass.center.x.5.."]], data=mass.clean, main="Test scatterplot matrix")

###############################################
# Plot #3: similar plot using ggplot2
# install.packages("ggplot2") ## uncomment to install ggplot2
library(ggplot2)
plotmatrix(mass.clean)

###############################################
~mass.clean[["c.remove_outliers.combined.data.Object.or.Target.X.."]],mass.clean[["c.remove_outliers.combined.data.mass.center.x.."]],
mass.clean[["c.remove_outliers.combined.data.mass.center.x.1.."]],mass.clean[["c.remove_outliers.combined.data.mass.center.x.2.."]],
mass.clean[["c.remove_outliers.combined.data.mass.center.x.3.."]],mass.clean[["c.remove_outliers.combined.data.mass.center.x.4.."]],
mass.clean[["c.remove_outliers.combined.data.mass.center.x.5.."]],mass.clean[["c.remove_outliers.combined.data.mass.center.x.6.."]],
mass.clean[["c.remove_outliers.combined.data.mass.center.x.7.."]],mass.clean[["c.remove_outliers.combined.data.mass.center.x.8.."]],
mass.clean[["c.remove_outliers.combined.data.mass.center.x.9.."]],mass.clean[["c.remove_outliers.combined.data.mass.center.x.10.."]],
mass.clean[["c.remove_outliers.combined.data.mass.center.x.11.."]]
###############################################

## splom
pl <- splom(mass.clean, data = mass.clean, pscales = 0, varnames = c("Object\nTarget\nX","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12"),auto.key = list(columns = 13, title = "X Coordinate Variables of Mass Center"))

pg <- plotmatrix(mass.clean)
plotmatrix(circle.clean.X)
scatterplotMatrix(circle.clean.Y)
scatterplotMatrix(feat.stddev)

contourplot(data.clean[["participant_number"]] ~ Feature.Mean + Mass.Center )