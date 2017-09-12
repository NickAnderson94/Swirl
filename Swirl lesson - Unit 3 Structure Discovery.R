##############################################################################################################
############################## R Script from Swirl  ##########################################################
###############################      HUDK 4050     ###########################################################
#-------------------------------------------------------------------------------------------------------------

library(swirl)
library(jpeg)
library(fields)
library(dplyr)
library(ggplot2)


# Instructions ------------------------------------------------------------


#| You can exit swirl and return to the R prompt (>) at any time by pressing the Esc key. If you are already at the prompt, type
#| bye() to exit and save your progress. When you exit properly, you'll see a short message letting you know you've done so.

#| When you are at the R prompt (>):
#| -- Typing skip() allows you to skip the current question.
#| -- Typing play() lets you experiment with R on your own; swirl will ignore what you do...
#| -- UNTIL you type nxt() which will regain swirl's attention.
#| -- Typing bye() causes swirl to exit. Your progress will be saved.
#| -- Typing main() returns you to swirl's main menu.
#| -- Typing info() displays these options again.


# 1 Base Plotting System  --------------------------------------------
head(airquality$Ozone,na.rm=TRUE)
range(airquality$Ozone,na.rm=TRUE)
hist(airquality$Ozone)
boxplot(airquality$Month)
table(airquality$Month)
boxplot(Ozone~Month, data = airquality)
boxplot(Ozone~Month, data = airquality, xlab = "Month", ylab = "Ozone (ppb)", col.axis = "blue", col.lab = "red" )
title(main="Ozone and Wind in New York City") #add title
with(airquality, plot(Wind, Ozone))
title(main="Ozone and Wind in New York City")
length(par())
names(par())

#Arguments for plot
par()$pin
par()$pch #plot characters, black circle is pch = 16
par($fg) #forground color
par($bg) #background color
par("pch")
par("xlab")
par()$ylab
par()$mar #margin
par()$oma #outer margin
par()$mfrow #numbner of plots per row
par()$mfcol #number of plots per column

#making plot
pdf("Better plot from base graphics.pdf")
par(mfrow=c(1,1))
plot(airquality$Wind, airquality$Ozone, type = "n")
title(main = "Wind and Ozone in NYC")
may <- subset(airquality, Month==5) #subset may plots
points(may$Wind,may$Ozone,col="blue",pch=17) #May add points to plot
notmay <- subset(airquality, Month!=5) #subset not-may plots
points(notmay$Wind,notmay$Ozone,col="red",pch=8) #May add points to plot
legend("topright",pch=c(17,8),col=c("blue","red"),legend=c("May","Other Months"))
abline(v = median(airquality$Wind), lty = 2, lwd = 2)
dev.of

#Two way plot
pdf("Two plots side by side.pdf")
par(mfrow=c(1,2)) 
plot(airquality$Wind, airquality$Ozone, main = "Ozone and Wind")
plot(airquality$Ozone, airquality$Solar.R, main = "Ozone and Solar Radiation")
dev.off()

pdf('Triple plot with global title.pdf')
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0 , 2, 0)) #set number of graphs and the outter margins
plot(airquality$Wind, airquality$Ozone, main = "Ozone and Wind")
plot(airquality$Solar.R, airquality$Ozone, main = "Ozone and Solar Radiation")
plot(airquality$Temp, airquality$Ozone, main = "Ozone and Temperature")
mtext("Ozone and Weather in New York City", outer = TRUE)
dev.off()

#Other code for saving plots
##> dev.copy(png,'myplot.png')
##> dev.off()
# 2 Clustering Example -----------------------------------------------
sub1 <- subset(ssd, subject == 1)
myplclust(hclustering, lab.col = unclass(sub1$activity))
kClust <- kmeans(sub1[,-c(562, 563)], centers = 6)


# 3 Dimension reduction  ----------------------------------------------------------------------------------------------------
#this shows how to do principal component analysis (PCA) and singular value descomposition (svd)
#these methods don't work well with missing data
head(dataMatrix)
heatmap(dataMatrix)
myedit("addPatt.R")
source("addPatt.R", local=TRUE)
#svd
#Two related solutions to these problems are PCA which stands for Principal Component Analysis and SVD, Singular
#Value Decomposition. This latter simply means that we express a matrix X of observations (rows) and variables
#(columns) as the product of 3 other matrices, i.e., X=UDV^t. This last term (V^t) represents the transpose of the
#matrix V.

#Here U and V each have orthogonal (uncorrelated) columns. U's columns are the left singular vectors of X and V's
#columns are the right singular vectors of X.  D is a diagonal matrix, by which we mean that all of its entries
#not on the diagonal are 0. The diagonal entries of D are the singular values of X.


#PCA
#a simple, non-parametric method for extracting relevant information from confusing data sets.
svd(scale(mat)) 
prcomp(scale(mat)) #get the principal components of mat
#Notice that the principal components of the scaled matrix, shown in the Rotation component of the prcomp output,
# ARE the columns of V, the right singular values. Thus, PCA of a scaled matrix yields the V matrix (right singular vectors) of the same scaled matrix.

#Why were the first columns of both the U and V matrices so special?  Well as it happens, the D matrix of the SVD
#explains this phenomenon. It is an aspect of SVD called variance explained. Recall that D is the diagonal matrix
#sandwiched in between U and V^t in the SVD representation of the data matrix. The diagonal entries of D are like
#weights for the U and V columns accounting for the variation in the data. They're given in decreasing order from
#highest to lowest. Look at these diagonal entries now. Recall that they're stored in svd1$d.

#very difficult to see patterns in the data looking at it normally, heatmaps help with this. Is the only help
#of heatmaps is to identify patterns in matricies?
#to get the proportion of variance explained by each principal component, you add up the eigenvalues, and then
#divide the element by the total variation that exists. 

svd1$d #like the weights of variance explained of the U and V columns

#Clustering straight to the point, the dataframe and # of desired clusters
kmeans(dataFrame, centers = 3)
plot(x, y, col = kmObj$cluster, pch = 19, cex = 2) #their example 
points(kmObj$centers, col = c("black", "red", "green"), pch = 3, cex = 3, lwd = 3)
plot(x, y, col = kmeans(dataFrame, 6)$cluster, pch = 19, cex = 2)



# 4 Exploratory Graphics ----------------------------------------------------------------------------------
head(pollution)
dim(pollution)
summary(pollution$pm25)
quantile(ppm)
boxplot(ppm, col = )
boxplot(ppm, col = "blue")
abline(h = 12)
hist(ppm, col = "green")
rug(ppm)
low
high
hist(ppm, col = "green", breaks = 100)
rug(ppm)
hist(ppm, col = "green")
abline(v = 12, lwd = 2)
abline(v = median(ppm), col = "magenta", lwd = 4)
reg <- table(pollution$region)
reg
barplot(reg, col = "wheat", main = "Number of Countries in Each Region")
barplot(reg, col = "wheat", main = "Number of Counties in Each Region")

#see the pollution data as a function of region
boxplot(pm25 ~ region, data = pollution, col = "red")
par(mfrow=c(2,1),mar=c(4,4,2,1))
subset(pollution, region == "east")
east <- subset(pollution, region == "east")
hist(east$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

#color scatter plot by groupping variable
plot(pollution$latitude, ppm, col = pollution$region)
abline(h =12, lwd =2, lty = 2)


# 5 Graphics Devices in R ---------------------------------------------------------------------------------

#how to save basic r plots
?Devices #see which graphics devices are available on your computer
with(faithful, plot(eruptions, waiting))
?with
title(main = "Old Faithful Geyser data")
dev.cur() #see current plotting device

pdf(file = "myplot.pdf")
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.off()

dev.cur()
dev.off()

with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.copy(png, file = "geyserplot.png")
dev.off()



# 6 Hierarchical Clustering -----------------------------------------------------------------------------
#Clustering organizes data points that are close into groups.
#Hierarchical clustering is an agglomerative, or bottom-up, approach. From Wikipedia
#(http://en.wikipedia.org/wiki/Hierarchical_clustering), we learn that in this method, "each observation starts in
#its own cluster, and pairs of clusters are merged as one moves up the hierarchy." This means that we'll find the
#closest two points and put them together in one cluster, then find the next closest pair in the updated picture,
#and so forth. We'll repeat this process until we reach a reasonable stopping place.

#Note the word "reasonable". There's a lot of flexibility in this field and how you perform your analysis depends
#on your problem. Again, Wikipedia tells us, "one can decide to stop clustering either when the clusters are too
#far apart to be merged (distance criterion) or when there is a sufficiently small number of clusters (number
#criterion)."

#First, how do we define close? This is the most important step and there are several possibilities depending on
#the questions you're trying to answer and the data you have. Distance or similarity are usually the metrics used.

#Euclidean distance and correlation similarity are continuous measures, while Manhattan distance is a binary measure.

#Euclidean distance is what you learned about in high school algebra. Given two points on a plane, (x1,y1) and
#(x2,y2), the Euclidean distance is the square root of the sums of the squares of the distances between the two
#x-coordinates (x1-x2) and the two y-coordinates (y1-y2). You probably recognize this as an application of the
#Pythagorean theorem which yields the length of the hypotenuse of a right triangle.

#More formally, Manhattan distance is the sum of the absolute values of the distances between each coordinate,
#so the distance between the points (x1,y1) and (x2,y2) is |x1-x2|+|y1-y2|. As with Euclidean distance, this too
#generalizes to more than 2 dimensions.

dist(dataFrame)
hc <- hclust(distxy)
plot(hc) #create cluster dendrogram
plot(as.dendrogram(hc))
abline(h = 1.5, col = "blue")
abline(h = .4, col = "red")

#We see that this blue line intersects 3 vertical lines and this tells us that using the distance 1.5
#(unspecified units) gives us 3 clusters (1 through 4), (9 through 12), and (5 through 8). We call this a "cut"
#of our dendrogram. Now cut the dendrogam by drawing a red horizontal line at .4.

#Heat maps
#| (http://en.wikipedia.org/wiki/Heat_map) tells us a heat map is "a graphical representation of data where the
#| individual values contained in a matrix are represented as colors. ... Heat maps originated in 2D displays of
#| the values in a data matrix. Larger values were represented by small dark gray or black squares (pixels) and
#| smaller values by lighter squares."

# Tutorial on creating heat maps
http://sebastianraschka.com/Articles/heatmaps_in_r.html#clustering
heatmap(dataMatrix, col = cm.colors(25))
heatmap(mt)




# 7 K-means clustering ----------------------------------------------------------------------------------

#the k-means method "aims to partition the points into k groups such that the sum of
#squares from points to the assigned cluster centres is minimized."

#k-means is a partioning approach which requires that you first guess how many clusters you have (or
#want). Once you fix this number, you randomly create a "centroid" (a phantom point) for each cluster and assign
#each point or observation in your dataset to the centroid to which it is closest. Once each point is assigned a
#centroid, you readjust the centroid's position by making it the average of the points assigned to it.

#So k-means clustering requires some distance metric (say Euclidean), a hypothesized fixed number of clusters, and
#an initial guess as to cluster centroids.



# 8 Principles of Analytic Graphs -----------------------------------------------------------------------

#When it comes to visualizations, you can use two graphs side by side to explain theoretical change, like the
#third air one shows. By showing the two sets of boxplots side by side you're explaining your theory of why the 
#air cleaner increases the number of symptom-free days.
#We first showed a comparision (sick days between control and treatment) and then the mechanism
#a third principal is to show multivariate data

#the pollution and mortality graph is deception - simpson's paradox, the trend appears broken becuase
#group means are not accounted for

