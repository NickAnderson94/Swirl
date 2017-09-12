# From Introduction to R --------------------------------------------------------------------------------

#subsetting
x <- c(NA,1:20)
x[1:10]
x[is.na(x)]
y <- x[!is.na(x)]
y
y[y>4]
y[y>0]
x[x.0]
x[x>0]
x[!is.na(x) & x > 0]
x[c(3, 5, 7)]
x[0]
x[3000]
x[c(-2, -10)]
x[-c(2, 10)]

#plots
swirl()
swirl(15)
data(cars)
head(cars)
plot(cars)
plot(x = cars$speed, cars$dist)
plot(x = cars$speed, y = cars$dist)
plot(x = cars$speed, y = cars$dist, xlab = "Speed")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance")
plot(x = cars$speed, y = cars$dist, main = "My Plot")
plot(cars, sub = "My Plot Subtitle")
?par
plot(cars, col = 2)
plot(cars, xlim = c(10, 15))
plot(cars, pch = 2)
data("mtcars")
data(mtcars)
boxplot(formula = mpg ~ cyl, data = mtcars)
hist(mtcars$mpg)

#Simulationsample(1:6, 4, replace = TRUE)
sample(1:20, size = 10)
LETTERS
sample(LETTERS)
sample(x = c(0, 1), size = 100, replace = TRUE, prob = c(0.3, 0.7))
flips <- sample(x = c(0, 1), size = 100, replace = TRUE, prob = c(0.3, 0.7))
sum(flips)
rbinom(1, size = 100, prob = 0.7)
rnorm(10)
rnorm(10, mean = 100, sd = 25)
rpois(n = 5, lambda = 10)
my_pois <- replicate(100, rpois(5, 10))
my_pois
cm <- colMeans(my_pois)
hist(cm)