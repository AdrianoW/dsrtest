###############
### R Day 3 ###
###############

x <- seq(0,10, length.out = 25)
y <- x^2 + rnorm(25,0,10)

plot(x,y)


# approxfun returns a function that is the interpolation of all the points generated above.
f <- approxfun(x,y)

curve(f(x),0,10,n=1000,add=TRUE)

g <- splinefun(x,y)

plot(x,y)
curve(g(x),-5,15,n=1000,add=TRUE


x <- cbind(a=x^2,b=x,c=1)


res <-lm.fit(x,y)
res2 <- lm.fit(x,y)
coef <- res$coefficients
coef2 <- res2$coefficients

plot(x,y, xlim = c(-5,15))
abline(a=coef[2],b=coef[1])
curve(x^2*coef2[1]+x*coef2[1]+coef2[3], add=TRUE)

### ROOT FINING ###

bisection <- function(f,a,b,eps=10^-9,maxiter=100) {
	stopifnot(is.numeric(a))
	stopifnot(is.numeric(b))

	x <- (a+b)/2
	
	for (i in 1:maxiter) {
		if (f(x) < eps) list(root=x,f.root=f(x),iter=i,estim.prec=((a-b)/2))
		else {
			if (f(x)*f(a) >= 0) a <- x
			else b <- x 
		}
	}
	list(root=x,f.root=f(x),iter=i,estim.prec=((a-b)/2))
}

bisection(function(x) x^2-1,a = -0.5, b= 7.81)

uniroot(function(x) x^2-1,c(-0.5, 7.81))



###OPTIM###
x <- seq(0,10, length.out = 25)
y <- x^2 + rnorm(25,0,10)

f <- function(ab) {
	sum((y-ab[1]*x-ab[2])^2)


optim(c(a=1,b=0),f)

ex <- function(x) x^2-1
optim(0,ex^2)
curve(ex(x),-2,2)
abline(h=0)


ex2 <- function(x) ex(x)^2

curve(ex2(x),-2,2)
abline(h=0)
optim(0,ex2)


###K MEANS OPTIM### --- 
 library(FNN)

plot(iris$Sepal.Length, iris$Sepal.Width)

irisset <- cbind(iris$Sepal.Length, iris$Sepal.Width)

km <- kmeans(x=irisset, centers=3, iter.max=100, nstart=3)

km$centers

plot(iris$Sepal.Length, iris$Sepal.Width, col=km$cluster)
points(km$centers,col='red',pch=2,cex=3)


f <- splinefun(c(0:4), c(10,9.5,10,0,10))

curve(f(x),0,4)
optim(0,f)
optim(2,f)

replicate(100, {
	x <- runif(1,0.4)
	optim(x,f)$par
	}) -> res




### Confidence Interval
x <- rnorm(25,pi)

seq(0,10, length.out = 25)
sigma <- 1

mean(x)
mean(x) - qnorm(0.975)*sigma/sqrt(length(x))
mean(x) + qnorm(0.975)*sigma/sqrt(length(x))

res <- replicate(10000, {
	x<- rnorm(25,pi)
	c(mean(x) - qnorm(0.975)*sigma/sqrt(length(x)),mean(x) + qnorm(0.975)*sigma/sqrt(length(x)))
	}
	)

res[,1:5]

mean(pi <= res[2,] & pi >= res[1,]) 

###Hypothesis Testing###

x <- iris$Petal.Length[iris$Species == "setosa"]
y <- iris$Petal.Length[iris$Species == "virginica"]
boxplot(list(x,y))

#hypothesis: Ho: Ut = Uc H1: Ut != Uc#

replicate(10000, {

	mx <- 100
	my <- 100	
	x <- rnorm(25, mx)
	y <- rnorm(25,my)

	(t.test(x,y)$p.value <0.05) #if the result of the t-test is higher than the p-value the
	#null hypothesis is rejected. 
}) -> res

mean(res)

##higher the alpha the higher the probbility we get a type 1 error.  However, the lower the
##probability we get a type 2 error.