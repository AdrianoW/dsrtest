x <- seq(0, 10, length.out=25)
y <- x^2 + rnorm(25, 0, 10)

f <- approxfun(x, y)
g <- splinefun(x, y)
f
f(0.5)

?curve
rm(x)
plot(x, y)

curve(f(x), 0, 10, n=1000, add=TRUE, col="red")
curve(g(x), 0, 10, n=1000, add=TRUE, col="blue")

curve(f(x), -5, 15, n=1000, add=FALSE, col="red")
curve(g(x), -5, 15, n=1000, add=TRUE, col="blue")



res <- lm.fit(cbind(a=x, b=1), y)
coef <- res$coefficients

coef
?abline

plot(x, y, xlim=c(-5, 15))
abline(coef["b"], coef["a"])

res <- lm.fit(cbind(a=x^2, b=x, c=1), y)
coef <- res$coefficients
coef
plot(x, y, xlim=c(-10, 25), ylim=c(-5, 150))
curve(x^2*coef[1]+x*coef[2]+coef[3],add=TRUE)

bisection <- function(f, a, b, eps=1e-9, maxiter=100) {


}

bisection(function(x) x^2-1, -0.5, 7.81)


uniroot(function(x) x^2-1, c(-0.5, 7.81))


x
y
f <- function(ab) {
   sum((y-ab[1]*x-ab[2])^2)
}

?optim
optim(c(a=1,b=0), f)
lm.fit(cbind(x,1),y)$coef

ex <- function(x) x^2-1
curve(ex(x), -2, 2)
abline(h=0)
ex2 <- function(x) ex(x)^2

curve(ex2(x), -2, 2)


optim(0, ex2)


iris
plot(iris$Petal.Length, iris$Petal.Width)
?kmeans
km<-kmeans(iris[c('Petal.Length','Petal.Width')],3)
km$centers
plot(iris$Petal.Length, iris$Petal.Width, col=km$cluster)
points(km$centers,col='blue',pch=20,cex=1)

plot(c(0,1,2,3,4), c(10,9.5,10,0,10))
f <- splinefun(c(0,1,2,3,4), c(10,9.5,10,0,10))
curve(f(x),0,4)
optim(1.5,f)

replicate(100, {
   x <- runif(1, 0,4)
   optim(x, f)$value
}) -> res

hist(res, prob=TRUE)
min(res)
