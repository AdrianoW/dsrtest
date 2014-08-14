
F <- function(n) {

#    stopifnot(is.numeric(n))
   n <- as.numeric(n)
   n <- n+1
   n <- n-1
   n <- floor(n)
   stopifnot(is.na(n) | n >= 0)

   unlist(lapply(n, function(ni) {
      if (is.na(ni))
         return(NA)
      else if (ni == 0 | ni == 1)
         return(1)
      else
         return(F(ni-1) + F(ni-2))
   }))
}



rnorm2 <- function(x) {
   U1 <- runif(ceiling(x/2))
   U2 <- runif(ceiling(x/2))
   Z1 <- sqrt(-2*log(U1))*cos(2*pi*U2)
   Z2 <- sqrt(-2*log(U1))*sin(2*pi*U2)
   c(Z1, Z2)[1:x]
}

hist(rnorm2(10000))





