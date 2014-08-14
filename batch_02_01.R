2+2

# ufuugshrghdrugrdugeshkgsuhgdsgs

?numeric
??"jelly"

TRUE
FALSE

TRUE
length(TRUE)
typeof(TRUE)
mode(TRUE)
class(TRUE)


c(TRUE)
c(TRUE, FALSE)

rep(TRUE, 5)
rep(c(TRUE, FALSE), 5)
rep(x=c(TRUE, FALSE), times=5)
rep(times=5, x=c(TRUE, FALSE))
rep(times=5, c(TRUE, FALSE))

rep(c(TRUE, FALSE), lenght.out=5)
rep(c(TRUE, FALSE), l=5)
rep(c(TRUE, FALSE), each=5)

rep(c(TRUE, FALSE), each=5)
rep(c(TRUE, FALSE), times=2, each=5)

?rep

###  numeric vector
###  integer, double
###

10
length(10)
mode(10)
typeof(10)
class(10)

1e34
2.2e3
-2.1e-2

1e34+1e-34-1e34-1e-34

0.1+0.1+0.1 == 0.3
print(0.1+0.1+0.1)
print(0.3)

print(0.1+0.1+0.1, digits=22)
print(0.3, digits=22)

1L
mode(1L)
typeof(1L)


c(1, 2, 3)
rep(c(1,2,3), 10)

1:10
10:1
seq(0, 10, 2)
?seq


seq(0, 1, len=5)

# character vectors

"a string"
'a string'
length('a string')
typeof('a string')
c('s1', 's2')

# special characters


'Alicia\'s dog'
"I said \"No!\""
cat("I said \"No!\"")

cat("I am \n happy")
print("I am \n happy")

?Quotes


# type hierarchy

c(TRUE, 1L, 1.0, "one")
c(TRUE, 1L, 1.0)
typeof(c(TRUE, 1L, 1.0))
typeof(c(TRUE, 1L))


as.double(TRUE)
as.numeric(FALSE)
is.numeric(TRUE)
is.logical(TRUE)
as.logical(0)
as.logical(-3.2)



# special values

NA

typeof(NA)
c(1,2,3,NA)
c("one", NA, "three")
NA_character_
NA_integer_


sqrt(-1)
1/0
Inf/(-Inf)


is.na(c(1, 2, NA, 4, NA))
is.finite(c(1, NA, Inf, NaN, 2))
# creating named variables

x <- 10
x*2

10 -> x
x = 10



## NULL
NULL
typeof(NULL)

x <- cat("123")
x


numeric(10)
logical(4)
character(4)
vector("integer", 3)

(2*pi)^(-0.5)*exp(-x^2/2)
1/sqrt(2*pi)*exp(-x^2/2)
exp(-x^2*0.5)/sqrt(2*pi)



as.character(structure(as.integer(sign(x)+2),
   class="factor",
   levels=c("n", "z", "p")))

ifelse(x < 0, "n", ifelse(x > 0, "p", "z"))

y <- rep("z", length(x))
y[x < 0] <- "n"
y[x > 0] <- "p"
y


x <- c(10, 20, 30, 10, 40)

order(x)
order(order(x))

sort(x)



F <- function(n) {

#    stopifnot(is.numeric(n))
   n <- as.numeric(n)
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

source("~/Dydaktyka/specjalne/dsr_berlin/batch_02_02.R")




x <- list("test", c("a", "b", "c"), "ddd")
"testabcddd"
paste

paste(unlist(x), collapse="")


lapply(list(mean, sd, median), do.call, list(c(1,2,3,4)))

x

mode <- function(x) {
   tmp <- rle(sort(x))
   tmp$values[tmp$lengths == max(tmp$lengths)]
}


mode(c(0,1,2,3,1,2,3,1,2,3))
