wc1 <- function(wrd) {
   tab <- new.env()
   for (w in wrd) {
      if (is.null(tab[[w]]))
         tab[[w]] <- 1
      else
         tab[[w]] <- tab[[w]]+1
   }

   tab <- as.list(tab)
#   data.frame(w=names(tab), c=unlist(tab))
}


wc2 <- function(wrd) {
   tab <- list()
   for (w in wrd) {
      if (is.null(tab[[w]]))
         tab[[w]] <- 1
      else
         tab[[w]] <- tab[[w]]+1
   }

   tab <- as.list(tab)
#   data.frame(w=names(tab), c=unlist(tab))
}


file <- readLines("~/R//powrot-taty-utf8.txt")
wrd <- stri_trans_tolower(unlist(
   stri_extract_words(file)
))
library("stringi")

wrd <- rep(wrd, 100)
wrd <- stri_rand_strings(10000, 10)
microbenchmark::microbenchmark(wc1(wrd), wc2(wrd), times = 10)

file <- readLines("~/R//powrot-taty-utf8.txt")


wrd <- c("dog", "cat", "dog", "elephant", "dog")


base::c
library(stringi)
library(magrittr)
search()
get("%>%")
stringi::%>%

   library(Matrix)
library(igraph)

"if"(TRUE, {x <- 7; print(x)}, 2+2)

iris2 <- iris
iris2$Species <- as.numeric(iris2$Species)
x <- names(iris2)[-ncol(iris)]
y <- names(iris2)[ncol(iris)]
co <- combn(x, 2)
for (j in 1:ncol(co)) {
   f <- (as.formula(call("~", as.name(y), call("+", as.name(co[1,j]), as.name(co[2,j])))))
   r <- t <- unlist(summary(lm(f, data=iris2)))$r.squared

   print(f)
   print(r)
}


distribs <- c("rnorm", "runif", "rexp", "rpois")
for (f in distribs)
   print(do.call(f, list(5, 0.5)))



#############################################################

extractURLs <- function(x) {
   stopifnot(is.character(x), length(x) == 1)
   txt <- readLines(x)
   res <- unlist(stri_extract_all_regex(txt,
      "(?i)(https?):\\/\\/(www\\.)?[a-z0-9.:/-]*"))
   res[!is.na(res)]
}

extractURLs("http://www.datascienceretreat.com")




##############################

library(data.table)
irisdt <- iris
irisdt$id1  <- paste0(irisdt$Species, rep(1:50, times=3))
irisdt$id2a <- as.character(irisdt$Species)
irisdt$id2b <- rep(1:50, times=3)
head(irisdt)
tail(irisdt)

irisdt <- irisdt[sample(1:nrow(irisdt)),]
irisdt <- data.table(irisdt, key="id1")
head(irisdt)


?setkeyv
irisdt

typeof(irisdt)
mode(irisdt)
class(irisdt)
unclass(irisdt)


irisdt[c(1,10,150),]
irisdt[rep(c(TRUE,FALSE,FALSE,FALSE,FALSE), len=150),]
get("[.data.frame")

getS3method("[", "data.table")



lapply(list(1:2,3:4,5:6), mean)
lapply(data.table(as.data.frame(list(1:2,3:4,5:6))), mean)

microbenchmark::microbenchmark(
irisdt["virginica50"],
irisdt[id1=="virginica50"],
irisdt[irisdt$id1=="virginica50"])

example(data.table)

