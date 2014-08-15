stri_extract_all_regex(stri_flatten(c(".abc; de214; pi; .; ..",
".2way; 235")), "(\\p{L}|\\p{N}|\\.|_)+")
stri_extract_all_regex(stri_flatten(c(".abc; de214; pi; .; ..",
".2way; 235")), "[\\p{L}\\.][\\p{L}\\p{N}\\._]*")
stri_extract_all_regex(stri_flatten(c(".abc; de214; pi; .; ..",
".2way; 235")), "\\p{L}[\\p{L}\\p{N}\\._]*|\\.[\\p{L}\\._][\\p{L}\\p{N}\\._]*|\\.")

stri_extract_all_regex("12.123, -53, +1e-9, -1.2423e10, .2, 4. aa3.14bb",
"\\b(([-+]?\\d+(\\.\\d*)?|[-+]?\\.\\d+)(e[-+]?\\d+)?)\\b")


res <- stri_extract_all_regex(readLines("http://en.wikipedia.org/wiki/2014_FIFA_World_Cup"),
   "\\d+-\\d+")
res[!is.na(res)]


stri_match_all_regex("...\n<p>This is para 1.</p>\n...\n<p>This is\npara 2.</p>...",
   "(?s)<p>(.*?)</p>")[[1]][2,]



adulttest <- function(x) {
   xd <- as.Date(strptime(x, "%Y-%m-%d"))
   y <- as.integer(strftime(xd, "%Y"))
   m <- as.integer(strftime(xd, "%m"))
   d <- as.integer(strftime(xd, "%d"))
   a <- (Sys.Date()-xd > 365.25*18)
#    a <- 1-1/(1+exp(as.double(-(xd - Sys.Date()+365.25*18))))
   data.frame(
      year=y, month=m, day=d, adult=a
   )
}
adulttest(c("1979-01-12", "1982-10-09", "2011-12-31", "1996-08-14"))


# ..............................................
today <- Sys.Date()
start <- today-30
baseurl <- "http://cran-logs.rstudio.com/2014/"
outdir <- tempdir() # "~/test/dsr"
setwd(outdir)
if (!file.exists(outdir))
   dir.create(outdir)

for (d in as.character(seq(start, today, by=1))) {
   u <- stri_paste(baseurl, d, ".csv.gz")
   o1 <- stri_paste(outdir, "/", d, ".csv.gz")
   o2 <- stri_paste(outdir, "/", d, ".csv")
   if (!file.exists(o1) && !file.exists(o2))
      download.file(u, o1)
   if (!file.exists(o2))
      if (0 != system2("gunzip", o1)) {
         message("error downloading ", o1)
         file.remove(o1)
      }
}


wc <- function(fname) {
   file <- readLines(fname)
   wrd <- stri_trans_tolower(unlist(stri_extract_words(file)))
   t <- table(wrd)
   d <- data.frame(words=names(t),
      counts=as.integer(t))
   d[order(d$counts,decreasing=TRUE),]
}

wc("~/R//powrot-taty-utf8.txt")


