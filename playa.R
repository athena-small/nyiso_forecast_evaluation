## The intersection of two sets can be defined via match():
## Simple version:
## intersect <- function(x, y) y[match(x, y, nomatch = 0)]
intersect # the R function in base, slightly more careful
intersect(1:10, 7:20)

1:10 %in% c(1,3,5,9)
sstr <- c("c","ab","B","bba","c",NA,"@","bla","a","Ba","%")
sstr[sstr %in% c(letters, LETTERS)]

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
(1:10) %w/o% c(3,7,12)

intersect(test[[1]],obs.df[[1]])

x <- ts(rnorm(21*24),f=24)
dow <- rep(rep(1:7,rep(24,7)),3)
business.dummy <- (dow<=5)
seasons <- cycle(x)
seasons[!business.dummy] <- seasons[!business.dummy] + 24
seasons <- factor(seasons,levels=1:48,
                  labels=c(paste("Week",1:24),paste("Weekend",1:24)))
fit <- tslm( x ~ seasons - 1)

hist(rnorm(100, m=5, sd=4))

data <- cbind(rnorm(100),rnorm(100, sd=2), rnorm(100, sd=4))
hist(data)