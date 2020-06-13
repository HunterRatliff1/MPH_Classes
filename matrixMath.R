
library(UsingR)
require(tidyverse)
data("father.son",package="UsingR")

father.son %>% glimpse()
father.son$sheight %>% mean()

father.son %>%
  mutate(fheight = round(fheight)) %>%
  filter(fheight==71) %>%
  .$sheight %>% mean()

rm(father.son)
# HW 2   #######################################################################
matrix(1:1000,100,10)[25,3]
cbind(1*1:10, 2*1:10, 3*1:10, 4*1:10, 5*1:10)[7,] %>% sum()

# Matrix math ##################################################################

# Given matrix below
x <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
x

# can multiple by scalar (a=3)
a <- 3
a*x

# Can multiply by matrix too
beta <- c(3,2,1)
x %*% beta # multiply col1 by 3, col2 by 2, col3 by 1
rm(x,beta)


################################################################################
# 3a + 4b - 5c +  d = 10
# 2a + 2b + 2c -  d = 5
# a    -b + 5c - 5d = 7
# 5a +            d = 4

X <- matrix(c(3,2,1,5,
              4,2,-1,0,
              -5,2,5,0,
              1,-1,-5,1),4,4)
y <- matrix(c(10,5,7,4),4,1)
solve(X )%*% y #equivalent to solve(X,y)
rm(X,y)



# Question 3 & 4
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)

# Matrix product
a %*% b
sum(a[3,] * b[,2])
rm(a,b)

################################################################################
# Week 2 HW
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
beta <- c(5, 2)
X

X %*% beta

# # #
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
beta <- c(10,3,-3)
X

X %*% beta
rm(X, beta)


################################################################################
set.seed(1)
g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n)
y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)

X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)


mc <- function(x) {
  g = 9.8 ## meters per second
  h0 = 56.67
  v0 = 0
  n = 25
  tt = seq(0,3.4,len=n)
  y= h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)
  
  X = cbind(1,tt,tt^2)
  A = solve(crossprod(X))%*%t(X)
  
  betahat <- -2 * (A %*% y) [3,]
  as.matrix(betahat)
  
}
sapply(1:100000, FUN = mc) %>% sd()
replicate(10, mc)


# Part 2

part2 <- function(x){
  library(UsingR)
  x = father.son$fheight
  y = father.son$sheight
  n = length(y)
  
  
  N =  50
  index = sample(n,N)
  sampledat = father.son[index,]
  x = sampledat$fheight
  y = sampledat$sheight
  betahat =  lm(y~x)$coef[1]
  betahat
}

part2(1)
sapply(1:10000, FUN = part2) %>% sd()

sapply(1:3, part2)

cov(father.son$fheight, father.son$sheight)


################################################################################

library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50
set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef

fit = lm(y ~ x)
fit$residuals # same as: y - fit$fitted.values
str(fit)
SSR <- sum((fit$residuals)^2)
SSR # answer

X = cbind(rep(1,N), x)
solve(t(X) %*% X)

# take diagonals
# Find sigma^2
sigma2 = SSR / 48


# Diags
diag(solve(t(X) %*% X))

# Find estimated variance of beta-hat (multiply our estimate of  𝜎2  and the diagonalsx
sigma2 * diag(solve(t(X) %*% X))

sqrt(sigma2 * diag(solve(t(X) %*% X)))


################################################################################
dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
options("contrasts")
model.matrix(~ a + b, dd)

Nx <- 5
Ny <- 7

X <- cbind(rep(1,Nx + Ny),rep(c(0,1),c(Nx, Ny)))
t(X) %*% X



# color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))
# n <- 1000
# X <- sample(ifelse(color == "Red", -1, 1),  n, replace = TRUE)
# X[1:10]
# X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
# X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
# S <- sum(X)
# S
n <- 1000
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
  sum(X)
}
S <- replicate(B, roulette_winnings(n))
mean(S<0)


################################################################################
spider <- paste0("https://raw.githubusercontent.com/genomicsclass/dagdata/master/",
       "inst/extdata/spider_wolff_gorb_2013.csv") %>%
  read_csv(skip=1)
# spider.sub <- spider[spider$leg == "L1",]
# split(spider.sub$friction, spider.sub$type)


species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))
model.matrix(~ species + condition)

fitTL <- lm(friction ~ type + leg, data=spider)
contrast::contrast(fitTL, list(leg="L4",type="pull"),list(leg="L2",type="pull"))$X


spider$log2friction <- log2(spider$friction)
boxplot(log2friction ~ type*leg, data=spider)
