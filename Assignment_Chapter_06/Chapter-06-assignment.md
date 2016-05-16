# Chapter-05-part2-assignment
# Statistical Rethinking Chapter 6 problems

__Name:__Emily Josephs




## 6E1
Information entropy is kind of like the amount of variation in outcomes for a type of event. The motivating criteria are 
1. Continuity -- changing the probability a small amount won't change the entropy that much and vice versa. This makes sense because being close to the correct probability is more accurate than being further away.
2. Entropy should grow as the number of events grow because it is harder to choose among a larger number of outcomes.
3. Additivity -- the order of predictions shouldn't change our measure of entropy.

## 6E2
Entropy = -sum(p x log(p)) 

```r
probs = c(0.7, 0.3)
fentropy = function(p){-p*log(p)}
entropy = sum(fentropy(probs))
entropy
```

```
## [1] 0.6108643
```

```r
x = seq(0,1,by = 0.01)
plot(x, -fentropy(x)-fentropy(1-x), ylab = "entropy", xlab = "freq of heads")
```

![](Chapter-06-assignment_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## 6E3


```r
probs = c(0.2, 0.25, 0.25, 0.3)
entropy = sum(fentropy(probs))
entropy
```

```
## [1] 1.376227
```

## 6M1 
* AIC = training deviance + the number of free parameters. This only is reliable if 1) there are flat priors   (or it doesn't  matter because the likelihood is strong enough that the priors are unimportant) 2) the postior distribution is multivariate normal, and 3) the sample size is much larger than the number of parameters being estimated.

* DIC = Dbar - pD, where D is the average posterior distribution of the deviance and pD is the difference between the average distribution of the deviance and the deviance at the posterior's mean. pD, or the penalty term, is the effective number of parameters. If priors are flat, this is the AIC.

* WAIC = -2(lppd - pWAIC) where lppd is the sum of logs of the average likelihood of each observation in the training sample and pWAIC is the sum of the variances in log-likehood for each observation in the training sample. This is really different than the DIC and AIC because it works with the data point by point.


## 6M5 
Informative priors reduce overfitting because they keep the model from getting "too excited" about the data and just mimicking the data when it constructs the posterior distribution. Providing an informative prior keeps the model from learning too much from the training data set.


## 6M6
Overly informative priors result in underfitting because not enough information from the training dataset is making it into the model.


##  6J1: explore how the code in Code Block 6.16 works.  Explain what is happening in each line.

```r
data(cars)
m <- map(
alist(
dist ~ dnorm(mu,sigma),
mu <- a + b*speed,
a ~ dnorm(0,100),
b ~ dnorm(0,10),
sigma ~ dunif(0,30)
) , data=cars )
post <- extract.samples(m,n=1000)
#the code above fits a model for distance as a function of speed

n_samples <- 1000 #setting the number of samples 
ll <- sapply( 1:n_samples ,  #doing something 1000 times
function(s) { #that something is the function s
mu <- post$a[s] + post$b[s]*cars$speed # setting mu equal to the speed predicted by the intercept (a) and coeff time speed
dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )  #getting the log-likelihood of mu, so that the function s is calculating the log likihoods for each sample from the posterior.
} )
```
##6M3



##6M4


## 6E4


