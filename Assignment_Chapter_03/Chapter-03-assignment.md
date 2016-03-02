# Statistical Rethinking Chapter 3 problems

__Name__: Em 

## 3E1

```
## Loading required package: rstan
```

```
## Loading required package: ggplot2
```

```
## rstan (Version 2.9.0-3, packaged: 2016-02-11 15:54:41 UTC, GitRev: 05c3d0058b6a)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.58)
```


```r
sum(samples<0.2)/length(samples) #the proportion of samples < 0.2 
```

```
## [1] 5e-04
```
## 3E2


```r
sum(samples>0.8)/length(samples) #the proprtion of samples >0.8
```

```
## [1] 0.1117
```

## 3E3

```r
sum(samples<0.8&samples>0.2)/length(samples) #the proprtion of samples <0.8 and > 0.2
```

```
## [1] 0.8878
```

## 3E4

```r
quantile(samples, 0.2) #20% of the samples lie below
```

```
##       20% 
## 0.5195195
```

```r
hist(samples, border = "white", col="lightgray")
abline(v=quantile(samples,0.2), col = "darkred", lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-5-1.png)


## 3E5

```r
quantile(samples, 0.8) #80% of the samples lie above
```

```
##       80% 
## 0.7567568
```

```r
hist(samples, border = "white", col="lightgray")
abline(v=quantile(samples,0.8), col = "darkred", lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-6-1.png)


## 3E6

```r
HPDI(samples, prob=0.66)
```

```
##     |0.66     0.66| 
## 0.5205205 0.7847848
```

```r
hist(samples, border = "white", col="lightgray")
abline(v=HPDI(samples, prob=0.66)[1], col = "darkred", lwd=2)
abline(v=HPDI(samples, prob=0.66)[2], col = "darkred", lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-7-1.png)

## 3E7

```r
PI(samples, prob=0.66)
```

```
##       17%       83% 
## 0.5005005 0.7687688
```

```r
hist(samples, border = "white", col="lightgray")
abline(v=PI(samples, prob=0.66)[1], col = "darkred", lwd=2)
abline(v=PI(samples, prob=0.66)[2], col = "darkred", lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-8-1.png)

## 3M1

```r
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
```


## 3M2

```r
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
hist(samples, col="lightgray", border="white")
HPDI(samples, prob=0.9)
```

```
##      |0.9      0.9| 
## 0.3243243 0.7157157
```

```r
abline(v=HPDI(samples, prob=0.9)[1], col = "darkred", lwd=2)
abline(v=HPDI(samples, prob=0.9)[2], col = "darkred", lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-10-1.png)

## 3M3

```r
dummy_w <- rbinom(1e5, size=15, prob=samples)
table(dummy_w)[9]/1e5
```

```
##       8 
## 0.14628
```


## 3M4

```r
dummy_prob = dummy_w/1e5
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1,1000) #flat prior
likelihood <- dbinom(6, size=9, prob=dummy_prob)
posterior = likelihood*prior
```

## 3M5

```r
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- c(rep( 0 , 500 ), rep(1,500))
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples2 <- sample( p_grid , prob=posterior , size=1e5 , replace=TRUE )
hist(samples2, xlim=c(0,1), col="darkgray", border="white")
HPDI(samples2, prob=0.9)
```

```
##      |0.9      0.9| 
## 0.5005005 0.7117117
```

```r
abline(v=HPDI(samples2, prob=0.9), col="darkred", lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-13-1.png)

```r
dummy_w2 <- rbinom(1e5, size=15, prob=samples2)
table(dummy_w2)[7]/1e5
```

```
##       6 
## 0.06916
```

```r
hist(dummy_w2, col="darkgray", border="white", xlab="number of water samples")
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-13-2.png)

```r
#checking it at 0.7???
```


## 3H1

```r
data(homeworkch3)
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
probbirth1 = sum(birth1)/length(birth1)
probbirth2 = sum(birth2)/length(birth2)
probboy = probbirth1/2 + probbirth2/2

likelihood <- dbinom( 111 , size=200 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
loss <- sapply(p_grid, function(d) sum(posterior*abs(d-p_grid)))
p_grid[which.min(loss)]
```

```
## [1] 0.5545546
```


## 3H2

```r
samples <- sample(p_grid, size=1e4, replace=TRUE, prob = posterior)
HPDI(samples,prob=0.5)
```

```
##      |0.5      0.5| 
## 0.5275275 0.5735736
```

```r
HPDI(samples,prob=0.89)
```

```
##     |0.89     0.89| 
## 0.4974975 0.6076076
```

```r
HPDI(samples,prob=0.97)
```

```
##     |0.97     0.97| 
## 0.4774775 0.6276276
```
## 3H3

```r
reps = rbinom(1e4, 200, prob=samples) 
dens(reps)
abline(v=111, col="darkred", lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-16-1.png)

## 3H4

```r
sum(birth1)
```

```
## [1] 51
```

```r
reps = rbinom(1e4, 100, prob=samples)
dens(reps)
abline(v=sum(birth1), col="darkred",lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-17-1.png)
The model looks bad now

## 3H5

```r
girlsfirst = birth2[which(birth1 ==0)]
BafterG = sum(girlsfirst)
reps = rbinom(1e4, 49, prob=samples)
dens(reps)
abline(v=BafterG, col="darkred", lwd=2)
```

![](Chapter-03-assignment_files/figure-html/unnamed-chunk-18-1.png)

```r
table(reps)[29]/1e4
```

```
##    40 
## 3e-04
```
It looks like the two births are not independent
