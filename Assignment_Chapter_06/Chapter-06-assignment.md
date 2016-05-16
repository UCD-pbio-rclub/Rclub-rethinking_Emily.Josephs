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

## 6E4


