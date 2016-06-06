# chapter7
em  
June 5, 2016  



##7E1

1) Temperature

2) Field of study

3) Is the car broken?

##7E2
3

##7E3
1) Onion caramelization ~ Bt * temperature + Bw * moisture

2) Car speed ~ Bc * cylinders + Bi * fuel injector goodness

3) Political beliefs ~ Gp * parents' beliefs + Bf * friends' beliefs
Gp = Bp + Bpf * friends' beliefs
The Bpf term specifies whether the friends' beliefs matter or not.

4) Intelligence ~ Bs * socialness + Ba * appendages

##7M1



There is clearly an interaction between temperature, water, and shade.


##7M2


```r
blooms ~ a + bW*water + bS*shade + bWS*water*shade + bWST*temp*water*shade 

When temp is cool, temp=0. When temp is warm, temp = 1 and bWST*water*shade is equal to the sum of the other terms times 1.
```


##7M3

I think the interaction here is that ravens can only survive if wolves are there. So if you have 100 habitats and 50 of them have wolves:


```r
library(rethinking)
```

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
wolves = c(rep(0,50), rep(1,50))
foodquality = rnorm(100)
wolves.c = wolves - mean(wolves)
food.c = foodquality - mean(foodquality)

d = data.frame("wolves.c" = wolves.c, "food.c" = food.c)
d$ravens = d$food.c*(d$wolves.c + 0.5)

mr <- map(
  alist(
    ravens ~ dnorm(mu,sigma),
    mu <- a + bF*food.c + bW*wolves.c,
    a ~ dnorm(0,100),
    bF ~ dnorm(0,100),
    bW ~ dnorm(0,100),
    #bWF ~ dnorm(0,100),
    sigma ~ dunif(0,100)
  ), data=d,  method="Nelder-Mead", control=list(maxit=1e4))

coeftab(mr)
```

```
##       mr     
## a       -0.09
## bF       0.47
## bW      -0.01
## sigma     0.6
## nobs      100
```

```r
precis(mr)
```

```
##        Mean StdDev  5.5% 94.5%
## a     -0.09   0.06 -0.18  0.01
## bF     0.47   0.05  0.39  0.55
## bW    -0.01   0.12 -0.20  0.18
## sigma  0.60   0.04  0.54  0.67
```

I think it could be linear if higher wolf populations increased the amount of food available to the ravens. However, I made it binary in my hypothetical dataset

##7H1

##7H2

##7H3

##7H4



