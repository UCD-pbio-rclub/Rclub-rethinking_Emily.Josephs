# Statistical Rethinking Chapter 2, sections 2.1 - 2.3

Name:

## 2E1
(2) Pr(rain|Monday)

## 2E2
(3) The probability that it is Monday, given that it is raining.


## 2E3
(1) Pr(Monday|rain)

## 2E4
It means that when we toss the globe, 0.7 times we will get water under our finger. It doesn't mean that the globe is 0.7 water, although I think we can infer that from the probability statement but we don't actully know.


## 2M3



```r
setwd("~/Documents/Rclub-rethinking_Emily.Josephs/Assignment_2016_02_11/")
PriorEarth = 0.5
PriorMars = 0.5
ProbLandEarth = 0.3 #the probability of getting land if you have earth
ProbLandMars = 1 #the prob of getting land if you have mars
LikelihoodEarth = dbinom(1, size=1, prob = ProbLandEarth)
LikelihoodMars = dbinom(1, size=1, prob = ProbLandMars)
AverageLikelihood = PriorEarth * LikelihoodEarth + PriorMars * LikelihoodMars
ProbEarthLand = (ProbLandEarth * PriorEarth)/AverageLikelihood
ProbEarthLand = 0.23
```


## 2M4

```r
bb = 2 #two ways for all black card to produce observation
bw = 1 #one way for the black + white card to produce observation
ww = 0 #the all white card could not produce observation
sumways = bb+bw+ww
plausbb = bb/sumways #plausibility of the card being all black is 0.666
plausbw = bw/sumways #plausibility of the card being black and while is 0.3333
plausww = ww/sumways #plausibility of the card being all white is 0
```


##2M5

```r
bb = 4 #there are now 4 ways that a black/black card could give you black side (each side of each card counts as one way)
bw = 1
ww = 0 
ways = c(bb,bw,ww)
sumWays = sum(ways)
plaus = ways/sumWays
probB = plaus[1] 
probB #probability that other side is black is 0.8
```

```
## [1] 0.8
```


##2M6

```r
oldways = c(2,1,0) #the counts for 2M3
newways = oldways*c(1,2,3) #weighting the counts
plaus = newways/sum(newways)
probB = plaus[1]
probB
```

```
## [1] 0.5
```

##2M7

```r
bb = 6 #first card is bb and second card is ww has 4 ways (b1w1, b1w2, b2w1, b2w2) + second card is bw gives 2 ways (b1w, b2w)
bw = 2 #first card is bw and second card is ww (bw1, bw2)
ww = 0
ways = c(bb,bw,ww)
plaus = ways/sum(ways)
probB = plaus[1]
probB
```

```
## [1] 0.75
```

##2H1

```r
priorPanda = c(0.5, 0.5) #each species is equally common
LikelihoodTwins = c(dbinom(1, size = 1, prob = 0.1), dbinom(1, size = 1, prob = 0.2)) #the likelihoods of each panda species having twins
AverageLikelihood = sum(priorPanda*LikelihoodTwins) #the average likelihood of having twins given an equal chance of being each species
PosteriorPanda = (LikelihoodTwins*priorPanda)/AverageLikelihood #bayes theorem
PosteriorPanda 
```

```
## [1] 0.3333333 0.6666667
```

```r
#so there's a 33% chance of being Panda 1 and a 66% chance of being Panda 2
ProbTwins = sum(PosteriorPanda*LikelihoodTwins) #The probability of having twins is just the probability of being each species * the probability of that species having twins
ProbTwins
```

```
## [1] 0.1666667
```

##2H2

```r
PosteriorPanda[1] #the probability of being species A
```

```
## [1] 0.3333333
```


##2H3

```r
PriorPanda = PosteriorPanda #from problem 2H1
LikelihoodSingleton = c(0.9, 0.8) #the likelihood of each species only giving birth to one
AverageLikelihoodSingleton = sum(PriorPanda*LikelihoodSingleton)
PosteriorPanda = (LikelihoodSingleton*PriorPanda)/AverageLikelihoodSingleton
PosteriorPanda[1] #the likelihood of being species A
```

```
## [1] 0.36
```

```r
#could we have done this all in one step?=
PriorPanda = c(0.5,0.5)
LikelihoodPanda = c(dbinom(1, size=2, prob=0.1), dbinom(1, size=2, prob=0.2))
AverageLikelihood = sum(PriorPanda*LikelihoodPanda)
PosteriorPandaAll = (LikelihoodPanda*priorPanda)/AverageLikelihood #bayes theorem
PosteriorPandaAll[1]
```

```
## [1] 0.36
```

##2H4

```r
#test only
PriorPanda = c(0.5, 0.5)
LikelihoodTest = c(0.8, 0.2) #this is explained vaguely in the text
AvgTest = sum(PriorPanda*LikelihoodTest)
PosteriorPandaTest = (PriorPanda*LikelihoodTest)/AvgTest
PosteriorPandaTest[1]
```

```
## [1] 0.8
```

```r
#test and births
PriorPanda = PosteriorPanda
PosteriorPandaAll = (PriorPanda*LikelihoodTest)/AvgTest
PosteriorPandaAll[1]
```

```
## [1] 0.576
```
