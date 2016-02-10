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

PriorEarth = 0.5

PriorMars = 0.5

ProbLandEarth = 0.3 #the probability of getting land if you have earth

ProbLandMars = 1 #the prob of getting land if you have mars

LikelihoodEarth = dbinom(1, size=1, prob = ProbLandEarth)

LikelihoodMars = dbinom(1, size=1, prob = ProbLandMars)

AverageLikelihood = PriorEarth * LikelihoodEarth + PriorMars * LikelihoodMars

ProbEarthLand = (ProbLandEarth * PriorEarth)/AverageLikelihood

ProbEarthLand = 0.23

## 2M4

bb = 2 #two ways for all black card to produce observation

bw = 1 #one way for the black + white card to produce observation

ww = 0 #the all white card could not produce observation

sumWays = bb+bw+ww

plausbb = bb/sumways #plausibility of the card being all black is 0.666

plausbw = bw/sumways #plausibility of the card being black and while is 0.3333

plausww = ww/sumways #plausibility of the card being all white is 0

