---
title: "VAM"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Creating the data set in the example.  Suggested changing the factor variables in factors and changing all variable names to lower case.
```{r}
dep = rnorm(50000)
time = rep(1:10, 5000)
female = as.factor(c(rep(1,25000), rep(0,25000)))
size = abs(rnorm(50000)*10000)
childID = as.factor(rep(1:5000, each = 10))
childID = as.factor(childID)
schoolID = as.factor(rep(1:500, each = 100))
vamData = cbind(dep, time, female, size, childID, schoolID)
names(vamData) = tolower(names(vamData))
head(vamData)
library(nlme)
```
Now we are exploring what type of model we should fit (e.g. linear, quadtraic).  Therefore we examaining the indivudal growth curves.  Select a radom sample to speed up the proces.  Need to randomly select from the levels so we get all of the data points for a person.
```{r}
vamData = data.frame(vamData)
vamData$female = factor(vamData$female)
vamData$childID = factor(vamData$childID)
vamData$schoolID = factor(vamData$schoolID)

head(vamData)
vamDataInd = groupedData(dep ~ time | schoolID/childID, data = vamData)
vamDataInd$female = factor(vamDataInd$female)
vamDataInd$childID = factor(vamDataInd$childID)
vamDataInd$schoolID = factor(vamDataInd$schoolID)
head(vamDataInd)

samp <- sample(levels(vamDataInd$childID), 30)

level12.subGroup = subset(vamDataInd, childID %in% samp)
level12 = lmList(dep ~ time | childID, data = level12.subGroup)
plot(augPred(level12))
summary(level12)
# Visually analyze each level (here just childID) to see if you need random slopes and random intercepts.  Remember that random intercepts means starting at different points and different slopes meaning moving in different directions. 
plot(intervals(level12))
summary(level12)
# Now we are fitting an unconditoinal model meaning that both the intercept and slope are free to vary or that we are estimating each of them separatetly for each person.  Can use model = "ML" for full MLE with missing data

unconVamData = lme(dep ~ time, random =~ time | schoolID/childID, data = vamDataInd, method = "ML")
summary(unconVamData)

# New model that includes female, size, and size by year
unconVamDataCovs = lme(fixed = dep ~ time*size+female, random =~ time | schoolID/childID, data = vamDataInd)
summary(unconVamDataCovs)

# New model that allows for the within group error terms (hetero) to be different controling for hetero
#hetVarVam = update(auto1Vam, weights = varIdent(form = ~ 1 | time))

# hetVar doesn't work, but you would want to test if you want to test if hetero model is better than the homo model. 
anova(unconVamData, hetVar)

# New model has the old model with hetero, but now includes the autocorrelation among year
auto1Vam = update(unconVamData, correlation=corCAR1(form = ~ time | schoolID/childID))

anova(unconVamData, auto1Vam)

```



