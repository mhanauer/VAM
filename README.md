---
title: "VAM"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
This post explains how to use nmle and lme4 package for value added modeling. First, we created an artificial data set. 
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
vamData = data.frame(vamData)
vamData$female = factor(vamData$female)
vamData$childID = factor(vamData$childID)
vamData$schoolID = factor(vamData$schoolID)
vamDataInd = groupedData(dep ~ time | schoolID/childID, data = vamData)
vamDataInd$female = factor(vamDataInd$female)
vamDataInd$childID = factor(vamDataInd$childID)
vamDataInd$schoolID = factor(vamDataInd$schoolID)
head(vamDataInd)
```
We then fit an unconditional model meaning that both the intercepts and slopes can vary by schools and for children nested within schools.  For the first model, we only include one variable time and evaluate the average effect of time on the dependent variable.  After the comma in the first model, we create the random part which says that we want to evaluate the differences in the schools and children within schools (random intercepts) on the dependent variable and how the differences in schools and children within schools varies over time (random slopes) which we evaluate across schools and children in schools.  We then select the method ML to use full maximum likelihood estimation, which is one way of handling missing data: http://www.statisticalhorizons.com/wp-content/uploads/MissingDataByML.pdf

The first part of the output focuses on the intercept, which is the average standard deviation difference between the different schools starting values on the dependent variable.  The slope is the average standard deviation difference between the slopes (i.e. time's effect on the dependent variable per school).  The Corr is the correlation between the intercept and slopes.  The next provides the same estimates, but for the student levels, which are nested within the schools.  Finally, we have the fixed part, which is where we can evaluate the effect of time on the dependent variable.
```{r}
unconVamData = lme(dep ~ time, random =~ time | schoolID/childID, data = vamDataInd, method = "ML")
summary(unconVamData)
```
The next model is the same as before except with two control variables (school size and female status of student).  Although, a VAM model will include more covariates, for this example two covariates will suffice.  To answer the question we are interested in, what is the effect of schools on student's dependent variable score, we need to look at the individual coefficients for schools.  Using the second model (unconVamDataCovs) we use the coef function to produce the random intercepts and slopes for the first five schools across time.  As we can see schools 2 and 3 experiencing positive growth while schools 1,4, and 5 are experiencing negative growth.   
```{r}
unconVamDataCovs = lme(fixed = dep ~ time + size+female, random =~ time | schoolID/childID, data = vamDataInd)
summary(unconVamDataCovs)
ranef(unconVamDataCovs, level = 1)[1:5,]
```
There are two other modifications that I would like to talk about.  First is that sometimes data over time are autocorrelated, which is not accounted for the in current model.  Therefore, we may need to explicitly specify the autocorrelation in the model.  The auto1Vam does that by using the update function to update the unconVamDataCovs with an ar1 model (i.e. each time point is correlated with its predecessor).  The model is interpreted in the same way as before.  
```{r}
auto1Vam = update(unconVamDataCovs, correlation=corCAR1(form = ~ time | schoolID/childID))
summary(auto1Vam)
```
The final model uses a different package to handle non or cross nested levels.  Unfortunately, the nlme package has difficultly handling non or cross nested data, so we will switch to the lme4 package, which more easily handles non or cross nested data.  In this example, we are using the InstEval data.  Y is the dependent variable, s is the student indicator and d is the instructor.  Sometimes students have more than one instructor therefore, we cannot evaluate the data as students nested within instructor.  Instead we model them as separate levels using the code below.  Here we have the intercepts for both the s students and teachers (d) demonstrating how students and teachers deviate from each other on the dependent variable y.  To  get the individual intercepts for students and instructors we use the coef function and grab the students and instructors individually. 

Although, this example does not include a time variable or covariates, they can be added and interpreted in the same way as the previous examples in this post.  
```{r}
library(lme4)
data("InstEval")
head(InstEval)
dim(InstEval)
fmCross = lmer(y~ 1 + (1|s) + (1|d), InstEval, REML = FALSE)
fmCross
fmCrossCoef =coef(fmCross)
head(fmCrossCoef$s)
head(fmCrossCoef$d)

```

