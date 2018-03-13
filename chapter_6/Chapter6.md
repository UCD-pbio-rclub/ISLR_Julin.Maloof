---
title: "Chapter 6"
author: "Julin N Maloof"
date: "2/27/2018"
output: 
  html_document: 
    keep_md: yes
---




## Q3

_Asks about adding an additional constraint to a standard least squares where we limit the size of the beta coefficients to be less than some value _s_._

_For parts (a) through (e), indicate which of i. through v. is correct. Justify your answer._

_i. Increase initially, and then eventually start decreasing in an inverted U shape._
_ii. Decrease initially, and then eventually start increasing in a U shape._
_iii. Steadily increase._
_iv. Steadily decrease._
_v. Remain constant._

_(a) As we increase s from 0, the training RSS will:_

iv. Steadliy decrease.  We have a more and more flexible model so it will better be able to fit the training set.  Actually it won't steadly decrease, it will plateau...

_(b) Repeat (a) for test RSS._

I think this depends on the scenario.  If we have a small number of good predictors then this will be pretty similar to (a).  OTOH if we have a large number of predictors, or some that aren't good predictors, then 

ii. Decrease initially, and then eventually start increasing in a U shape.

_(c) variance_

Variance is high if there are a lot of differences in models when we use different training sets.

with low s we won't fit anything well (and the model won't change much from training to training) so variance will be low.  As s increases there will start to be more and more difference betweenthe models, so I expect Variance to increase.

_(d) Squared bias._

Bias represents the difference between our model and the data.  When s = 0 then there is no flexibility and bias will be high.  This will decrease as s increases.

_(e) irreducible error._

By definition this should remain constant.

## Q4

_This asks the same questions as 3, but here the model is the Ridge Regression model and penalizes based on lambda, the tuning coefficent that is amultiplier for the sum of the squred beta coefficients._

_As we increase lambda from 0..._

_(a) training RSS_

Will steadily increase as we fit less and less flexible models

_(b) test RSS_

Will decrease as we reduce variance, and then increase as eventually we increase bias

_(c) variance_

Will decrease steadily

_(d) bias_

will increase steadily

_(e) irreducible error_

unchanged

## Q5 a, b

## Q9 a, b, c

_9. In this exercise, we will predict the number of applications received using the other variables in the College data set._

_(a) Split the data set into a training set and a test set._


```r
data(College)
head(College)
```

```
##                              Private Apps Accept Enroll Top10perc
## Abilene Christian University     Yes 1660   1232    721        23
## Adelphi University               Yes 2186   1924    512        16
## Adrian College                   Yes 1428   1097    336        22
## Agnes Scott College              Yes  417    349    137        60
## Alaska Pacific University        Yes  193    146     55        16
## Albertson College                Yes  587    479    158        38
##                              Top25perc F.Undergrad P.Undergrad Outstate
## Abilene Christian University        52        2885         537     7440
## Adelphi University                  29        2683        1227    12280
## Adrian College                      50        1036          99    11250
## Agnes Scott College                 89         510          63    12960
## Alaska Pacific University           44         249         869     7560
## Albertson College                   62         678          41    13500
##                              Room.Board Books Personal PhD Terminal
## Abilene Christian University       3300   450     2200  70       78
## Adelphi University                 6450   750     1500  29       30
## Adrian College                     3750   400     1165  53       66
## Agnes Scott College                5450   450      875  92       97
## Alaska Pacific University          4120   800     1500  76       72
## Albertson College                  3335   500      675  67       73
##                              S.F.Ratio perc.alumni Expend Grad.Rate
## Abilene Christian University      18.1          12   7041        60
## Adelphi University                12.2          16  10527        56
## Adrian College                    12.9          30   8735        54
## Agnes Scott College                7.7          37  19016        59
## Alaska Pacific University         11.9           2  10922        15
## Albertson College                  9.4          11   9727        55
```

```r
College$Private <- as.numeric(College$Private)-1 # 0 = no, 1 = yes
set.seed(11)
train <- sample(c(TRUE,FALSE),size = nrow(College),prob = c(0.50,0.50),replace=TRUE)
college.train <- College %>% as_tibble() %>% filter(train)
college.test <- College %>% as_tibble() %>% filter(!train)
```


_(b) Fit a linear model using least squares on the training set, and report the test error obtained._


```r
lm9b <- lm(Apps ~ . , data = college.train)
summary(lm9b)
```

```
## 
## Call:
## lm(formula = Apps ~ ., data = college.train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5716.1  -450.2   -20.6   354.8  6246.3 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.280e+03  6.137e+02  -2.086 0.037660 *  
## Private     -3.565e+02  2.134e+02  -1.670 0.095769 .  
## Accept       1.665e+00  5.540e-02  30.047  < 2e-16 ***
## Enroll      -1.027e+00  2.823e-01  -3.637 0.000315 ***
## Top10perc    4.775e+01  9.520e+00   5.016 8.29e-07 ***
## Top25perc   -1.457e+01  7.267e+00  -2.005 0.045732 *  
## F.Undergrad  6.168e-02  5.037e-02   1.225 0.221487    
## P.Undergrad  9.894e-02  4.505e-02   2.196 0.028712 *  
## Outstate    -1.430e-01  3.136e-02  -4.559 7.03e-06 ***
## Room.Board   1.665e-01  8.198e-02   2.031 0.042958 *  
## Books       -2.989e-01  3.387e-01  -0.883 0.378073    
## Personal     2.846e-02  9.729e-02   0.293 0.770013    
## PhD         -1.512e+01  6.782e+00  -2.230 0.026342 *  
## Terminal     1.833e+00  7.345e+00   0.250 0.803034    
## S.F.Ratio    4.230e+01  2.036e+01   2.078 0.038390 *  
## perc.alumni  5.802e+00  6.495e+00   0.893 0.372242    
## Expend       1.594e-01  2.601e-02   6.130 2.29e-09 ***
## Grad.Rate    1.276e+01  4.693e+00   2.719 0.006855 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1150 on 363 degrees of freedom
## Multiple R-squared:  0.9256,	Adjusted R-squared:  0.9222 
## F-statistic: 265.8 on 17 and 363 DF,  p-value: < 2.2e-16
```


```r
lm9c.predict <- predict(lm9b,newdata = college.test)
test.error <- (college.test$Apps - lm9c.predict)^2 %>% mean()
test.error
```

```
## [1] 1101422
```


_(c) Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained._


```r
ridge9c <- college.train %>% select(-Apps) %>% as.matrix() %>%
  glmnet(y=college.train$Apps, alpha = 0)
```


```r
cv.out <- college.train %>% select(-Apps) %>% as.matrix() %>%
  cv.glmnet(y=college.train$Apps, alpha = 0)
```


```r
plot(cv.out)
```

![](Chapter6_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
lam1se=cv.out$lambda.1se
lam1se
```

```
## [1] 2476.486
```

```r
log(lam1se)
```

```
## [1] 7.814596
```


```r
ridge9c.predict <- college.test %>% select(-Apps) %>% as.matrix() %>%
  predict(ridge9c,s=lam1se,newx=.)
(college.test$Apps - ridge9c.predict)^2 %>% mean()
```

```
## [1] 1207395
```
Something wrong??


```r
ridge9c.predict <- college.test %>% select(-Apps) %>% as.matrix() %>%
  predict(ridge9c,s=cv.out$lambda.min,newx=.)
(college.test$Apps - ridge9c.predict)^2 %>% mean()
```

```
## [1] 1035787
```

Still high...
