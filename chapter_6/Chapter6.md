# Chapter 6
Julin N Maloof  
2/27/2018  




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

Will steadily decrease as we fit less and less flexible models

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
set.seed(123)
train <- sample(c(TRUE,FALSE),size = nrow(College),prob = c(0.80,0.20),replace=TRUE)
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
## -4717.8  -369.3   -18.5   274.6  7587.3 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -688.43195  444.05806  -1.550 0.121591    
## Private     -425.27078  153.35092  -2.773 0.005723 ** 
## Accept         1.61595    0.04351  37.136  < 2e-16 ***
## Enroll        -1.08780    0.20094  -5.414 8.93e-08 ***
## Top10perc     38.90903    6.14557   6.331 4.76e-10 ***
## Top25perc     -8.48165    4.81936  -1.760 0.078931 .  
## F.Undergrad    0.07257    0.03486   2.082 0.037794 *  
## P.Undergrad    0.07315    0.03602   2.031 0.042688 *  
## Outstate      -0.07236    0.02125  -3.405 0.000707 ***
## Room.Board     0.10194    0.05350   1.905 0.057213 .  
## Books         -0.13449    0.26193  -0.513 0.607835    
## Personal       0.07104    0.06988   1.017 0.309756    
## PhD           -8.21216    5.21613  -1.574 0.115926    
## Terminal      -3.73357    5.63224  -0.663 0.507654    
## S.F.Ratio     24.11325   14.18632   1.700 0.089695 .  
## perc.alumni    4.73124    4.59216   1.030 0.303290    
## Expend         0.09618    0.01416   6.791 2.68e-11 ***
## Grad.Rate      6.97940    3.23140   2.160 0.031178 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1038 on 601 degrees of freedom
## Multiple R-squared:  0.9336,	Adjusted R-squared:  0.9317 
## F-statistic: 496.9 on 17 and 601 DF,  p-value: < 2.2e-16
```


```r
lm9c.predict <- predict(lm9b,newdata = college.test)
test.error <- (college.test$Apps - lm9c.predict)^2 %>% mean()
test.error
```

```
## [1] 1203826
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
## [1] 1827.768
```

```r
log(lam1se)
```

```
## [1] 7.510851
```


```r
ridge9c.predict <- college.test %>% select(-Apps) %>% as.matrix() %>%
  predict(ridge9c,s=lam1se,newx=.)
(college.test$Apps - ridge9c.predict)^2 %>% mean()
```

```
## [1] 1663179
```
Something wrong??


```r
ridge9c.predict <- college.test %>% select(-Apps) %>% as.matrix() %>%
  predict(ridge9c,s=cv.out$lambda.min,newx=.)
(college.test$Apps - ridge9c.predict)^2 %>% mean()
```

```
## [1] 1292858
```

Still high...
