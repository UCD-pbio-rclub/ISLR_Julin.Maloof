# Chapter 3
Julin N Maloof  
12/5/2017  



# Dec 6th

Questions 8, 13 (13g optional), 15(a)

## Q1


_Describe the null hypotheses to which the p-values given in Table 3.4 correspond. Explain what conclusions you can draw based on these p-values. Your explanation should be phrased in terms of sales, TV, radio, and newspaper, rather than in terms of the coefficients of the linear model._

* __Intercept:__ the null hypothesis is that there are no sales when there is no advertising.  The p-value is significant, indicating that there are some sales without advertising.
* __TV:__ the null hypothesis is that changing TV advertising while holding radio and newspaper constant does not change sales. The significant p-value indicates that changing TV advertising does affect sales when holding the other predictors constant.
* __radio:__ the null hypothesis is that changing radio advertising while holding TV and newspaper constant does not change sales. The significant p-value indicates that changing radio advertising does affect sales when holding the other predictors constant.
* __newspaper:__ the null hypothesis is that changing newspaper advertising while holding radio and TV constant does not change sales. The insignificant p-value indicates that changing newspaper advertising does not affect sales when holding the other predictors constant.

## Q2
_Carefully explain the differences between the KNN classifier and KNN regression methods._

KNN classifier is when we are trying to predict which group an observation belongs to.  The proportion of K nearest neighbors belonging to each group is used for this classification.  KNN regression is used when we are tyring to predict a numeric value given predictors.  Here the average value for K nearest neighbors, given the predictors, is used.

## Q3

_Suppose we have a data set with five predictors, X1 = GPA, X2 = IQ, X3 = Gender (1 for Female and 0 for Male), X4 = Interaction between GPA and IQ, and X5 = Interaction between GPA and Gender. The response is starting salary after graduation (in thousands of dollars). Suppose we use least squares to fit the model, and get βˆ0 = 50, βˆ1 = 20 , βˆ2 = 0.07 , βˆ3 = 35 , βˆ4 = 0.01 , βˆ5 = −10 ._

_(a) Which answer is correct, and why?_
_i. For a fixed value of IQ and GPA, males earn more on average than females._
_ii. For a fixed value of IQ and GPA, females earn more on average than males._
_iii. For a fixed value of IQ and GPA, males earn more on average than females provided that the GPA is high enough._
_iv. For a fixed value of IQ and GPA, females earn more on average than males provided that the GPA is high enough._

The correct answer is _iii_.  Because $\hat \beta _5$ is negative the postive effect of female gender ($\hat \beta _3$) is offset at higher GPAs

_(b) Predict the salary of a female with IQ of 110 and a GPA of 4.0._


```r
50 + 20*4 + 110 * 0.07 + 1*35 + 4*110*0.01 + 1*4*-10
```

```
## [1] 137.1
```

_(c) True or false: Since the coefficient for the GPA/IQ interaction term is very small, there is very little evidence of an interaction effect. Justify your answer._

FALSE.  We need to look at the p-values.  also the size of the coefficient is dependent on the scale of the predictor, not signficance per se.  


## Q8

_8. This question involves the use of simple linear regression on the Auto data set._


```r
library(MASS)
library(ISLR)
```

```
## Warning: package 'ISLR' was built under R version 3.4.2
```

```r
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Warning: package 'tidyr' was built under R version 3.4.2
```

```
## Warning: package 'purrr' was built under R version 3.4.2
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
## select(): dplyr, MASS
```

```r
data(Auto)
auto <- as.tibble(Auto)
auto
```

```
## # A tibble: 392 x 9
##      mpg cylinders displacement horsepower weight acceleration  year
##  * <dbl>     <dbl>        <dbl>      <dbl>  <dbl>        <dbl> <dbl>
##  1    18         8          307        130   3504         12.0    70
##  2    15         8          350        165   3693         11.5    70
##  3    18         8          318        150   3436         11.0    70
##  4    16         8          304        150   3433         12.0    70
##  5    17         8          302        140   3449         10.5    70
##  6    15         8          429        198   4341         10.0    70
##  7    14         8          454        220   4354          9.0    70
##  8    14         8          440        215   4312          8.5    70
##  9    14         8          455        225   4425         10.0    70
## 10    15         8          390        190   3850          8.5    70
## # ... with 382 more rows, and 2 more variables: origin <dbl>, name <fctr>
```

_(a) Use the lm() function to perform a simple linear regression with mpg as the response and horsepower as the predictor. Use the summary() function to print the results. Comment on the output. For example:_


```r
lm1 <- lm(mpg ~ horsepower, data = auto)

summary(lm1)
```

```
## 
## Call:
## lm(formula = mpg ~ horsepower, data = auto)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.5710  -3.2592  -0.3435   2.7630  16.9240 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 39.935861   0.717499   55.66   <2e-16 ***
## horsepower  -0.157845   0.006446  -24.49   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.906 on 390 degrees of freedom
## Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
## F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16
```


_i. Is there a relationship between the predictor and the response?_

Yes, there is a highly significant (negative) relationship between horsepower and mpg

_ii. How strong is the relationship between the predictor and the response?_

given very low p-value and very non-zero t-value,  strong

_iii. Is the relationship between the predictor and the response positive or negative?_

negative

_iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?_


```r
cat("predition at horsepower 98 and 95% confidence intervals:\n")
```

```
## predition at horsepower 98 and 95% confidence intervals:
```

```r
predict(lm1,data.frame(horsepower=98),interval = "conf")
```

```
##        fit      lwr      upr
## 1 24.46708 23.97308 24.96108
```

```r
cat("\npredition at horsepower 98 and 95% prediction intervals:\n")
```

```
## 
## predition at horsepower 98 and 95% prediction intervals:
```

```r
predict(lm1,data.frame(horsepower=98),interval = "pred")
```

```
##        fit     lwr      upr
## 1 24.46708 14.8094 34.12476
```

_(b) Plot the response and the predictor. Use the abline() function to display the least squares regression line._


```r
auto %>% 
  ggplot(aes(x=horsepower, y=mpg)) +
           geom_point() +
           geom_smooth(method = "lm", se=FALSE)
```

![](Chapter_3_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



_(c) Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with the fit._


```r
library(modelr)
auto %>% 
  add_residuals(lm1) %>%
  ggplot(aes(x=horsepower, y = resid)) +
  geom_point() +
  geom_hline(yintercept=0)
```

![](Chapter_3_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
Residuals not evenly distributed!



```r
plot(lm1,ask = FALSE)
```

![](Chapter_3_files/figure-html/unnamed-chunk-7-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-7-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-7-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-7-4.png)<!-- -->
## Q9. _This question involves the use of multiple linear regression on the Auto data set._

_(a) Produce a scatterplot matrix which includes all of the variables in the data set._


```r
data(Auto)
auto <- as_tibble(Auto)
pairs(auto)
```

![](Chapter_3_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

_(b) Compute the matrix of correlations between the variables using the function cor(). You will need to exclude the name variable, which is qualitative._


```r
auto %>% select(-name) %>% cor() %>% knitr::kable(digits = 3)
```

                   mpg   cylinders   displacement   horsepower   weight   acceleration     year   origin
-------------  -------  ----------  -------------  -----------  -------  -------------  -------  -------
mpg              1.000      -0.778         -0.805       -0.778   -0.832          0.423    0.581    0.565
cylinders       -0.778       1.000          0.951        0.843    0.898         -0.505   -0.346   -0.569
displacement    -0.805       0.951          1.000        0.897    0.933         -0.544   -0.370   -0.615
horsepower      -0.778       0.843          0.897        1.000    0.865         -0.689   -0.416   -0.455
weight          -0.832       0.898          0.933        0.865    1.000         -0.417   -0.309   -0.585
acceleration     0.423      -0.505         -0.544       -0.689   -0.417          1.000    0.290    0.213
year             0.581      -0.346         -0.370       -0.416   -0.309          0.290    1.000    0.182
origin           0.565      -0.569         -0.615       -0.455   -0.585          0.213    0.182    1.000



_(c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors. Use the summary() function to print the results. Comment on the output._


```r
lm.auto <- lm(mpg ~ cylinders +
                displacement +
                horsepower +
                weight +
                acceleration +
                year + 
                origin,
              data=auto)
summary(lm.auto)
```

```
## 
## Call:
## lm(formula = mpg ~ cylinders + displacement + horsepower + weight + 
##     acceleration + year + origin, data = auto)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.5903 -2.1565 -0.1169  1.8690 13.0604 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -17.218435   4.644294  -3.707  0.00024 ***
## cylinders     -0.493376   0.323282  -1.526  0.12780    
## displacement   0.019896   0.007515   2.647  0.00844 ** 
## horsepower    -0.016951   0.013787  -1.230  0.21963    
## weight        -0.006474   0.000652  -9.929  < 2e-16 ***
## acceleration   0.080576   0.098845   0.815  0.41548    
## year           0.750773   0.050973  14.729  < 2e-16 ***
## origin         1.426141   0.278136   5.127 4.67e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.328 on 384 degrees of freedom
## Multiple R-squared:  0.8215,	Adjusted R-squared:  0.8182 
## F-statistic: 252.4 on 7 and 384 DF,  p-value: < 2.2e-16
```


For instance:
_i. Is there a relationship between the predictors and the re- sponse?_

Yes, based on over-all p-value of < 2.2 e -16

_ii. Which predictors appear to have a statistically significant relationship to the response?_

displacement, weight, year, origin

_iii. What does the coefficient for the year variable suggest?_

mpg increases ~ 0.75 per year when other predictors are held constant.

_(d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?_


```r
plot(lm.auto)
```

![](Chapter_3_files/figure-html/unnamed-chunk-11-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-11-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-11-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

Residuals vs fitted suggest there may be some non-linearity since there is a u-shaped curve.

QQ plot: a few more high residuals than expected.

There is a high leverage point but its residual is not that big so no serious problems there.

_(e) Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?_

Only use those predictors that were significant in the MR.  Only fit two way interactions


```r
lm.auto.int <- lm(mpg ~ 
                displacement +
                weight +
                year + 
                origin +
                  displacement:weight +
                  displacement:year +
                  displacement:origin +
                  weight:year +
                  weight:origin +
                  year:origin,
              data=auto)
summary(lm.auto.int)
```

```
## 
## Call:
## lm(formula = mpg ~ displacement + weight + year + origin + displacement:weight + 
##     displacement:year + displacement:origin + weight:year + weight:origin + 
##     year:origin, data = auto)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.8970 -1.5806 -0.1199  1.2215 14.1451 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -1.792e+01  2.496e+01  -0.718  0.47325    
## displacement         3.382e-02  8.295e-02   0.408  0.68370    
## weight              -8.284e-03  1.119e-02  -0.740  0.45970    
## year                 9.045e-01  3.237e-01   2.795  0.00546 ** 
## origin              -5.649e+00  5.352e+00  -1.055  0.29195    
## displacement:weight  1.806e-05  2.762e-06   6.540 1.98e-10 ***
## displacement:year   -1.593e-03  1.137e-03  -1.401  0.16189    
## displacement:origin  1.605e-02  1.276e-02   1.258  0.20930    
## weight:year          5.751e-06  1.512e-04   0.038  0.96968    
## weight:origin       -1.343e-03  9.465e-04  -1.418  0.15688    
## year:origin          9.457e-02  6.619e-02   1.429  0.15387    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.95 on 381 degrees of freedom
## Multiple R-squared:  0.8608,	Adjusted R-squared:  0.8571 
## F-statistic: 235.6 on 10 and 381 DF,  p-value: < 2.2e-16
```

```r
plot(lm.auto.int)
```

![](Chapter_3_files/figure-html/unnamed-chunk-12-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-12-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-12-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-12-4.png)<!-- -->

The non-linearity suggested from the fitted vs residuals is gone, but there may now be some heteroscedasctity.  QQ plot shows some outliers.

_(f) Try a few different transformations of the variables, such as log(X), √X, X2. Comment on your findings._

First lets look again at a pairs plot using the significant predictors from the MR:


```r
auto %>% 
  select(mpg, displacement, weight, year, origin) %>%
  pairs()
```

![](Chapter_3_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Both displacement and weight show some non-linearity w.r.t. mpg


```r
auto %>% select(mpg,displacement) %>%
  mutate(dis2 = displacement^2, 
         logdisp=log(displacement),
         sqrtdispl=sqrt(displacement)) %>%
  pairs()
```

![](Chapter_3_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
auto %>% select(mpg,weight) %>%
  mutate(weight2 = weight^2, 
         logweight=log(weight),
         sqrtweight=sqrt(weight)) %>%
  pairs()
```

![](Chapter_3_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

The log transformations appear to provide the best linearity with mpg


```r
lm.auto.trans <- lm(mpg ~ 
                log(displacement) +
                log(weight) +
                year + 
                origin,
              data=auto)
summary(lm.auto.trans)
```

```
## 
## Call:
## lm(formula = mpg ~ log(displacement) + log(weight) + year + origin, 
##     data = auto)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.0612  -1.9499  -0.0303   1.6006  13.0971 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       112.42433    9.88747  11.370   <2e-16 ***
## log(displacement)  -0.51061    0.97706  -0.523   0.6016    
## log(weight)       -18.38713    1.70131 -10.808   <2e-16 ***
## year                0.77502    0.04569  16.962   <2e-16 ***
## origin              0.69822    0.26673   2.618   0.0092 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.126 on 387 degrees of freedom
## Multiple R-squared:  0.8412,	Adjusted R-squared:  0.8395 
## F-statistic: 512.5 on 4 and 387 DF,  p-value: < 2.2e-16
```

```r
plot(lm.auto.trans)
```

![](Chapter_3_files/figure-html/unnamed-chunk-15-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-15-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-15-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-15-4.png)<!-- -->

compaere this to unrransformed

```r
lm.auto.small <- lm(mpg ~ 
                displacement +
                weight +
                year + 
                origin,
              data=auto)
summary(lm.auto.small)
```

```
## 
## Call:
## lm(formula = mpg ~ displacement + weight + year + origin, data = auto)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.8102 -2.1129 -0.0388  1.7725 13.2085 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.861e+01  4.028e+00  -4.620 5.25e-06 ***
## displacement  5.588e-03  4.768e-03   1.172    0.242    
## weight       -6.575e-03  5.571e-04 -11.802  < 2e-16 ***
## year          7.714e-01  4.981e-02  15.486  < 2e-16 ***
## origin        1.226e+00  2.670e-01   4.593 5.92e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.346 on 387 degrees of freedom
## Multiple R-squared:  0.8181,	Adjusted R-squared:  0.8162 
## F-statistic: 435.1 on 4 and 387 DF,  p-value: < 2.2e-16
```

```r
plot(lm.auto.small)
```

![](Chapter_3_files/figure-html/unnamed-chunk-16-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-16-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-16-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-16-4.png)<!-- -->

The R-squared is slightly better using the transformed predictors.

## Q10 _This question should be answered using the Carseats data set._

_(a) Fit a multiple regression model to predict Sales using Price,
Urban, and US._


```r
data("Carseats")
lm10a <- lm(Sales ~ Price + Urban + US,data = Carseats)
summary(lm10a)
```

```
## 
## Call:
## lm(formula = Sales ~ Price + Urban + US, data = Carseats)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.9206 -1.6220 -0.0564  1.5786  7.0581 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 13.043469   0.651012  20.036  < 2e-16 ***
## Price       -0.054459   0.005242 -10.389  < 2e-16 ***
## UrbanYes    -0.021916   0.271650  -0.081    0.936    
## USYes        1.200573   0.259042   4.635 4.86e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.472 on 396 degrees of freedom
## Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2335 
## F-statistic: 41.52 on 3 and 396 DF,  p-value: < 2.2e-16
```

_(b) Provide an interpretation of each coefficient in the model. Be
careful—some of the variables in the model are qualitative!_

* Intercept: The predicted base level of saled if carseats were free
* Price: Sales decreased by 54 units per increase in price (if other predicotrs held constant).
* Urban: Are sales affected by urban vs rural store location?  N.S.
* US: Being in the US increases sales by 1200 units (when other factors are constant).

_(c) Write out the model in equation form, being careful to handle the qualitative variables properly._

$$ Sales \sim \beta_0 + \beta_1*Price + \beta_2*Urban + \beta_3*US + \epsilon $$

Or did they mean

$$
Sales = 13.04 - 0.05 * Price - 0.02*UrbanYes + 1.2*USYes
$$
_(d) For which of the predictors can you reject the null hypothesis H0 :βj =0?_

Intercept, price, US

_(e) On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the outcome._


```r
lm10e <- update(lm10a, ~ . - Urban)
summary(lm10e)
```

```
## 
## Call:
## lm(formula = Sales ~ Price + US, data = Carseats)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.9269 -1.6286 -0.0574  1.5766  7.0515 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 13.03079    0.63098  20.652  < 2e-16 ***
## Price       -0.05448    0.00523 -10.416  < 2e-16 ***
## USYes        1.19964    0.25846   4.641 4.71e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.469 on 397 degrees of freedom
## Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2354 
## F-statistic: 62.43 on 2 and 397 DF,  p-value: < 2.2e-16
```

_(f) How well do the models in (a) and (e) fit the data?_

Adjusted R-squared is a bit below 0.24 meaning there is a lot of unexplained variance.

_(g) Using the model from (e), obtain 95% confidence intervals for the coefficient(s)._


```r
confint(lm10e)
```

```
##                   2.5 %      97.5 %
## (Intercept) 11.79032020 14.27126531
## Price       -0.06475984 -0.04419543
## USYes        0.69151957  1.70776632
```

_(h) Is there evidence of outliers or high leverage observations in the model from (e)?_


```r
plot(lm10e)
```

![](Chapter_3_files/figure-html/unnamed-chunk-20-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-20-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-20-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-20-4.png)<!-- -->

nope, looks pretty good.

Also try some 3D plotting:


```r
library(plot3D)
library(broom)
```

```
## 
## Attaching package: 'broom'
```

```
## The following object is masked from 'package:modelr':
## 
##     bootstrap
```

```r
library(modelr)
```

## Q 13

_13. In this exercise you will create some simulated data and will fit simple linear regression models to it. Make sure to use set.seed(1) prior to starting part (a) to ensure consistent results._

_(a) Using the rnorm() function, create a vector, x, containing 100 observations drawn from a N (0, 1) distribution. This represents a feature, X._
_(b) Using the rnorm() function, create a vector, eps, containing 100 observations drawn from a N(0,0.25) distribution i.e. a normal distribution with mean zero and variance 0.25._
_(c) Using x and eps, generate a vector y according to the model Y =−1+0.5X+ε. (3.39)_
_What is the length of the vector y? What are the values of β0 and β1 in this linear model?_


```r
set.seed(1)
data13 <- tibble(
  x = rnorm(100,mean = 0, sd = 1),
  eps = rnorm(100,0, sqrt(0.25)),
  y = 1 + 0.5*x + eps)

data13
```

```
## # A tibble: 100 x 3
##             x         eps         y
##         <dbl>       <dbl>     <dbl>
##  1 -0.6264538 -0.31018334 0.3765898
##  2  0.1836433  0.02105794 1.1128796
##  3 -0.8356286 -0.45546082 0.1267249
##  4  1.5952808  0.07901439 1.8766548
##  5  0.3295078 -0.32729232 0.8374616
##  6 -0.8204684  0.88364363 1.4734094
##  7  0.4874291  0.35835374 1.6020683
##  8  0.7383247  0.45508711 1.8242495
##  9  0.5757814  0.19209268 1.4799834
## 10 -0.3053884  0.84108804 1.6883938
## # ... with 90 more rows
```

$$\beta_0 = 1$$

$$\beta_1=0.5$$
length of y is 100

_(d) Create a scatterplot displaying the relationship between x and y. Comment on what you observe._


```r
data13 %>% qplot(x,y,data=.)
```

![](Chapter_3_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

Pretty nice linear relationship, but some scatter...

_(e) Fit a least squares linear model to predict y using x. Comment on the model obtained. How do βˆ0 and βˆ1 compare to β0 and β1?_


```r
lm13 <- lm(y~x, data=data13)
summary(lm13)
```

```
## 
## Call:
## lm(formula = y ~ x, data = data13)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.93842 -0.30688 -0.06975  0.26970  1.17309 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.98115    0.04849  20.233  < 2e-16 ***
## x            0.49947    0.05386   9.273 4.58e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4814 on 98 degrees of freedom
## Multiple R-squared:  0.4674,	Adjusted R-squared:  0.4619 
## F-statistic: 85.99 on 1 and 98 DF,  p-value: 4.583e-15
```

```r
confint(lm13)
```

```
##                 2.5 %    97.5 %
## (Intercept) 0.8849196 1.0773878
## x           0.3925794 0.6063602
```

The estimates are near the real values and the 95% confidence intervals include the real values

_(f) Display the least squares line on the scatterplot obtained in (d). Draw the population regression line on the plot, in a different color. Use the legend() command to create an appropriate legend._


```r
data13 %>% ggplot(aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(aes(color="blue"),method="lm",se=FALSE) +
  geom_abline(aes(intercept=1,slope=.5,color="red")) +
  scale_color_manual(name="legend",values=c("red"="red","blue"="blue"),labels=c("population","sample"), guide="legend")
```

![](Chapter_3_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

_(g) Now fit a polynomial regression model that predicts y using x and x2. Is there evidence that the quadratic term improves the model fit? Explain your answer._


```r
lm13b <- lm(y ~ x + I(x^2), data=data13)
summary(lm13b)
```

```
## 
## Call:
## lm(formula = y ~ x + I(x^2), data = data13)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.98252 -0.31270 -0.06441  0.29014  1.13500 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.02836    0.05883  17.481  < 2e-16 ***
## x            0.50858    0.05399   9.420  2.4e-15 ***
## I(x^2)      -0.05946    0.04238  -1.403    0.164    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.479 on 97 degrees of freedom
## Multiple R-squared:  0.4779,	Adjusted R-squared:  0.4672 
## F-statistic:  44.4 on 2 and 97 DF,  p-value: 2.038e-14
```

```r
confint(lm13b)
```

```
##                  2.5 %     97.5 %
## (Intercept)  0.9116007 1.14511430
## x            0.4014226 0.61573832
## I(x^2)      -0.1435788 0.02465763
```

The fits are pretty similar, so no real improvement.  RSE and R-squeared similar.

_(h) Repeat (a)–(f) after modifying the data generation process in such a way that there is less noise in the data. The model (3.39) should remain the same. You can do this by decreasing the variance of the normal distribution used to generate the error term ε in (b). Describe your results._


```r
data13h <- tibble(
  x = rnorm(100,mean = 0, sd = 1),
  eps = rnorm(100,0, sqrt(0.1)),
  y = 1 + 0.5*x + eps)

data13h
```

```
## # A tibble: 100 x 3
##              x         eps         y
##          <dbl>       <dbl>     <dbl>
##  1  0.40940184  0.28260444 1.4873054
##  2  1.68887329 -0.33118475 1.5132519
##  3  1.58658843  0.62339162 2.4166858
##  4 -0.33090780 -0.12131512 0.7132310
##  5 -2.28523554  0.52308667 0.3804689
##  6  2.49766159  0.47820364 2.7270344
##  7  0.66706617  0.02623607 1.3597692
##  8  0.54132734  0.17937100 1.4500347
##  9 -0.01339952 -0.32399068 0.6693096
## 10  0.51010842  0.10214362 1.3571978
## # ... with 90 more rows
```


```r
lm13h <- lm(y ~ x, data=data13h)
summary(lm13h)
```

```
## 
## Call:
## lm(formula = y ~ x, data = data13h)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.86703 -0.17753 -0.00553  0.21495  0.58452 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.01532    0.03134   32.40   <2e-16 ***
## x            0.53359    0.03044   17.53   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3133 on 98 degrees of freedom
## Multiple R-squared:  0.7582,	Adjusted R-squared:  0.7557 
## F-statistic: 307.3 on 1 and 98 DF,  p-value: < 2.2e-16
```

```r
confint(lm13h)
```

```
##                 2.5 %    97.5 %
## (Intercept) 0.9531317 1.0775107
## x           0.4731823 0.5939962
```


```r
data13h %>%
  ggplot(aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(aes(color="blue"),method="lm",se=FALSE) +
  geom_abline(aes(intercept=1,slope=.5,color="red")) +
  scale_color_manual(name="legend",values=c("red"="red","blue"="blue"),labels=c("population","sample"), guide="legend")
```

![](Chapter_3_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

smaller confidence intervals, lower RSE, higher R^2

_(i) Repeat (a)–(f) after modifying the data generation process in such a way that there is more noise in the data. The model (3.39) should remain the same. You can do this by increasing the variance of the normal distribution used to generate the error term ε in (b). Describe your results._


```r
data13i <- tibble(
  x = rnorm(100,mean = 0, sd = 1),
  eps = rnorm(100,0, sqrt(0.5)),
  y = 1 + 0.5*x + eps)

data13h
```

```
## # A tibble: 100 x 3
##              x         eps         y
##          <dbl>       <dbl>     <dbl>
##  1  0.40940184  0.28260444 1.4873054
##  2  1.68887329 -0.33118475 1.5132519
##  3  1.58658843  0.62339162 2.4166858
##  4 -0.33090780 -0.12131512 0.7132310
##  5 -2.28523554  0.52308667 0.3804689
##  6  2.49766159  0.47820364 2.7270344
##  7  0.66706617  0.02623607 1.3597692
##  8  0.54132734  0.17937100 1.4500347
##  9 -0.01339952 -0.32399068 0.6693096
## 10  0.51010842  0.10214362 1.3571978
## # ... with 90 more rows
```


```r
lm13i <- lm(y ~ x, data=data13i)
summary(lm13i)
```

```
## 
## Call:
## lm(formula = y ~ x, data = data13i)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.7749 -0.4281  0.0146  0.4984  1.4777 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.96645    0.06842  14.125  < 2e-16 ***
## x            0.44700    0.05876   7.607 1.73e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6838 on 98 degrees of freedom
## Multiple R-squared:  0.3713,	Adjusted R-squared:  0.3648 
## F-statistic: 57.87 on 1 and 98 DF,  p-value: 1.731e-11
```

```r
confint(lm13i)
```

```
##                 2.5 %    97.5 %
## (Intercept) 0.8306640 1.1022281
## x           0.3303927 0.5636136
```


```r
data13i %>%
  ggplot(aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(aes(color="blue"),method="lm",se=FALSE) +
  geom_abline(aes(intercept=1,slope=.5,color="red")) +
  scale_color_manual(name="legend",values=c("red"="red","blue"="blue"),labels=c("population","sample"), guide="legend")
```

![](Chapter_3_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

_(j) What are the confidence intervals for β0 and β1 based on the original data set, the noisier data set, and the less noisy data set? Comment on your results._

Already discussed above in part, but...


```r
map(list(original=lm13,lower_var=lm13h,higher_var=lm13i),confint)
```

```
## $original
##                 2.5 %    97.5 %
## (Intercept) 0.8849196 1.0773878
## x           0.3925794 0.6063602
## 
## $lower_var
##                 2.5 %    97.5 %
## (Intercept) 0.9531317 1.0775107
## x           0.4731823 0.5939962
## 
## $higher_var
##                 2.5 %    97.5 %
## (Intercept) 0.8306640 1.1022281
## x           0.3303927 0.5636136
```

```r
map(list(original=lm13,lower_var=lm13h,higher_var=lm13i),confint) %>%
  map(as.data.frame) %>%
  map(rownames_to_column,var="coefficient") %>%
  bind_rows(.id="data_source") %>%
  ggplot(aes(x=data_source,ymin=`2.5 %`,ymax=`97.5 %`,color=coefficient)) +
  geom_linerange(lwd=3) + 
  ggtitle("95% Confidence Intervals") +
  geom_hline(yintercept = c(0.5,1))
```

![](Chapter_3_files/figure-html/unnamed-chunk-33-1.png)<!-- -->




## Q14. This problem focuses on the collinearity problem.
_(a) Perform the following commands in R:_


```r
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
```

_The last line corresponds to creating a linear model in which y is a function of x1 and x2. Write out the form of the linear model. What are the regression coefficients?_

$$
y = \beta_0 + \beta_1 * x1 + \beta_2 * x2 + \epsilon
$$
$$
where \beta_0=2, \beta_1=2, and \beta_2=0.3
$$
_(b) What is the correlation between x1 and x2? Create a scatterplot displaying the relationship between the variables._

```r
cor(x1,x2)
```

```
## [1] 0.8351212
```

```r
plot(x1,x2)
```

![](Chapter_3_files/figure-html/unnamed-chunk-35-1.png)<!-- -->


_(c) Using this data, fit a least squares regression to predict y using x1 and x2. Describe the results obtained. What are βˆ0, βˆ1, and βˆ2? How do these relate to the true β0, β1, and β2? Can you reject the null hypothesis H0 : β1 = 0? How about the null hypothesis H0 : β2 = 0?_


```r
lm14c <- lm(y ~ x1 + x2)
summary(lm14c)
```

```
## 
## Call:
## lm(formula = y ~ x1 + x2)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8311 -0.7273 -0.0537  0.6338  2.3359 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.1305     0.2319   9.188 7.61e-15 ***
## x1            1.4396     0.7212   1.996   0.0487 *  
## x2            1.0097     1.1337   0.891   0.3754    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.056 on 97 degrees of freedom
## Multiple R-squared:  0.2088,	Adjusted R-squared:  0.1925 
## F-statistic:  12.8 on 2 and 97 DF,  p-value: 1.164e-05
```

$\beta_1$ is underestimated and $\beta_2$ is overestimated.  Overall poor fit based on R-squared.  $\beta_1$ significant but not $\beta_2$ 

_(d) Now fit a least squares regression to predict y using only x1. Comment on your results. Can you reject the null hypothesis H0 :β1 =0?_


```r
lm14d <- lm(y ~ x1)
summary(lm14d)
```

```
## 
## Call:
## lm(formula = y ~ x1)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.89495 -0.66874 -0.07785  0.59221  2.45560 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.1124     0.2307   9.155 8.27e-15 ***
## x1            1.9759     0.3963   4.986 2.66e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.055 on 98 degrees of freedom
## Multiple R-squared:  0.2024,	Adjusted R-squared:  0.1942 
## F-statistic: 24.86 on 1 and 98 DF,  p-value: 2.661e-06
```

R2 is the same, but now $\beta_1$ is estimated much better.

_(e) Now fit a least squares regression to predict y using only x2. Comment on your results. Can you reject the null hypothesis H0 :β1 =0?_



```r
lm14e <- lm(y ~ x2)
summary(lm14e)
```

```
## 
## Call:
## lm(formula = y ~ x2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.62687 -0.75156 -0.03598  0.72383  2.44890 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.3899     0.1949   12.26  < 2e-16 ***
## x2            2.8996     0.6330    4.58 1.37e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.072 on 98 degrees of freedom
## Multiple R-squared:  0.1763,	Adjusted R-squared:  0.1679 
## F-statistic: 20.98 on 1 and 98 DF,  p-value: 1.366e-05
```

R2 is less; $\beta_2$ now even more overestimated, but it is significant.

_(f) Do the results obtained in (c)–(e) contradict each other? Explain your answer._

Yes, in that $\beta_2$ is not significant in c but it is in e.  This is because of the co-linearity.  

_(g) Now suppose we obtain one additional observation, which was unfortunately mismeasured._


```r
x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)
```


_Re-fit the linear models from (c) to (e) using this new data. What effect does this new observation have on the each of the models? In each model, is this observation an outlier? A high-leverage point? Both? Explain your answers._


```r
lm14c2 <- lm(y ~ x1 + x2)
summary(lm14c2)
```

```
## 
## Call:
## lm(formula = y ~ x1 + x2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.73348 -0.69318 -0.05263  0.66385  2.30619 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.2267     0.2314   9.624 7.91e-16 ***
## x1            0.5394     0.5922   0.911  0.36458    
## x2            2.5146     0.8977   2.801  0.00614 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.075 on 98 degrees of freedom
## Multiple R-squared:  0.2188,	Adjusted R-squared:  0.2029 
## F-statistic: 13.72 on 2 and 98 DF,  p-value: 5.564e-06
```

```r
plot(lm14c2)
```

![](Chapter_3_files/figure-html/unnamed-chunk-40-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-40-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-40-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-40-4.png)<!-- -->


```r
lm14d2 <- lm(y ~ x1)
summary(lm14d2)
```

```
## 
## Call:
## lm(formula = y ~ x1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8897 -0.6556 -0.0909  0.5682  3.5665 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.2569     0.2390   9.445 1.78e-15 ***
## x1            1.7657     0.4124   4.282 4.29e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.111 on 99 degrees of freedom
## Multiple R-squared:  0.1562,	Adjusted R-squared:  0.1477 
## F-statistic: 18.33 on 1 and 99 DF,  p-value: 4.295e-05
```

```r
plot(lm14d2)
```

![](Chapter_3_files/figure-html/unnamed-chunk-41-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-41-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-41-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-41-4.png)<!-- -->


```r
lm14e2 <- lm(y ~ x2)
summary(lm14e2)
```

```
## 
## Call:
## lm(formula = y ~ x2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.64729 -0.71021 -0.06899  0.72699  2.38074 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.3451     0.1912  12.264  < 2e-16 ***
## x2            3.1190     0.6040   5.164 1.25e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.074 on 99 degrees of freedom
## Multiple R-squared:  0.2122,	Adjusted R-squared:  0.2042 
## F-statistic: 26.66 on 1 and 99 DF,  p-value: 1.253e-06
```

```r
plot(lm14e2)
```

![](Chapter_3_files/figure-html/unnamed-chunk-42-1.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-42-2.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-42-3.png)<!-- -->![](Chapter_3_files/figure-html/unnamed-chunk-42-4.png)<!-- -->

This point is a high-leverage outlier.  It flips which coefficient is significant in the MR

## 15a

_15. This problem involves the Boston data set, which we saw in the lab for this chapter. We will now try to predict per capita crime rate using the other variables in this data set. In other words, per capita crime rate is the response, and the other variables are the predictors._

_(a) For each predictor, fit a simple linear regression model to predict the response. Describe your results. In which of the models is there a statistically significant association between the predictor and the response? Create some plots to back up your assertions._


```r
data(Boston)
boston <- as_tibble(Boston)
boston
```

```
## # A tibble: 506 x 14
##       crim    zn indus  chas   nox    rm   age    dis   rad   tax ptratio
##  *   <dbl> <dbl> <dbl> <int> <dbl> <dbl> <dbl>  <dbl> <int> <dbl>   <dbl>
##  1 0.00632  18.0  2.31     0 0.538 6.575  65.2 4.0900     1   296    15.3
##  2 0.02731   0.0  7.07     0 0.469 6.421  78.9 4.9671     2   242    17.8
##  3 0.02729   0.0  7.07     0 0.469 7.185  61.1 4.9671     2   242    17.8
##  4 0.03237   0.0  2.18     0 0.458 6.998  45.8 6.0622     3   222    18.7
##  5 0.06905   0.0  2.18     0 0.458 7.147  54.2 6.0622     3   222    18.7
##  6 0.02985   0.0  2.18     0 0.458 6.430  58.7 6.0622     3   222    18.7
##  7 0.08829  12.5  7.87     0 0.524 6.012  66.6 5.5605     5   311    15.2
##  8 0.14455  12.5  7.87     0 0.524 6.172  96.1 5.9505     5   311    15.2
##  9 0.21124  12.5  7.87     0 0.524 5.631 100.0 6.0821     5   311    15.2
## 10 0.17004  12.5  7.87     0 0.524 6.004  85.9 6.5921     5   311    15.2
## # ... with 496 more rows, and 3 more variables: black <dbl>, lstat <dbl>,
## #   medv <dbl>
```


```r
predictors <- colnames(boston)[-1]
lmfits <- map(predictors,function(x) lm(crim ~ get(x), data=boston))
```


```r
lmsummaries <- lapply(lmfits,summary)
names(lmsummaries) <- predictors
lmsummaries
```

```
## $zn
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.429 -4.222 -2.620  1.250 84.523 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.45369    0.41722  10.675  < 2e-16 ***
## get(x)      -0.07393    0.01609  -4.594 5.51e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.435 on 504 degrees of freedom
## Multiple R-squared:  0.04019,	Adjusted R-squared:  0.03828 
## F-statistic:  21.1 on 1 and 504 DF,  p-value: 5.506e-06
## 
## 
## $indus
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.972  -2.698  -0.736   0.712  81.813 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.06374    0.66723  -3.093  0.00209 ** 
## get(x)       0.50978    0.05102   9.991  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.866 on 504 degrees of freedom
## Multiple R-squared:  0.1653,	Adjusted R-squared:  0.1637 
## F-statistic: 99.82 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## $chas
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.738 -3.661 -3.435  0.018 85.232 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3.7444     0.3961   9.453   <2e-16 ***
## get(x)       -1.8928     1.5061  -1.257    0.209    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.597 on 504 degrees of freedom
## Multiple R-squared:  0.003124,	Adjusted R-squared:  0.001146 
## F-statistic: 1.579 on 1 and 504 DF,  p-value: 0.2094
## 
## 
## $nox
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.371  -2.738  -0.974   0.559  81.728 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -13.720      1.699  -8.073 5.08e-15 ***
## get(x)        31.249      2.999  10.419  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.81 on 504 degrees of freedom
## Multiple R-squared:  0.1772,	Adjusted R-squared:  0.1756 
## F-statistic: 108.6 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## $rm
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.604 -3.952 -2.654  0.989 87.197 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   20.482      3.365   6.088 2.27e-09 ***
## get(x)        -2.684      0.532  -5.045 6.35e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.401 on 504 degrees of freedom
## Multiple R-squared:  0.04807,	Adjusted R-squared:  0.04618 
## F-statistic: 25.45 on 1 and 504 DF,  p-value: 6.347e-07
## 
## 
## $age
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.789 -4.257 -1.230  1.527 82.849 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.77791    0.94398  -4.002 7.22e-05 ***
## get(x)       0.10779    0.01274   8.463 2.85e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.057 on 504 degrees of freedom
## Multiple R-squared:  0.1244,	Adjusted R-squared:  0.1227 
## F-statistic: 71.62 on 1 and 504 DF,  p-value: 2.855e-16
## 
## 
## $dis
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.708 -4.134 -1.527  1.516 81.674 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   9.4993     0.7304  13.006   <2e-16 ***
## get(x)       -1.5509     0.1683  -9.213   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.965 on 504 degrees of freedom
## Multiple R-squared:  0.1441,	Adjusted R-squared:  0.1425 
## F-statistic: 84.89 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## $rad
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.164  -1.381  -0.141   0.660  76.433 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.28716    0.44348  -5.157 3.61e-07 ***
## get(x)       0.61791    0.03433  17.998  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.718 on 504 degrees of freedom
## Multiple R-squared:  0.3913,	Adjusted R-squared:   0.39 
## F-statistic: 323.9 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## $tax
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.513  -2.738  -0.194   1.065  77.696 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -8.528369   0.815809  -10.45   <2e-16 ***
## get(x)       0.029742   0.001847   16.10   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.997 on 504 degrees of freedom
## Multiple R-squared:  0.3396,	Adjusted R-squared:  0.3383 
## F-statistic: 259.2 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## $ptratio
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -7.654 -3.985 -1.912  1.825 83.353 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -17.6469     3.1473  -5.607 3.40e-08 ***
## get(x)        1.1520     0.1694   6.801 2.94e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.24 on 504 degrees of freedom
## Multiple R-squared:  0.08407,	Adjusted R-squared:  0.08225 
## F-statistic: 46.26 on 1 and 504 DF,  p-value: 2.943e-11
## 
## 
## $black
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.756  -2.299  -2.095  -1.296  86.822 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 16.553529   1.425903  11.609   <2e-16 ***
## get(x)      -0.036280   0.003873  -9.367   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.946 on 504 degrees of freedom
## Multiple R-squared:  0.1483,	Adjusted R-squared:  0.1466 
## F-statistic: 87.74 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## $lstat
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.925  -2.822  -0.664   1.079  82.862 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.33054    0.69376  -4.801 2.09e-06 ***
## get(x)       0.54880    0.04776  11.491  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.664 on 504 degrees of freedom
## Multiple R-squared:  0.2076,	Adjusted R-squared:  0.206 
## F-statistic:   132 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## $medv
## 
## Call:
## lm(formula = crim ~ get(x), data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.071 -4.022 -2.343  1.298 80.957 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 11.79654    0.93419   12.63   <2e-16 ***
## get(x)      -0.36316    0.03839   -9.46   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.934 on 504 degrees of freedom
## Multiple R-squared:  0.1508,	Adjusted R-squared:  0.1491 
## F-statistic: 89.49 on 1 and 504 DF,  p-value: < 2.2e-16
```


```r
sapply(lmsummaries, function(x) x$coefficients["get(x)","Pr(>|t|)"])
```

```
##           zn        indus         chas          nox           rm 
## 5.506472e-06 1.450349e-21 2.094345e-01 3.751739e-23 6.346703e-07 
##          age          dis          rad          tax      ptratio 
## 2.854869e-16 8.519949e-19 2.693844e-56 2.357127e-47 2.942922e-11 
##        black        lstat         medv 
## 2.487274e-19 2.654277e-27 1.173987e-19
```


```r
sapply(lmsummaries, function(x) x$adj.r.squared)
```

```
##         zn      indus       chas        nox         rm        age 
## 0.03828352 0.16365394 0.00114594 0.17558468 0.04618036 0.12268419 
##        dis        rad        tax    ptratio      black      lstat 
## 0.14245126 0.39004886 0.33830395 0.08225111 0.14658431 0.20601869 
##       medv 
## 0.14909551
```

### OR

(see http://r4ds.had.co.nz/many-models.html)

create a nested data object, one data frame for each predictor

```r
library(modelr)
boston.nest <- boston %>% 
  gather(key="predictor",value="value",-crim) %>%
  group_by(predictor) %>%
  nest()
boston.nest # a set of data frames, one for each predictor
```

```
## # A tibble: 13 x 2
##    predictor               data
##        <chr>             <list>
##  1        zn <tibble [506 x 2]>
##  2     indus <tibble [506 x 2]>
##  3      chas <tibble [506 x 2]>
##  4       nox <tibble [506 x 2]>
##  5        rm <tibble [506 x 2]>
##  6       age <tibble [506 x 2]>
##  7       dis <tibble [506 x 2]>
##  8       rad <tibble [506 x 2]>
##  9       tax <tibble [506 x 2]>
## 10   ptratio <tibble [506 x 2]>
## 11     black <tibble [506 x 2]>
## 12     lstat <tibble [506 x 2]>
## 13      medv <tibble [506 x 2]>
```

```r
boston.nest$data[[1]]
```

```
## # A tibble: 506 x 2
##       crim value
##      <dbl> <dbl>
##  1 0.00632  18.0
##  2 0.02731   0.0
##  3 0.02729   0.0
##  4 0.03237   0.0
##  5 0.06905   0.0
##  6 0.02985   0.0
##  7 0.08829  12.5
##  8 0.14455  12.5
##  9 0.21124  12.5
## 10 0.17004  12.5
## # ... with 496 more rows
```

Fit a model to each dataframe

```r
fitModel <- function(df) lm(crim ~ value, data=df)
boston.nest <- boston.nest%>%
  mutate(model=map(data,fitModel),
         model.summary=map(model,summary))
boston.nest
```

```
## # A tibble: 13 x 4
##    predictor               data    model    model.summary
##        <chr>             <list>   <list>           <list>
##  1        zn <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
##  2     indus <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
##  3      chas <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
##  4       nox <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
##  5        rm <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
##  6       age <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
##  7       dis <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
##  8       rad <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
##  9       tax <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
## 10   ptratio <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
## 11     black <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
## 12     lstat <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
## 13      medv <tibble [506 x 2]> <S3: lm> <S3: summary.lm>
```

Now, use broom::glance to get summaries

```r
library(broom)
boston_glance <- boston.nest %>%
  mutate(glance=map(model,glance)) %>%
  unnest(glance,.drop = TRUE)
boston_glance
```

```
## # A tibble: 13 x 12
##    predictor   r.squared adj.r.squared    sigma  statistic      p.value
##        <chr>       <dbl>         <dbl>    <dbl>      <dbl>        <dbl>
##  1        zn 0.040187908    0.03828352 8.435290  21.102782 5.506472e-06
##  2     indus 0.165310070    0.16365394 7.866281  99.817037 1.450349e-21
##  3      chas 0.003123869    0.00114594 8.596615   1.579364 2.094345e-01
##  4       nox 0.177217182    0.17558468 7.809972 108.555329 3.751739e-23
##  5        rm 0.048069117    0.04618036 8.400586  25.450204 6.346703e-07
##  6       age 0.124421452    0.12268419 8.056649  71.619402 2.854869e-16
##  7       dis 0.144149375    0.14245126 7.965369  84.887810 8.519949e-19
##  8       rad 0.391256687    0.39004886 6.717752 323.935172 2.693844e-56
##  9       tax 0.339614243    0.33830395 6.996901 259.190294 2.357127e-47
## 10   ptratio 0.084068439    0.08225111 8.240212  46.259453 2.942922e-11
## 11     black 0.148274239    0.14658431 7.946150  87.739763 2.487274e-19
## 12     lstat 0.207590933    0.20601869 7.664461 132.035125 2.654277e-27
## 13      medv 0.150780469    0.14909551 7.934451  89.486115 1.173987e-19
## # ... with 6 more variables: df <int>, logLik <dbl>, AIC <dbl>, BIC <dbl>,
## #   deviance <dbl>, df.residual <int>
```

### old way...

print out the summaries (but no names!)


```r
boston.nest$model.summary
```

```
## [[1]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.429 -4.222 -2.620  1.250 84.523 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.45369    0.41722  10.675  < 2e-16 ***
## value       -0.07393    0.01609  -4.594 5.51e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.435 on 504 degrees of freedom
## Multiple R-squared:  0.04019,	Adjusted R-squared:  0.03828 
## F-statistic:  21.1 on 1 and 504 DF,  p-value: 5.506e-06
## 
## 
## [[2]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.972  -2.698  -0.736   0.712  81.813 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.06374    0.66723  -3.093  0.00209 ** 
## value        0.50978    0.05102   9.991  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.866 on 504 degrees of freedom
## Multiple R-squared:  0.1653,	Adjusted R-squared:  0.1637 
## F-statistic: 99.82 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## [[3]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.738 -3.661 -3.435  0.018 85.232 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3.7444     0.3961   9.453   <2e-16 ***
## value        -1.8928     1.5061  -1.257    0.209    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.597 on 504 degrees of freedom
## Multiple R-squared:  0.003124,	Adjusted R-squared:  0.001146 
## F-statistic: 1.579 on 1 and 504 DF,  p-value: 0.2094
## 
## 
## [[4]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.371  -2.738  -0.974   0.559  81.728 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -13.720      1.699  -8.073 5.08e-15 ***
## value         31.249      2.999  10.419  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.81 on 504 degrees of freedom
## Multiple R-squared:  0.1772,	Adjusted R-squared:  0.1756 
## F-statistic: 108.6 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## [[5]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.604 -3.952 -2.654  0.989 87.197 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   20.482      3.365   6.088 2.27e-09 ***
## value         -2.684      0.532  -5.045 6.35e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.401 on 504 degrees of freedom
## Multiple R-squared:  0.04807,	Adjusted R-squared:  0.04618 
## F-statistic: 25.45 on 1 and 504 DF,  p-value: 6.347e-07
## 
## 
## [[6]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.789 -4.257 -1.230  1.527 82.849 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.77791    0.94398  -4.002 7.22e-05 ***
## value        0.10779    0.01274   8.463 2.85e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.057 on 504 degrees of freedom
## Multiple R-squared:  0.1244,	Adjusted R-squared:  0.1227 
## F-statistic: 71.62 on 1 and 504 DF,  p-value: 2.855e-16
## 
## 
## [[7]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.708 -4.134 -1.527  1.516 81.674 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   9.4993     0.7304  13.006   <2e-16 ***
## value        -1.5509     0.1683  -9.213   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.965 on 504 degrees of freedom
## Multiple R-squared:  0.1441,	Adjusted R-squared:  0.1425 
## F-statistic: 84.89 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## [[8]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.164  -1.381  -0.141   0.660  76.433 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.28716    0.44348  -5.157 3.61e-07 ***
## value        0.61791    0.03433  17.998  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.718 on 504 degrees of freedom
## Multiple R-squared:  0.3913,	Adjusted R-squared:   0.39 
## F-statistic: 323.9 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## [[9]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.513  -2.738  -0.194   1.065  77.696 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -8.528369   0.815809  -10.45   <2e-16 ***
## value        0.029742   0.001847   16.10   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.997 on 504 degrees of freedom
## Multiple R-squared:  0.3396,	Adjusted R-squared:  0.3383 
## F-statistic: 259.2 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## [[10]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -7.654 -3.985 -1.912  1.825 83.353 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -17.6469     3.1473  -5.607 3.40e-08 ***
## value         1.1520     0.1694   6.801 2.94e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.24 on 504 degrees of freedom
## Multiple R-squared:  0.08407,	Adjusted R-squared:  0.08225 
## F-statistic: 46.26 on 1 and 504 DF,  p-value: 2.943e-11
## 
## 
## [[11]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.756  -2.299  -2.095  -1.296  86.822 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 16.553529   1.425903  11.609   <2e-16 ***
## value       -0.036280   0.003873  -9.367   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.946 on 504 degrees of freedom
## Multiple R-squared:  0.1483,	Adjusted R-squared:  0.1466 
## F-statistic: 87.74 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## [[12]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.925  -2.822  -0.664   1.079  82.862 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.33054    0.69376  -4.801 2.09e-06 ***
## value        0.54880    0.04776  11.491  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.664 on 504 degrees of freedom
## Multiple R-squared:  0.2076,	Adjusted R-squared:  0.206 
## F-statistic:   132 on 1 and 504 DF,  p-value: < 2.2e-16
## 
## 
## [[13]]
## 
## Call:
## lm(formula = crim ~ value, data = df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.071 -4.022 -2.343  1.298 80.957 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 11.79654    0.93419   12.63   <2e-16 ***
## value       -0.36316    0.03839   -9.46   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.934 on 504 degrees of freedom
## Multiple R-squared:  0.1508,	Adjusted R-squared:  0.1491 
## F-statistic: 89.49 on 1 and 504 DF,  p-value: < 2.2e-16
```

We can add columns for whatever we want to extract from the summaries....

```r
boston.nest.sum <- boston.nest %>%
  mutate(p_value=map_dbl(model.summary,function(x) x$coefficients["value","Pr(>|t|)"]),
         adj.r.squared=map_dbl(model.summary, function(x) x$adj.r.squared))
boston.nest.sum
```

```
## # A tibble: 13 x 6
##    predictor               data    model    model.summary      p_value
##        <chr>             <list>   <list>           <list>        <dbl>
##  1        zn <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 5.506472e-06
##  2     indus <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 1.450349e-21
##  3      chas <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 2.094345e-01
##  4       nox <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 3.751739e-23
##  5        rm <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 6.346703e-07
##  6       age <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 2.854869e-16
##  7       dis <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 8.519949e-19
##  8       rad <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 2.693844e-56
##  9       tax <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 2.357127e-47
## 10   ptratio <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 2.942922e-11
## 11     black <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 2.487274e-19
## 12     lstat <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 2.654277e-27
## 13      medv <tibble [506 x 2]> <S3: lm> <S3: summary.lm> 1.173987e-19
## # ... with 1 more variables: adj.r.squared <dbl>
```

(b) Fit a multiple regression model to predict the response using all of the predictors. Describe your results. For which predictors can we reject the null hypothesis H0 : βj = 0?


```r
library(stringr)
fo <- as.formula(str_c("crim ~ ", str_c(predictors, collapse = " + ")))
lm15b <- lm(fo,data = boston)
summary(lm15b)
```

```
## 
## Call:
## lm(formula = fo, data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.924 -2.120 -0.353  1.019 75.051 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  17.033228   7.234903   2.354 0.018949 *  
## zn            0.044855   0.018734   2.394 0.017025 *  
## indus        -0.063855   0.083407  -0.766 0.444294    
## chas         -0.749134   1.180147  -0.635 0.525867    
## nox         -10.313535   5.275536  -1.955 0.051152 .  
## rm            0.430131   0.612830   0.702 0.483089    
## age           0.001452   0.017925   0.081 0.935488    
## dis          -0.987176   0.281817  -3.503 0.000502 ***
## rad           0.588209   0.088049   6.680 6.46e-11 ***
## tax          -0.003780   0.005156  -0.733 0.463793    
## ptratio      -0.271081   0.186450  -1.454 0.146611    
## black        -0.007538   0.003673  -2.052 0.040702 *  
## lstat         0.126211   0.075725   1.667 0.096208 .  
## medv         -0.198887   0.060516  -3.287 0.001087 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.439 on 492 degrees of freedom
## Multiple R-squared:  0.454,	Adjusted R-squared:  0.4396 
## F-statistic: 31.47 on 13 and 492 DF,  p-value: < 2.2e-16
```
This model explains about 44% of the variance, with zn, dis, rad, black, and medv being significant predictors

_(c) How do your results from (a) compare to your results from (b)? Create a plot displaying the univariate regression coefficients from (a) on the x-axis, and the multiple regression coefficients from (b) on the y-axis. That is, each predictor is displayed as a single point in the plot. Its coefficient in a simple linear regres- sion model is shown on the x-axis, and its coefficient estimate in the multiple linear regression model is shown on the y-axis._

get the simple model regression coefficients

```r
coefs <- boston.nest %>%
  mutate(coefficients=map(model,tidy)) %>%
  unnest(coefficients,.drop=TRUE) %>%
  filter(term=="value") %>%
  dplyr::select(predictor,simple.est=estimate)
coefs
```

```
## # A tibble: 13 x 2
##    predictor  simple.est
##        <chr>       <dbl>
##  1        zn -0.07393498
##  2     indus  0.50977633
##  3      chas -1.89277655
##  4       nox 31.24853120
##  5        rm -2.68405122
##  6       age  0.10778623
##  7       dis -1.55090168
##  8       rad  0.61791093
##  9       tax  0.02974225
## 10   ptratio  1.15198279
## 11     black -0.03627964
## 12     lstat  0.54880478
## 13      medv -0.36315992
```

add the multiple regression coefficients

```r
coefs <- as.data.frame(coef(lm15b)) %>%
  rownames_to_column(var="predictor") %>%
  rename(multiple.est=`coef(lm15b)`) %>%
  right_join(coefs)
```

```
## Joining, by = "predictor"
```
plot it

```r
coefs %>% 
  ggplot(aes(x=simple.est,y=multiple.est,label=predictor)) +
  geom_point() +
  geom_text(nudge_x = 1)
```

![](Chapter_3_files/figure-html/unnamed-chunk-56-1.png)<!-- -->

Whoa, radically different!

(d) Is there evidence of non-linear association between any of the predictors and the response? To answer this question, for each predictor X, fit a model of the form
Y = β0 +β1X +β2X2 +β3X3 +ε.


```r
fitModelNL <- function(df) lm(crim ~ value + I(value^2) + I(value^3), data=df)
boston.nest <- boston.nest %>%
  mutate(modelNL=map(data,fitModelNL))
boston.nest  
```

```
## # A tibble: 13 x 5
##    predictor               data    model    model.summary  modelNL
##        <chr>             <list>   <list>           <list>   <list>
##  1        zn <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
##  2     indus <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
##  3      chas <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
##  4       nox <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
##  5        rm <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
##  6       age <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
##  7       dis <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
##  8       rad <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
##  9       tax <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
## 10   ptratio <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
## 11     black <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
## 12     lstat <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
## 13      medv <tibble [506 x 2]> <S3: lm> <S3: summary.lm> <S3: lm>
```


```r
boston_glanceNL <- boston.nest %>%
  mutate(glanceNL=map(modelNL,glance)) %>%
  unnest(glanceNL,.drop = TRUE) %>%
right_join(boston_glance,by="predictor",suffix=c(".poly",".linear"))

boston_glanceNL %>%
  ggplot(aes(x=adj.r.squared.linear,y=adj.r.squared.poly,label=predictor)) +
  geom_point() +
  geom_text(nudge_x = .015) +
  geom_abline(slope = 1, intercept = 0)
```

![](Chapter_3_files/figure-html/unnamed-chunk-58-1.png)<!-- -->

Improved fit for some.
