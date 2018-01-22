# Chapter 4
Julin N Maloof  
1/21/2018  




 ## Q6
 
_Suppose we collect data for a group of students in a statistics class with variables X1 = hours studied, X2 = undergrad GPA, and Y = receive an A. We fit a logistic regression and produce estimated coefficient, βˆ0 = −6, βˆ1 = 0.05, βˆ2 = 1_

_(a) Estimate the probability that a student who studies for 40 h and has an undergrad GPA of 3.5 gets an A in the class._


```r
exp(-6 + 0.05*40 +1*3.5) /
  (1 + exp(-6 + 0.05*40 +1*3.5) )
```

```
## [1] 0.3775407
```


_(b) How many hours would the student in part (a) need to study to have a 50 % chance of getting an A in the class?_

Easier for me to think about this if I log transform the equation

log(0.5/0.5) = -6 + hours * 0.05 + 3.5


```r
log(1) # 0
```

```
## [1] 0
```

```r
hours <- (6 -3.5) / 0.05

hours
```

```
## [1] 50
```

50 hours

 
 ## Q 10 a-d
 
 _10. This question should be answered using the Weekly data set, which is part of the ISLR package. This data is similar in nature to the Smarket data from this chapter’s lab, except that it contains 1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010._
 
_(a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?_


```r
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
```

```r
data("Weekly")
?Weekly
weekly <- as.tibble(Weekly)
weekly
```

```
## # A tibble: 1,089 x 9
##     Year   Lag1   Lag2   Lag3   Lag4   Lag5    Volume  Today Direction
##  * <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>     <dbl>  <dbl>    <fctr>
##  1  1990  0.816  1.572 -3.936 -0.229 -3.484 0.1549760 -0.270      Down
##  2  1990 -0.270  0.816  1.572 -3.936 -0.229 0.1485740 -2.576      Down
##  3  1990 -2.576 -0.270  0.816  1.572 -3.936 0.1598375  3.514        Up
##  4  1990  3.514 -2.576 -0.270  0.816  1.572 0.1616300  0.712        Up
##  5  1990  0.712  3.514 -2.576 -0.270  0.816 0.1537280  1.178        Up
##  6  1990  1.178  0.712  3.514 -2.576 -0.270 0.1544440 -1.372      Down
##  7  1990 -1.372  1.178  0.712  3.514 -2.576 0.1517220  0.807        Up
##  8  1990  0.807 -1.372  1.178  0.712  3.514 0.1323100  0.041        Up
##  9  1990  0.041  0.807 -1.372  1.178  0.712 0.1439720  1.253        Up
## 10  1990  1.253  0.041  0.807 -1.372  1.178 0.1336350 -2.678      Down
## # ... with 1,079 more rows
```

```r
summary(weekly)
```

```
##       Year           Lag1               Lag2               Lag3         
##  Min.   :1990   Min.   :-18.1950   Min.   :-18.1950   Min.   :-18.1950  
##  1st Qu.:1995   1st Qu.: -1.1540   1st Qu.: -1.1540   1st Qu.: -1.1580  
##  Median :2000   Median :  0.2410   Median :  0.2410   Median :  0.2410  
##  Mean   :2000   Mean   :  0.1506   Mean   :  0.1511   Mean   :  0.1472  
##  3rd Qu.:2005   3rd Qu.:  1.4050   3rd Qu.:  1.4090   3rd Qu.:  1.4090  
##  Max.   :2010   Max.   : 12.0260   Max.   : 12.0260   Max.   : 12.0260  
##       Lag4               Lag5              Volume       
##  Min.   :-18.1950   Min.   :-18.1950   Min.   :0.08747  
##  1st Qu.: -1.1580   1st Qu.: -1.1660   1st Qu.:0.33202  
##  Median :  0.2380   Median :  0.2340   Median :1.00268  
##  Mean   :  0.1458   Mean   :  0.1399   Mean   :1.57462  
##  3rd Qu.:  1.4090   3rd Qu.:  1.4050   3rd Qu.:2.05373  
##  Max.   : 12.0260   Max.   : 12.0260   Max.   :9.32821  
##      Today          Direction 
##  Min.   :-18.1950   Down:484  
##  1st Qu.: -1.1540   Up  :605  
##  Median :  0.2410             
##  Mean   :  0.1499             
##  3rd Qu.:  1.4050             
##  Max.   : 12.0260
```

```r
pairs(weekly)
```

![](Chapter_4_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
weekly %>% mutate(Direction = as.numeric(Direction)) %>% cor() %>% round(2)
```

```
##            Year  Lag1  Lag2  Lag3  Lag4  Lag5 Volume Today Direction
## Year       1.00 -0.03 -0.03 -0.03 -0.03 -0.03   0.84 -0.03     -0.02
## Lag1      -0.03  1.00 -0.07  0.06 -0.07 -0.01  -0.06 -0.08     -0.05
## Lag2      -0.03 -0.07  1.00 -0.08  0.06 -0.07  -0.09  0.06      0.07
## Lag3      -0.03  0.06 -0.08  1.00 -0.08  0.06  -0.07 -0.07     -0.02
## Lag4      -0.03 -0.07  0.06 -0.08  1.00 -0.08  -0.06 -0.01     -0.02
## Lag5      -0.03 -0.01 -0.07  0.06 -0.08  1.00  -0.06  0.01     -0.02
## Volume     0.84 -0.06 -0.09 -0.07 -0.06 -0.06   1.00 -0.03     -0.02
## Today     -0.03 -0.08  0.06 -0.07 -0.01  0.01  -0.03  1.00      0.72
## Direction -0.02 -0.05  0.07 -0.02 -0.02 -0.02  -0.02  0.72      1.00
```

Volume and year are correlated, and Today and direction, but that is no surprise...overall not much going on

_(b) Use the full data set to perform a logistic regression with Direction as the response and the five lag variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors appear to be statistically significant? If so, which ones?_


```r
mod10b <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=weekly,family = "binomial")
summary(mod10b)
```

```
## 
## Call:
## glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
##     Volume, family = "binomial", data = weekly)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.6949  -1.2565   0.9913   1.0849   1.4579  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)  0.26686    0.08593   3.106   0.0019 **
## Lag1        -0.04127    0.02641  -1.563   0.1181   
## Lag2         0.05844    0.02686   2.175   0.0296 * 
## Lag3        -0.01606    0.02666  -0.602   0.5469   
## Lag4        -0.02779    0.02646  -1.050   0.2937   
## Lag5        -0.01447    0.02638  -0.549   0.5833   
## Volume      -0.02274    0.03690  -0.616   0.5377   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1496.2  on 1088  degrees of freedom
## Residual deviance: 1486.4  on 1082  degrees of freedom
## AIC: 1500.4
## 
## Number of Fisher Scoring iterations: 4
```

2 weeks prior has some predicitive value...

_(c) Compute the confusion matrix and overall fraction of correct predictions. Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression._



```r
mod10b.pred <- predict(mod10b,type = "response")
head(mod10b.pred)
```

```
##         1         2         3         4         5         6 
## 0.6086249 0.6010314 0.5875699 0.4816416 0.6169013 0.5684190
```

```r
mod10b.pred <- ifelse(mod10b.pred>.5,"Up","Down")
table(mod10b.pred,weekly$Direction)
```

```
##            
## mod10b.pred Down  Up
##        Down   54  48
##        Up    430 557
```

```r
mean(mod10b.pred==weekly$Direction)
```

```
## [1] 0.5610652
```

56.1% of the predictions were correct.  This model does a pretty good job of predicting up weeks, that is it correctly called 557 of 605 or 92.07% of the up markets (Good sensitivity).  However it has lousy specificity, classifying 430 of the 484 down weeks incorrectly. The specificity is 11.16%

_(d) Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions for the held out data (that is, the data from 2009 and 2010)._


```r
train <- weekly %>% filter(Year<2009)
test <- weekly %>% filter(Year>=2009)
mod10d <- glm(Direction ~ Lag2, data=train, family="binomial")
mod10d.pred <- predict(mod10d,newdata = test, response="binomial")
mod10d.pred <- ifelse(mod10d.pred>.5,"Up","Down")
mean(mod10d.pred==test$Direction)
```

```
## [1] 0.4423077
```

```r
table(mod10d.pred, test$Direction)
```

```
##            
## mod10d.pred Down Up
##        Down   41 56
##        Up      2  5
```

OK so now it does really poorly!

_(e) Repeat (d) using LDA._

_(f) Repeat (d) using QDA._

_(g) Repeat (d) using KNN with K = 1._

_(h) Which of these methods appears to provide the best results on this data?_

_(i) Experiment with different combinations of predictors, includ- ing possible transformations and interactions, for each of the methods. Report the variables, method, and associated confu- sion matrix that appears to provide the best results on the held out data. Note that you should also experiment with values for K in the KNN classifier._
 
 ## Q 11 a-c,f
 
 (a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median. You can compute the median using the median() function. Note you may find it helpful to use the data.frame() function to create a single data set containing both mpg01 and the other Auto variables.
4.7 Exercises 171
172
4. Classification
(b) Explore the data graphically in order to investigate the associ- ation between mpg01 and the other features. Which of the other features seem most likely to be useful in predicting mpg01? Scat- terplots and boxplots may be useful tools to answer this ques- tion. Describe your findings.
(c) Split the data into a training set and a test set.
(d) Perform LDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?
(e) Perform QDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?
(f) Perform logistic regression on the training data in order to pre- dict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?
(g) Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the variables that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of K seems to perform the best on this data set?
 
