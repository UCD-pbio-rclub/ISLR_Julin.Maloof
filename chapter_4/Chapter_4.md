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
mean(mod10d.pred!=test$Direction)
```

```
## [1] 0.5576923
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

OK so now it does really poorly!  56% error rate

_(e) Repeat (d) using LDA._


```r
library(MASS)
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
mod10e <- lda(Direction ~ Lag2, data=train)
mod10e
```

```
## Call:
## lda(Direction ~ Lag2, data = train)
## 
## Prior probabilities of groups:
##      Down        Up 
## 0.4477157 0.5522843 
## 
## Group means:
##             Lag2
## Down -0.03568254
## Up    0.26036581
## 
## Coefficients of linear discriminants:
##            LD1
## Lag2 0.4414162
```

```r
plot(mod10e)
```

![](Chapter_4_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
pred.10e <- predict(mod10e,newdata = test)
table(pred.10e$class,test$Direction)
```

```
##       
##        Down Up
##   Down    9  5
##   Up     34 56
```

```r
mean(pred.10e$class!=test$Direction)
```

```
## [1] 0.375
```
Error rate drops to 37.5%.  False positive rate has dropped dramtically, but the False Negative rate has increased.

_(f) Repeat (d) using QDA._

_(g) Repeat (d) using KNN with K = 1._

_(h) Which of these methods appears to provide the best results on this data?_

_(i) Experiment with different combinations of predictors, including possible transformations and interactions, for each of the methods. Report the variables, method, and associated confusion matrix that appears to provide the best results on the held out data. Note that you should also experiment with values for K in the KNN classifier._
 
 ## Q 11 a-c,f
 
_(a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median. You can compute the median using the median() function. Note you may find it helpful to use the data.frame() function to create a single data set containing both mpg01 and the other Auto variables._


```r
library(ISLR)
library(tidyverse)
data(Auto)
auto <- as.tibble(Auto)
auto <- auto %>% mutate(mpg01=ifelse(mpg>median(mpg),1,0))
auto
```

```
## # A tibble: 392 x 10
##      mpg cylinders displacement horsepower weight acceleration  year
##    <dbl>     <dbl>        <dbl>      <dbl>  <dbl>        <dbl> <dbl>
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
## # ... with 382 more rows, and 3 more variables: origin <dbl>, name <fctr>,
## #   mpg01 <dbl>
```


_(b) Explore the data graphically in order to investigate the association between mpg01 and the other features. Which of the other features seem most likely to be useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question. Describe your findings._


```r
auto %>% dplyr::select(-mpg, -name) %>%
  gather(key="key", value="value", -mpg01) %>%
  ggplot(aes(x=as.factor(mpg01),y=value,color=as.factor(mpg01))) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free")
```

![](Chapter_4_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


_(c) Split the data into a training set and a test set._


```r
nrow(auto) #392
```

```
## [1] 392
```

```r
set.seed(123)
test.index <- sample(c(0,1),size = nrow(auto), replace = TRUE,prob = c(.75,.25)) 

auto.train <- auto[test.index==0,]
auto.test <- auto[test.index==1,]
```


_(d) Perform LDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?_


```r
mod11d <- auto.train %>% dplyr::select(-acceleration, -mpg, -name) %>%
  lda(mpg01 ~ . , data = .)
mod11d
```

```
## Call:
## lda(mpg01 ~ ., data = .)
## 
## Prior probabilities of groups:
##         0         1 
## 0.5050505 0.4949495 
## 
## Group means:
##   cylinders displacement horsepower   weight     year   origin
## 0  6.746667     273.2267  129.58000 3626.520 74.39333 1.160000
## 1  4.149660     113.6259   79.07483 2322.714 77.57143 2.013605
## 
## Coefficients of linear discriminants:
##                       LD1
## cylinders    -0.491278370
## displacement -0.001471414
## horsepower    0.015765991
## weight       -0.001227812
## year          0.129503798
## origin        0.156495445
```

```r
plot(mod11d)
```

![](Chapter_4_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
pred.11d <- auto.test %>% dplyr::select(-acceleration, -mpg, -name) %>%
  predict(mod11d, .)
table(pred.11d$class, auto.test$mpg01)
```

```
##    
##      0  1
##   0 38  5
##   1  8 44
```

```r
mean(pred.11d$class!=auto.test$mpg01)
```

```
## [1] 0.1368421
```


_(e) Perform QDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?_

_(f) Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?_


```r
mod11f <- auto.train %>% dplyr::select(-acceleration, -mpg, -name) %>%
  glm(mpg01 ~ . , data = ., family = "binomial")
summary(mod11f)
```

```
## 
## Call:
## glm(formula = mpg01 ~ ., family = "binomial", data = .)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.37593  -0.06007  -0.00005   0.16481   2.37014  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -21.953771   6.326524  -3.470  0.00052 ***
## cylinders      0.238961   0.578418   0.413  0.67951    
## displacement  -0.019802   0.018692  -1.059  0.28941    
## horsepower    -0.037802   0.023631  -1.600  0.10967    
## weight        -0.004373   0.001349  -3.242  0.00119 ** 
## year           0.517378   0.101596   5.093 3.53e-07 ***
## origin         0.215269   0.442729   0.486  0.62680    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 411.70  on 296  degrees of freedom
## Residual deviance: 100.63  on 290  degrees of freedom
## AIC: 114.63
## 
## Number of Fisher Scoring iterations: 8
```


```r
mod11f.pred <- auto.test %>% dplyr::select(-acceleration, -mpg, -name) %>%
  predict(mod11f, ., type = "response")
mod11f.pred <- ifelse(mod11f.pred>0.5,1,0)
table(mod11f.pred,auto.test$mpg01)
```

```
##            
## mod11f.pred  0  1
##           0 40  7
##           1  6 42
```


```r
mean(mod11f.pred!=auto.test$mpg01)
```

```
## [1] 0.1368421
```

compared to LDA, same error rate, but lower sensitivity (worse false positive), better specificity (better false negative).

_(g) Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the variables that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of K seems to perform the best on this data set?_
 
