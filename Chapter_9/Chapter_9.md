---
title: "Untitled"
author: "Julin N Maloof"
date: "5/3/2018"
output: 
  html_document: 
    keep_md: yes
---




## 9.6.1


```r
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))
```

![](Chapter_9_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


```r
dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y ~ ., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svmfit , dat)
```

![](Chapter_9_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(svmfit)
```

```
## 
## Call:
## svm(formula = y ~ ., data = dat, kernel = "linear", cost = 10, 
##     scale = FALSE)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  10 
##       gamma:  0.5 
## 
## Number of Support Vectors:  7
## 
##  ( 4 3 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
```


1 (optional)
3
6
7 a,b
8 8-e

## Q1

Sketch the hyperplane 1 + 3X1 − X2 = 0. Indicate the set of points for which 1+3X1 −X2 > 0, as well as the set of points for which 1 + 3X1 − X2 < 0.

X2 = 3X1 + 1


```r
hyperplane <- tibble(
  X1=seq(-10,10,.1),
  X2=3*X1 + 1)

grid <- expand.grid(X1=-10:10,X2=seq(-25,25,5)) %>% as_tibble() %>%
  mutate(position=ifelse(1+3*X1-X2>0,"above",ifelse(1+3*X1-X2<0,"below","on")))
```


```r
pl <- hyperplane %>% ggplot(aes(x=X1,y=X2)) +
  geom_line() +
  geom_point(aes(color=position),data=grid)

pl
```

![](Chapter_9_files/figure-html/unnamed-chunk-4-1.png)<!-- -->




## Q3

Here we explore the maximal margin classifier on a toy data set.

(a) We are given n = 7 observations in p = 2 dimensions. For each observation, there is an associated class label.


```r
dataq3 <- tribble(
  ~X1, ~X2, ~Y,
  3, 4, "red",
  2, 2, "red",
  4, 4, "red",
  1, 4, "red",
  2, 1, "blue",
  4, 3, "blue",
  4, 1, "blue"
)
```

sketch the observations


```r
pl <- dataq3 %>%
  ggplot(aes(x=X1,y=X2,color=Y)) +
  scale_color_identity() +
  geom_point()
pl
```

![](Chapter_9_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

(b) Sketch the optimal separating hyperplane, and provide the equa- tion for this hyperplane (of the form (9.1)).

The hyperplane goes through  2, 1.5; 3, 2.5; 4,3.5etc,

X2 = X1 - 0.5

Or X1 - X2 -.5 = 0


```r
hyperplane <- tibble(X1=1:4,X2=X1-0.5)
pl <- pl + geom_line(data=hyperplane,color="black")
pl
```

![](Chapter_9_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


(C) Describe the classification rule for the maximal margin classifier. It should be something along the lines of “Classify to Red if β0 + β1X1 + β2X2 > 0, and classify to Blue otherwise.” Provide the values for β0, β1, and β2.

Classify to red if X1 - X2 -.5 > 0; otherwise to blue

(D)On your sketch, indicate the margin for the maximal margin hyperplane.


```r
upper_margin <- hyperplane %>% mutate(X2=X2 + .5)
lower_margin <- hyperplane %>% mutate(X2=X2 - .5)
pl <- pl +
  geom_line(data=upper_margin, color="gray30", lty=2) +
  geom_line(data=lower_margin, color="gray30", lty=2)
pl
```

![](Chapter_9_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

(E) Indicate the support vectors for the maximal margin classifier.  

These are the points touching the dotted lines

(f) Argue that a slight movement of the seventh observation would not affect the maximal margin hyperplane.

The seventh observation is far from the margins and this would not affect their placement.

(g) Sketch a hyperplane that is not the optimal separating hyper- plane, and provide the equation for this hyperplane.

1.1?*X1 - X2 -.5 = 0


```r
pl + geom_abline(intercept=-.5, slope=1.1, lty=3)
```

![](Chapter_9_files/figure-html/unnamed-chunk-10-1.png)<!-- -->




(h) Draw an additional observation on the plot so that the two classes are no longer separable by a hyperplane.


```r
pl + geom_point(x=3,y=1,color="red")
```

![](Chapter_9_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


## Q6

_At the end of Section 9.6.1, it is claimed that in the case of data that is just barely linearly separable, a support vector classifier with a small value of cost that misclassifies a couple of training observations may perform better on test data than one with a huge value of cost that does not misclassify any training observations. You will now investigate this claim._

_(a) Generate two-class data with p = 2 in such a way that the classes are just barely linearly separable._


```r
set.seed(23)
x <- matrix(rnorm(20*2),ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 2.0
plot(x, col=(3-y))
```

![](Chapter_9_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

_(b) Compute the cross-validation error rates for support vector classifiers with a range of cost values. How many training errors are misclassified for each value of cost considered, and how does this relate to the cross-validation errors obtained?_


```r
dat <- data.frame(x,y=factor(y))
tune.out <- tune(svm,y~., data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))

summary(tune.out)
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##   0.1
## 
## - best performance: 0.1 
## 
## - Detailed performance results:
##    cost error dispersion
## 1 1e-03  0.65  0.4743416
## 2 1e-02  0.65  0.4743416
## 3 1e-01  0.10  0.2108185
## 4 1e+00  0.10  0.2108185
## 5 5e+00  0.10  0.2108185
## 6 1e+01  0.10  0.2108185
## 7 1e+02  0.15  0.2415229
```

Is the error rate just the proportion of observations mis-classified?


```r
svm.models <- map(c(0.001,0.01,0.1,1,5,10,100), function(cost) {
  svm(y ~ ., data=dat, kernel = "linear", cost=cost) 
})

map(svm.models,summary)
```

```
## [[1]]
## 
## Call:
## svm(formula = y ~ ., data = dat, kernel = "linear", cost = cost)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  0.001 
##       gamma:  0.5 
## 
## Number of Support Vectors:  20
## 
##  ( 10 10 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
## 
## 
## 
## 
## [[2]]
## 
## Call:
## svm(formula = y ~ ., data = dat, kernel = "linear", cost = cost)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  0.01 
##       gamma:  0.5 
## 
## Number of Support Vectors:  20
## 
##  ( 10 10 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
## 
## 
## 
## 
## [[3]]
## 
## Call:
## svm(formula = y ~ ., data = dat, kernel = "linear", cost = cost)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  0.1 
##       gamma:  0.5 
## 
## Number of Support Vectors:  15
## 
##  ( 7 8 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
## 
## 
## 
## 
## [[4]]
## 
## Call:
## svm(formula = y ~ ., data = dat, kernel = "linear", cost = cost)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  1 
##       gamma:  0.5 
## 
## Number of Support Vectors:  9
## 
##  ( 5 4 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
## 
## 
## 
## 
## [[5]]
## 
## Call:
## svm(formula = y ~ ., data = dat, kernel = "linear", cost = cost)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  5 
##       gamma:  0.5 
## 
## Number of Support Vectors:  6
## 
##  ( 3 3 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
## 
## 
## 
## 
## [[6]]
## 
## Call:
## svm(formula = y ~ ., data = dat, kernel = "linear", cost = cost)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  10 
##       gamma:  0.5 
## 
## Number of Support Vectors:  5
## 
##  ( 2 3 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
## 
## 
## 
## 
## [[7]]
## 
## Call:
## svm(formula = y ~ ., data = dat, kernel = "linear", cost = cost)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  100 
##       gamma:  0.5 
## 
## Number of Support Vectors:  3
## 
##  ( 1 2 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
```

```r
map(svm.models,function(m) {
  table(predict=predict(m), truth=dat$y)  
})
```

```
## [[1]]
##        truth
## predict -1  1
##      -1 10  1
##      1   0  9
## 
## [[2]]
##        truth
## predict -1  1
##      -1 10  1
##      1   0  9
## 
## [[3]]
##        truth
## predict -1  1
##      -1 10  1
##      1   0  9
## 
## [[4]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[5]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[6]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[7]]
##        truth
## predict -1  1
##      -1 10  0
##      1   0 10
```

Only the model with the highest cost correctly classifies everything

_(c) Generate an appropriate test data set, and compute the test errors corresponding to each of the values of cost considered. Which value of cost leads to the fewest test errors, and how does this compare to the values of cost that yield the fewest training errors and the fewest cross-validation errors?_


```r
set.seed(45)
x.test <- matrix(rnorm(20*2),ncol=2)
y.test <- c(rep(-1,10), rep(1,10))
x.test[y.test==1,]=x.test[y.test==1,] + 2
dat.test <- data.frame(x.test,y=factor(y.test))
```


```r
map(svm.models,function(m) {
  table(predict=predict(m,newdata=dat.test), truth=dat.test$y)  
})
```

```
## [[1]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[2]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[3]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[4]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[5]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[6]]
##        truth
## predict -1  1
##      -1  9  0
##      1   1 10
## 
## [[7]]
##        truth
## predict -1 1
##      -1  9 1
##      1   1 9
```

(D)

The model with the highest cost (best on the training data) mis-classified 2 items in the test set, whereas all others only misclassified one.

## Q7

_7. In this problem, you will use support vector approaches in order to predict whether a given car gets high or low gas mileage based on the Auto data set._

_(a) Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 for cars with gas mileage below the median._


```r
library(ISLR)
data(Auto)
Auto <- Auto %>% mutate(mpg.class = as.numeric(mpg > median(mpg)))
```


_(b) Fit a support vector classifier to the data with various values of cost, in order to predict whether a car gets high or low gas mileage. Report the cross-validation errors associated with different values of this parameter. Comment on your results._


```r
tune.out <- tune(svm, mpg.class ~ . - mpg, data=Auto, range = list (cost=c(0.001,0.01,0.1,1,5,10,50,100,500,1000)))
summary(tune.out)
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##   100
## 
## - best performance: 0.08219788 
## 
## - Detailed performance results:
##     cost      error dispersion
## 1  1e-03 0.48638164 0.02893842
## 2  1e-02 0.40507947 0.03278970
## 3  1e-01 0.11430862 0.02455578
## 4  1e+00 0.09789265 0.04606625
## 5  5e+00 0.09228337 0.05656164
## 6  1e+01 0.08914199 0.06007395
## 7  5e+01 0.08488229 0.05414754
## 8  1e+02 0.08219788 0.04571770
## 9  5e+02 0.08491985 0.04239629
## 10 1e+03 0.08909065 0.04437455
```

Best is cost = 100

## Q8

_8. This problem involves the OJ data set which is part of the ISLR package._

_(a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations._


```r
set.seed(050418)
data(OJ)
train <- sample(1:nrow(OJ),size=800)
OJ.train <- OJ[train,]
OJ.test <- OJ[-train,]
```

_(b) Fit a support vector classifier to the training data using cost=0.01, with Purchase as the response and the other variables as predictors. Use the summary() function to produce summary statistics, and describe the results obtained._


```r
svm8.1 <- svm(Purchase ~ ., data=OJ.train, cost=0.01, kernel="linear")
summary(svm8.1)
```

```
## 
## Call:
## svm(formula = Purchase ~ ., data = OJ.train, cost = 0.01, kernel = "linear")
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  0.01 
##       gamma:  0.05555556 
## 
## Number of Support Vectors:  437
## 
##  ( 218 219 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  CH MM
```


_(c) What are the training and test error rates?_

training:

```r
table(predict=predict(svm8.1),observed=OJ.train$Purchase)
```

```
##        observed
## predict  CH  MM
##      CH 431  76
##      MM  55 238
```

```r
mean(predict(svm8.1)!=OJ.train$Purchase)
```

```
## [1] 0.16375
```

16% traiing error rate

test:


```r
table(predict=predict(svm8.1,newdata = OJ.test),observed=OJ.test$Purchase)
```

```
##        observed
## predict  CH  MM
##      CH 144  24
##      MM  23  79
```

```r
mean(predict(svm8.1,newdata = OJ.test)!=OJ.test$Purchase)
```

```
## [1] 0.1740741
```

Test error 17.4 %.

_(d) Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10._


```r
tune.out <- tune(svm,Purchase ~ ., data=OJ.train, kernel="linear", ranges=list(cost=c(0.01,0.02,0.05,1,5,10)))
summary(tune.out)
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##    10
## 
## - best performance: 0.16875 
## 
## - Detailed performance results:
##    cost   error dispersion
## 1  0.01 0.17000 0.04216370
## 2  0.02 0.17000 0.04090979
## 3  0.05 0.17250 0.04281744
## 4  1.00 0.17375 0.03606033
## 5  5.00 0.17500 0.03996526
## 6 10.00 0.16875 0.04177070
```


_(e) Compute the training and test error rates using this new value for cost._


```r
predict.train <- predict(tune.out$best.model)
table(predict.train,OJ.train$Purchase)
```

```
##              
## predict.train  CH  MM
##            CH 433  70
##            MM  53 244
```

```r
mean(predict.train!=OJ.train$Purchase)
```

```
## [1] 0.15375
```


```r
predict.test <- predict(tune.out$best.model,newdata = OJ.test)
table(predict.test,OJ.test$Purchase)
```

```
##             
## predict.test  CH  MM
##           CH 141  23
##           MM  26  80
```

```r
mean(predict.test!=OJ.test$Purchase)
```

```
## [1] 0.1814815
```

slightly worse; presumably within the error range on this.

