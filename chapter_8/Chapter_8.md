---
title: "Chapter 8"
author: "Julin N Maloof"
date: "4/5/2018"
output: 
  html_document: 
    keep_md: yes
---




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
## ✔ readr   1.1.1     ✔ forcats 0.3.0
```

```
## ── Conflicts ────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(ISLR)
library(tree)
library(randomForest)
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
library(gbm)
```

```
## Loading required package: survival
```

```
## Loading required package: lattice
```

```
## Loading required package: splines
```

```
## Loading required package: parallel
```

```
## Loaded gbm 2.1.3
```

```r
library(glmnet)
```

```
## Loading required package: Matrix
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following object is masked from 'package:tidyr':
## 
##     expand
```

```
## Loading required package: foreach
```

```
## 
## Attaching package: 'foreach'
```

```
## The following objects are masked from 'package:purrr':
## 
##     accumulate, when
```

```
## Loaded glmnet 2.0-13
```

```r
library(class)
```

## Q1

_Draw an example (of your own invention) of a partition of two- dimensional feature space that could result from recursive binary splitting. Your example should contain at least six regions. Draw a decision tree corresponding to this partition. Be sure to label all as- pects of your figures, including the regions R1, R2, . . ., the cutpoints t1,t2,..., and so forth.
Hint: Your result should look something like Figures 8.1 and 8.2._

Average ride speed based on elevation and number in group

![](decisionTree.jpeg)

## Q3

_3. Consider the Gini index, classification error, and entropy in a simple classification setting with two classes. Create a single plot that displays each of these quantities as a function of pˆm1. The x- axis should display pˆm1, ranging from 0 to 1, and the y-axis should display the value of the Gini index, classification error, and entropy._

_Hint: In a setting with two classes, pˆm1 = 1 − pˆm2. You could make this plot by hand, but it will be much easier to make in R._


```r
data3 <- tibble(
  pm1 = seq(0,1,by=.01), #proportion in region m that is class 1
  pm2 = 1-pm1            #proportion in region m that is class 2
)
```


```r
data3 <- data3 %>% 
  mutate(
    E=1-map2_dbl(pm1,pm2,max),
    G=2*pm1*pm2,
    Ent=map2_dbl(pm1,pm2, function(pm1,pm2) {
      -sum(pm1*log(pm1) , pm2*log(pm2))
    })
  )
```


```r
data3 %>% 
  gather(key="criteria",value="value",-pm1,-pm2) %>%
  ggplot(aes(x=pm1,y=value,color=criteria)) +
  geom_line(size=2)
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![](Chapter_8_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Q5

_Suppose we produce ten bootstrapped samples from a data set containing red and green classes. We then apply a classification tree to each bootstrapped sample and, for a specific value of X, produce 10 estimates of P(Class is Red|X):_
`0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, and 0.75.`
_There are two common ways to combine these results together into a single class prediction. One is the majority vote approach discussed in this chapter. The second approach is to classify based on the average probability. In this example, what is the final classification under each of these two approaches?_


```r
prob <- c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75)

# Majority Vote:
mean(prob>.5)
```

```
## [1] 0.6
```

```r
  # red

# Average:
mean(prob)
```

```
## [1] 0.45
```

```r
 # green
```

## Q7
_In the lab, we applied random forests to the Boston data using mtry=6 and using ntree=25 and ntree=500. Create a plot displaying the test error resulting from random forests on this data set for a more com- prehensive range of values for mtry and ntree. You can model your plot after Figure 8.10. Describe the results obtained._


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
data(Boston)
set.seed(1234)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.train <- Boston[train,]
boston.test <- Boston[-train,]
```


```r
mtry <- c(1,3,6,9,13)
ntree <- c(10,25,50,75,100,150)

get.mse <- function(mtry,ntree) {
  fit <- randomForest(medv ~ ., data=boston.train, mtry=mtry, ntree=ntree)
  predict <- predict(fit,newdata=boston.test)
  mse <- mean((boston.test$medv - predict) ^ 2)
  mse
}

mse <- expand.grid(mtry=mtry,ntree=ntree) %>% 
  mutate(mse = map2(mtry,ntree,get.mse) %>% unlist())

head(mse)
```

```
##   mtry ntree      mse
## 1    1    10 30.25074
## 2    3    10 22.44489
## 3    6    10 20.01430
## 4    9    10 21.27604
## 5   13    10 19.34381
## 6    1    25 32.90058
```


```r
mse %>% 
  ggplot(aes(x=ntree,y=mse,color=factor(mtry),shape=factor(mtry))) +
  geom_line() +
  geom_point()
```

![](Chapter_8_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

We don't see a lot of improvement beyond 50 trees or so.

We 1 and 3 predictors, and possibly 13 underperform 6 and 9.


## Q8 (a,b,c)

_8. In the lab, a classification tree was applied to the Carseats data set after converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable._

_(a) Split the data set into a training set and a test set._


```r
data("Carseats")
head(Carseats)
```

```
##   Sales CompPrice Income Advertising Population Price ShelveLoc Age
## 1  9.50       138     73          11        276   120       Bad  42
## 2 11.22       111     48          16        260    83      Good  65
## 3 10.06       113     35          10        269    80    Medium  59
## 4  7.40       117    100           4        466    97    Medium  55
## 5  4.15       141     64           3        340   128       Bad  38
## 6 10.81       124    113          13        501    72       Bad  78
##   Education Urban  US
## 1        17   Yes Yes
## 2        10   Yes Yes
## 3        12   Yes Yes
## 4        14   Yes Yes
## 5        13   Yes  No
## 6        16    No Yes
```

```r
dim(Carseats)
```

```
## [1] 400  11
```

```r
set.seed(040518)
train <- sample(1:nrow(Carseats),size=250)
carseats.train <- Carseats[train,]
carseats.test <- Carseats[-train,]
```

_(b) Fit a regression tree to the training set. Plot the tree, and inter- pret the results. What test MSE do you obtain?_


```r
tree1 <- tree(Sales ~ . , data= carseats.train)
summary(tree1)
```

```
## 
## Regression tree:
## tree(formula = Sales ~ ., data = carseats.train)
## Variables actually used in tree construction:
## [1] "ShelveLoc"   "Price"       "CompPrice"   "Age"         "Advertising"
## [6] "Income"      "Education"  
## Number of terminal nodes:  18 
## Residual mean deviance:  2.318 = 537.9 / 232 
## Distribution of residuals:
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -3.79600 -1.02200 -0.09198  0.00000  0.93380  4.54000
```

```r
plot(tree1)
text(tree1, pretty = 0)
```

![](Chapter_8_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
tree1.predict <- predict(tree1,newdata = carseats.test)
(MSE <- mean((tree1.predict-carseats.test$Sales)^2))
```

```
## [1] 4.972922
```

_(c) Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test MSE?_


```r
tree1.cv <- cv.tree(tree1)
plot(tree1.cv)
```

![](Chapter_8_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
tree1.cv
```

```
## $size
##  [1] 18 17 16 15 14 12 11 10  9  8  7  6  5  4  3  2  1
## 
## $dev
##  [1] 1321.321 1324.543 1321.404 1341.311 1357.982 1376.471 1421.077
##  [8] 1441.257 1441.236 1491.909 1543.009 1617.856 1598.927 1598.927
## [15] 1725.255 1757.929 2086.721
## 
## $k
##  [1]      -Inf  22.51503  24.47178  26.60338  28.75840  32.19059  32.47045
##  [8]  41.41291  46.89361  52.47458  63.71281  76.79418  90.85924  90.95556
## [15] 147.63294 215.70903 498.53629
## 
## $method
## [1] "deviance"
## 
## attr(,"class")
## [1] "prune"         "tree.sequence"
```

CV would suggest not pruning.  I guess we could prune to 16...


```r
tree1.prune <- prune.tree(tree1,best=16)
tree1.prune.predict <- predict(tree1.prune, newdata = carseats.test)
(MSE <- mean((tree1.prune.predict-carseats.test$Sales)^2))
```

```
## [1] 4.860422
```

slight reduction in MSE

_(d) Use the bagging approach in order to analyze this data. What test MSE do you obtain? Use the importance() function to determine which variables are most important._


```r
carseats.bag <- randomForest(Sales ~ ., mtry=ncol(carseats.train)-1, importance=TRUE, data=carseats.train)
carseats.bag
```

```
## 
## Call:
##  randomForest(formula = Sales ~ ., data = carseats.train, mtry = ncol(carseats.train) -      1, importance = TRUE) 
##                Type of random forest: regression
##                      Number of trees: 500
## No. of variables tried at each split: 10
## 
##           Mean of squared residuals: 2.571417
##                     % Var explained: 68.82
```

```r
importance(carseats.bag)
```

```
##                %IncMSE IncNodePurity
## CompPrice   30.3367177    221.063875
## Income      13.1142485    130.027024
## Advertising 15.0339375    121.373841
## Population  -2.8953994     66.727070
## Price       67.5646245    672.380894
## ShelveLoc   58.0665625    541.435544
## Age         19.7199999    194.791271
## Education    0.4794359     49.322006
## Urban       -1.7961440     10.170825
## US           0.5421650      5.867115
```

```r
carseats.bag.predict <- predict(carseats.bag, newdata = carseats.test)
(bag.mse <- mean((carseats.bag.predict-carseats.test$Sales)^2))
```

```
## [1] 2.540514
```

A substantial reduction in MSE

(e) Use random forests to analyze this data. What test MSE do you obtain? Use the importance() function to determine which variables are most important. Describe the effect of m, the number of variables considered at each split, on the error rate obtained.


```r
mtry <- c(2,3,5,7,9)

fits <- lapply(mtry,function(m) {
  randomForest(Sales ~ ., mtry=m, importance=TRUE, data=carseats.train)
})

names(fits) <- mtry

lapply(fits,importance)
```

```
## $`2`
##               %IncMSE IncNodePurity
## CompPrice    9.743809     169.77059
## Income       5.195792     170.94856
## Advertising 12.229973     177.32724
## Population  -1.964894     145.83512
## Price       35.571952     481.77187
## ShelveLoc   32.313092     371.03675
## Age         13.009792     216.58845
## Education    3.719462      94.03611
## Urban       -3.366824      23.63474
## US           4.347546      27.81436
## 
## $`3`
##                %IncMSE IncNodePurity
## CompPrice   11.7529331     173.06070
## Income       7.4433965     164.50349
## Advertising 13.8783876     175.89342
## Population  -1.9374691     122.51547
## Price       43.4591454     553.22470
## ShelveLoc   39.2189848     422.11251
## Age         15.6290515     233.96335
## Education    1.6994622      79.53712
## Urban        0.6658229      18.45452
## US           2.4858280      19.84302
## 
## $`5`
##                %IncMSE IncNodePurity
## CompPrice   20.4956597     192.45097
## Income      10.1993421     141.96134
## Advertising 14.7321105     152.29454
## Population  -1.7533424      91.47421
## Price       52.4903686     625.46840
## ShelveLoc   51.5993412     477.11855
## Age         18.6593249     231.95567
## Education   -0.7494469      62.92695
## Urban       -3.6838321      13.85700
## US           0.5833544      11.53222
## 
## $`7`
##                %IncMSE IncNodePurity
## CompPrice   25.3449424    206.403909
## Income      11.2122513    134.578323
## Advertising 15.3798320    138.649723
## Population  -3.3878912     77.345292
## Price       61.8946318    648.340117
## ShelveLoc   51.7773778    503.808323
## Age         20.5402134    211.864657
## Education   -0.4601599     52.235881
## Urban       -4.9269586     10.581121
## US           1.0248077      8.941671
## 
## $`9`
##                %IncMSE IncNodePurity
## CompPrice   29.0675746    225.847350
## Income      12.1520247    127.595582
## Advertising 16.4354121    120.504843
## Population  -2.3961686     67.253385
## Price       68.6323577    675.351024
## ShelveLoc   56.2862067    515.816616
## Age         20.0759373    202.023981
## Education    1.8218418     51.631022
## Urban       -2.5900915      9.666680
## US           0.6008354      6.469581
```

```r
sapply(fits, function(f) {
  mean( (predict(f,newdata=carseats.test)-carseats.test$Sales)^2)
}
)
```

```
##        2        3        5        7        9 
## 3.066124 2.732182 2.446714 2.510834 2.497168
```

Increasing the number of variables considered at each split improves the MSE when only a small number of variables are being used and then things level out.

## Q9

_9. This problem involves the OJ data set which is part of the ISLR package._


```r
data(OJ)
dim(OJ)
```

```
## [1] 1070   18
```

```r
?OJ
head(OJ) 
```

```
##   Purchase WeekofPurchase StoreID PriceCH PriceMM DiscCH DiscMM SpecialCH
## 1       CH            237       1    1.75    1.99   0.00    0.0         0
## 2       CH            239       1    1.75    1.99   0.00    0.3         0
## 3       CH            245       1    1.86    2.09   0.17    0.0         0
## 4       MM            227       1    1.69    1.69   0.00    0.0         0
## 5       CH            228       7    1.69    1.69   0.00    0.0         0
## 6       CH            230       7    1.69    1.99   0.00    0.0         0
##   SpecialMM  LoyalCH SalePriceMM SalePriceCH PriceDiff Store7 PctDiscMM
## 1         0 0.500000        1.99        1.75      0.24     No  0.000000
## 2         1 0.600000        1.69        1.75     -0.06     No  0.150754
## 3         0 0.680000        2.09        1.69      0.40     No  0.000000
## 4         0 0.400000        1.69        1.69      0.00     No  0.000000
## 5         0 0.956535        1.69        1.69      0.00    Yes  0.000000
## 6         1 0.965228        1.99        1.69      0.30    Yes  0.000000
##   PctDiscCH ListPriceDiff STORE
## 1  0.000000          0.24     1
## 2  0.000000          0.24     1
## 3  0.091398          0.23     1
## 4  0.000000          0.00     1
## 5  0.000000          0.00     0
## 6  0.000000          0.30     0
```


_(a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations._


```r
train <- sample(1:nrow(OJ), size = 800)
oj.train <- OJ[train,]
oj.test <- OJ[-train,]
```

_(b) Fit a tree to the training data, with Purchase as the response and the other variables as predictors. Use the summary() function to produce summary statistics about the tree, and describe the results obtained. What is the training error rate? How many terminal nodes does the tree have?_


```r
tree1 <- tree(Purchase ~ . , data=oj.train)
summary(tree1)
```

```
## 
## Classification tree:
## tree(formula = Purchase ~ ., data = oj.train)
## Variables actually used in tree construction:
## [1] "LoyalCH"       "PriceDiff"     "ListPriceDiff"
## Number of terminal nodes:  8 
## Residual mean deviance:  0.7289 = 577.3 / 792 
## Misclassification error rate: 0.1512 = 121 / 800
```

8 nodes with three predictors, good training prediction (0.155)

_(c) Type in the name of the tree object in order to get a detailed text output. Pick one of the terminal nodes, and interpret the information displayed._


```r
tree1
```

```
## node), split, n, deviance, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 800 1063.000 CH ( 0.61875 0.38125 )  
##    2) LoyalCH < 0.461965 280  298.600 MM ( 0.22500 0.77500 )  
##      4) LoyalCH < 0.070051 66   10.360 MM ( 0.01515 0.98485 ) *
##      5) LoyalCH > 0.070051 214  257.600 MM ( 0.28972 0.71028 )  
##       10) PriceDiff < 0.31 161  166.000 MM ( 0.21118 0.78882 ) *
##       11) PriceDiff > 0.31 53   73.300 CH ( 0.52830 0.47170 ) *
##    3) LoyalCH > 0.461965 520  472.900 CH ( 0.83077 0.16923 )  
##      6) LoyalCH < 0.753545 248  307.300 CH ( 0.68952 0.31048 )  
##       12) PriceDiff < 0.015 83  112.300 MM ( 0.40964 0.59036 )  
##         24) ListPriceDiff < 0.235 57   65.700 MM ( 0.26316 0.73684 ) *
##         25) ListPriceDiff > 0.235 26   30.290 CH ( 0.73077 0.26923 ) *
##       13) PriceDiff > 0.015 165  150.300 CH ( 0.83030 0.16970 ) *
##      7) LoyalCH > 0.753545 272   92.120 CH ( 0.95956 0.04044 )  
##       14) PriceDiff < -0.39 7    9.561 CH ( 0.57143 0.42857 ) *
##       15) PriceDiff > -0.39 265   71.760 CH ( 0.96981 0.03019 ) *
```

For node 7, these are customers whose CH brand loyalty is > 0.76.  There are 265 such customers.  The deviance at this node is 97.72 (what does this mean) and 95% of the customers at this node bought CH.

_(d) Create a plot of the tree, and interpret the results._


```r
plot(tree1)
text(tree1,pretty=0)
```

![](Chapter_8_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

Loyalty to CH brand is the biggest predictor.  

_(e) Predict the response on the test data, and produce a confusion matrix comparing the test labels to the predicted test labels. What is the test error rate?_


```r
oj.prediction <- predict(tree1,newdata = oj.test,type="class")
table(prediction=oj.prediction,observed=oj.test$Purchase)
```

```
##           observed
## prediction  CH  MM
##         CH 129  30
##         MM  29  82
```


```r
(tree1.accuracy <- mean(oj.prediction==oj.test$Purchase))
```

```
## [1] 0.7814815
```

81% prediction accuracy

_(f) Apply the cv.tree() function to the training set in order to determine the optimal tree size._


```r
set.seed(1354)
tree1.cv <- cv.tree(tree1,FUN=prune.misclass)
tree1.cv
```

```
## $size
## [1] 8 7 5 2 1
## 
## $dev
## [1] 141 140 145 162 305
## 
## $k
## [1]  -Inf   0.0   1.5   9.0 154.0
## 
## $method
## [1] "misclass"
## 
## attr(,"class")
## [1] "prune"         "tree.sequence"
```

best size appears to be 7

_(g) Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis._


```r
plot(tree1.cv$size, tree1.cv$dev / nrow(oj.test),type="l")
```

![](Chapter_8_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

_(h) Which tree size corresponds to the lowest cross-validated classi- fication error rate?_

2

_(i) Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation. If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with five terminal nodes._


```r
tree1.prune <- prune.misclass(tree1,best = 2)
```

_(j) Compare the training error rates between the pruned and un- pruned trees. Which is higher?_


```r
summary(tree1)
```

```
## 
## Classification tree:
## tree(formula = Purchase ~ ., data = oj.train)
## Variables actually used in tree construction:
## [1] "LoyalCH"       "PriceDiff"     "ListPriceDiff"
## Number of terminal nodes:  8 
## Residual mean deviance:  0.7289 = 577.3 / 792 
## Misclassification error rate: 0.1512 = 121 / 800
```

```r
summary(tree1.prune)
```

```
## 
## Classification tree:
## snip.tree(tree = tree1, nodes = 2:3)
## Variables actually used in tree construction:
## [1] "LoyalCH"
## Number of terminal nodes:  2 
## Residual mean deviance:  0.9667 = 771.4 / 798 
## Misclassification error rate: 0.1888 = 151 / 800
```

The unpruned tree has lower training error rate

_(k) Compare the test error rates between the pruned and unpruned trees. Which is higher?_


```r
prune.predict <- predict(tree1.prune,newdata = oj.test, type="class")
table(prediction=prune.predict,observed=oj.test$Purchase)
```

```
##           observed
## prediction  CH  MM
##         CH 135  32
##         MM  23  80
```

```r
tree1.accuracy
```

```
## [1] 0.7814815
```

```r
(prune.accuracy <- mean(prune.predict==oj.test$Purchase))
```

```
## [1] 0.7962963
```

Test accuracy is better on the pruned tree

## Q10

_We now use boosting to predict Salary in the Hitters data set._

_(a) Remove the observations for whom the salary information is unknown, and then log-transform the salaries._


```r
data("Hitters")

hitters <- Hitters %>% as_tibble() %>%
  filter(!is.na(Salary)) %>%
  mutate(Salary=log(Salary))
```


_(b) Create a training set consisting of the first 200 observations, and a test set consisting of the remaining observations._


```r
hitters.train <- hitters[1:200,]
hitters.test <- hitters[201:nrow(hitters),]
```

_(c) Perform boosting on the training set with 1,000 trees for a range of values of the shrinkage parameter λ. Produce a plot with different shrinkage values on the x-axis and the corresponding training set MSE on the y-axis._


```r
lambdas <- c(.2,.1,.01,.001,.0001)

hitters.gbm <- lapply(lambdas, function(l) { 
  gbm(Salary ~ ., distribution = "gaussian", n.trees=1000, shrinkage=l,data=hitters.train) #interaction.depth = 1
}
)

names(hitters.gbm) <- lambdas

gbm.train.mse <- sapply(hitters.gbm,function(x) x$train.error[1000])

plot(lambdas,gbm.train.mse,type="b")
```

![](Chapter_8_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

_(d) Produce a plot with different shrinkage values on the x-axis and the corresponding test set MSE on the y-axis._


```r
gbm.test.mse <- sapply(hitters.gbm, function(f) {
  mean( (predict(f,newdata=hitters.test,n.trees=1000)-hitters.test$Salary)^2)
}
)

plot(lambdas,gbm.test.mse,type="b")
```

![](Chapter_8_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

The best lambda in this case seems to be 0.10


```r
hitters.gbm.best <- hitters.gbm[["0.1"]]
gbm.best.predict <- predict(hitters.gbm.best,newdata=hitters.test,n.trees=1000)
cat("gbm MSE: ")
```

```
## gbm MSE:
```

```r
(hitters.gbm.mse <- mean( (gbm.best.predict-hitters.test$Salary)^2))
```

```
## [1] 0.2784941
```

```r
cat("gbm R2: ")
```

```
## gbm R2:
```

```r
(hitters.gbm.R2 <- cor(gbm.best.predict,hitters.test$Salary)^2)
```

```
## [1] 0.5962744
```


_(e) Compare the test MSE of boosting to the test MSE that results from applying two of the regression approaches seen in Chapters 3 and 6._

First just try multiple regression


```r
mse.R2 <- function(object,newdata) {
  observed <- rlang::f_lhs(formula(object)) %>% 
    as.character() %>%
    get(newdata)
  
  predicted <- predict(object,newdata=newdata)
  
  mse <- mean( (observed-predicted)^2)
  R2 <- cor(observed,predicted)^2
  
  return(list(mse=mse,R2=R2))
  }

hitters.lm <- lm(Salary ~ ., data=hitters.train)
summary(hitters.lm)
```

```
## 
## Call:
## lm(formula = Salary ~ ., data = hitters.train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.44628 -0.43844  0.02835  0.39266  2.83081 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.4531779  0.1953605  22.795  < 2e-16 ***
## AtBat       -0.0041511  0.0015441  -2.688 0.007852 ** 
## Hits         0.0189964  0.0054505   3.485 0.000618 ***
## HmRun        0.0094426  0.0134247   0.703 0.482728    
## Runs        -0.0029961  0.0067142  -0.446 0.655968    
## RBI         -0.0026030  0.0056677  -0.459 0.646590    
## Walks        0.0113452  0.0039819   2.849 0.004894 ** 
## Years        0.0686664  0.0259832   2.643 0.008949 ** 
## CAtBat       0.0001479  0.0002850   0.519 0.604507    
## CHits       -0.0012085  0.0014250  -0.848 0.397515    
## CHmRun       0.0004085  0.0034671   0.118 0.906335    
## CRuns        0.0025268  0.0016188   1.561 0.120299    
## CRBI         0.0003625  0.0014589   0.248 0.804062    
## CWalks      -0.0016141  0.0006971  -2.316 0.021712 *  
## LeagueN      0.1487966  0.1654780   0.899 0.369751    
## DivisionW   -0.1359398  0.0880186  -1.544 0.124237    
## PutOuts      0.0005631  0.0001856   3.034 0.002770 ** 
## Assists      0.0008936  0.0005092   1.755 0.080969 .  
## Errors      -0.0099497  0.0100046  -0.995 0.321308    
## NewLeagueN  -0.0315309  0.1655198  -0.190 0.849135    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5966 on 180 degrees of freedom
## Multiple R-squared:  0.6149,	Adjusted R-squared:  0.5743 
## F-statistic: 15.13 on 19 and 180 DF,  p-value: < 2.2e-16
```

```r
hitters.lm1 <- lm(Salary ~ AtBat + Hits + Walks + Years + CWalks + PutOuts + Assists, data = hitters.train)
summary(hitters.lm1)
```

```
## 
## Call:
## lm(formula = Salary ~ AtBat + Hits + Walks + Years + CWalks + 
##     PutOuts + Assists, data = hitters.train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3922 -0.4601 -0.0601  0.4241  3.1771 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.2570351  0.1593809  26.710  < 2e-16 ***
## AtBat       -0.0034505  0.0013319  -2.591  0.01031 *  
## Hits         0.0176725  0.0040593   4.354 2.18e-05 ***
## Walks        0.0078510  0.0032758   2.397  0.01750 *  
## Years        0.0855157  0.0182500   4.686 5.27e-06 ***
## CWalks       0.0001572  0.0003627   0.433  0.66520    
## PutOuts      0.0004838  0.0001799   2.689  0.00781 ** 
## Assists      0.0003199  0.0003467   0.923  0.35736    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6045 on 192 degrees of freedom
## Multiple R-squared:  0.5784,	Adjusted R-squared:  0.563 
## F-statistic: 37.62 on 7 and 192 DF,  p-value: < 2.2e-16
```

```r
cat("lm R2: ")
```

```
## lm R2:
```

```r
(lm.test.R2 <- mse.R2(hitters.lm1,hitters.test)$R2)
```

```
## [1] 0.3074694
```

```r
cat("lm MSE: ")
```

```
## lm MSE:
```

```r
(lm.test.mse <- mse.R2(hitters.lm1,hitters.test)$mse)
```

```
## [1] 0.5113022
```

ridge regression

```r
hitters.mod <- model.matrix(Salary ~ ., data=hitters.train)
hitters.ridge.cv <- cv.glmnet(hitters.mod, hitters.train$Salary, alpha=0)
plot(hitters.ridge.cv)
```

![](Chapter_8_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

```r
hitters.mod.test <- model.matrix(Salary ~ . , data = hitters.test)
hitters.ridge.predict <- predict(hitters.ridge.cv, newx = hitters.mod.test) # uses lambsa.1se by default

cat("ridge R2: ")
```

```
## ridge R2:
```

```r
(hitters.ridge.R2 <- cor(hitters.ridge.predict, hitters.test$Salary)^2)
```

```
##        [,1]
## 1 0.3109871
```

```r
cat("ridge MSE: ")
```

```
## ridge MSE:
```

```r
(hitters.ridge.mse <- mean( (hitters.ridge.predict - hitters.test$Salary)^2))
```

```
## [1] 0.4498941
```


_(f) Which variables appear to be the most important predictors in the boosted model?_


```r
summary(hitters.gbm.best)
```

![](Chapter_8_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

```
##                 var     rel.inf
## CAtBat       CAtBat 18.04430905
## CRuns         CRuns 13.30989485
## PutOuts     PutOuts  8.63247018
## Walks         Walks  7.81818822
## CRBI           CRBI  6.28300770
## CWalks       CWalks  5.53872369
## Assists     Assists  5.49647822
## CHmRun       CHmRun  5.20138838
## Years         Years  4.96502148
## Hits           Hits  4.91165923
## AtBat         AtBat  4.59180982
## RBI             RBI  4.43146072
## HmRun         HmRun  3.23322339
## Runs           Runs  2.59142725
## Errors       Errors  2.47430484
## CHits         CHits  1.14679280
## Division   Division  0.78327742
## NewLeague NewLeague  0.50121275
## League       League  0.04535001
```

AtBat is most informative.

_(g) Now apply bagging to the training set. What is the test set MSE for this approach?_


```r
hitters.bag <- randomForest(Salary ~ ., data=hitters.train,mtry=ncol(hitters.train)-1)

mse.R2(hitters.bag,hitters.test)
```

```
## $mse
## [1] 0.2352637
## 
## $R2
## [1] 0.6545249
```

Bag is the best!

Would random forest be better? (use default mtry of p/3)


```r
hitters.rf <- randomForest(Salary ~ ., data=hitters.train)

mse.R2(hitters.rf,hitters.test)
```

```
## $mse
## [1] 0.2162978
## 
## $R2
## [1] 0.6764298
```

yes.  although really the differences are small and we should probably be doing CV to determine the variance.

## Q11

_11. This question uses the Caravan data set._

_(a) Create a training set consisting of the first 1,000 observations, and a test set consisting of the remaining observations._


```r
data("Caravan")
?Caravan
head(Caravan)
```

```
##   MOSTYPE MAANTHUI MGEMOMV MGEMLEEF MOSHOOFD MGODRK MGODPR MGODOV MGODGE
## 1      33        1       3        2        8      0      5      1      3
## 2      37        1       2        2        8      1      4      1      4
## 3      37        1       2        2        8      0      4      2      4
## 4       9        1       3        3        3      2      3      2      4
## 5      40        1       4        2       10      1      4      1      4
## 6      23        1       2        1        5      0      5      0      5
##   MRELGE MRELSA MRELOV MFALLEEN MFGEKIND MFWEKIND MOPLHOOG MOPLMIDD
## 1      7      0      2        1        2        6        1        2
## 2      6      2      2        0        4        5        0        5
## 3      3      2      4        4        4        2        0        5
## 4      5      2      2        2        3        4        3        4
## 5      7      1      2        2        4        4        5        4
## 6      0      6      3        3        5        2        0        5
##   MOPLLAAG MBERHOOG MBERZELF MBERBOER MBERMIDD MBERARBG MBERARBO MSKA
## 1        7        1        0        1        2        5        2    1
## 2        4        0        0        0        5        0        4    0
## 3        4        0        0        0        7        0        2    0
## 4        2        4        0        0        3        1        2    3
## 5        0        0        5        4        0        0        0    9
## 6        4        2        0        0        4        2        2    2
##   MSKB1 MSKB2 MSKC MSKD MHHUUR MHKOOP MAUT1 MAUT2 MAUT0 MZFONDS MZPART
## 1     1     2    6    1      1      8     8     0     1       8      1
## 2     2     3    5    0      2      7     7     1     2       6      3
## 3     5     0    4    0      7      2     7     0     2       9      0
## 4     2     1    4    0      5      4     9     0     0       7      2
## 5     0     0    0    0      4      5     6     2     1       5      4
## 6     2     2    4    2      9      0     5     3     3       9      0
##   MINKM30 MINK3045 MINK4575 MINK7512 MINK123M MINKGEM MKOOPKLA PWAPART
## 1       0        4        5        0        0       4        3       0
## 2       2        0        5        2        0       5        4       2
## 3       4        5        0        0        0       3        4       2
## 4       1        5        3        0        0       4        4       0
## 5       0        0        9        0        0       6        3       0
## 6       5        2        3        0        0       3        3       0
##   PWABEDR PWALAND PPERSAUT PBESAUT PMOTSCO PVRAAUT PAANHANG PTRACTOR
## 1       0       0        6       0       0       0        0        0
## 2       0       0        0       0       0       0        0        0
## 3       0       0        6       0       0       0        0        0
## 4       0       0        6       0       0       0        0        0
## 5       0       0        0       0       0       0        0        0
## 6       0       0        6       0       0       0        0        0
##   PWERKT PBROM PLEVEN PPERSONG PGEZONG PWAOREG PBRAND PZEILPL PPLEZIER
## 1      0     0      0        0       0       0      5       0        0
## 2      0     0      0        0       0       0      2       0        0
## 3      0     0      0        0       0       0      2       0        0
## 4      0     0      0        0       0       0      2       0        0
## 5      0     0      0        0       0       0      6       0        0
## 6      0     0      0        0       0       0      0       0        0
##   PFIETS PINBOED PBYSTAND AWAPART AWABEDR AWALAND APERSAUT ABESAUT AMOTSCO
## 1      0       0        0       0       0       0        1       0       0
## 2      0       0        0       2       0       0        0       0       0
## 3      0       0        0       1       0       0        1       0       0
## 4      0       0        0       0       0       0        1       0       0
## 5      0       0        0       0       0       0        0       0       0
## 6      0       0        0       0       0       0        1       0       0
##   AVRAAUT AAANHANG ATRACTOR AWERKT ABROM ALEVEN APERSONG AGEZONG AWAOREG
## 1       0        0        0      0     0      0        0       0       0
## 2       0        0        0      0     0      0        0       0       0
## 3       0        0        0      0     0      0        0       0       0
## 4       0        0        0      0     0      0        0       0       0
## 5       0        0        0      0     0      0        0       0       0
## 6       0        0        0      0     0      0        0       0       0
##   ABRAND AZEILPL APLEZIER AFIETS AINBOED ABYSTAND Purchase
## 1      1       0        0      0       0        0       No
## 2      1       0        0      0       0        0       No
## 3      1       0        0      0       0        0       No
## 4      1       0        0      0       0        0       No
## 5      1       0        0      0       0        0       No
## 6      0       0        0      0       0        0       No
```


```r
Caravan$Purchase <- ifelse(Caravan$Purchase=="Yes",1,0)
caravan.train <- Caravan[1:1000,]
caravan.test <- Caravan[1001:nrow(Caravan),]
```


_(b) Fit a boosting model to the training set with Purchase as the response and the other variables as predictors. Use 1,000 trees, and a shrinkage value of 0.01. Which predictors appear to be the most important?_


```r
caravan.gbm <- gbm(Purchase ~ ., data=caravan.train, n.trees=1000,shrinkage=0.01)
```

```
## Distribution not specified, assuming bernoulli ...
```

```
## Warning in gbm.fit(x, y, offset = offset, distribution = distribution, w =
## w, : variable 50: PVRAAUT has no variation.
```

```
## Warning in gbm.fit(x, y, offset = offset, distribution = distribution, w =
## w, : variable 71: AVRAAUT has no variation.
```

```r
summary(caravan.gbm)
```

![](Chapter_8_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

```
##               var     rel.inf
## PPERSAUT PPERSAUT 14.81153585
## MKOOPKLA MKOOPKLA 10.18352711
## MOPLHOOG MOPLHOOG  7.46988461
## MBERMIDD MBERMIDD  5.35893681
## PBRAND     PBRAND  5.14612619
## ABRAND     ABRAND  4.58876466
## MGODGE     MGODGE  4.12489272
## MINK3045 MINK3045  3.46849740
## MOSTYPE   MOSTYPE  3.13912112
## PWAPART   PWAPART  2.81524768
## MAUT1       MAUT1  2.42529780
## MAUT2       MAUT2  2.19602290
## MSKC         MSKC  2.18016892
## MINKGEM   MINKGEM  2.16053533
## MGODPR     MGODPR  2.08904955
## MSKA         MSKA  2.05967049
## MSKB1       MSKB1  1.78156480
## PBYSTAND PBYSTAND  1.71637936
## MBERHOOG MBERHOOG  1.60728021
## MFWEKIND MFWEKIND  1.59335727
## MGODOV     MGODOV  1.53576117
## MINK7512 MINK7512  1.46317251
## MBERARBG MBERARBG  1.21610582
## MRELGE     MRELGE  1.05713192
## MINK4575 MINK4575  1.02240580
## MGODRK     MGODRK  0.96755268
## MRELOV     MRELOV  0.87606466
## MFGEKIND MFGEKIND  0.87038875
## MAUT0       MAUT0  0.84516241
## MHHUUR     MHHUUR  0.84007321
## MSKD         MSKD  0.74253898
## MOPLMIDD MOPLMIDD  0.74238890
## APERSAUT APERSAUT  0.70020841
## MINKM30   MINKM30  0.68078142
## MOSHOOFD MOSHOOFD  0.65049654
## MGEMOMV   MGEMOMV  0.60486890
## MBERBOER MBERBOER  0.58170840
## PMOTSCO   PMOTSCO  0.55944989
## MHKOOP     MHKOOP  0.55577465
## MZFONDS   MZFONDS  0.48789413
## PLEVEN     PLEVEN  0.34296701
## MBERARBO MBERARBO  0.33844492
## MGEMLEEF MGEMLEEF  0.33333983
## MFALLEEN MFALLEEN  0.29498678
## MRELSA     MRELSA  0.28358522
## MZPART     MZPART  0.21904907
## MOPLLAAG MOPLLAAG  0.10621390
## MSKB2       MSKB2  0.06223572
## MINK123M MINK123M  0.05872135
## MBERZELF MBERZELF  0.04466625
## MAANTHUI MAANTHUI  0.00000000
## PWABEDR   PWABEDR  0.00000000
## PWALAND   PWALAND  0.00000000
## PBESAUT   PBESAUT  0.00000000
## PVRAAUT   PVRAAUT  0.00000000
## PAANHANG PAANHANG  0.00000000
## PTRACTOR PTRACTOR  0.00000000
## PWERKT     PWERKT  0.00000000
## PBROM       PBROM  0.00000000
## PPERSONG PPERSONG  0.00000000
## PGEZONG   PGEZONG  0.00000000
## PWAOREG   PWAOREG  0.00000000
## PZEILPL   PZEILPL  0.00000000
## PPLEZIER PPLEZIER  0.00000000
## PFIETS     PFIETS  0.00000000
## PINBOED   PINBOED  0.00000000
## AWAPART   AWAPART  0.00000000
## AWABEDR   AWABEDR  0.00000000
## AWALAND   AWALAND  0.00000000
## ABESAUT   ABESAUT  0.00000000
## AMOTSCO   AMOTSCO  0.00000000
## AVRAAUT   AVRAAUT  0.00000000
## AAANHANG AAANHANG  0.00000000
## ATRACTOR ATRACTOR  0.00000000
## AWERKT     AWERKT  0.00000000
## ABROM       ABROM  0.00000000
## ALEVEN     ALEVEN  0.00000000
## APERSONG APERSONG  0.00000000
## AGEZONG   AGEZONG  0.00000000
## AWAOREG   AWAOREG  0.00000000
## AZEILPL   AZEILPL  0.00000000
## APLEZIER APLEZIER  0.00000000
## AFIETS     AFIETS  0.00000000
## AINBOED   AINBOED  0.00000000
## ABYSTAND ABYSTAND  0.00000000
```


_(c) Use the boosting model to predict the response on the test data. Predict that a person will make a purchase if the estimated prob- ability of purchase is greater than 20 %. Form a confusion ma- trix. What fraction of the people predicted to make a purchase do in fact make one? How does this compare with the results obtained from applying KNN or logistic regression to this data set?_


```r
caravan.gbm.predict <- predict(caravan.gbm,newdata = caravan.test,n.trees=1000,type="response")
caravan.gbm.predict <- ifelse(caravan.gbm.predict>.2,1,0)
table(predicted=caravan.gbm.predict,observed=caravan.test$Purchase)
```

```
##          observed
## predicted    0    1
##         0 4411  254
##         1  122   35
```

```r
cat("fraction of predicted purchasers making purchase: ")
```

```
## fraction of predicted purchasers making purchase:
```

```r
35/(128+35)
```

```
## [1] 0.2147239
```

```r
cat("fraction of all purchasers making purchase (in test set): ")
```

```
## fraction of all purchasers making purchase (in test set):
```

```r
mean(caravan.test$Purchase)
```

```
## [1] 0.05993364
```

So a 3.5X improvement over random guessing.

KNN:

Try a few different values of K

```r
ks <- c(1:15)

knn.prediction <- lapply(ks,function(k) {
  knn(caravan.train[,-86], caravan.test[,-86], cl=caravan.train$Purchase,k=k)  %>% as.numeric()-1 }
) 

names(knn.prediction) <- ks

sapply(knn.prediction,function(k.pred) {
  sum(k.pred==1 & caravan.test$Purchase==1)  / sum(k.pred) } ) %>% round(3)
```

```
##     1     2     3     4     5     6     7     8     9    10    11    12 
## 0.084 0.083 0.108 0.138 0.118 0.179 0.111 0.154 0.222 0.000 0.000 0.500 
##    13    14    15 
##   NaN   NaN   NaN
```

However, in the book they standardized the predictors first


```r
ks <- c(1:15)

caravanX <- scale(Caravan[,-86])

caravanX.train <- caravanX[1:1000,]
caravanX.test <- caravanX[1001:nrow(caravanX),]


knn.prediction <- lapply(ks,function(k) {
  knn(caravanX.train, caravanX.test, cl=caravan.train$Purchase,k=k)  %>% as.numeric()-1 }
) 

names(knn.prediction) <- ks

sapply(knn.prediction,function(k.pred) {
    sum(k.pred==1 & caravan.test$Purchase==1, na.rm=TRUE)  / sum(k.pred, na.rm = TRUE) } ) %>% round(3)
```

```
##     1     2     3     4     5     6     7     8     9    10    11    12 
## 0.118 0.095 0.208 0.190 0.278 0.242 0.111 0.143 0.000 0.000   NaN   NaN 
##    13    14    15 
##   NaN   NaN   NaN
```

KNN is better(!)..although we didn't really explore the boost parameter space.

