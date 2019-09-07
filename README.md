-   [<span style="color:blue">*Introduction*</span>](#introduction)
-   [<span style="color:blue">*Exploring the Data Set*</span>](#exploring-the-data-set)
    -   [<span style="color:green">\* Dealing with the variable: `date`\*</span>](#dealing-with-the-variable-date)
    -   [<span style="color:green">\* Dealing with the variable: `long, lat and zipcode`\*</span>](#dealing-with-the-variable-long-lat-and-zipcode)
    -   [<span style="color:green">\* Dropping the variables: `date, id, long, lat, zipcode`\*</span>](#dropping-the-variables-date-id-long-lat-zipcode)
    -   [<span style="color:blue">\* Visualizing the data\*</span>](#visualizing-the-data)
    -   [<span style="color:blue">*Preparing the Data Set*</span>](#preparing-the-data-set)
-   [<span style="color:blue">*Fitting models using `lm` and `rpart`*</span>](#fitting-models-using-lm-and-rpart)
    -   [<span style="color:red">*OLSR Linear (lm) Model:*</span>](#olsr-linear-lm-model)
    -   [<span style="color:red">*Comparison of the `lm` models:*</span>](#comparison-of-the-lm-models)
    -   [<span style="color:green">\* CART (rpart) Model:\*</span>](#cart-rpart-model)
-   [<span style="color:blue">*Conclusion*</span>](#conclusion)

<span style="color:blue">*Introduction*</span>
----------------------------------------------

In this work, our aim is to do regression analysis using an OLSR linear (lm) model and a CART (rpart) model on the dataset `kc_house_data`. The dataset is obtained from Kaggle.

The dataset consists of data of houses sold between May 2014 to May 2015. Our `lm` and `rpart` models will predict the sales of houses in King County with an accuracy of at least 70-78%.

<span style="color:blue">*Exploring the Data Set*</span>
--------------------------------------------------------

First let’s read the data `kc_house_data` and explore the data.

``` r
kc_house_data<-read.csv('kc_house_data.csv')
dim(kc_house_data)
```

    ## [1] 21613    21

``` r
names(kc_house_data)
```

    ##  [1] "id"            "date"          "price"         "bedrooms"     
    ##  [5] "bathrooms"     "sqft_living"   "sqft_lot"      "floors"       
    ##  [9] "waterfront"    "view"          "condition"     "grade"        
    ## [13] "sqft_above"    "sqft_basement" "yr_built"      "yr_renovated" 
    ## [17] "zipcode"       "lat"           "long"          "sqft_living15"
    ## [21] "sqft_lot15"

In this data set, we have 21,613 observations and 21 variables. In this problem, our response varibale is `price`. The explanatory variables are the rest of the variables. Now let’s classify the variables.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
glimpse(kc_house_data)
```

    ## Observations: 21,613
    ## Variables: 21
    ## $ id            <dbl> 7129300520, 6414100192, 5631500400, 2487200875, ...
    ## $ date          <fct> 20141013T000000, 20141209T000000, 20150225T00000...
    ## $ price         <dbl> 221900, 538000, 180000, 604000, 510000, 1225000,...
    ## $ bedrooms      <int> 3, 3, 2, 4, 3, 4, 3, 3, 3, 3, 3, 2, 3, 3, 5, 4, ...
    ## $ bathrooms     <dbl> 1.00, 2.25, 1.00, 3.00, 2.00, 4.50, 2.25, 1.50, ...
    ## $ sqft_living   <int> 1180, 2570, 770, 1960, 1680, 5420, 1715, 1060, 1...
    ## $ sqft_lot      <int> 5650, 7242, 10000, 5000, 8080, 101930, 6819, 971...
    ## $ floors        <dbl> 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0...
    ## $ waterfront    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ view          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, ...
    ## $ condition     <int> 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3, ...
    ## $ grade         <int> 7, 7, 6, 7, 8, 11, 7, 7, 7, 7, 8, 7, 7, 7, 7, 9,...
    ## $ sqft_above    <int> 1180, 2170, 770, 1050, 1680, 3890, 1715, 1060, 1...
    ## $ sqft_basement <int> 0, 400, 0, 910, 0, 1530, 0, 0, 730, 0, 1700, 300...
    ## $ yr_built      <int> 1955, 1951, 1933, 1965, 1987, 2001, 1995, 1963, ...
    ## $ yr_renovated  <int> 0, 1991, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ zipcode       <int> 98178, 98125, 98028, 98136, 98074, 98053, 98003,...
    ## $ lat           <dbl> 47.5112, 47.7210, 47.7379, 47.5208, 47.6168, 47....
    ## $ long          <dbl> -122.257, -122.319, -122.233, -122.393, -122.045...
    ## $ sqft_living15 <int> 1340, 1690, 2720, 1360, 1800, 4760, 2238, 1650, ...
    ## $ sqft_lot15    <int> 5650, 7639, 8062, 5000, 7503, 101930, 6819, 9711...

By looking at the data, we can tell that we have one factor variable: `date` and the other variables are numerical.

### <span style="color:green">\* Dealing with the variable: `date`\*</span>

We need to work on the variable `date` since it is not in a format we want it to be. We install the package `lubridate` for that purpose. We would like to categorize the date data into four classes: `winter`, `spring`, `summer` and `fall`. The following R-code does what we want.

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
month_sold<-month(lubridate::parse_date_time(kc_house_data$date,"ymdHMS"))
kc_house_data$seasons<- 
  cut((month_sold)%%12, breaks = c(-0.1,2.1, 5.1, 8.1, 11.1),
      labels = c("winter", "spring", "summer", "fall"),
      include.lowest = TRUE)
```

### <span style="color:green">\* Dealing with the variable: `long, lat and zipcode`\*</span>

We need to work on these variables since we believe that the association with the house prices are highly not linear. There are several ways to deal with these variables but to make it simple, in this work, we will add a new variable that will somehow represent these variables. We will take the mean of the houses that belong to the same zipcode and call this variable as `zipcode_avg`.

``` r
kc_house_data<-kc_house_data%>%
group_by(zipcode)%>%
  mutate(zipcode_avg=mean(price), zipcode_med=median(price))
```

    ## Warning: The `printer` argument is deprecated as of rlang 0.3.0.
    ## This warning is displayed once per session.

``` r
glimpse(kc_house_data)
```

    ## Observations: 21,613
    ## Variables: 24
    ## $ id            <dbl> 7129300520, 6414100192, 5631500400, 2487200875, ...
    ## $ date          <fct> 20141013T000000, 20141209T000000, 20150225T00000...
    ## $ price         <dbl> 221900, 538000, 180000, 604000, 510000, 1225000,...
    ## $ bedrooms      <int> 3, 3, 2, 4, 3, 4, 3, 3, 3, 3, 3, 2, 3, 3, 5, 4, ...
    ## $ bathrooms     <dbl> 1.00, 2.25, 1.00, 3.00, 2.00, 4.50, 2.25, 1.50, ...
    ## $ sqft_living   <int> 1180, 2570, 770, 1960, 1680, 5420, 1715, 1060, 1...
    ## $ sqft_lot      <int> 5650, 7242, 10000, 5000, 8080, 101930, 6819, 971...
    ## $ floors        <dbl> 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0...
    ## $ waterfront    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ view          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, ...
    ## $ condition     <int> 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3, ...
    ## $ grade         <int> 7, 7, 6, 7, 8, 11, 7, 7, 7, 7, 8, 7, 7, 7, 7, 9,...
    ## $ sqft_above    <int> 1180, 2170, 770, 1050, 1680, 3890, 1715, 1060, 1...
    ## $ sqft_basement <int> 0, 400, 0, 910, 0, 1530, 0, 0, 730, 0, 1700, 300...
    ## $ yr_built      <int> 1955, 1951, 1933, 1965, 1987, 2001, 1995, 1963, ...
    ## $ yr_renovated  <int> 0, 1991, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ zipcode       <int> 98178, 98125, 98028, 98136, 98074, 98053, 98003,...
    ## $ lat           <dbl> 47.5112, 47.7210, 47.7379, 47.5208, 47.6168, 47....
    ## $ long          <dbl> -122.257, -122.319, -122.233, -122.393, -122.045...
    ## $ sqft_living15 <int> 1340, 1690, 2720, 1360, 1800, 4760, 2238, 1650, ...
    ## $ sqft_lot15    <int> 5650, 7639, 8062, 5000, 7503, 101930, 6819, 9711...
    ## $ seasons       <fct> fall, winter, winter, winter, winter, spring, su...
    ## $ zipcode_avg   <dbl> 310612.8, 469455.8, 462480.0, 551688.7, 685605.8...
    ## $ zipcode_med   <dbl> 278277, 425000, 445000, 489950, 642000, 635000, ...

``` r
length(kc_house_data$zipcode_avg)
```

    ## [1] 21613

### <span style="color:green">\* Dropping the variables: `date, id, long, lat, zipcode`\*</span>

It makes sense to drop the variable `date` since we add a new variable `seasons`. Similarly we will drop `zipcode`, `lat` and `long`. We also drop the variable `id` since we believe that it has no effect on `price`.

``` r
kc_house_data<-kc_house_data%>%
  ungroup(-zipcode)%>%
select(-c(id, date,zipcode,lat,long))
```

    ## Warning: `lang()` is deprecated as of rlang 0.2.0.
    ## Please use `call2()` instead.
    ## This warning is displayed once per session.

    ## Warning: `new_overscope()` is deprecated as of rlang 0.2.0.
    ## Please use `new_data_mask()` instead.
    ## This warning is displayed once per session.

    ## Warning: `overscope_eval_next()` is deprecated as of rlang 0.2.0.
    ## Please use `eval_tidy()` with a data mask instead.
    ## This warning is displayed once per session.

``` r
glimpse(kc_house_data)
```

    ## Observations: 21,613
    ## Variables: 19
    ## $ price         <dbl> 221900, 538000, 180000, 604000, 510000, 1225000,...
    ## $ bedrooms      <int> 3, 3, 2, 4, 3, 4, 3, 3, 3, 3, 3, 2, 3, 3, 5, 4, ...
    ## $ bathrooms     <dbl> 1.00, 2.25, 1.00, 3.00, 2.00, 4.50, 2.25, 1.50, ...
    ## $ sqft_living   <int> 1180, 2570, 770, 1960, 1680, 5420, 1715, 1060, 1...
    ## $ sqft_lot      <int> 5650, 7242, 10000, 5000, 8080, 101930, 6819, 971...
    ## $ floors        <dbl> 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0...
    ## $ waterfront    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ view          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, ...
    ## $ condition     <int> 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3, ...
    ## $ grade         <int> 7, 7, 6, 7, 8, 11, 7, 7, 7, 7, 8, 7, 7, 7, 7, 9,...
    ## $ sqft_above    <int> 1180, 2170, 770, 1050, 1680, 3890, 1715, 1060, 1...
    ## $ sqft_basement <int> 0, 400, 0, 910, 0, 1530, 0, 0, 730, 0, 1700, 300...
    ## $ yr_built      <int> 1955, 1951, 1933, 1965, 1987, 2001, 1995, 1963, ...
    ## $ yr_renovated  <int> 0, 1991, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ sqft_living15 <int> 1340, 1690, 2720, 1360, 1800, 4760, 2238, 1650, ...
    ## $ sqft_lot15    <int> 5650, 7639, 8062, 5000, 7503, 101930, 6819, 9711...
    ## $ seasons       <fct> fall, winter, winter, winter, winter, spring, su...
    ## $ zipcode_avg   <dbl> 310612.8, 469455.8, 462480.0, 551688.7, 685605.8...
    ## $ zipcode_med   <dbl> 278277, 425000, 445000, 489950, 642000, 635000, ...

### <span style="color:blue">\* Visualizing the data\*</span>

In this section, we are going to pick some random variables and visually observe if they have some effects on the variable `price`.

``` r
ggplot(data = kc_house_data, aes(x = factor(seasons), y = price)) +
geom_boxplot()
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-7-1.png)

The boxplot `price` vs `seasons` shows us `seasons` does not play a significant role in predicting `price` since the statistics of each category seem close.

\`

Now let's see if `grade` has some effect on `price`.

``` r
ggplot(data = kc_house_data, aes(x = factor(grade), y = log(price))) +
geom_boxplot()
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-8-1.png) The boxplot shows us the variable `grade` has a significant effect on the `price` variable since the statistics of each class highly varies from each other.

Similarly, we can see if `view` has some effect on the price.

``` r
ggplot(data = kc_house_data, aes(x = factor(view), y = price)) +
geom_boxplot()
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-9-1.png)

We see that there is a difference between `view=0` and `view=4`. So we expect `view` to have an effect on the variable `price`.

Now, let's study the relation between `sqft_living` and `price`.

``` r
ggplot(data=kc_house_data,aes(x = sqft_living, y = price )) + 
geom_point()+
geom_smooth(method=lm)
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-10-1.png)

We see a fan shape, so the relation between `sqft_living` and `price` is nonlinear. We will try taking the `log` of the response variable to get better results.

``` r
ggplot(data=kc_house_data,aes(x = sqft_living, y = log(price) )) + 
geom_point()+
geom_smooth()
```

    ## `geom_smooth()` using method = 'gam'

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-11-1.png)

By looking at the plot, we can say that `log(price)` and `sqft_living` has some nonlinear relationship. The regression curve (blue) looks like a square root function. So we are going to add `sqrt(sqft_living)` in our `lm` model when we take the log of `price`.

``` r
ggplot(data=kc_house_data,aes(x = sqrt(sqft_living), y = log(price) )) + 
geom_point()+
geom_smooth()
```

    ## `geom_smooth()` using method = 'gam'

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-12-1.png)

As seen in the plot, the relation between `sqrt(sqrt_living)` and `log(price)` looks like linear.

We expect a relationship between the mean price of each zipcode and the proce of the houses, Below we see that relation.

``` r
ggplot(data=kc_house_data,aes(x = log(zipcode_avg), y = log(price))) + 
geom_point()+
geom_smooth()
```

    ## `geom_smooth()` using method = 'gam'

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-13-1.png)

### <span style="color:blue">*Preparing the Data Set*</span>

Since we are going to compare models and select the best model at the end of the section, we will split our data into training and testing (80/20). Then we will split the training data into training and validation (again, 80/20).

Below is the R-code that helps us to split our data set into two sets: Training and Testing data.

``` r
set.seed(123)
rows <- sample(nrow(kc_house_data)) #randomly order the sampled data
kc_house_data <- kc_house_data[rows, ]
split <- round(nrow(kc_house_data) * .80)
train <- kc_house_data[1:split, ]
test_set <- kc_house_data[(split + 1):nrow(kc_house_data), ]
```

Now, we split our Training Data into two sets: Training and Validation data.

``` r
set.seed(123)
rows <- sample(nrow(train)) #randomly order the sampled data
train <- train[rows, ]
split <- round(nrow(train) * .80)
train_set <- train[1:split, ]
validation_set <- train[(split + 1):nrow(train), ]
```

In short, we have three sets now: train\_set which has 64% of the data, validation\_set which has 16% of the data and test\_set which has the 20% of the data.

``` r
print(c(dim(train_set),dim(validation_set),dim(test_set)))
```

    ## [1] 13832    19  3458    19  4323    19

<span style="color:blue">*Fitting models using `lm` and `rpart`*</span>
-----------------------------------------------------------------------

In this section, we will present `lm` models and then `rpart` models. We will pick the winner in each category based on their RMSE values. Finally, we will announce the winner at the end, in the conclusion part.

### <span style="color:red">*OLSR Linear (lm) Model:*</span>

In this section, we will present a few `lm` models and calculate their corresponding `RMSE` values. We will pick the most optimal one at the end of this section.

#### <span style="color:red">\* Simple Model:\*</span> Our first linear model will be the one that anyone can start without exploring any kind of relationsip between the response variable and the features. We will run our model in the train set: train\_set. Here is the R-command for the model:

``` r
mod_1<-lm(price~., data=train_set)
summary(mod_1)
```

    ## 
    ## Call:
    ## lm(formula = price ~ ., data = train_set)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1049975   -77888     -761    68983  4320242 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.146e+06  1.469e+05  21.423  < 2e-16 ***
    ## bedrooms      -3.493e+04  2.061e+03 -16.948  < 2e-16 ***
    ## bathrooms      3.022e+04  3.585e+03   8.429  < 2e-16 ***
    ## sqft_living    1.669e+02  4.858e+00  34.356  < 2e-16 ***
    ## sqft_lot       1.491e-01  5.456e-02   2.733 0.006276 ** 
    ## floors        -8.314e+03  3.923e+03  -2.119 0.034089 *  
    ## waterfront     6.219e+05  1.865e+04  33.352  < 2e-16 ***
    ## view           5.992e+04  2.349e+03  25.507  < 2e-16 ***
    ## condition      9.380e+03  2.573e+03   3.646 0.000267 ***
    ## grade          6.509e+04  2.395e+03  27.173  < 2e-16 ***
    ## sqft_above     4.197e+01  4.736e+00   8.862  < 2e-16 ***
    ## sqft_basement         NA         NA      NA       NA    
    ## yr_built      -1.921e+03  7.557e+01 -25.424  < 2e-16 ***
    ## yr_renovated   4.676e+00  4.050e+00   1.155 0.248299    
    ## sqft_living15 -3.662e+01  3.764e+00  -9.730  < 2e-16 ***
    ## sqft_lot15    -3.654e-01  8.089e-02  -4.518 6.30e-06 ***
    ## seasonsspring  2.101e+04  4.572e+03   4.596 4.35e-06 ***
    ## seasonssummer -7.425e+03  4.599e+03  -1.614 0.106471    
    ## seasonsfall   -9.840e+03  4.801e+03  -2.050 0.040410 *  
    ## zipcode_avg    8.211e-01  5.090e-02  16.130  < 2e-16 ***
    ## zipcode_med   -1.928e-01  6.077e-02  -3.172 0.001519 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 178100 on 13812 degrees of freedom
    ## Multiple R-squared:  0.778,  Adjusted R-squared:  0.7777 
    ## F-statistic:  2548 on 19 and 13812 DF,  p-value: < 2.2e-16

``` r
par(mfrow=c(2,2))
plot(mod_1)
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-18-1.png)

#### <span style="color:red">\* Discussion of the summary statistics of the model:\*</span>

Before we discuss about the p-values, notice that we have `NA` values in the row of `sqft_basement`. The reason is the collinearity. `sqft_basement` linearly depends on the other two variables: `sqft_living` and `sqft_above`, i.e., we can write `sqft_basement=sqft_living-sqft_above`.

Looking at the `lm` model, we see that `floors` has the highest p-value. `sqft_lot` has the second highest p-value. We are going to drop both of these variables. However, note that the right thing to do is to take each variable (starting with the hightest p-value) out respectively.

For the next model, we will drop the variables: `floors`, `sqft_lot`, `sqft_basement` and `seasons`. Even though the p-values for the variables `long` and `lat` are very small, we will drop those variables, too. There is another way to handle `long` and `lat` but we will not discuss it here but in our later projects. We call this new `lm model` as mod\_2.

#### <span style="color:red">\* Model with excluding insignificant variables:\*</span>

``` r
mod_2<-lm(price~.-floors-sqft_lot-sqft_basement-seasons-yr_renovated, data=train_set)
summary(mod_2)
```

    ## 
    ## Call:
    ## lm(formula = price ~ . - floors - sqft_lot - sqft_basement - 
    ##     seasons - yr_renovated, data = train_set)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1063951   -78060     -218    68603  4313713 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.258e+06  1.384e+05  23.534  < 2e-16 ***
    ## bedrooms      -3.489e+04  2.064e+03 -16.903  < 2e-16 ***
    ## bathrooms      2.848e+04  3.430e+03   8.302  < 2e-16 ***
    ## sqft_living    1.705e+02  4.643e+00  36.721  < 2e-16 ***
    ## waterfront     6.220e+05  1.868e+04  33.303  < 2e-16 ***
    ## view           5.989e+04  2.351e+03  25.477  < 2e-16 ***
    ## condition      8.732e+03  2.534e+03   3.446 0.000570 ***
    ## grade          6.491e+04  2.387e+03  27.196  < 2e-16 ***
    ## sqft_above     3.747e+01  4.283e+00   8.749  < 2e-16 ***
    ## yr_built      -1.979e+03  7.125e+01 -27.778  < 2e-16 ***
    ## sqft_living15 -3.595e+01  3.712e+00  -9.685  < 2e-16 ***
    ## sqft_lot15    -1.922e-01  5.691e-02  -3.377 0.000735 ***
    ## zipcode_avg    8.181e-01  5.101e-02  16.038  < 2e-16 ***
    ## zipcode_med   -1.918e-01  6.089e-02  -3.151 0.001632 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 178700 on 13818 degrees of freedom
    ## Multiple R-squared:  0.7766, Adjusted R-squared:  0.7764 
    ## F-statistic:  3696 on 13 and 13818 DF,  p-value: < 2.2e-16

``` r
par(mfrow=c(2,2))
plot(mod_2)
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-20-1.png)

Looking at the summary, we can tell that all the variables play a significant role assuming the significance level as (=0.05).

#### <span style="color:red">*Model with the log of the response variable:*</span>

As we mentioned earlier, `sqft_living` vs `price` plot has a fan shape. It is worth trying to take the log of the response variable: `price`. We will work on this as our next model.

``` r
mod_3<-lm(log(price)~.-floors-sqft_lot-sqft_basement-seasons-yr_renovated-zipcode_avg-zipcode_med+log(zipcode_med)+log(zipcode_avg), data=train_set)
summary(mod_3)
```

    ## 
    ## Call:
    ## lm(formula = log(price) ~ . - floors - sqft_lot - sqft_basement - 
    ##     seasons - yr_renovated - zipcode_avg - zipcode_med + log(zipcode_med) + 
    ##     log(zipcode_avg), data = train_set)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.08674 -0.12034  0.00371  0.12360  1.03214 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       7.443e+00  1.864e-01  39.922  < 2e-16 ***
    ## bedrooms         -7.433e-03  2.399e-03  -3.098  0.00195 ** 
    ## bathrooms         6.444e-02  3.988e-03  16.158  < 2e-16 ***
    ## sqft_living       1.500e-04  5.394e-06  27.806  < 2e-16 ***
    ## waterfront        3.883e-01  2.170e-02  17.896  < 2e-16 ***
    ## view              6.860e-02  2.735e-03  25.079  < 2e-16 ***
    ## condition         2.738e-02  2.943e-03   9.304  < 2e-16 ***
    ## grade             1.166e-01  2.787e-03  41.858  < 2e-16 ***
    ## sqft_above        2.361e-05  4.977e-06   4.745  2.1e-06 ***
    ## yr_built         -2.545e-03  8.292e-05 -30.691  < 2e-16 ***
    ## sqft_living15     5.552e-06  4.304e-06   1.290  0.19707    
    ## sqft_lot15        1.534e-07  6.607e-08   2.321  0.02029 *  
    ## log(zipcode_med)  3.819e-01  3.797e-02  10.056  < 2e-16 ***
    ## log(zipcode_avg)  3.175e-01  3.581e-02   8.866  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2076 on 13818 degrees of freedom
    ## Multiple R-squared:  0.8453, Adjusted R-squared:  0.8452 
    ## F-statistic:  5808 on 13 and 13818 DF,  p-value: < 2.2e-16

``` r
par(mfrow=c(2,2))
plot(mod_3)
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-22-1.png)

When we look at the statistics, it looks like we get better results as compared to the previous `lm` models. However, we have to be careful because our response variable is in the log form. In the next section, we will compare all these three models.

### <span style="color:red">*Comparison of the `lm` models:*</span>

Now let's compare our three models. We will pick the model as our optimal one with the lowest RMSE and the lowest relative error. We will apply our three models on the validation\_set and evaluate the RMSE values and also the relative errors. The following R-chunk helps us to achieve this aim. Note that we have to take the exponential of the prediction when we use the last model: `mod_3` since the response variable was in the log form.

``` r
pred1 <- predict(object = mod_1,newdata = validation_set)
```

    ## Warning in predict.lm(object = mod_1, newdata = validation_set): prediction
    ## from a rank-deficient fit may be misleading

``` r
pred2 <- predict(object = mod_2,newdata = validation_set)
pred3 <- exp(predict(object = mod_3,newdata = validation_set)) #since the prediction is in log form.

actual<-validation_set$price

re_mod1<-median(abs((pred1 - actual)/actual)) #relative error
re_mod2<-median(abs((pred2 - actual)/actual)) 
re_mod3<-median(abs((pred3 - actual)/actual)) 

re_lm_all<-c(re_mod1, re_mod2, re_mod3)
print(re_lm_all)
```

    ## [1] 0.1552964 0.1578398 0.1193639

Looking at the R-squared values and the relative errors calculated above, we can say the best linear model out of three models is `mod_3` with the relative error 11.9%.

### <span style="color:green">\* CART (rpart) Model:\*</span>

In this section, we will fit our data to rpart model. Our parameters will be taken as the default values. Following this model, we will work on the hyperparameters. First we will play with the complexity parameter and present a model with the optimal complex parameter cp. Given that fixed cp, we will do grid search. We will present RMSE values for every optimal model we find in this section. At the end of the section, we will present our winner.

#### <span style="color:green">*Simple `rpart` model with default settings:*</span>

We start with the basic rpart model where the default values for the hyperparameters are used. The model is fit to our train\_set. Then we calculate the RMSE for the model on the test set. For visualization, we present the plot of the regression tree.

``` r
library(rpart)
set.seed(1)
rpart_model <- rpart(formula = price ~.,
data = train_set,
method = "anova") #since it is regression model, we pick "anova".
rpart_model
```

    ## n= 13832 
    ## 
    ## node), split, n, deviance, yval
    ##       * denotes terminal node
    ## 
    ##  1) root 13832 1.974421e+15  540698.2  
    ##    2) grade< 9.5 12772 7.395538e+14  479434.0  
    ##      4) zipcode_avg< 539824.9 7255 1.546703e+14  360781.9  
    ##        8) sqft_living< 2275 5432 6.088037e+13  317628.7 *
    ##        9) sqft_living>=2275 1823 5.353355e+13  489365.4 *
    ##      5) zipcode_avg>=539824.9 5517 3.484306e+14  635464.8  
    ##       10) sqft_living< 2345.5 3819 1.003059e+14  543762.5 *
    ##       11) sqft_living>=2345.5 1698 1.437788e+14  841713.9  
    ##         22) zipcode_avg< 861255 1449 7.679216e+13  784451.1 *
    ##         23) zipcode_avg>=861255 249 3.458621e+13 1174942.0 *
    ##    3) grade>=9.5 1060 6.093331e+14 1278873.0  
    ##      6) sqft_living< 5005 929 2.340396e+14 1131966.0  
    ##       12) view< 3.5 867 1.532965e+14 1068380.0  
    ##         24) zipcode_avg< 861255 722 6.730145e+13  957969.1 *
    ##         25) zipcode_avg>=861255 145 3.336759e+13 1618150.0 *
    ##       13) view>=3.5 62 2.821806e+13 2021145.0 *
    ##      7) sqft_living>=5005 131 2.130613e+14 2320682.0  
    ##       14) sqft_living< 7940 123 1.068015e+14 2114984.0  
    ##         28) zipcode_avg< 861255 89 5.365050e+13 1801511.0 *
    ##         29) zipcode_avg>=861255 34 2.151245e+13 2935546.0 *
    ##       15) sqft_living>=7940 8 2.103882e+13 5483288.0 *

This summary tells us which variables and the cutoff values are taken in the construction of the regression tree but it helps more when we see those variables and the values visually. Below is the regression tree for this model:

``` r
library(rpart.plot)
rpart.plot(rpart_model)
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-25-1.png)

We see that `root`, `grade`, `lat`, `sqft_living`, `grade`, `yr_built`, `long`, `waterfront`, `sqft_above` are taken as variables in this regression tree. For instance a house with `grade`=8, `lat`=62, `sqft_living`=3000, `zip_code`=98028 has a prediction of price as 768K. As observed no\_children, gender and region do not play a role in predicting costs.

Now, let’s evaluate the RMSE for our first rpart model: rpart\_model on the validation\_set.

``` r
pred <- predict(object = rpart_model,newdata = validation_set)
actual<-validation_set$price
rmse<-sqrt(mean((pred - actual)^2))
print(rmse)
```

    ## [1] 211764.2

#### <span style="color:green">\* Playing with the hyperparameters:\*</span>

In this section, for a better model, we will work on the hyperparameters. First, we will start with finding the optimal complex parameter cp. Then we will do the grid search.

#### <span style="color:green">\* Finding an optimal `cp`:\*</span>

First, let’s plot X-val Relative Error vs cp plot.

``` r
plotcp(rpart_model)
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-27-1.png) This `cp` plot shows the X-val Relative Error for cp∈(0.01,∞). Since X-val Relative Error&lt;0.2, any value close to 0.01 is acceptable. Note that our default cp is 0.01. Below is a table for a few cp values within the range (0.01, ∞) and their X-val Relative Error.

``` r
print(rpart_model$cptable)
```

    ##            CP nsplit rel error    xerror       xstd
    ## 1  0.31681893      0 1.0000000 1.0001658 0.05667259
    ## 2  0.11975813      1 0.6831811 0.6900816 0.04062542
    ## 3  0.08216701      2 0.5634229 0.5703713 0.04027944
    ## 4  0.05284883      3 0.4812559 0.5339332 0.03037637
    ## 5  0.04316249      4 0.4284071 0.4708683 0.02913376
    ## 6  0.02662869      5 0.3852446 0.4220598 0.02339724
    ## 7  0.02038897      7 0.3319872 0.3822336 0.01772466
    ## 8  0.01641010      8 0.3115982 0.3493672 0.01754946
    ## 9  0.01602423      9 0.2951881 0.3318806 0.01733064
    ## 10 0.01000000     10 0.2791639 0.3176185 0.01639752

``` r
opt_index <- which.min(rpart_model$cptable[, "xerror"])
cp_opt <- rpart_model$cptable[opt_index, "CP"]
print(cp_opt)
```

    ## [1] 0.01

``` r
model_opt <- prune(tree = rpart_model, cp = cp_opt)
```

Since the optimal cp is the default value, model\_opt is same as our first rpart model: rpart\_model.

However, we are curious if we get smaller RMSE values with lower cp values. We believe it is worth trying. So we write a for loop for cp values less than 0.01.

``` r
set.seed(1)
 small_cp_models <- list()
for (i in 1:9) {
    small_cp_models[[i]] <- rpart(formula = price ~ ., 
                               data =train_set , 
                               method = "anova",
                               cp=0.001*i)
}
```

The R-code above gives us 9 models with cp values ∈ {0.001,0.002,...,0.009}. We calculate their RMSE values on the validation set as follows:

``` r
rmse_values <- c() 
for (i in 1:length(small_cp_models)) {
model <-small_cp_models[[i]]
pred <- predict(object = model,newdata = validation_set)
actual<-validation_set$price
rmse_values[i] <- sqrt(mean((pred - actual)^2)) #rmse values for cp=c(0.001,0.002,...,0.009)
}
print(rmse_values)
```

    ## [1] 172974.6 179510.6 182077.6 189410.4 196169.1 201752.6 204598.2 208069.8
    ## [9] 208069.8

The RMSE value takes its minimum value when cp=0.001. Note that, we have to check if the X-val Relative Error remains small, too. Below, we present the cp plot for small cp values as the verification.

``` r
rpart_cp_0001 <- small_cp_models[[1]] # cp=0.001
plotcp(rpart_cp_0001)
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-33-1.png)

``` r
print(rpart_cp_0001$cptable)
```

    ##             CP nsplit rel error    xerror       xstd
    ## 1  0.316818928      0 1.0000000 1.0001658 0.05667259
    ## 2  0.119758132      1 0.6831811 0.6900816 0.04062542
    ## 3  0.082167006      2 0.5634229 0.5703713 0.04027944
    ## 4  0.052848835      3 0.4812559 0.5339332 0.03037637
    ## 5  0.043162494      4 0.4284071 0.4708683 0.02913376
    ## 6  0.026628694      5 0.3852446 0.4220598 0.02339724
    ## 7  0.020388972      7 0.3319872 0.3822336 0.01772466
    ## 8  0.016410096      8 0.3115982 0.3493672 0.01754946
    ## 9  0.016024226      9 0.2951881 0.3318806 0.01733064
    ## 10 0.009719506     10 0.2791639 0.3158539 0.01637058
    ## 11 0.007510740     11 0.2694444 0.3032235 0.01581010
    ## 12 0.007231187     12 0.2619337 0.3021959 0.01580910
    ## 13 0.006895537     13 0.2547025 0.2990206 0.01580090
    ## 14 0.006340971     14 0.2478070 0.2918059 0.01579570
    ## 15 0.005773166     15 0.2414660 0.2834448 0.01563505
    ## 16 0.005631819     16 0.2356928 0.2762926 0.01553289
    ## 17 0.005303182     17 0.2300610 0.2732020 0.01547893
    ## 18 0.005094794     18 0.2247578 0.2739784 0.01551404
    ## 19 0.004433305     19 0.2196630 0.2686693 0.01504971
    ## 20 0.004333166     20 0.2152297 0.2674352 0.01498714
    ## 21 0.004314247     21 0.2108966 0.2690030 0.01512344
    ## 22 0.003932079     22 0.2065823 0.2649323 0.01504716
    ## 23 0.003358539     23 0.2026502 0.2595672 0.01472451
    ## 24 0.003318801     24 0.1992917 0.2543504 0.01422536
    ## 25 0.002997214     25 0.1959729 0.2521422 0.01419418
    ## 26 0.002402645     26 0.1929757 0.2458176 0.01405187
    ## 27 0.002382178     27 0.1905730 0.2437603 0.01398625
    ## 28 0.002049804     28 0.1881908 0.2401804 0.01369221
    ## 29 0.001846032     29 0.1861410 0.2397908 0.01374196
    ## 30 0.001769763     30 0.1842950 0.2381558 0.01368065
    ## 31 0.001648874     31 0.1825252 0.2373698 0.01366638
    ## 32 0.001515892     32 0.1808764 0.2348040 0.01358423
    ## 33 0.001362942     33 0.1793605 0.2330222 0.01359590
    ## 34 0.001354388     34 0.1779975 0.2326789 0.01358343
    ## 35 0.001340002     35 0.1766432 0.2325094 0.01358502
    ## 36 0.001329951     36 0.1753032 0.2310113 0.01356901
    ## 37 0.001262755     37 0.1739732 0.2309890 0.01359134
    ## 38 0.001231803     38 0.1727104 0.2294592 0.01352520
    ## 39 0.001219289     39 0.1714786 0.2296899 0.01355550
    ## 40 0.001175385     40 0.1702594 0.2292397 0.01354997
    ## 41 0.001146385     41 0.1690840 0.2286708 0.01354032
    ## 42 0.001126042     42 0.1679376 0.2294673 0.01358001
    ## 43 0.001091547     43 0.1668115 0.2291943 0.01350868
    ## 44 0.001088783     44 0.1657200 0.2290920 0.01347336
    ## 45 0.001037583     45 0.1646312 0.2283578 0.01349078
    ## 46 0.001000000     46 0.1635936 0.2273886 0.01348361

Since we picked cp=0.001, as seen in the above plot, we expect to see the size of the trees as 20 in the tree plot. As compared to our first tree plot, both tree plots look more complicated since we increased the level of complexity.

``` r
rpart.plot(rpart_cp_0001)
```

![](LinearRegression_Project_files/figure-markdown_github/unnamed-chunk-35-1.png)

#### <span style="color:green">\* Doing a grid search:\*</span>

Now, let’s set up the grid by establishing a list of possible values for minsplit and maxdepth.

``` r
minsplit <- seq(2, 10, 1)
maxdepth <- seq(4, 10, 1)
hyper_grid <- expand.grid(minsplit = minsplit,maxdepth = maxdepth)
head(hyper_grid)
```

    ##   minsplit maxdepth
    ## 1        2        4
    ## 2        3        4
    ## 3        4        4
    ## 4        5        4
    ## 5        6        4
    ## 6        7        4

In the above R-code, minsplit takes values from 2 to 10 and similarly, maxdepth takes values from 4 to 10. So in total, we have 9×7=63 pairs. Below, we write `rpart` models for these 63 pairs. Thus in total we present 63 models. As usual, we fit all these models on the train\_set. Note that, we will take cp as 0.001 since it was the optimal cp value we found earlier.

``` r
set.seed(1)
# Number of potential models in the grid
num_models <- nrow(hyper_grid)

# Create an empty list to store models
rpart_grid_models <- list()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:num_models) {

    # Get minsplit, maxdepth values at row i
    minsplit <- hyper_grid$minsplit[i]
    maxdepth <- hyper_grid$maxdepth[i]

    # Train a model and store in the list
    rpart_grid_models[[i]] <- rpart(formula = price ~ ., 
                               data =train_set , 
                               method = "anova",
                               minsplit = minsplit,
                               maxdepth = maxdepth,
                               cp=0.001
                               )
}
```

Now, we constructed 63 rpart models but we have to pick the optimal one. The one with the smallest RMSE on the validation\_set will be our optimal one.

``` r
rmse_values <- c() 
for (i in 1:length(rpart_grid_models)) {
model <- rpart_grid_models[[i]]
pred <- predict(object = model,newdata = validation_set)
actual<-validation_set$price
rmse_values[i] <- sqrt(mean((pred - actual)^2))
}
rmse_values
```

    ##  [1] 204961.3 204961.3 204961.3 204961.3 204961.3 204961.3 204961.3
    ##  [8] 200808.7 200808.7 186597.6 186597.6 186597.6 186597.6 186597.6
    ## [15] 186597.6 186597.6 182026.5 182026.5 179209.5 179209.5 179209.5
    ## [22] 179104.1 179104.1 179104.1 179319.0 174557.4 174557.4 177151.5
    ## [29] 177151.5 177151.5 177044.8 177044.8 177044.8 177267.9 172449.7
    ## [36] 172449.7 176704.9 176704.9 176704.9 176597.9 176597.9 176597.9
    ## [43] 176821.6 171990.9 171990.9 176704.9 176704.9 176704.9 176597.9
    ## [50] 176597.9 176597.9 176821.6 171990.9 171990.9 176704.9 176704.9
    ## [57] 176704.9 176597.9 176597.9 176597.9 176821.6 171990.9 171990.9

``` r
best_grid_model <- rpart_grid_models[[which.min(rmse_values)]]
best_grid_model$control
```

    ## $minsplit
    ## [1] 9
    ## 
    ## $minbucket
    ## [1] 3
    ## 
    ## $cp
    ## [1] 0.001
    ## 
    ## $maxcompete
    ## [1] 4
    ## 
    ## $maxsurrogate
    ## [1] 5
    ## 
    ## $usesurrogate
    ## [1] 2
    ## 
    ## $surrogatestyle
    ## [1] 0
    ## 
    ## $maxdepth
    ## [1] 8
    ## 
    ## $xval
    ## [1] 10

Several rpart models above have the smallest RMSE value. However, we will pick the one with the smallest minsplit and maxdepth to keep the model simple. The simplest model with the smallest RMSE occurs when cp=0.001, minsplit=9 and maxdepth=10. and it is 166203.8. This is the winner rpart model in this section.

#### <span style="color:green">\* Comparison of the `rpart` models:\*</span>

Although we calculated the RMSE values for the rpart models above, we will this time calculate the relative error as we did for the linear models, pick the one with the smallest relative error. Below is the R-code that compares the models on the validation set and calculates the RMSE values.

``` r
pred1 <- predict(object = rpart_model,newdata = validation_set)
pred2 <- predict(object = rpart_cp_0001,newdata = validation_set)
pred3 <- predict(object = best_grid_model,newdata = validation_set)


actual<-validation_set$price
re_1<-median(abs((pred1 - actual)/actual)) 
re_2<-median(abs((pred2 - actual)/actual)) 
re_3<-median(abs((pred3 - actual)/actual)) 

re_rpart_all<-c(re_1,re_2,re_3)
print(re_rpart_all)
```

    ## [1] 0.2052452 0.1405045 0.1405045

<span style="color:blue">*Conclusion*</span>
--------------------------------------------

The winner of lm models has RMSE= 193099.9 and the winner of rpart models has RMSE=166203.8. Thus the winner overall is the winner of rpart models: best\_grid\_model.

Finally, we will apply this winner rpart model on the full dataset healthcare. Note that we are taking exactly the same hyperparameters as best\_grid\_model.

``` r
best_grid_model_full<- rpart(formula = price ~ ., 
                               data =kc_house_data, 
                               method = "anova",
                               minsplit = 9,
                               maxdepth = 10,
                               cp=0.001
                               )
```

Now we apply our model best\_grid\_model\_full to test\_set.

``` r
pred <- predict(object = best_grid_model_full,newdata = test_set)
actual<-test_set$price
re_mod3<-median(abs((pred - actual)/actual)) 
print(c(rmse,re_mod3))
```

    ## [1] 2.117642e+05 1.417461e-01

The final RMSE value is 2.117642e+05 with the relative error 14.17%.
