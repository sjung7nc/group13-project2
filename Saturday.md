Bike Share Project
================
Soohee Jung, Kera Whitley

# Set up

Libraries and other set up should be in this chunk

``` r
library(tidyverse)
library(caret)
library(gbm)
library(shiny)
```

# Introduction

This dataset compiles the daily casual, registered and total (combined
casual and residual) bikers using this bike share. Looking at the
available variables in the dataset, there are several that are
attributes of the date, and the rest are attributes of the weather. We
will specifically be looking at the temperature, season, year and the
weather to predict the total number of bikers using the bike share.

\[Explain the variables\]

# Data

``` r
# Read in dataset
day.data <- read_csv("day.csv")
```

    ## 
    ## -- Column specification -------------------------------------------------------------------------------------------------------------------
    ## cols(
    ##   instant = col_double(),
    ##   dteday = col_date(format = ""),
    ##   season = col_double(),
    ##   yr = col_double(),
    ##   mnth = col_double(),
    ##   holiday = col_double(),
    ##   weekday = col_double(),
    ##   workingday = col_double(),
    ##   weathersit = col_double(),
    ##   temp = col_double(),
    ##   atemp = col_double(),
    ##   hum = col_double(),
    ##   windspeed = col_double(),
    ##   casual = col_double(),
    ##   registered = col_double(),
    ##   cnt = col_double()
    ## )

``` r
# Convert weekday column from numeric to character value
day.data$days[day.data$weekday==0] <- "Sunday"
day.data$days[day.data$weekday==1] <- "Monday"
day.data$days[day.data$weekday==2] <- "Tuesday"
day.data$days[day.data$weekday==3] <- "Wednesday"
day.data$days[day.data$weekday==4] <- "Thursday"
day.data$days[day.data$weekday==5] <- "Friday"
day.data$days[day.data$weekday==6] <- "Saturday"

# Get unique days
weekdays <- unique(day.data$days)

day <- day.data %>% filter(days == params$Day)

# Converting variables that should be factors into factor variables
day$season <- factor(day$season)
levels(day$season) <- c("Winter","Spring", "Summer", "Fall")

day$yr <- factor(day$yr)
levels(day$yr) <- c("2011", "2012")

day$mnth <- factor(day$mnth)
levels(day$mnth) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

day$holiday <- factor(day$holiday)
levels(day$holiday) <- c("Not Holiday", "Holiday")

day$workingday <- factor(day$workingday)
levels(day$workingday) <- c("Not Working", "Working")

day$weathersit <- factor(day$weathersit)
levels(day$weathersit) <- c("Fair", "Misty", "Light Snow/Rain", "Heavy Rain/Ice/Snow")

set.seed(13)
# The training set should be 70% of the data
n <- nrow(day) * 0.7
train <- sample_n(day, n, replace = FALSE)
test <- anti_join(day, train, by = "dteday")
```

# Summarizations

``` r
# Numerical summaries
summary(train)
```

    ##     instant          dteday              season      yr          mnth           holiday      weekday        workingday
    ##  Min.   :  1.0   Min.   :2011-01-01   Winter:20   2011:31   Jan    : 8   Not Holiday:73   Min.   :6   Not Working:73  
    ##  1st Qu.:211.0   1st Qu.:2011-07-30   Spring:18   2012:42   Dec    : 8   Holiday    : 0   1st Qu.:6   Working    : 0  
    ##  Median :421.0   Median :2012-02-25   Summer:20             Mar    : 7                    Median :6                   
    ##  Mean   :388.6   Mean   :2012-01-23   Fall  :15             Jul    : 7                    Mean   :6                   
    ##  3rd Qu.:568.0   3rd Qu.:2012-07-21                         Aug    : 7                    3rd Qu.:6                   
    ##  Max.   :729.0   Max.   :2012-12-29                         Apr    : 6                    Max.   :6                   
    ##                                                             (Other):30                                                
    ##                weathersit      temp             atemp              hum           windspeed          casual       registered  
    ##  Fair               :49   Min.   :0.05913   Min.   :0.07907   Min.   :0.1879   Min.   :0.0454   Min.   :  67   Min.   : 654  
    ##  Misty              :23   1st Qu.:0.32417   1st Qu.:0.32448   1st Qu.:0.4988   1st Qu.:0.1461   1st Qu.: 724   1st Qu.:1949  
    ##  Light Snow/Rain    : 1   Median :0.49500   Median :0.48673   Median :0.6012   Median :0.1928   Median :1448   Median :3248  
    ##  Heavy Rain/Ice/Snow: 0   Mean   :0.48666   Mean   :0.46660   Mean   :0.6007   Mean   :0.2012   Mean   :1525   Mean   :3173  
    ##                           3rd Qu.:0.65917   3rd Qu.:0.61238   3rd Qu.:0.7129   3rd Qu.:0.2351   3rd Qu.:2352   3rd Qu.:4280  
    ##                           Max.   :0.86167   Max.   :0.80491   Max.   :0.9292   Max.   :0.5075   Max.   :3410   Max.   :5966  
    ##                                                                                                                              
    ##       cnt           days          
    ##  Min.   : 981   Length:73         
    ##  1st Qu.:2732   Class :character  
    ##  Median :4758   Mode  :character  
    ##  Mean   :4698                     
    ##  3rd Qu.:6624                     
    ##  Max.   :8714                     
    ## 

``` r
train %>% select(casual, registered) %>% colSums()
```

    ##     casual registered 
    ##     111329     231653

``` r
table(train$season, train$weathersit)
```

    ##         
    ##          Fair Misty Light Snow/Rain Heavy Rain/Ice/Snow
    ##   Winter   13     7               0                   0
    ##   Spring   13     5               0                   0
    ##   Summer   12     7               1                   0
    ##   Fall     11     4               0                   0

``` r
table(train$workingday, train$weathersit)
```

    ##              
    ##               Fair Misty Light Snow/Rain Heavy Rain/Ice/Snow
    ##   Not Working   49    23               1                   0
    ##   Working        0     0               0                   0

``` r
train %>% group_by(yr) %>% summarize(Total.Bikers=sum(cnt))
```

    ## # A tibble: 2 x 2
    ##   yr    Total.Bikers
    ##   <fct>        <dbl>
    ## 1 2011         98829
    ## 2 2012        244153

``` r
# We can inspect the trend of users across years
ggplot(train, aes(x = yr, y = cnt)) + 
  geom_violin(fill = "dark grey", color = "dark red") + 
  geom_jitter(shape = 16) + 
  labs(x = "Year", y = "Number of Bikers") + 
  theme_minimal()
```

![](Saturday_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
train %>% group_by(mnth) %>% 
  summarize(Total.casual=sum(casual),Total.registered=sum(registered),
            Total.Bikers=sum(cnt))
```

    ## # A tibble: 12 x 4
    ##    mnth  Total.casual Total.registered Total.Bikers
    ##    <fct>        <dbl>            <dbl>        <dbl>
    ##  1 Jan           3014            13636        16650
    ##  2 Feb           2672             8490        11162
    ##  3 Mar          10574            18680        29254
    ##  4 Apr          12330            18630        30960
    ##  5 May          11790            18636        30426
    ##  6 Jun          14447            27360        41807
    ##  7 Jul          12265            24371        36636
    ##  8 Aug          14187            26489        40676
    ##  9 Sep          12170            25610        37780
    ## 10 Oct           9897            18232        28129
    ## 11 Nov           3753            10393        14146
    ## 12 Dec           4230            21126        25356

``` r
# We can inspect the trend of users across months using this plot.
# There may be a seasonal effect present.
ggplot(train, aes(x = mnth, y = cnt, fill = "red")) + 
  geom_col() + 
  geom_col(data = train, aes(x = mnth, y = casual, fill = "blue")) +
  labs(x = "Month", y = "Total Number of Bikers") +
  scale_fill_discrete(name = "Biker Type", labels = c("Casual", "Registered"))
```

![](Saturday_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
train %>% group_by(season,mnth) %>% 
  summarize(Total.casual=sum(casual),Total.registered=sum(registered),
            Total.Bikers=sum(cnt))
```

    ## `summarise()` has grouped output by 'season'. You can override using the `.groups` argument.

    ## # A tibble: 16 x 5
    ## # Groups:   season [4]
    ##    season mnth  Total.casual Total.registered Total.Bikers
    ##    <fct>  <fct>        <dbl>            <dbl>        <dbl>
    ##  1 Winter Jan           3014            13636        16650
    ##  2 Winter Feb           2672             8490        11162
    ##  3 Winter Mar           6259            10892        17151
    ##  4 Winter Dec           1029             4546         5575
    ##  5 Spring Mar           4315             7788        12103
    ##  6 Spring Apr          12330            18630        30960
    ##  7 Spring May          11790            18636        30426
    ##  8 Spring Jun          10335            18327        28662
    ##  9 Summer Jun           4112             9033        13145
    ## 10 Summer Jul          12265            24371        36636
    ## 11 Summer Aug          14187            26489        40676
    ## 12 Summer Sep           9581            19644        29225
    ## 13 Fall   Sep           2589             5966         8555
    ## 14 Fall   Oct           9897            18232        28129
    ## 15 Fall   Nov           3753            10393        14146
    ## 16 Fall   Dec           3201            16580        19781

``` r
# We can inspect the trend of all users across season using this plot.
# There may be weather or temperature effect present.
ggplot(train, aes(x = season, y = cnt)) + 
  geom_violin() +
  geom_jitter(shape = 16, size = 3, aes(color = mnth)) +
  labs(x = "Season", y = "Number of Bikers") +
  scale_color_manual(name = "Month", values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", 
                                                "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", 
                                                "#cab2d6", "#6a3d9a", "#dfc27d", "#b15928")) +
  theme_minimal()
```

![](Saturday_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

``` r
by.weather <- train %>% group_by(weathersit) %>% 
  summarize(Total.casual=sum(casual),Total.registered=sum(registered),
            Total.Bikers=sum(cnt))
by.weather
```

    ## # A tibble: 3 x 4
    ##   weathersit      Total.casual Total.registered Total.Bikers
    ##   <fct>                  <dbl>            <dbl>        <dbl>
    ## 1 Fair                   82962           164678       247640
    ## 2 Misty                  27103            63780        90883
    ## 3 Light Snow/Rain         1264             3195         4459

``` r
# We can inspect the trend of all users across weather condition using this plot.
ggplot(by.weather, aes(x=weathersit, y=Total.Bikers))+geom_col(fill="cornflowerblue", width = 0.8)+
  scale_x_discrete(name="Weather")
```

![](Saturday_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->

``` r
# We can inspect the trend of all users across temperature using this plot.
ggplot(train, aes(x=temp, y=cnt)) + geom_point() + geom_smooth()+
  scale_x_continuous(name="Temperature")+scale_y_discrete(name="Bikers")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Saturday_files/figure-gfm/unnamed-chunk-18-5.png)<!-- -->

``` r
by.holi <- train %>% group_by(holiday) %>% 
  summarize(Total.casual=sum(casual),Total.registered=sum(registered),
            Total.Bikers=sum(cnt))
by.holi
```

    ## # A tibble: 1 x 4
    ##   holiday     Total.casual Total.registered Total.Bikers
    ##   <fct>              <dbl>            <dbl>        <dbl>
    ## 1 Not Holiday       111329           231653       342982

``` r
# We can inspect the trend of all users across whether holiday or not using this plot.
ggplot(by.holi, aes(x=holiday, y=Total.Bikers)) + geom_col(fill="darkgoldenrod1", width = 0.7)+
  scale_x_discrete(name="Holiday")
```

![](Saturday_files/figure-gfm/unnamed-chunk-18-6.png)<!-- -->

``` r
# We can inspect the trend of casual users across whether holiday or not using this plot.
ggplot(train, aes(x=holiday, y=casual))+geom_boxplot(fill="darkmagenta")+
  scale_x_discrete(name="Holiday")+scale_y_continuous(name="Casual Users")
```

![](Saturday_files/figure-gfm/unnamed-chunk-18-7.png)<!-- -->

``` r
# We can inspect the trend of registered users across whether holiday or not using this plot.
ggplot(train, aes(x=holiday, y=registered))+geom_boxplot(fill="darkorchid")+
  scale_x_discrete(name="Holiday")+scale_y_continuous(name="Registered Users")
```

![](Saturday_files/figure-gfm/unnamed-chunk-18-8.png)<!-- -->

# Modeling

## Linear Regression Model

*Linear regression* tries to find a linear equation which describe the
relationship between a response variable and a explanation variable. The
best model fit is made by minimizing the sum of squared residuals.
Simple linear regression model can be extended in many ways and we call
them *Multiple Linear Regression*.

``` r
set.seed(13)
# multiple linear regression model 1
lmFit <- train(cnt ~ season + temp + I(temp^2), data=train, method="lm",
               trControl=trainControl(method="cv",number=10))
summary(lmFit)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2750.58  -945.77   -48.81  1075.89  2990.32 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -2874.8     1135.0  -2.533  0.01366 *  
    ## seasonSpring   1031.7      615.9   1.675  0.09855 .  
    ## seasonSummer    587.1      886.1   0.663  0.50990    
    ## seasonFall     1189.7      537.0   2.215  0.03013 *  
    ## temp          23543.0     5399.5   4.360 4.58e-05 ***
    ## `I(temp^2)`  -16585.1     5737.0  -2.891  0.00517 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1491 on 67 degrees of freedom
    ## Multiple R-squared:  0.5984, Adjusted R-squared:  0.5684 
    ## F-statistic: 19.97 on 5 and 67 DF,  p-value: 3.916e-12

``` r
lmPred <- predict(lmFit, newdata=test)

# multiple linear regression model 2
set.seed(13)
mlrFit <- train(cnt ~ season + temp + yr, data = train, method = "lm", 
               trControl = trainControl(method = "cv", number = 10))
summary(mlrFit)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2963.10  -617.09   -13.71   575.62  2903.88 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -735.746    493.140  -1.492 0.140403    
    ## seasonSpring 1295.462    486.325   2.664 0.009669 ** 
    ## seasonSummer   -5.843    672.997  -0.009 0.993099    
    ## seasonFall   1675.777    418.644   4.003 0.000159 ***
    ## temp         7335.399   1359.631   5.395 9.65e-07 ***
    ## yr2012       2089.343    288.267   7.248 5.44e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1184 on 67 degrees of freedom
    ## Multiple R-squared:  0.7468, Adjusted R-squared:  0.7279 
    ## F-statistic: 39.53 on 5 and 67 DF,  p-value: < 2.2e-16

``` r
mlrPred <- predict(mlrFit, newdata = test)
```

## Random Forest Model

*Random forest model* is one of 3 major methods of *Ensemble tree
model*. Create a tree from a random subset of predictors for a bootstrap
sample and then train the tree. Repeat this for many times, say 100 or
1000 repeats. The final prediction is average of these predictions.

``` r
set.seed(13)
# Get random forest model fit
rfFit <- train(cnt ~ season + temp + weathersit, data=train,
               method="rf", 
               trControl=trainControl(method="cv",number=10))
rfFit
```

    ## Random Forest 
    ## 
    ## 73 samples
    ##  3 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 67, 67, 65, 65, 66, 66, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared   MAE     
    ##   2     1540.116  0.5933214  1348.783
    ##   4     1486.283  0.6010737  1249.174
    ##   7     1576.560  0.5743263  1317.772
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 4.

``` r
rfPred <- predict(rfFit, newdata=test)
```

## Boosted Tree Model

The *boosted tree model* is a type of *ensemble tree model*. The way the
boosted tree works is that the trees are fit sequentially. Each new tree
is fit on on a modified version of the original data and the predictions
are updated as the trees are grown.

``` r
library(gbm)
set.seed(13)

boostFit <- gbm(cnt ~ season + temp + yr + weathersit, data = train, distribution = "gaussian", n.trees = 5000, 
                shrinkage = 0.1, interaction.depth = 4)
boostFit
```

    ## gbm(formula = cnt ~ season + temp + yr + weathersit, distribution = "gaussian", 
    ##     data = train, n.trees = 5000, interaction.depth = 4, shrinkage = 0.1)
    ## A gradient boosted model with gaussian loss function.
    ## 5000 iterations were performed.
    ## There were 4 predictors of which 4 had non-zero influence.

``` r
boostPred <- predict(boostFit, newdata = test, n.trees = 5000)
boostPred
```

    ##  [1]  922.3353 1408.4523  479.1223 4525.9857 2828.6216 5597.0962 6620.1368 4714.1429 4756.8839 4441.7813 4407.5285 4407.5285 5685.5292
    ## [14] 5100.4383 4242.1458 3946.1828 6306.0430 5469.3458 2108.3055 3707.2122 4247.0181 2477.9896 1572.1056 1308.0219 2224.4592 3129.5166
    ## [27] 5973.4445 7520.6866 7860.2638 6803.6972 5052.4404 4711.0908

# Comparison

``` r
set.seed(13)
# multiple linear regression model 1
multiRMSE <- postResample(lmPred, test$cnt)
multiRMSE
```

    ##         RMSE     Rsquared          MAE 
    ## 1675.3391484    0.4082355 1318.4319877

``` r
# multiple linear regression model 2
mlrRMSE <- postResample(mlrPred, test$cnt)
mlrRMSE
```

    ##        RMSE    Rsquared         MAE 
    ## 1102.348416    0.691651  818.066409

``` r
# random forest model
rfRMSE <- postResample(rfPred, test$cnt)
rfRMSE
```

    ##         RMSE     Rsquared          MAE 
    ## 1782.5831393    0.3444367 1464.2832200

``` r
# boosted tree model
boostRMSE <- postResample(boostPred, test$cnt)
boostRMSE
```

    ##         RMSE     Rsquared          MAE 
    ## 1376.2412186    0.5611671 1012.4733248

``` r
# compare
lowestRMSE <- c(MultipleLR1=multiRMSE[1],MultipleLR2=mlrRMSE[1],RandomForest=rfRMSE[1],Boosting=boostRMSE[1])
lowestRMSE
```

    ##  MultipleLR1.RMSE  MultipleLR2.RMSE RandomForest.RMSE     Boosting.RMSE 
    ##          1675.339          1102.348          1782.583          1376.241

The preferred model has the lowest RMSE. The model that has the lowest
RMSE for \[day\] is 2, and has the model \[equation\].
