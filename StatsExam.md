PHSL4005 Stats. Exam
================
Vishaylin J. Mahadeo
August 31, 2016

Question 1
==========

Body Temperature between males and female heart rates
-----------------------------------------------------

``` r
#load data set
df_Q1 <- read_csv('question1.csv')

#view first 6 lines of data set
head(df_Q1, 6)
```

    ## # A tibble: 6 x 3
    ##   body_temperature  male female
    ##              <dbl> <int>  <int>
    ## 1             35.7    70     NA
    ## 2             35.8    NA     69
    ## 3             35.9    71     NA
    ## 4             35.9    NA     62
    ## 5             36.0    NA     75
    ## 6             36.1    74     NA

``` r
#tidy data (Collect males)
Q1_male <- df_Q1 %>% 
  filter(male != 'NA') %>%
  select(body_temperature, male)
Q1_male
```

    ## # A tibble: 65 x 2
    ##    body_temperature  male
    ##               <dbl> <int>
    ## 1              35.7    70
    ## 2              35.9    71
    ## 3              36.1    74
    ## 4              36.1    80
    ## 5              36.2    73
    ## 6              36.2    75
    ## 7              36.2    82
    ## 8              36.2    64
    ## 9              36.3    69
    ## 10             36.3    70
    ## # ... with 55 more rows

``` r
#tidy data (Collect females)
Q1_female <- df_Q1 %>%
  filter(female != 'NA') %>%
  select(body_temperature, female)
Q1_female
```

    ## # A tibble: 65 x 2
    ##    body_temperature female
    ##               <dbl>  <int>
    ## 1              35.8     69
    ## 2              35.9     62
    ## 3              36.0     75
    ## 4              36.2     66
    ## 5              36.2     68
    ## 6              36.3     57
    ## 7              36.4     61
    ## 8              36.5     84
    ## 9              36.5     61
    ## 10             36.6     77
    ## # ... with 55 more rows

``` r
#plot data (males)
male_plot <- plot(x = Q1_male$male, y = Q1_male$body_temperature,
                  main = 'Scatter plot showing male heart rate vs. body temperature',
                  xlab = 'Male heart rate (BPM)',
                  ylab = 'Body temperature (degrees Celcius)')

#linear regression for males
male_rgl <- lm(body_temperature~male, data = Q1_male)
abline(male_rgl)
```

<img src="figure/Q1_load-1.png" style="display: block; margin: auto;" />

``` r
#print regression line for males
summary(male_rgl)
```

    ## 
    ## Call:
    ## lm(formula = body_temperature ~ male, data = Q1_male)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.98425 -0.28425  0.05305  0.24062  0.80280 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 35.813752   0.601771  59.514   <2e-16 ***
    ## male         0.012436   0.008176   1.521    0.133    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3843 on 63 degrees of freedom
    ## Multiple R-squared:  0.03542,    Adjusted R-squared:  0.02011 
    ## F-statistic: 2.313 on 1 and 63 DF,  p-value: 0.1333

``` r
#check homoskedasticity
male_homosk <- plot(x = male_rgl$fitted, y = male_rgl$residuals,
                    main = 'Homoskedasticity check for males',
                    xlab = 'Fitted', ylab = 'Residuals')
abline(h=0)
```

<img src="figure/Q1_load-2.png" style="display: block; margin: auto;" />

``` r
#Check Gaussian distribution
qqnorm(male_rgl$residuals)
qqline(male_rgl$residuals)
```

<img src="figure/Q1_load-3.png" style="display: block; margin: auto;" />

``` r
#plot data (females)
female_plot <- plot(x = Q1_female$female, y = Q1_female$body_temperature,
                    main = 'Scatter plot showing female heart rate vs. body temperature',
                    xlab = 'Female heart rate (BPM)',
                    ylab = 'Body temperature (degrees Celcius)')

#linear regression for females
female_rgl <- lm(body_temperature~female, data = Q1_female)
abline(female_rgl)
```

<img src="figure/Q1_load-4.png" style="display: block; margin: auto;" />

``` r
#print regression line for females
summary(female_rgl)
```

    ## 
    ## Call:
    ## lm(formula = body_temperature ~ female, data = Q1_female)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.01469 -0.21469 -0.01469  0.24192  1.26961 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 35.816769   0.458825  78.062   <2e-16 ***
    ## female       0.014463   0.006151   2.351   0.0219 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3989 on 63 degrees of freedom
    ## Multiple R-squared:  0.08066,    Adjusted R-squared:  0.06607 
    ## F-statistic: 5.528 on 1 and 63 DF,  p-value: 0.02186

``` r
#check homoskedasticity
female_homosk <- plot(x = female_rgl$fitted, y = female_rgl$residuals,
                    main = 'Homoskedasticity check for females',
                    xlab = 'Fitted', ylab = 'Residuals')
abline(h=0)
```

<img src="figure/Q1_load-5.png" style="display: block; margin: auto;" />

``` r
#Check Gaussian distribution
qqnorm(female_rgl$residuals)
qqline(female_rgl$residuals)
```

<img src="figure/Q1_load-6.png" style="display: block; margin: auto;" /> **Null Hypothesis:** Heart rates in males and females have no association with body temperature.

**Alternative Hypothesis:** Heart rates in males and females have an association with body temperature.

**Assumptions:**

1.  a = 0.05

2.  Paired, continuous data set.

3.  Parametric data set because normal distribution indicated by a normal Q-Q plot.

4.  Residuals are normally distributed as shown by homoskedasticity check.

5.  Reject null hypothesis if p &lt; a.

6.  Pearson's correlation will be used to test hypothesis.

``` r
#male Pearson's correlation
male_cor <- with(Q1_male,
                 cor.test(x = male, y = body_temperature,
                          method = 'pearson'))
male_cor
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  male and body_temperature
    ## t = 1.521, df = 63, p-value = 0.1333
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.05837844  0.41313575
    ## sample estimates:
    ##       cor 
    ## 0.1882004

``` r
#male Pearson's correlation
female_cor <- with(Q1_female,
                 cor.test(x = female, y = body_temperature,
                          method = 'pearson'))
female_cor
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  female and body_temperature
    ## t = 2.3511, df = 63, p-value = 0.02186
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.04310142 0.49371392
    ## sample estimates:
    ##       cor 
    ## 0.2840149

**Results:**

Males presented with t(63) = 1.521; p = 0.1333 and R<sup>2</sup> (-0.058 to 0.413)= 0.188.

Females presented with t(63) = 2.351; p = 0.022 and R<sup>2</sup> (0.043 to 0.494)= 0.284.

p &lt; a, thus reject null hypotheis and accept alternative.

**Conclusion:** In conclusion there is a weak, positive association between heart rates in males and females and their body temperature.

------------------------------------------------------------------------

Question 2
==========

Handedness compared to ataxic walking and intoxication
------------------------------------------------------

``` r
#load data set
df_Q2 <- read.csv('question2.csv')

#plot data set
```

**Null Hypothesis:**

**Alternative Hypothesis:**

**Assumptions:**

1.  2.  3.  4.  5.  

------------------------------------------------------------------------

Question 3
==========

Can running time be used to predict calories burned
---------------------------------------------------

``` r
#load data set
df_Q3 <- read_csv('question3.csv')

#plot data set
Q3_plot <- plot(time~calories, data = df_Q3,
                main = 'Scatter plot representing running time vs. calories burnt' ,
                xlab = 'Running Time (s)', ylab = 'Calories consumed(cal)')

#tidy data
Q3_run <- select(df_Q3, time, calories)

#linear regression 
run_rgl <- lm(calories~time, data = Q3_run)
abline(run_rgl)
```

<img src="figure/Q3_load-1.png" style="display: block; margin: auto;" />

``` r
#print regression line for males
summary(run_rgl)
```

    ## 
    ## Call:
    ## lm(formula = calories ~ time, data = Q3_run)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -60.76 -15.09   4.04  15.67  55.21 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 939.36422  150.70365   6.233 9.09e-06 ***
    ## time         -0.28031    0.07432  -3.772  0.00152 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 30.17 on 17 degrees of freedom
    ## Multiple R-squared:  0.4556, Adjusted R-squared:  0.4235 
    ## F-statistic: 14.22 on 1 and 17 DF,  p-value: 0.001522

``` r
#check homoskedasticity
run_homosk <- plot(x = run_rgl$fitted, y = run_rgl$residuals,
                    main = 'Homoskedasticity check for running time vs. calories consumed',
                    xlab = 'Fitted', ylab = 'Residuals')
abline(h=0)
```

<img src="figure/Q3_load-2.png" style="display: block; margin: auto;" />

``` r
#Check Gaussian distribution
qqnorm(run_rgl$residuals)
qqline(run_rgl$residuals)
```

<img src="figure/Q3_load-3.png" style="display: block; margin: auto;" />

**Null Hypothesis:** Running time (s) cannot be used to predict number of calories consumed (cal) during a run.

**Alternative Hypothesis:** Running time (s) can be used to predict number of calories consumed (cal) during a run.

**Assumptions:**

1.  a = 0.05

2.  Paired, interval data set with no outliers.

3.  Gaussian residual distribution(i.e.normal distribution) indicated by normal Q-Q plot.

4.  Parametric data set.

5.  Residuals are heteroskedastic because they show spread around the abline at 0.

6.  Running time is measured without error.

7.  Observations are independent.

8.  If p &lt; a reject null hypothesis.

9.  A Pearson's correlation will be used to test hypothesis.

``` r
#Pearson's correlation
run_cor <- with(Q3_run,
                 cor.test(x = time, y = calories,
                          method = 'pearson'))
run_cor
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  time and calories
    ## t = -3.7715, df = 17, p-value = 0.001522
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8642170 -0.3183295
    ## sample estimates:
    ##        cor 
    ## -0.6749491

``` r
#calculate regression coefficient
regco <- coefficients(run_rgl)
regco
```

    ## (Intercept)        time 
    ## 939.3642218  -0.2803149

``` r
#fit coeffiecients into formula
origTime <- 30
decTimeSec <- origTime * 60
run_time <- mean(df_Q3$run) - decTimeSec
CaloriesC <- regco[1] + regco[2] * run_time
CaloriesC
```

    ## (Intercept) 
    ##    1441.128

**Results:**

t(17) = -3.771; p = 0.002 and R<sup>2</sup>(-0.864 to -0.318) = -0.674.

p &lt; a, thus reject null hypothesis.

**Conclusion:** Running time has a strong, negative correlation with calories burnt. Therefore the shorter runs burn more calories than longer runs.

------------------------------------------------------------------------

Questions 4
===========

------------------------------------------------------------------------
