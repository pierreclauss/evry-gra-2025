Workshop 1 - Equity portfolio choice, Global Minimum Variance portfolio
and factor models
================
Pierre Clauss
September 2025

*The following R Markdown document has to be read with my course notes
(in particular for the details of the analysis framework).*

*Not all the R codes are displayed but only some of them to help you to
succeed the workshop.*

## Foreword

To begin these workshops, I have to precise the 3 necessary steps for a
data science project:

1.  data: (i) importation, (ii) wrangling and (iii) visualisation (or
    named also *exploratory data analysis*)
2.  modelling
3.  results communication

To achieve these 3 steps, the universe of the package **tidyverse** is
essential for R nowadays.

``` r
library(tidyverse)
```

The first step on data seems often a thankless task but it takes time
(we can estimate it to more than 50% of a data science project) and it
is essential for the success of the project.

The second step is the most grateful for a data scientist. But it is
important to know that the first step and the second one are overlapped:
indeed, when I wrangle or I visualise my data, I often have an idea of
the models I will try.

Finally, the third step is often neglected to a simple ppt, word or tex
document. But it has to be more appreciated with the new tools and this
example of R Markdown document and github website is an attempt to show
you how to communicate better the results of a data science project.

## 1 Data

### 1.1 Importation

I import the data with the package **tidyquant**, which allows to import
financial data directly from Yahoo Finance.

``` r
library(tidyquant)
symbols <-
  c(
    "AI.PA",
    "CS.PA",
    "BNP.PA",
    "EN.PA",
    "CA.PA",
    "BN.PA",
    "ENGI.PA",
    "EL.PA",
    "LR.PA",
    "OR.PA",
    "MC.PA",
    "ML.PA",
    "RI.PA",
    "SGO.PA",
    "SAN.PA",
    "SU.PA",
    "GLE.PA",
    "STLAP.PA",
    "TTE.PA",
    "VIE.PA",
    "CAP.PA",
    "AIR.PA",
    "KER.PA",
    "ORA.PA"
  )

stock_prices <- symbols %>%
  tq_get(get  = "stock.prices",
         from = "2006-05-02",
         to   = "2025-08-31") %>%
  group_by(symbol)
```

The start date is 2nd May 2006.

### 1.2 Wrangling

The wrangling (*démêlage* in French) consists in the storage and the
transformation of the data. “Tidying and transforming are called
**wrangling**, because getting your data in a form that’s natural to
work with often feels like a fight” [R for Data
Science](https://r4ds.had.co.nz/introduction.html) (Grolemund G. and
Wickham H.).

I need for the workshop returns to construct portfolios on: so I
calculate monthly returns from daily stock prices and define the right
format to obtain a matrix of the returns by stock.

``` r
monthly_returns <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    type       = "arithmetic",
    col_rename = "monthly.returns"
  )

table_returns <- monthly_returns %>%
  pivot_wider(names_from = symbol, values_from = monthly.returns) %>%
  select(-"date") %>%
  na.omit()
```

Then, the wrangling is quite simple here: data are tidy - each column is
a variable (a stock) and each line is an observation (a month) - and
data have been transformed in financial returns, which are used to model
a portfolio.

As I said just before, data and modelling steps are effectively
overlapped. I can confirm this relation between the two steps with the
choice of monthly observations. Indeed, monthly returns are nearer to
gaussian data than weekly and daily data: they present less extreme
variations (see next section on data viz). As theoretical framework of
H. Markowitz behaves well with gaussian data, this choice is relevant.

To simplify our communication of results, I focus on the 6 first
variables.

``` r
(table_returns_first <- table_returns %>% select(1:6))
```

    ## # A tibble: 232 × 6
    ##       AI.PA   CS.PA   BNP.PA   EN.PA    CA.PA   BN.PA
    ##       <dbl>   <dbl>    <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 -0.0345   0.0152  0.00185  0.0115  0.00524 -0.0301
    ##  2  0.0365  -0.0199  0.0671  -0.0519  0.0130   0.0552
    ##  3  0.0374   0.0385  0.0174  -0.0276  0.0648   0.0412
    ##  4  0.0424   0.0741  0.0892   0.0514 -0.0133   0.0377
    ##  5 -0.0231   0       0.0229   0.0263  0.0347   0.0307
    ##  6  0.0367   0.0345  0.0153   0.0830 -0.0419   0.0370
    ##  7  0.0246  -0.0333 -0.0557  -0.0186 -0.0111   0.0122
    ##  8  0.0527   0.0690  0.0160   0.0848 -0.0269  -0.0120
    ##  9 -0.00667  0.0323  0.0351   0.0662 -0.0379   0.0296
    ## 10 -0.0304   0      -0.0783   0.0164  0.142    0.0127
    ## # ℹ 222 more rows

We can see below, thanks to the package **DataExplorer**, a summary of
the tidy data observed for the first 6 variables.

``` r
library(DataExplorer)
plot_intro(table_returns_first)
```

![](workshop1_files/figure-gfm/wrangling_2-1.png)<!-- -->

I can conclude that data are tidy without missing values.

### 1.3 Visualisation

Data viz has to be thought in relation with modelling. We have just seen
that the modelling framework needs gaussian data; it needs also not
perfectly correlated data. Then, I am interested by visualising the
distribution of the returns and the structure of the correlations
between them.

First of all, I can observe the evolutions of stock prices.

``` r
monthly_returns %>%
  mutate(price.index = 100 * cumprod(1 + monthly.returns)) %>%
  ggplot(aes(x = date, y = price.index, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Stock Prices in the same initial base") +
  theme_tq() +
  scale_color_tq()
```

![](workshop1_files/figure-gfm/dataviz_1-1.png)<!-- -->

``` r
monthly_prices <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = to.monthly,
               indexAt = "lastof")

monthly_prices %>%
  ungroup() %>%
  slice_head(n = nrow(table_returns) * 6) %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Stock Prices",
    x = "Date",
    y = "Adjusted Prices",
    color = ""
  ) +
  facet_wrap( ~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "€", prefix = "")) +
  theme_tq() +
  scale_color_tq()
```

![](workshop1_files/figure-gfm/dataviz_1-2.png)<!-- -->

Some statistics to sum up the distribution are shown below: I can
observe symmetric data with a median and a mean which are quite equal.

``` r
summary(table_returns_first)
```

    ##      AI.PA              CS.PA              BNP.PA              EN.PA          
    ##  Min.   :-0.13454   Min.   :-0.39935   Min.   :-0.371703   Min.   :-0.246900  
    ##  1st Qu.:-0.01769   1st Qu.:-0.03356   1st Qu.:-0.049140   1st Qu.:-0.035168  
    ##  Median : 0.01178   Median : 0.01488   Median : 0.013508   Median : 0.006217  
    ##  Mean   : 0.01002   Mean   : 0.01144   Mean   : 0.008645   Mean   : 0.006123  
    ##  3rd Qu.: 0.03687   3rd Qu.: 0.05880   3rd Qu.: 0.067126   3rd Qu.: 0.051570  
    ##  Max.   : 0.13924   Max.   : 0.42841   Max.   : 0.439122   Max.   : 0.268997  
    ##      CA.PA               BN.PA          
    ##  Min.   :-0.243071   Min.   :-0.207393  
    ##  1st Qu.:-0.045363   1st Qu.:-0.022308  
    ##  Median : 0.003178   Median : 0.006851  
    ##  Mean   :-0.000773   Mean   : 0.005468  
    ##  3rd Qu.: 0.042806   3rd Qu.: 0.037865  
    ##  Max.   : 0.159289   Max.   : 0.152109

I can go deeper thanks to distribution graphics: the non-parametric
(kernel method) estimation of the distribution and QQ-plots.

``` r
monthly_returns %>%
  ungroup() %>%
  slice_head(n = nrow(table_returns) * 6) %>%
  ggplot(aes(x = monthly.returns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densities of arithmetic monthly returns",
       x = "Monthly returns", y = "Density") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2)
```

![](workshop1_files/figure-gfm/dataviz_2-1.png)<!-- -->

``` r
monthly_returns %>%
  ungroup() %>%
  slice_head(n = nrow(table_returns) * 6) %>%
  ggplot(aes(sample = monthly.returns, fill = symbol)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-plots versus Normal distribution",
       x = "Normal returns", y = "Monthly returns") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbol, ncol = 2)
```

![](workshop1_files/figure-gfm/dataviz_2-2.png)<!-- -->

Finally, I can visualize the correlations between each of the 24 stock
returns. To obtain efficient diversification between assets, we need
correlations smaller than 1, which can be observed in the graph below.

``` r
library(corrplot)
corrplot(
  cor(table_returns),
  type = 'upper',
  tl.col = 'black',
  tl.cex = 0.1
)
```

![](workshop1_files/figure-gfm/dataviz_3-1.png)<!-- -->

## 2 Modelling

Before all, I need to load the package **scales** to communicate with a
pretty way the results of our allocations.

``` r
library(scales)
```

### 2.1 Analysis framework

The analysis framework of our modelling is the Modern Portfolio Theory
initiated by H. Markowitz in the 1950s. An essential portfolio which is
agnostic on expected returns is the *Global Minimum Variance* (GMV)
portfolio for which the weights
![\omega](https://latex.codecogs.com/png.latex?%5Comega "\omega") are
equal to :

![\omega = \frac{1}{C}\Sigma^{-1}e](https://latex.codecogs.com/png.latex?%5Comega%20%3D%20%5Cfrac%7B1%7D%7BC%7D%5CSigma%5E%7B-1%7De "\omega = \frac{1}{C}\Sigma^{-1}e")

with ![\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\Sigma")
the covariance matrix between assets returns,
![C = e'\Sigma^{-1}e](https://latex.codecogs.com/png.latex?C%20%3D%20e%27%5CSigma%5E%7B-1%7De "C = e'\Sigma^{-1}e")
and ![e](https://latex.codecogs.com/png.latex?e "e") a vector of
![1](https://latex.codecogs.com/png.latex?1 "1") of length
![n](https://latex.codecogs.com/png.latex?n "n"), the number of assets
in the portfolio.

### 2.2 Estimation methodologies

I can propose 2 plug-in methodologies to estimate the GMV portfolio and
to achieve our objective:

1.  classical estimators without bias
2.  estimators constructed with a more robust method to decrease the
    noise of the data (factorial modelling)

Before modelling, I separate the initial sample between a learning
sample and a backtest sample to evaluate the performance of our
modelling. I choose December 2019 as a separation date to backtest the
strategy on the last 2 years of the sample.

``` r
end_date <- nrow(table_returns)
table_returns_learning <- table_returns %>% slice(1:164)
table_returns_backtest <- table_returns %>% slice(165:end_date)
```

There are 164 learning observations and 68 backtest observations. My
objective is to observe if the ex-ante (or anticipated) volatility
(equal to
![\displaystyle\sqrt\frac{1}{C}](https://latex.codecogs.com/png.latex?%5Cdisplaystyle%5Csqrt%5Cfrac%7B1%7D%7BC%7D "\displaystyle\sqrt\frac{1}{C}")
for the GMV portfolio) is near to the ex-post (or realised) volatility
and if these volatilities are well minimised.

#### 2.2.1 Unbiased GMV portfolio

The GMV portfolio is only based on the covariance matrix estimation
![\hat\Sigma](https://latex.codecogs.com/png.latex?%5Chat%5CSigma "\hat\Sigma").
This matrix estimator is inversed in the formula to determine the
weights of the portfolio. Then I can propose an unbiased estimator of
the inverse covariance matrix:

![\hat\Sigma\_\text{unbiased} = \frac{1}{T-n-2}\sum\_{t=1}^T\left(r_t-\hat\mu\right)\left(r_t-\hat\mu\right)'](https://latex.codecogs.com/png.latex?%5Chat%5CSigma_%5Ctext%7Bunbiased%7D%20%3D%20%5Cfrac%7B1%7D%7BT-n-2%7D%5Csum_%7Bt%3D1%7D%5ET%5Cleft%28r_t-%5Chat%5Cmu%5Cright%29%5Cleft%28r_t-%5Chat%5Cmu%5Cright%29%27 "\hat\Sigma_\text{unbiased} = \frac{1}{T-n-2}\sum_{t=1}^T\left(r_t-\hat\mu\right)\left(r_t-\hat\mu\right)'")

with
![\displaystyle\hat\mu=\frac{1}{T}\sum\_{t=1}^Tr_t](https://latex.codecogs.com/png.latex?%5Cdisplaystyle%5Chat%5Cmu%3D%5Cfrac%7B1%7D%7BT%7D%5Csum_%7Bt%3D1%7D%5ETr_t "\displaystyle\hat\mu=\frac{1}{T}\sum_{t=1}^Tr_t"),
![T](https://latex.codecogs.com/png.latex?T "T") the number of
observations, ![n](https://latex.codecogs.com/png.latex?n "n") the
number of assets and
![r_t](https://latex.codecogs.com/png.latex?r_t "r_t") the vector of
financial returns for the
![n](https://latex.codecogs.com/png.latex?n "n") assets observed at time
![t](https://latex.codecogs.com/png.latex?t "t").

Then, I can plug-in this estimate in the formula of the GMV portfolio to
obtain unbiased estimators of GMV weights.

``` r
n <- ncol(table_returns_learning)
T <- nrow(table_returns_learning)
e <- rep(1, n)
perio <- 12

Sigma <- cov(table_returns_learning) * (T - 1) / (T - n - 2) * perio
C <- t(e) %*% solve(Sigma) %*% e
sigmag <- sqrt(1 / C)
omega <- 1 / as.numeric(C) * solve(Sigma) %*% e
barplot(as.numeric(omega), col = 'black')
```

![](workshop1_files/figure-gfm/gmv_empirical-1.png)<!-- -->

The anticipated volatility of the portfolio constructed on the learning
sample is equal to 11.40%.

The realised volatility of the portfolio observed on the backtest sample
is equal to 13.45%.

**Code for the realised volatility (with Alt Gr 7 at the beginning and
at the end): r percent(sd(as.matrix(table_returns_backtest) %*% omega) *
sqrt(perio), accuracy = 0.01)**

I am going to improve these results thanks to a more robust statistical
approach.

#### 2.2.2 GMV portfolio with factorial modelling (1 factor)

I use Principal Component Analysis (PCA) to improve
![\hat\Sigma](https://latex.codecogs.com/png.latex?%5Chat%5CSigma "\hat\Sigma").
Because of its high dimensionality, the inversion of the matrix could be
noisy. Then I reduce the dimensionality to one systematic factor (and 3
factors in the next section). This modelling is inspired by W. Sharpe
who was the first to propose a factor structure of the covariance matrix
with the *Single-Index Model* in 1963. To go further see pages 304-305
of the [chapter written by M. Brandt in 2010 in the Handbook of
Financial
Econometrics](https://www.sciencedirect.com/science/article/abs/pii/B9780444508973500080?via%3Dihub).

I can write the estimator (remind my course for the proof) with the
following formula:

![\hat\Sigma\_\text{1factor} = \lambda_1\phi_1\phi_1'+\Sigma\_{\varepsilon}](https://latex.codecogs.com/png.latex?%5Chat%5CSigma_%5Ctext%7B1factor%7D%20%3D%20%5Clambda_1%5Cphi_1%5Cphi_1%27%2B%5CSigma_%7B%5Cvarepsilon%7D "\hat\Sigma_\text{1factor} = \lambda_1\phi_1\phi_1'+\Sigma_{\varepsilon}")

with
![\lambda_1](https://latex.codecogs.com/png.latex?%5Clambda_1 "\lambda_1")
the first eigenvalue of the unbiased estimator of the covariance matrix,
![\phi_1](https://latex.codecogs.com/png.latex?%5Cphi_1 "\phi_1") the
first eigenvector and
![\Sigma\_\varepsilon](https://latex.codecogs.com/png.latex?%5CSigma_%5Cvarepsilon "\Sigma_\varepsilon")
the diagonal residual covariance matrix determined for each asset
![i](https://latex.codecogs.com/png.latex?i "i") thanks to:

![\text{Var}\left(\varepsilon_i\right) = \text{Var}\left(r_i\right) - \phi\_{1i}^2\lambda_1](https://latex.codecogs.com/png.latex?%5Ctext%7BVar%7D%5Cleft%28%5Cvarepsilon_i%5Cright%29%20%3D%20%5Ctext%7BVar%7D%5Cleft%28r_i%5Cright%29%20-%20%5Cphi_%7B1i%7D%5E2%5Clambda_1 "\text{Var}\left(\varepsilon_i\right) = \text{Var}\left(r_i\right) - \phi_{1i}^2\lambda_1")

Then, I can plug-in this estimator of covariance matrix in the formula
of the GMV portfolio to obtain a more robust estimator of GMV weights.

``` r
valp <- eigen(Sigma)$values
vecp <- eigen(Sigma)$vectors
vp1 <- vecp[, 1]
lambda1 <- valp[1]
varepsilon1 <- diag(Sigma) - vp1 ^ 2 * lambda1
Sigma_epsilon1 <- diag(varepsilon1, n, n)
Sigma1 <- (lambda1 * vp1 %*% t(vp1) + Sigma_epsilon1)
C1 <- t(e) %*% solve(Sigma1) %*% e
sigmag1 <- sqrt(1 / C1)
omega1 <- 1 / as.numeric(C1) * solve(Sigma1) %*% e
barplot(as.numeric(omega1), col = 'black')
```

![](workshop1_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

The anticipated volatility of the portfolio constructed on the learning
sample is equal to 8.76%.

The realised volatility of the portfolio observed on the backtest sample
is equal to 12.47%.

#### 2.2.3 GMV portfolio with factorial modelling (3 factors)

I can write the estimator with the following formula:

![\hat\Sigma\_\text{3factors} = \Phi_f\Lambda_f\Phi_f'+\Sigma\_{\varepsilon}](https://latex.codecogs.com/png.latex?%5Chat%5CSigma_%5Ctext%7B3factors%7D%20%3D%20%5CPhi_f%5CLambda_f%5CPhi_f%27%2B%5CSigma_%7B%5Cvarepsilon%7D "\hat\Sigma_\text{3factors} = \Phi_f\Lambda_f\Phi_f'+\Sigma_{\varepsilon}")

with
![\Lambda_f](https://latex.codecogs.com/png.latex?%5CLambda_f "\Lambda_f")
the diagonal matrix of the three first eigenvalues of the unbiased
estimator of the covariance matrix,
![\Phi_f](https://latex.codecogs.com/png.latex?%5CPhi_f "\Phi_f") the
matrix with the first three eigenvectors and
![\Sigma\_\varepsilon](https://latex.codecogs.com/png.latex?%5CSigma_%5Cvarepsilon "\Sigma_\varepsilon")
the diagonal residual covariance matrix determined for each asset
![i](https://latex.codecogs.com/png.latex?i "i") thanks to:

![\text{Var}\left(\varepsilon_i\right) = \text{Var}\left(r_i\right) - \phi\_{1i}^2\lambda_1 - \phi\_{2i}^2\lambda_2 - \phi\_{3i}^2\lambda_3](https://latex.codecogs.com/png.latex?%5Ctext%7BVar%7D%5Cleft%28%5Cvarepsilon_i%5Cright%29%20%3D%20%5Ctext%7BVar%7D%5Cleft%28r_i%5Cright%29%20-%20%5Cphi_%7B1i%7D%5E2%5Clambda_1%20-%20%5Cphi_%7B2i%7D%5E2%5Clambda_2%20-%20%5Cphi_%7B3i%7D%5E2%5Clambda_3 "\text{Var}\left(\varepsilon_i\right) = \text{Var}\left(r_i\right) - \phi_{1i}^2\lambda_1 - \phi_{2i}^2\lambda_2 - \phi_{3i}^2\lambda_3")

Then, I can plug-in this estimator of covariance matrix in the formula
of the GMV portfolio to obtain a more robust estimator of GMV weights.

``` r
vp3 <- cbind(vecp[, 1], vecp[, 2], vecp[, 3])
lambda3 <- diag(c(valp[1], valp[2], valp[3]), 3, 3)
varepsilon3 <- diag(Sigma) - vp3 ^ 2 %*% diag(lambda3)
Sigma_epsilon3 <- diag(as.numeric(varepsilon3), n, n)
Sigma3 <- (vp3 %*% lambda3 %*% t(vp3) + Sigma_epsilon3)
C3 <- t(e) %*% solve(Sigma3) %*% e
sigmag3 <- sqrt(1 / C3)
omega3 <- 1 / as.numeric(C3) * solve(Sigma3) %*% e
barplot(as.numeric(omega3), col = 'black')
```

![](workshop1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The anticipated volatility of the portfolio constructed on the learning
sample is equal to 9.72%.

The realised volatility of the portfolio observed on the backtest sample
is equal to 13.38%.

## To conclude the first workshop

This workshop is the first of my course on Asset Management dedicated to
equities and GMV portfolio. I present some improvements of the classical
plug-in estimator of the covariance matrix thanks to factorial
modelling.
