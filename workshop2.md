Workshop 2 - Multi-assets tactical allocation, Tangency Portfolio and
Black-Litterman approach with ESG ratings
================
Pierre Clauss
September 2025

*The following R Markdown document has to be read with my course notes
(in particular for the details of the analysis framework).*

*Not all the R codes are displayed but only some of them to help you to
succeed the workshop.*

## 1 Data

### 1.1 Importation

I import the data with the package **tidyquant**, which allows to import
financial data directly from Yahoo Finance. I import 6 equities from CAC
40 and with a good balance between brown and green features: Bouygues,
Engie, TotalEnergies on one side and Schneider Electric, Capgemini, LVMH
on the other side. I import also a green bond ETF proposed by BlackRock.

``` r
library(tidyverse)
library(tidyquant)

symbols <-
  c("EN.PA",
    "ENGI.PA",
    "TTE.PA",
    "SU.PA",
    "CAP.PA",
    "MC.PA",
    "BGRN")

stock_prices <- symbols %>%
  tq_get(get  = "stock.prices",
         from = "2018-11-27",
         to   = "2025-09-30") %>%
  group_by(symbol)
```

The start date is November 2018.

### 1.2 Wrangling

For the workshop, I need only returns to do calculation on: so I
determine monthly financial returns on stock prices. Returns are monthly
returns because they are nearer to gaussian data than weekly and daily
data.

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
a variable (an equity) and each line is an observation (a month) - and
data have been transformed in financial returns, which are used to model
a portfolio.

We can see below, thanks to the package **DataExplorer**, a summary of
the observed data.

``` r
library(DataExplorer)
plot_intro(table_returns)
```

![](workshop2_files/figure-gfm/wrangling-1.png)<!-- -->

I can conclude that data are tidy without missing values.

### 1.3 Visualisation

Data viz has to be thought in relation with modelling. As for workshop
1, I am interested by visualising the distributions of the returns and
the structure of the correlations between them.

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

![](workshop2_files/figure-gfm/dataviz_1-1.png)<!-- -->

Some statistics to sum up the distribution are shown below: I cannot
observe for all returns symmetric data with a median and a mean which
could be quite equal.

``` r
summary(table_returns)
```

    ##      EN.PA              ENGI.PA             TTE.PA              SU.PA         
    ##  Min.   :-0.246900   Min.   :-0.37379   Min.   :-0.142474   Min.   :-0.13394  
    ##  1st Qu.:-0.025070   1st Qu.:-0.02141   1st Qu.:-0.038134   1st Qu.:-0.01744  
    ##  Median : 0.013494   Median : 0.01593   Median : 0.003991   Median : 0.02217  
    ##  Mean   : 0.007877   Mean   : 0.01213   Mean   : 0.008655   Mean   : 0.01935  
    ##  3rd Qu.: 0.051419   3rd Qu.: 0.05334   3rd Qu.: 0.046467   3rd Qu.: 0.06024  
    ##  Max.   : 0.185791   Max.   : 0.19114   Max.   : 0.387490   Max.   : 0.19167  
    ##      CAP.PA              MC.PA               BGRN          
    ##  Min.   :-0.223947   Min.   :-0.17729   Min.   :-0.040008  
    ##  1st Qu.:-0.052590   1st Qu.:-0.03600   1st Qu.:-0.006876  
    ##  Median : 0.008067   Median : 0.01426   Median : 0.002721  
    ##  Mean   : 0.006886   Mean   : 0.01192   Mean   : 0.001922  
    ##  3rd Qu.: 0.074056   3rd Qu.: 0.06242   3rd Qu.: 0.013077  
    ##  Max.   : 0.173089   Max.   : 0.20035   Max.   : 0.039630

I can go deeper thanks to distribution graphics: the non-parametric
(kernel method) estimation of the distribution and QQ-plots.

``` r
monthly_returns %>%
  ggplot(aes(x = monthly.returns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densities of arithmetic monthly returns",
       x = "Daily returns", y = "Density") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap( ~ symbol, ncol = 2)
```

![](workshop2_files/figure-gfm/dataviz_2-1.png)<!-- -->

``` r
monthly_returns %>%
  ggplot(aes(sample = monthly.returns, fill = symbol)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-plots versus Normal distribution",
       x = "Normal returns", y = "Monthly returns") +
  theme_tq() +
  scale_fill_tq() +
  facet_wrap( ~ symbol, ncol = 2)
```

![](workshop2_files/figure-gfm/dataviz_2-2.png)<!-- -->

Finally, I can visualize the correlations between each of the equities
and bond returns. To obtain efficient diversification between assets, we
need correlations smaller than 1, which can be observed in the graph
below.

``` r
library(corrplot)
corrplot(
  cor(table_returns),
  type = 'upper',
  tl.col = 'black',
  tl.cex = 0.5
)
```

![](workshop2_files/figure-gfm/dataviz_3-1.png)<!-- -->

## 2 Modelling

Before all, I need to load the package **scales** to communicate with a
pretty way the results of our allocations.

``` r
library(scales)
```

### 2.1 Analysis framework

The analysis framework of our modelling is the Modern Portfolio Theory
initiated by H. Markowitz in the 1950s. An essential portfolio, with the
*Global Minimum Variance* (GMV) portfolio studied in the workshop 1, is
the *Tangency Portfolio* (TP), also named *Maximum Sharpe ratio
portfolio*, for which the weights
![\omega](https://latex.codecogs.com/png.latex?%5Comega "\omega") are
equal to :

![\omega = \frac{1}{\tilde A}\Sigma^{-1}\tilde\mu](https://latex.codecogs.com/png.latex?%5Comega%20%3D%20%5Cfrac%7B1%7D%7B%5Ctilde%20A%7D%5CSigma%5E%7B-1%7D%5Ctilde%5Cmu "\omega = \frac{1}{\tilde A}\Sigma^{-1}\tilde\mu")

with ![\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\Sigma")
the covariance matrix between assets returns of length
![n\*n](https://latex.codecogs.com/png.latex?n%2An "n*n"), with
![n](https://latex.codecogs.com/png.latex?n "n") the number of assets in
the portfolio,
![\tilde\mu](https://latex.codecogs.com/png.latex?%5Ctilde%5Cmu "\tilde\mu")
the vector of the expected excess returns equal to
![\tilde\mu = \mu - r_fe](https://latex.codecogs.com/png.latex?%5Ctilde%5Cmu%20%3D%20%5Cmu%20-%20r_fe "\tilde\mu = \mu - r_fe")
with ![\mu](https://latex.codecogs.com/png.latex?%5Cmu "\mu") the
expected returns, ![r_f](https://latex.codecogs.com/png.latex?r_f "r_f")
the risk-free rate and ![e](https://latex.codecogs.com/png.latex?e "e")
a vector of ![1](https://latex.codecogs.com/png.latex?1 "1") of length
![n](https://latex.codecogs.com/png.latex?n "n") and finally
![\tilde{A} = \tilde\mu'\Sigma^{-1}e](https://latex.codecogs.com/png.latex?%5Ctilde%7BA%7D%20%3D%20%5Ctilde%5Cmu%27%5CSigma%5E%7B-1%7De "\tilde{A} = \tilde\mu'\Sigma^{-1}e").

Contrary to GMV portfolio, we have to estimate also
![\mu](https://latex.codecogs.com/png.latex?%5Cmu "\mu") with
![\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\Sigma"). Here,
I will present the estimation methodology proposed by F. Black and R.
Litterman (BL) in 1990 and 1992 to estimate
![\mu](https://latex.codecogs.com/png.latex?%5Cmu "\mu"): see [the
following
paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=334304) to
demystify this approach.

I then construct a multi-assets allocation with equity, bond and
monetary assets (null weight for this last asset in the TP).

### 2.2 Estimation methodology

Before modelling, I separate the initial sample between a learning
sample and a backtest sample to evaluate the performance of our
modelling. I choose May 2023 as a separation date to backtest the
strategy on the last 2.5 year of the sample.

``` r
end_date <- nrow(table_returns)
table_returns_learning <- table_returns %>% slice(1:55)
table_returns_backtest <- table_returns %>% slice(56:end_date)
```

There are 55 learning observations and 26 backtest observations. My
objective is to observe if I can obtain better performance with a
tactical allocation on the backtest sample. Before all, I present the
construction of the strategic allocation.

#### 2.2.1 Strategic allocation for the Tangency Portfolio

I begin with classical unbiased estimators for
![\mu](https://latex.codecogs.com/png.latex?%5Cmu "\mu") and
![\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\Sigma"):

![\hat\mu = \frac{1}{T}\sum\_{t=1}^Tr_t - r_f](https://latex.codecogs.com/png.latex?%5Chat%5Cmu%20%3D%20%5Cfrac%7B1%7D%7BT%7D%5Csum_%7Bt%3D1%7D%5ETr_t%20-%20r_f "\hat\mu = \frac{1}{T}\sum_{t=1}^Tr_t - r_f")

![\hat\Sigma = \frac{1}{T-n-2}\sum\_{t=1}^T\left(r_t-\hat\mu\right)\left(r_t-\hat\mu\right)'](https://latex.codecogs.com/png.latex?%5Chat%5CSigma%20%3D%20%5Cfrac%7B1%7D%7BT-n-2%7D%5Csum_%7Bt%3D1%7D%5ET%5Cleft%28r_t-%5Chat%5Cmu%5Cright%29%5Cleft%28r_t-%5Chat%5Cmu%5Cright%29%27 "\hat\Sigma = \frac{1}{T-n-2}\sum_{t=1}^T\left(r_t-\hat\mu\right)\left(r_t-\hat\mu\right)'")

Then, I can plug-in these estimates on the formula of the TP to obtain
unbiased estimators of its weights. I assume that
![r_f=0\\](https://latex.codecogs.com/png.latex?r_f%3D0%5C%25 "r_f=0\%").

``` r
n <- ncol(table_returns_learning)
T <- nrow(table_returns_learning)
e <- rep(1, n)
perio <- 12
rf <- 0

mu <- colMeans(table_returns_learning) * perio - rf
Sigma <- cov(table_returns_learning) * (T - 1) / (T - n - 2) * perio
A <- t(e) %*% solve(Sigma) %*% mu
omega <- 1 / as.numeric(A) * solve(Sigma) %*% mu

barnames <-
  c('Bouygues',
    'Engie',
    'Total',
    'Schneider',
    'Cap Gemini',
    'LVMH',
    'Green Bond')
barplot(
  as.numeric(omega),
  col = 'black',
  names.arg = barnames,
  ylim = c(-1, 2),
  cex.names = 0.7
)
```

![](workshop2_files/figure-gfm/TP-1.png)<!-- -->

The realised return observed on the backtest sample of the portfolio
constructed on the learning sample is equal to 13.92%.

**Code for the realised return (with Alt Gr 7 at the beginning and at
the end): r percent(mean(as.matrix(table_returns_backtest) %*% omega) *
perio, accuracy = 0.01)**

I am going to try to improve this result thanks to a more robust
statistical approach integrating ESG ratings in the allocation.

#### 2.2.2 Tactical allocation for the Tangency Portfolio with Black-Litterman approach

Tactical allocation is a process that deviates the strategic allocation
thanks to new information: for instance, if I think that one equity will
perform better in the near future, then I will increase its weight
relative to the others.

Black-Litterman approach is one of a quantitative methodology to
integrate these predictions, views or ratings in a relevant way. This
approach adds to predictions statistical uncertainty: the econometric
approach to estimate portfolios is no more a plug-in approach but a
decision-bayesian approach.

The Black-Litterman returns are the following mixed estimates:

![\hat\mu\_\text{mixed} = \left\[\left(\tau\hat\Sigma\right)^{-1} + \Omega^{-1}\right\]^{-1} \left\[\left(\tau\hat\Sigma\right)^{-1}\hat\mu + \Omega^{-1}Q\right\]](https://latex.codecogs.com/png.latex?%5Chat%5Cmu_%5Ctext%7Bmixed%7D%20%3D%20%5Cleft%5B%5Cleft%28%5Ctau%5Chat%5CSigma%5Cright%29%5E%7B-1%7D%20%2B%20%5COmega%5E%7B-1%7D%5Cright%5D%5E%7B-1%7D%20%5Cleft%5B%5Cleft%28%5Ctau%5Chat%5CSigma%5Cright%29%5E%7B-1%7D%5Chat%5Cmu%20%2B%20%5COmega%5E%7B-1%7DQ%5Cright%5D "\hat\mu_\text{mixed} = \left[\left(\tau\hat\Sigma\right)^{-1} + \Omega^{-1}\right]^{-1} \left[\left(\tau\hat\Sigma\right)^{-1}\hat\mu + \Omega^{-1}Q\right]")

with ![Q](https://latex.codecogs.com/png.latex?Q "Q") the economic views
quantified by average returns,
![\tau](https://latex.codecogs.com/png.latex?%5Ctau "\tau") the
confidence parameter in the views and
![\Omega](https://latex.codecogs.com/png.latex?%5COmega "\Omega") the
matrix of uncertainty associated to the economic views; we assume that
![\Omega](https://latex.codecogs.com/png.latex?%5COmega "\Omega") is a
diagonal matrix with diagonal elements equal to variances of assets
returns.

Economic views here will be based on ESG ratings. I assume that the
future performance of stocks could be influenced by their ESG ratings: a
good ESG rating will lead to good performance, and a bad one will lead
to poor performance.

For this, I use [Sustainalytics’ ESG risk
assessment](https://www.sustainalytics.com/esg-data) to obtain ESG
ratings. These ratings measure the risk to which a company’s value is
exposed due to environmental, social and governance issues. They assess
the risk on an absolute scale from 0 to 100. The lowest score indicates
that ESG risk is the best managed. They are available since 2014 and
published on Yahoo Finance since 2018.

Finally, I transform these ratings in returns to obtain
![Q](https://latex.codecogs.com/png.latex?Q "Q").

``` r
# Example of parameters for BL approach
note <- numeric(n)
note[1] <- -2 # Bouygues: negative rating 32.5
note[2] <- -1 # Engie: negative rating 27.4
note[3] <- -2 # TotalEnergies: negative rating 31.9
note[4] <- 1 # Schneider Electric: positive rating 9.4
note[5] <- 2 # Capgemini: positive rating 7.6
note[6] <- 1 # LVMH: positive rating 13.8
note[7] <- 2 # Green Bond: positive rating
vol <- sqrt(diag(Sigma))
theta <- 0.2
Q <- mu + vol * note * theta
tau <- 0.5
```

``` r
# Mixed estimation of returns
Omega <- diag(diag(Sigma), n, n)
mu_mixed <-
  solve(solve(tau * Sigma) + solve(Omega)) %*% (solve(tau * Sigma) %*% mu + solve(Omega) %*% Q)

# Tactical allocation with views directly
A_Q <- t(e) %*% solve(Sigma) %*% Q
omega_Q <- 1 / as.numeric(A_Q) * solve(Sigma) %*% Q

# Tactical allocation with mixed estimation 
A_mixed <- t(e) %*% solve(Sigma) %*% mu_mixed
omega_mixed <- 1 / as.numeric(A_mixed) * solve(Sigma) %*% mu_mixed
barplot(
  as.numeric(omega_mixed),
  col = 'black',
  names.arg = barnames,
  ylim = c(-1, 2),
  cex.names = 0.7
)
```

![](workshop2_files/figure-gfm/BL-1.png)<!-- -->

The realised return observed on the backtest sample of the BL portfolio
constructed on the learning sample is equal to 18.17%.

I can compare it to the portfolio constructed directly with views and
without uncertainty on the predictions. The realised return observed on
the backtest sample of this portfolio constructed on the learning sample
is equal to 37.17%. BL approach integrates uncertainty and then less
confidence in the views.

## To conclude the second workshop

This workshop is the second of my course on Asset Management dedicated
to multi-assets allocation and Tangency Portfolio. I present some
improvements of the classical plug-in estimators thanks to the
Black-Litterman approach and an integration of ESG ratings.

To go further, I recommend reading the academic paper by Pastor,
Stambaugh, and Taylor [“Dissecting green
returns”](https://www.nber.org/system/files/working_papers/w28940/w28940.pdf),
where they show that green stocks do not guarantee outperformance
compared to brown stocks.
