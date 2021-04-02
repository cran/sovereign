# sovereign: State-Dependent Empirical Analysis  

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![codecov](https://codecov.io/gh/tylerJPike/sovereign/branch/main/graph/badge.svg?token=WXLWR6H93B)](https://codecov.io/gh/tylerJPike/sovereign)
[![Build Status](https://travis-ci.org/tylerJPike/sovereign.svg?branch=main)](https://travis-ci.org/tylerJPike/sovereign)  
<!-- badges: end -->

The sovereign package introduces a set of tools for state-dependent empirical analysis through both VAR- and local projection-based state-dependent forecasts, impulse response functions, and forecast error variance decomposition. 

See the sovereign package [website](https://tylerjpike.github.io/sovereign/) for examples and documentation. 

----

## Available Tools  

Unsupervised Regime Assignment
1. random forest  
2. k-means clustering  
3. EM 
4. Bai & Perron (2003)  

Local Projections (LP)
1. direct projection forecasting*  
1. impulse responses*

Vector Auto-Regression (VAR)
1. recursive forecasting*
2. forecast error variance decomposition*
3. impulse responses*

*charting functions available   

----

## Basic Workflow 
    # load packages
    library(sovereign)         # analysis
    library(dplyr)             # general cleaning
    library(lubridate)         # date functions

    #-------------------------------------------
    # create data
    #-------------------------------------------
    # pull and prepare data from FRED
    quantmod::getSymbols.FRED(
        c('UNRATE','INDPRO','GS10'), 
        env = globalenv())

    Data = cbind(UNRATE, INDPRO, GS10)

    Data = data.frame(Data, date = zoo::index(Data)) %>%
        filter(lubridate::year(date) >= 1990) %>% 
        na.omit()

    # create a regime explicitly   
    Data.threshold = Data %>%
        mutate(mp = if_else(GS10 > median(GS10), 1, 0))

    #------------------------------------------
    # learn regimes
    #------------------------------------------
    # assign regimes based on unsurpervised kmeans
    #  (will not be used further)
    regimes = 
        regimes(
            data = Data, 
            regime.n = 3, 
            engine = 'kmeans')

    #------------------------------------------
    # single-regime var
    #------------------------------------------
    # estimate VAR
    # (using IC lag selection0
    var =
        VAR(
            data = Data,
            horizon = 10,
            freq = 'month',
            lag.ic = 'BIC',
            lag.max = 4)

    # plot forecasts
    plot_forecast(var$forecasts[[1]])

    # plot residuals
    plot_error(var$residuals[[1]])

    # estimate IRF
    irf =
        var_irf(
            var,
            bootstraps.num = 10,
            CI = c(0.05,0.95))

    # plot IRF
    plot_irf(irf)

    # estimate forecast error variance decomposition
    fevd =
        var_fevd(
            var,
            horizon = 10)

    # plot FEVD
    plot_fevd(fevd)

    #-------------------------------------------
    # multi-regime var
    #-------------------------------------------
    # estimate multi-regime VAR
    var =
        RVAR(
            data = Data.threshold,
            regime = 'mp',
            p = 1,
            horizon = 1,
            freq = 'month')
    
    # estimate IRF
    rvar.irf =
        rvar_irf(
            rvar,
            horizon = 10,
            bootstraps.num = 10,
            CI = c(0.05,0.95))

    # plot IRF
    # regime 1: low interest rates
    plot_irf(rvar.irf[[1]])
    # regime 2: high interest rates
    plot_irf(rvar.irf[[2]])

    # estimate forecast error variance decomposition
    rvar.fevd =
        rvar_fevd(
            rvar,
            horizon = 10)

    # plot FEVD
    # regime 1: low interest rates
    plot_fevd(rvar.fevd[[1]])
    # regime 2: high interest rates
    plot_fevd(rvar.fevd[[2]])

    #-------------------------------------------
    # single-regime local projections
    #-------------------------------------------
    # estimate single-regime forecasts 
    #  (one or multiple horizons may be estimated)
    lp = 
        LP(
            data = Data,
            p = 1,
            horizon = 1,
            freq = 'month')

    # estimate single-regime IRF
    lp.irf = lp_irf(lp)

    # plot IRF
    plot_irf(lp.irf)

    #-------------------------------------------
    # multi-regime local projections
    #-------------------------------------------
    # estimate multi-regime IRF
    rlp = 
        RLP(
            data = Data,
            regime = 'mp',
            p = 1,
            horizon = 1,
            freq = 'month')

    # estimate multi-regime IRF
    rlp.irf = rlp_irf(rlp)

    # plot IRF
    # regime 1: low interest rates
    plot_irf(rlp.irf[[1]])
    # regime 2: high interest rates
    plot_irf(rlp.irf[[2]])
    

---
## Contact
If you should have questions, concerns, or wish to collaborate, please contact [Tyler J. Pike](https://tylerjpike.github.io/)
