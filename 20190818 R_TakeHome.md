\#1. Visual story telling part 1: green buildings

    ## Warning: package 'mosaic' was built under R version 3.6.1

    ## Loading required package: dplyr

    ## Warning: package 'dplyr' was built under R version 3.6.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: lattice

    ## Loading required package: ggformula

    ## Warning: package 'ggformula' was built under R version 3.6.1

    ## Loading required package: ggplot2

    ## Loading required package: ggstance

    ## Warning: package 'ggstance' was built under R version 3.6.1

    ## 
    ## Attaching package: 'ggstance'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

    ## 
    ## New to ggformula?  Try the tutorials: 
    ##  learnr::run_tutorial("introduction", package = "ggformula")
    ##  learnr::run_tutorial("refining", package = "ggformula")

    ## Loading required package: mosaicData

    ## Warning: package 'mosaicData' was built under R version 3.6.1

    ## Loading required package: Matrix

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.

    ## 
    ## Attaching package: 'mosaic'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median,
    ##     prop.test, quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

    ## Warning: package 'tidyverse' was built under R version 3.6.1

    ## -- Attaching packages -------------------------------------------------------------- tidyverse 1.2.1 --

    ## v tibble  2.1.3     v purrr   0.3.2
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'tibble' was built under R version 3.6.1

    ## Warning: package 'tidyr' was built under R version 3.6.1

    ## Warning: package 'readr' was built under R version 3.6.1

    ## Warning: package 'purrr' was built under R version 3.6.1

    ## Warning: package 'stringr' was built under R version 3.6.1

    ## Warning: package 'forcats' was built under R version 3.6.1

    ## -- Conflicts ----------------------------------------------------------------- tidyverse_conflicts() --
    ## x mosaic::count()            masks dplyr::count()
    ## x purrr::cross()             masks mosaic::cross()
    ## x mosaic::do()               masks dplyr::do()
    ## x tidyr::expand()            masks Matrix::expand()
    ## x dplyr::filter()            masks stats::filter()
    ## x ggstance::geom_errorbarh() masks ggplot2::geom_errorbarh()
    ## x dplyr::lag()               masks stats::lag()
    ## x mosaic::stat()             masks ggplot2::stat()
    ## x mosaic::tally()            masks dplyr::tally()

We started by checking potential confounders in the dataset and diving
into those that appear to have the potential, namely “age” and
“class\_a.”

    # Check whether age is a plausible confounder
    g = ggplot(green)
    g + geom_histogram(aes(x = age, y=stat(density))) +
      facet_grid(green_rating~.) +
      labs(title="Density Distribution of Property Ages",
           subtitle = "Non-Green vs. Green Properties",
           x ="Property Age",
           y = "Density")

<img src="20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />
**Comments:** Non-green buildings have two “clusters” of ages - one
similar to those of green buildings (&lt; 50), the other between 75 and
125. Given that green buildings do not have the older “cluster” and new
buildings tend to have higher rents, Age of the properties is a
confounding variable.

    # Try to hold age roguhly constant
    # define some age groupings
    green = mutate(green,
                   agecat = cut(age, c(0, 10, 25, 50, 75, 200),include.lowest = TRUE))

    # compare rent within age groupings
    rentByAge = green %>%
      group_by(agecat, green_rating) %>%
      summarize(median_rent = median(Rent), n=n()) #do median to eliminate the effects of outlier

    ggplot(rentByAge) +
      geom_bar(stat = 'identity', aes(x = agecat,
                                      y = median_rent,
                                      fill = factor(green_rating,)),
               position = 'dodge') +
      labs(title="Median Property Rent by Property Age Categories",
           subtitle = "Non-Green vs. Green Properties",
           x ="Property Age Category",
           y = "Median Property Rent ($/sqft)") +
      scale_fill_discrete(name = "Building Rating",
                          labels=c("Non-Green-Rated", "Green-Rated"))

<img src="20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />
**Comment**: Holding property age constant, the median rents for
green-rated buildings older than 10 years in age do appear to be higher
than those for non-green-rated buildings.

    # Check whether class_a is a plausible confounder
    g = ggplot(green)
    g + geom_bar(aes(factor(class_a), fill =  factor(green_rating)),
               position = 'dodge') +
      labs(title="Numbers of Non-Class A vs. Class A Buildings",
           x ="Non-Class A / Class A",
           y = "Counts") +
      scale_fill_discrete(name = "Building Rating",
                          labels=c("Non-Green-Rated", "Green-Rated"))

<img src="20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
**Comment:** Class A seems like a potential confounder as more green
buildings appear to be classified as Class A. Higher-quality properties
tend to translate into higher prices. So the higher average rent
associated with green buildings may be at least partially attributed to
building qualities.

    # Try to hold class-A roguhly constant
    rentByclassA = green %>%
      group_by(class_a, green_rating) %>%
      summarize(median_rent = median(Rent), n=n())
    ggplot(rentByclassA) +
      geom_bar(stat = 'identity', aes(x = factor(class_a),
                                      y = median_rent,
                                      fill = factor(green_rating,)),
               position = 'dodge') +
      labs(title="Median Rents for Non-Class A vs. Class A Properties",
           subtitle = "between Non-Green and Green Properties",
           x ="Non-Class A / Class A",
           y = "Average Property Rent ($/sqft)") +
      scale_fill_discrete(name = "Building Rating",
                          labels=c("Non-Green-Rated", "Green-Rated"))

<img src="20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />
**Comment:** For Class A buildings, the rents of green buildings do not
appear to be statistically different from non-green buildings. However,
for non-Class A buildings, the green-rated buildings do appear to have
higher rents.

**Concluding Thoughts:** One should not simply conclude that green
buildings have higher rents because they are green. In reality, other
factors such as the age and the quality of a property also affect rents.
That said, as seen from the graphs above, when holding the property age
constant, older green buildings do appear to be of higher rents than
non-green buildings, though their rents may be lower during the first
ten years post construction. Moreover, if the client intends to build a
class A property, there is no significant difference in rent between
green and non-green buildings. In conclusion, if the client plans to
build a non-Class-A building, they may expect to charge higher rents in
the long run, but not in the short term.

\#2. Visual story telling part 2: flights at ABIA

    #Setup
    rm(list=ls())
    library(mosaic)
    library(tidyverse)

    ABIA = read.csv("ABIA.csv")

    # Get Airline names instead of IATACode
    airlinecode = read.csv("IATA-Airline.csv", sep = ',', header = TRUE)
    ABIAAirline = merge(ABIA, airlinecode, by.x = "UniqueCarrier", by.y = "ï..IATA")

**Thesis:** We would like to explore outbound flight cancellations from
ABIA in 2008. Are there particular airlines that had more cancellations?
Were these cancellations out of their control? We start by level setting
the flight volume by airline, then diving into the cancellation volume,
cancellations as a percentage of total flights, and finally reasons for
cancellations.

    # Dummy variable setup
    n = dim(ABIAAirline)[1]
    CanCarrier = rep(0,n)
    CanCarrier[ABIAAirline$CancellationCode=='A']=1
    CanWeather = rep(0,n)
    CanWeather[ABIAAirline$CancellationCode=='B']=1
    CanNAS = rep(0,n)
    CanNAS[ABIAAirline$CancellationCode=='C']=1

    ABIAAirline['CanCarrier'] = CanCarrier
    ABIAAirline['CanWeather'] = CanWeather
    ABIAAirline['CanNAS'] = CanNAS

    FlightCancellations = ABIAAirline %>%
      filter(Origin == 'AUS') %>%
      group_by(AirlineName) %>%
      summarize(flight_count = n() , # Flights out of AUS by airline
                cancelled_count = sum(Cancelled), # Cancels out of AUS by airline
                cancelRatio = cancelled_count / flight_count, # % cancels out of AUS by airline
                CarrierCancellation = sum(CanCarrier), # reasons for cancels
                WeatherCancellation = sum(CanWeather),
                NASCancellation = sum(CanNAS),
                CarrierCanPerc = round(CarrierCancellation / cancelled_count,3),
                WeatherCanPerc = round(WeatherCancellation / cancelled_count,3),
                NASCanPerc = round(NASCancellation / cancelled_count,3))

    # Plot flight count
    ggplot(FlightCancellations[,1:2]) + geom_bar(stat = 'identity',
                            aes(x = reorder(AirlineName, -flight_count),
                                y = flight_count)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title="Number of Outbound Flights from AUS, by Airline - 2008",
           x ="Airline",
           y = "Number of Outbound Flights Scheduled")

<img src="20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

    # Plot cancel count
    ggplot(FlightCancellations[,c(1,3)]) + geom_bar(stat = 'identity',
                            aes(x = reorder(AirlineName, -cancelled_count),
                                y = cancelled_count)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title="Number of Outbound Flights Cancelled, by Airline - 2008",
           x ="Airline",
           y = "Number of Flights Cancelled")

<img src="20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-7-2.png" style="display: block; margin: auto;" />

    # Plot cancel %
    ggplot(FlightCancellations[,c(1,4)]) + geom_bar(stat = 'identity',
                            aes(x = reorder(AirlineName, -cancelRatio),
                                y = cancelRatio)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title="Percentage of Outbound Flights Cancelled, by Airline - 2008",
           x ="Airline",
           y = "Percentage of Flights Cancelled")

<img src="20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-7-3.png" style="display: block; margin: auto;" />

    # Get Plot 3's order for Plot 4 
    Xorder = reorder(FlightCancellations$AirlineName, -FlightCancellations$cancelRatio)

    # Plot cancellation reasons by airline
    library(reshape2)

    ## Warning: package 'reshape2' was built under R version 3.6.1

    df <- melt(FlightCancellations[c(1,8,9,10)], id.vars = "AirlineName")
    df$value[is.na(df$value)] <- 0
    vec = levels(Xorder)
    df$AirlineName <- factor(df$AirlineName, levels=vec) # reordering the columns as # of delays out
    ggplot(df, aes(x = AirlineName,
                   y = value,
                   fill = variable)) +
      geom_bar(stat = 'identity') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title="AUS Outbound Flight Cancellation Reasons, by Airline - 2008",
           x ="Airline", 
           y = "Percentage of Cancelled Flights") +
      scale_fill_discrete(name = "Cancellation Reason",
                          labels=c("Carrier", "Weather", "NAS"))

<img src="20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-7-4.png" style="display: block; margin: auto;" />

**Concluding Thoughts:** As one can see from the first and second plots,
while Southwest flew the most flights out of Austin in 2008 (~70% more
than second place American Airlines), its cancellation volume is about a
third of that of American. In fact, when looking at percentage of
flights cancelled (third plot), Southwest was the fourth lowest. Lastly,
among the five airlines that had the most cancellations by percentage,
only American Eagle and Comair had less than half of the flights
cancelled due to factors outside of their control. Mesa especially had
over 80% of its cancellations due to carrier-controlled factors.

**Food for thought:** Among those with relatively high cancellation
percentages:

-   Pinnacle and Comair are no longer operating
-   American Eagle is part of American Airlines
-   Mesa operates for American Airlines and United Airlines
-   SkyWest operates for American, United, and Alaska

\#3. Portfolio modeling \#\# Portfolio 1:

-   Diversified ETF: GCE (Clymore CEF GS Connect ETN); diversified =
    safe; highest YTD growth
-   Health & Biotech: IHI (iShares US Medical Devices ETF); growing
    market, high volatility = high upside
-   S. America: FBZ (First Trust Brazil Alpha DEX Fund)

<!-- -->

    rm(list=ls())
    library(mosaic)
    library(quantmod)

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## Loading required package: TTR

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

    library(foreach)

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    # Import ETF
    myETF = c("GCE","IHI","FBZ")
    getSymbols(myETF, from = "2014-08-01") # Last five years of data

    ## 'getSymbols' currently uses auto.assign=TRUE by default, but will
    ## use auto.assign=FALSE in 0.5-0. You will still be able to use
    ## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
    ## and getOption("getSymbols.auto.assign") will still be checked for
    ## alternate defaults.
    ## 
    ## This message is shown once per session and may be disabled by setting 
    ## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.

    ## [1] "GCE" "IHI" "FBZ"

    # Adjust for splits and dividends
    for(ticker in myETF) {
        expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
        eval(parse(text=expr))
    }
    # head(GCE)

    # Combine, in col, all the returns (close-to-close, 24-hr, changes) in a matrix
    all_returns = cbind(ClCl(GCE),
                                    ClCl(IHI),
                                    ClCl(FBZ))
    # head(all_returns)
    all_returns = as.matrix(na.omit(all_returns)) # drop the first row

    initial_wealth = 100000
    sim1 = foreach(i=1:10000, .combine='rbind') %do% { #for each - rowbind each wealth tracker
        total_wealth = initial_wealth
        weights = c(0.4, 0.3, 0.3)
        holdings = weights * total_wealth
        n_days = 20
        wealthtracker = rep(0, n_days)
        for(today in 1:n_days) {
            return.today = resample(all_returns, 1, orig.ids=FALSE)
            holdings = holdings + holdings*return.today
            total_wealth = sum(holdings)
            wealthtracker[today] = total_wealth
            
            # rebalancing
          weights = c(0.4, 0.3, 0.3)
          holdings = weights * total_wealth
            
        }
        wealthtracker
    }

    # Profit/loss
    mean(sim1[,n_days]) 

    ## [1] 100481.3

    hist(sim1[,n_days]- initial_wealth, breaks=30,
         main = "Histogram of Potential Profit/Loss - Portfolio 1",
         xlab = "Potential Profit/Loss",
         col = "light blue")

![](20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-13-1.png)

    # Calculate VaR
    VaR5 = 100000 - quantile(sim1[,n_days], 0.05)
    VaR5

    ##       5% 
    ## 7741.201

\#\#Portfolio 2:

-   Large cap blend ETF: VTV (Vanguard Value ETF); popular ETF / large
    cap and blend between growth and value = reliable
-   Emerging market: GREK (Global X FTSE Greece 20 ETF); relative stable
    emerging market
-   Asia Pacific: THD (iShares MSCI Thailand ETF)
-   Fin. Services: KIE (SPDR S&P Insurance ETF)

<!-- -->

    # Import ETF
    myETF2 = c("VTV","GREK","THD","KIE")
    getSymbols(myETF2, from = "2014-08-01") # Last five years of data

    ## [1] "VTV"  "GREK" "THD"  "KIE"

    # Adjust for splits and dividends
    for(ticker in myETF2) {
        expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
        eval(parse(text=expr))
    }
    # head(VTV)

    # Combine, in col, all the returns (close-to-close, 24-hr, changes) in a matrix
    all_returns2 = cbind(ClCl(VTV),
                                    ClCl(GREK),
                                    ClCl(THD),
                                    ClCl(KIE))
    # head(all_returns2)
    all_returns2 = as.matrix(na.omit(all_returns2)) # drop the first row

    initial_wealth = 100000
    sim1 = foreach(i=1:10000, .combine='rbind') %do% { #for each - rowbind each wealth tracker
        total_wealth = initial_wealth
        weights = c(0.25, 0.25, 0.25, 0.25)
        holdings = weights * total_wealth
        n_days = 20
        wealthtracker = rep(0, n_days)
        for(today in 1:n_days) {
            return.today = resample(all_returns2, 1, orig.ids=FALSE)
            holdings = holdings + holdings*return.today
            total_wealth = sum(holdings)
            wealthtracker[today] = total_wealth
            
            # rebalancing
          weights = c(0.25, 0.25, 0.25, 0.25)
          holdings = weights * total_wealth
            
        }
        wealthtracker # record wealth of day 20
    }

    # Profit/loss
    mean(sim1[,n_days]) 

    ## [1] 100165

    hist(sim1[,n_days]- initial_wealth, breaks=30,
         main = "Histogram of Potential Profit/Loss - Portfolio 2",
         xlab = "Potential Profit/Loss",
         col = "light green")

![](20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-21-1.png)

    # Calculate VaR
    VaR5 = 100000 - quantile(sim1[,n_days], 0.05)
    VaR5

    ##       5% 
    ## 7581.028

Porfolio 3:
-----------

-   Emerging market: FRN (Invesco Frontier markets)
-   Asia Pacific: ENZL (iShares MSCI New Zealand ETF)
-   Technology: VGT (Vanguard Info Tech)

<!-- -->

    # Import ETF
    myETF3 = c("FRN","ENZL","VGT")
    getSymbols(myETF3, from = "2014-08-01") # Last five years of data

    ## [1] "FRN"  "ENZL" "VGT"

    #Adjust for splits and dividends
    for(ticker in myETF3) {
        expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
        eval(parse(text=expr))
    }
    #head(FRN)

    # Combine, in col, all the returns (close-to-close, 24-hr, changes) in a matrix
    all_returns3 = cbind(ClCl(FRN),
                                    ClCl(ENZL),
                                    ClCl(VGT))
    # head(all_returns3)
    all_returns3 = as.matrix(na.omit(all_returns3)) # drop the first row

    initial_wealth = 100000
    sim1 = foreach(i=1:10000, .combine='rbind') %do% { #for each - rowbind each wealth tracker
        total_wealth = initial_wealth
        weights = c(0.2, 0.4, 0.4)
        holdings = weights * total_wealth
        n_days = 20
        wealthtracker = rep(0, n_days)
        for(today in 1:n_days) {
            return.today = resample(all_returns3, 1, orig.ids=FALSE)
            holdings = holdings + holdings*return.today
            total_wealth = sum(holdings)
            wealthtracker[today] = total_wealth
            
            # rebalancing
          weights = c(0.2, 0.4, 0.4)
          holdings = weights * total_wealth
            
        }
        wealthtracker
    }

    # Profit/loss
    mean(sim1[,n_days]) 

    ## [1] 100682.9

    hist(sim1[,n_days]- initial_wealth, breaks=30,
         main = "Histogram of Potential Profit/Loss - Portfolio 3",
         xlab = "Potential Profit/Loss",
         col = "cyan")

![](20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-29-1.png)

    # Calculate VaR
    VaR5 = 100000 - quantile(sim1[,n_days], 0.05)
    VaR5

    ##       5% 
    ## 5426.676

**Report:** Below is a brief description of each of our ETF portfolios,
along with their values at right at the 5% level according to our
simulations. The first two portfolios have similar VaR at over $7k.
Porfolio 3 is safer than the other two, at about $5.5k of VaR. Note that
given the above resampling was generated from Monte Carlo simulations,
our answers in the report will not be exactly as the results knitted
above, and will not be exactly replicable.

**Porfolio 1:** Medium-High aggressiveness - This portfolio contains 1.
Clymore CEF GS Connect ETN, a currently high-performing diverifised ETF
(i.e., it has the highest YTD growth among all diverisifed ETFs).
Diversified ETFs are generally safer, though we increased the
aggressiveness by choosing the one with the highest YTD growth. 2.
iShares US Medical Devices ETF, a health & biotech ETF. The US health
industry is one of the most lucrative in the world (albeit at the
detriment of the American people) and continues to grow. While the
volatility is high, the upside is as well. 3. First Trust Brazil Apha
DEX Fund, a South American ETF. We decided to ensure more than one
geography in each portfolio to guard against potential recession in any
region, and picked a Brazilian one to go with te US for Portfolio 1.
Though many still see Brazil as an emerging market, this particular ETF
has been churning out high returns in recent years. Therefore, we
believe it is a safe option to add to the portfolio.

This potfolio has a 5% VaR of about $7,712 and the distrbution of its
returns is wide.

**Portfolio 2:** High aggressiveness - This portfolio contains 1.
Vanguard Value ETF, a Large cap blend ETF. We have three other
aggressive ETFs in this portfolio, so included a large cap/growth and
value ETF to have a more reliable component for the portfolio. 2. GREK
(Global X FTSE Greece 20 ETF), an emerging market ETF. Greece,
stabilizing post the bankruptcy and Grexit crisis, provides a good
opportunity for high returns, though the risks are high as well. 3.
iShares MSCI Thailand ETF, an Asia Pacific ETF. We added a different,
yet still highly volatile geopgrahy into the mix. 4. SPDR S&P Insurance
ETF, a financial services ETF. The financial services industry in
general is highly lucrative, though the risks can be high as well (read:
cause of the 2008 recession, high insurance losses from numerous natural
disasters). We decided to include it, again, to be aggressive with the
potential high returns.

This potfolio has a 5% VaR of about $7,342, and the distrbution of its
returns is similar but slightly more concentrated than Portfolio 1.

**Portfolio 3:** Safe - This portfolio contains 1. iShares MSCI New
Zealand ETF, an Asia Pacific ETF. This is a safe bet on a developed
country with steady growth. 2. Invesco Frontier markets, an emerging
market ETF. This is the only semi-aggressive ETF in the portfolio. That
said, it is not focused on one single emerging market, so the risks are
still diversified. 3. Vanguard Information Technology, a technology ETF.
Technology is the present and the future. Including a technology ETF is
vital in capturing and profiting from the growth of the world.

This potfolio has a 5% VaR of about $5,454 and the distrbution of its
returns is noticeably more concentrated than the two portfolios above.

\#4. Market segmentation

    rm(list=ls())
    library(ggplot2)
    library(LICORS)  # for kmeans++

    ## Warning: package 'LICORS' was built under R version 3.6.1

    library(foreach)
    library(mosaic)

    social = read.csv('social_marketing.csv', header=TRUE)
    # summary(social) # Not many spammers (at most 2)
    #Top groups: Chatter, nutrition, photo sharing, cooking, politics

    x = social[,-1] #removes the first column

**Note:** Because the numbers of tweets by users vary greatly, we
decided to scale the dataset by determining the proportions of each
user’s tweets that belong to different categories. That is, we want to
use the % of tweets in each category rather than the absolute counts to
cluster.

    X = x/rowSums(x)

**k-means clustering**

**Choosing K**

    k_grid = seq(2, 15, by = 1)
    SSE_grid = foreach(k = k_grid, .combine = 'c') %do%
      {
        cluster_k = kmeanspp(X, k, nstart = 50)
        cluster_k$tot.withinss
      }

    plot(k_grid, SSE_grid)

![](20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-36-1.png)

    # K = 7 looks like the elbow

    # Using kmeans++ initialization with 7 clusters
    clust2 = kmeanspp(X, k=7, nstart=50)
    clust2$centers

    ##      chatter current_events     travel photo_sharing uncategorized
    ## 1 0.07297447     0.03637618 0.02857758    0.09574802    0.02233269
    ## 2 0.08065045     0.03930292 0.08995848    0.03821091    0.01794340
    ## 3 0.07680646     0.03874510 0.02588941    0.04104678    0.01650558
    ## 4 0.07677266     0.03454619 0.02685292    0.04017548    0.01940061
    ## 5 0.09726093     0.06111745 0.04712967    0.04975079    0.03673647
    ## 6 0.24029440     0.05484896 0.03499178    0.12131898    0.02495694
    ## 7 0.07992847     0.03193644 0.02875994    0.04763627    0.01738807
    ##      tv_film sports_fandom   politics       food     family
    ## 1 0.01592520    0.01988144 0.01931440 0.01751907 0.01472571
    ## 2 0.02254455    0.04375804 0.15484337 0.02468965 0.01875494
    ## 3 0.01808430    0.11052033 0.01851078 0.08201691 0.04649887
    ## 4 0.01737646    0.02233951 0.02223362 0.03749700 0.01446296
    ## 5 0.07442123    0.02771083 0.02539880 0.02855757 0.01807483
    ## 6 0.02019522    0.02596722 0.03012559 0.01862833 0.02091444
    ## 7 0.02421170    0.02380386 0.02012666 0.02341523 0.01988005
    ##   home_and_garden      music       news online_gaming   shopping
    ## 1      0.01191760 0.01993107 0.01438429    0.01589280 0.02699315
    ## 2      0.01230270 0.01271554 0.10175879    0.01397165 0.01753607
    ## 3      0.01255777 0.01426978 0.01620151    0.01545007 0.02189573
    ## 4      0.01202962 0.01335322 0.01956643    0.01637570 0.02188338
    ## 5      0.01941604 0.02779711 0.02124041    0.01752705 0.02764997
    ## 6      0.01545597 0.01796103 0.01387401    0.01519057 0.07016288
    ## 7      0.01119192 0.01253074 0.01323330    0.17843770 0.01833002
    ##   health_nutrition college_uni sports_playing    cooking         eco
    ## 1       0.02803988  0.02110110     0.01357590 0.18950095 0.008976371
    ## 2       0.01995962  0.01919527     0.01195992 0.01810022 0.009274309
    ## 3       0.02274450  0.01679587     0.01274007 0.01945934 0.012406587
    ## 4       0.21671098  0.01599393     0.01224264 0.05557171 0.015593603
    ## 5       0.02095615  0.04600860     0.01723630 0.01922111 0.014019096
    ## 6       0.02162757  0.02123662     0.01370180 0.01987762 0.015111763
    ## 7       0.02230930  0.18922953     0.04269272 0.02201772 0.008543361
    ##     computers    business   outdoors      crafts automotive         art
    ## 1 0.011630346 0.009180693 0.01353622 0.009245801 0.01291096 0.013656519
    ## 2 0.035772062 0.010578269 0.01724355 0.010327248 0.04633045 0.009520355
    ## 3 0.013397446 0.009116653 0.01179156 0.017273178 0.01913820 0.012787199
    ## 4 0.010165845 0.007878379 0.04380997 0.010245425 0.01116318 0.012644172
    ## 5 0.012117665 0.014689449 0.01525257 0.016476752 0.01627938 0.047274204
    ## 6 0.014009646 0.012635290 0.00926237 0.012380264 0.01911280 0.008461305
    ## 7 0.009398171 0.007379090 0.01043721 0.009354959 0.01505526 0.019774941
    ##     religion      beauty  parenting      dating      school
    ## 1 0.01414612 0.060834696 0.01285687 0.009528852 0.015726423
    ## 2 0.01624773 0.008110217 0.01686959 0.014960133 0.012590558
    ## 3 0.09116630 0.017651856 0.06871207 0.010274490 0.044504005
    ## 4 0.01395865 0.008450889 0.01276188 0.016303076 0.010215000
    ## 5 0.01900443 0.014274846 0.01408414 0.029607225 0.016982793
    ## 6 0.01106146 0.009399047 0.01248186 0.014819006 0.014973526
    ## 7 0.01147869 0.007519962 0.01242132 0.011296043 0.009224471
    ##   personal_fitness     fashion small_business         spam       adult
    ## 1       0.01777459 0.092977391    0.007256422 2.834024e-05 0.005021892
    ## 2       0.01355091 0.009738987    0.007778548 9.325976e-05 0.002857334
    ## 3       0.01657562 0.014949975    0.007409727 6.020845e-05 0.006045755
    ## 4       0.10921876 0.012564505    0.005141202 7.978964e-05 0.004420663
    ## 5       0.01602209 0.018602038    0.013811051 5.212482e-04 0.037769706
    ## 6       0.01744707 0.014289624    0.009326927 7.648706e-05 0.003821608
    ## 7       0.01529368 0.012636260    0.007459737 1.957139e-04 0.005471466

    clust2$size/7882

    ## [1] 0.09667597 0.13562548 0.13067749 0.16911951 0.15630551 0.24092870
    ## [7] 0.07066734

**Market Segment Identification:** We clustered NutrientH2O’s social
media audience into seven clusters based on the percentage of tweets
each user had in each categor. Each cluster has at least 7% (557/7882)
of the audience sample; so we feel each cluster is meaningful. We
identified the following segments amongst the audience: - **Middle
America:** This segment (10% of the sample) includes those who are
particularly interested in sports (11% of tweets), religion (9%), and
parenting (7%). - **The Worldly:** This segment (16% of the sample)
includes those who regularly discuss politics (15)%, news (10%), and
travel (9%). - **College Gamers:** This segment (24% of the sample)
often discusses online gaming (18%) and college-related events (19%). -
**The Health Hurus:** This segment (17% of the sample) is particularly
interested in health\_nutrition (22%) and personal fitness (11%). They
also often discuss cooking (6%) and outdoors (4%). - **Avg. College
Students:** This segment (13% of the sample) discusses a healthy amount
of tv & film (7%), current events (5%), college events (5%), art (5%),
music (3%), and dating (3%) - **The Social Media Influencers:** This
segment (7% of the sample) often tweets about cooking (19%), photo
sharing (10%), fashion (9%), and beauty (6%). - **The Others:** This
segment (14% of the sample) does not have many targeted interests. That
said, they have many chatter tweets (24%), often shares photos (12%),and
discuss a healthy amount of current events (5%) and shopping (7%).

\#5. Author Attribution

    # Setup
    rm(list = ls())
    library(tm) 

    ## Warning: package 'tm' was built under R version 3.6.1

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

    ## 
    ## Attaching package: 'tm'

    ## The following object is masked from 'package:mosaic':
    ## 
    ##     inspect

    library(magrittr)

    ## Warning: package 'magrittr' was built under R version 3.6.1

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    library(slam)
    library(proxy)

    ## Warning: package 'proxy' was built under R version 3.6.1

    ## 
    ## Attaching package: 'proxy'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     as.matrix

    ## The following objects are masked from 'package:stats':
    ## 
    ##     as.dist, dist

    ## The following object is masked from 'package:base':
    ## 
    ##     as.matrix

    readerPlain = function(fname){
      readPlain(elem=list(content=readLines(fname)), 
               id=fname, language='en') }

Prepare training set

    # Read in files
    dir.list = list.files('ReutersC50/C50train/')
    file_list2 = c()
    authors = c()
    for (x in dir.list){
      # read files into one DF, ensure to set up an author vector for response variable
      files = Sys.glob(paste('ReutersC50/C50train/', x,'/*.txt', sep=''))
      file_list2 = c(file_list2, files)
      authors = c(authors, rep(x,50))
    }

    # clean up file names
    all.authors = lapply(file_list2, readerPlain) 
    mynames = file_list2 %>%
    { strsplit(., '/', fixed=TRUE) } %>%
    { lapply(., tail, n=2) } %>%
    { lapply(., paste0, collapse = '') } %>%
      unlist

    # Rename all files
    mynames

    ##    [1] "AaronPressman106247newsML.txt"    
    ##    [2] "AaronPressman120600newsML.txt"    
    ##    [3] "AaronPressman120683newsML.txt"    
    ##    [4] "AaronPressman136958newsML.txt"    
    ##    [5] "AaronPressman137498newsML.txt"    
    ##    [6] "AaronPressman14014newsML.txt"     
    ##    [7] "AaronPressman156814newsML.txt"    
    ##    [8] "AaronPressman182596newsML.txt"    
    ##    [9] "AaronPressman186392newsML.txt"    
    ##   [10] "AaronPressman193495newsML.txt"    
    ##   [11] "AaronPressman196805newsML.txt"    
    ##   [12] "AaronPressman197734newsML.txt"    
    ##   [13] "AaronPressman206838newsML.txt"    
    ##   [14] "AaronPressman231479newsML.txt"    
    ##   [15] "AaronPressman233150newsML.txt"    
    ##   [16] "AaronPressman237175newsML.txt"    
    ##   [17] "AaronPressman249407newsML.txt"    
    ##   [18] "AaronPressman2537newsML.txt"      
    ##   [19] "AaronPressman266038newsML.txt"    
    ##   [20] "AaronPressman269995newsML.txt"    
    ##   [21] "AaronPressman269999newsML.txt"    
    ##   [22] "AaronPressman270046newsML.txt"    
    ##   [23] "AaronPressman270084newsML.txt"    
    ##   [24] "AaronPressman270134newsML.txt"    
    ##   [25] "AaronPressman270346newsML.txt"    
    ##   [26] "AaronPressman275174newsML.txt"    
    ##   [27] "AaronPressman277117newsML.txt"    
    ##   [28] "AaronPressman277513newsML.txt"    
    ##   [29] "AaronPressman290125newsML.txt"    
    ##   [30] "AaronPressman299375newsML.txt"    
    ##   [31] "AaronPressman312178newsML.txt"    
    ##   [32] "AaronPressman324896newsML.txt"    
    ##   [33] "AaronPressman325347newsML.txt"    
    ##   [34] "AaronPressman330967newsML.txt"    
    ##   [35] "AaronPressman331411newsML.txt"    
    ##   [36] "AaronPressman347226newsML.txt"    
    ##   [37] "AaronPressman354135newsML.txt"    
    ##   [38] "AaronPressman354285newsML.txt"    
    ##   [39] "AaronPressman357147newsML.txt"    
    ##   [40] "AaronPressman366020newsML.txt"    
    ##   [41] "AaronPressman369570newsML.txt"    
    ##   [42] "AaronPressman371380newsML.txt"    
    ##   [43] "AaronPressman372744newsML.txt"    
    ##   [44] "AaronPressman372989newsML.txt"    
    ##   [45] "AaronPressman372995newsML.txt"    
    ##   [46] "AaronPressman378457newsML.txt"    
    ##   [47] "AaronPressman394237newsML.txt"    
    ##   [48] "AaronPressman398094newsML.txt"    
    ##   [49] "AaronPressman401260newsML.txt"    
    ##   [50] "AaronPressman407599newsML.txt"    
    ##   [51] "AlanCrosby101797newsML.txt"       
    ##   [52] "AlanCrosby10306newsML.txt"        
    ##   [53] "AlanCrosby104277newsML.txt"       
    ##   [54] "AlanCrosby104278newsML.txt"       
    ##   [55] "AlanCrosby10650newsML.txt"        
    ##   [56] "AlanCrosby109906newsML.txt"       
    ##   [57] "AlanCrosby110434newsML.txt"       
    ##   [58] "AlanCrosby113639newsML.txt"       
    ##   [59] "AlanCrosby116293newsML.txt"       
    ##   [60] "AlanCrosby119516newsML.txt"       
    ##   [61] "AlanCrosby122806newsML.txt"       
    ##   [62] "AlanCrosby122816newsML.txt"       
    ##   [63] "AlanCrosby123025newsML.txt"       
    ##   [64] "AlanCrosby125627newsML.txt"       
    ##   [65] "AlanCrosby126217newsML.txt"       
    ##   [66] "AlanCrosby128393newsML.txt"       
    ##   [67] "AlanCrosby128405newsML.txt"       
    ##   [68] "AlanCrosby128844newsML.txt"       
    ##   [69] "AlanCrosby133183newsML.txt"       
    ##   [70] "AlanCrosby134960newsML.txt"       
    ##   [71] "AlanCrosby135777newsML.txt"       
    ##   [72] "AlanCrosby136471newsML.txt"       
    ##   [73] "AlanCrosby139110newsML.txt"       
    ##   [74] "AlanCrosby140051newsML.txt"       
    ##   [75] "AlanCrosby142624newsML.txt"       
    ##   [76] "AlanCrosby143222newsML.txt"       
    ##   [77] "AlanCrosby156046newsML.txt"       
    ##   [78] "AlanCrosby156330newsML.txt"       
    ##   [79] "AlanCrosby185955newsML.txt"       
    ##   [80] "AlanCrosby188747newsML.txt"       
    ##   [81] "AlanCrosby194349newsML.txt"       
    ##   [82] "AlanCrosby194437newsML.txt"       
    ##   [83] "AlanCrosby194611newsML.txt"       
    ##   [84] "AlanCrosby194667newsML.txt"       
    ##   [85] "AlanCrosby196144newsML.txt"       
    ##   [86] "AlanCrosby199535newsML.txt"       
    ##   [87] "AlanCrosby205959newsML.txt"       
    ##   [88] "AlanCrosby206307newsML.txt"       
    ##   [89] "AlanCrosby20910newsML.txt"        
    ##   [90] "AlanCrosby212203newsML.txt"       
    ##   [91] "AlanCrosby212274newsML.txt"       
    ##   [92] "AlanCrosby212834newsML.txt"       
    ##   [93] "AlanCrosby21296newsML.txt"        
    ##   [94] "AlanCrosby213609newsML.txt"       
    ##   [95] "AlanCrosby213833newsML.txt"       
    ##   [96] "AlanCrosby213896newsML.txt"       
    ##   [97] "AlanCrosby216602newsML.txt"       
    ##   [98] "AlanCrosby217025newsML.txt"       
    ##   [99] "AlanCrosby224640newsML.txt"       
    ##  [100] "AlanCrosby225900newsML.txt"       
    ##  [101] "AlexanderSmith107525newsML.txt"   
    ##  [102] "AlexanderSmith109096newsML.txt"   
    ##  [103] "AlexanderSmith110282newsML.txt"   
    ##  [104] "AlexanderSmith134290newsML.txt"   
    ##  [105] "AlexanderSmith134584newsML.txt"   
    ##  [106] "AlexanderSmith134595newsML.txt"   
    ##  [107] "AlexanderSmith134983newsML.txt"   
    ##  [108] "AlexanderSmith135627newsML.txt"   
    ##  [109] "AlexanderSmith141391newsML.txt"   
    ##  [110] "AlexanderSmith141943newsML.txt"   
    ##  [111] "AlexanderSmith144367newsML.txt"   
    ##  [112] "AlexanderSmith16167newsML.txt"    
    ##  [113] "AlexanderSmith162656newsML.txt"   
    ##  [114] "AlexanderSmith162657newsML.txt"   
    ##  [115] "AlexanderSmith164269newsML.txt"   
    ##  [116] "AlexanderSmith164287newsML.txt"   
    ##  [117] "AlexanderSmith164558newsML.txt"   
    ##  [118] "AlexanderSmith165875newsML.txt"   
    ##  [119] "AlexanderSmith172146newsML.txt"   
    ##  [120] "AlexanderSmith179671newsML.txt"   
    ##  [121] "AlexanderSmith18111newsML.txt"    
    ##  [122] "AlexanderSmith18227newsML.txt"    
    ##  [123] "AlexanderSmith184789newsML.txt"   
    ##  [124] "AlexanderSmith185613newsML.txt"   
    ##  [125] "AlexanderSmith188417newsML.txt"   
    ##  [126] "AlexanderSmith21127newsML.txt"    
    ##  [127] "AlexanderSmith219512newsML.txt"   
    ##  [128] "AlexanderSmith219521newsML.txt"   
    ##  [129] "AlexanderSmith220666newsML.txt"   
    ##  [130] "AlexanderSmith223283newsML.txt"   
    ##  [131] "AlexanderSmith223300newsML.txt"   
    ##  [132] "AlexanderSmith223793newsML.txt"   
    ##  [133] "AlexanderSmith224655newsML.txt"   
    ##  [134] "AlexanderSmith225590newsML.txt"   
    ##  [135] "AlexanderSmith236412newsML.txt"   
    ##  [136] "AlexanderSmith237953newsML.txt"   
    ##  [137] "AlexanderSmith238090newsML.txt"   
    ##  [138] "AlexanderSmith23876newsML.txt"    
    ##  [139] "AlexanderSmith239202newsML.txt"   
    ##  [140] "AlexanderSmith240608newsML.txt"   
    ##  [141] "AlexanderSmith251540newsML.txt"   
    ##  [142] "AlexanderSmith268005newsML.txt"   
    ##  [143] "AlexanderSmith272743newsML.txt"   
    ##  [144] "AlexanderSmith274612newsML.txt"   
    ##  [145] "AlexanderSmith289578newsML.txt"   
    ##  [146] "AlexanderSmith29098newsML.txt"    
    ##  [147] "AlexanderSmith300452newsML.txt"   
    ##  [148] "AlexanderSmith302382newsML.txt"   
    ##  [149] "AlexanderSmith304231newsML.txt"   
    ##  [150] "AlexanderSmith307212newsML.txt"   
    ##  [151] "BenjaminKangLim102444newsML.txt"  
    ##  [152] "BenjaminKangLim106762newsML.txt"  
    ##  [153] "BenjaminKangLim110733newsML.txt"  
    ##  [154] "BenjaminKangLim112125newsML.txt"  
    ##  [155] "BenjaminKangLim114204newsML.txt"  
    ##  [156] "BenjaminKangLim115482newsML.txt"  
    ##  [157] "BenjaminKangLim118614newsML.txt"  
    ##  [158] "BenjaminKangLim118687newsML.txt"  
    ##  [159] "BenjaminKangLim12228newsML.txt"   
    ##  [160] "BenjaminKangLim129162newsML.txt"  
    ##  [161] "BenjaminKangLim133520newsML.txt"  
    ##  [162] "BenjaminKangLim135232newsML.txt"  
    ##  [163] "BenjaminKangLim14439newsML.txt"   
    ##  [164] "BenjaminKangLim145148newsML.txt"  
    ##  [165] "BenjaminKangLim145736newsML.txt"  
    ##  [166] "BenjaminKangLim146225newsML.txt"  
    ##  [167] "BenjaminKangLim150365newsML.txt"  
    ##  [168] "BenjaminKangLim151860newsML.txt"  
    ##  [169] "BenjaminKangLim155059newsML.txt"  
    ##  [170] "BenjaminKangLim155439newsML.txt"  
    ##  [171] "BenjaminKangLim15741newsML.txt"   
    ##  [172] "BenjaminKangLim166435newsML.txt"  
    ##  [173] "BenjaminKangLim174581newsML.txt"  
    ##  [174] "BenjaminKangLim177874newsML.txt"  
    ##  [175] "BenjaminKangLim178518newsML.txt"  
    ##  [176] "BenjaminKangLim182490newsML.txt"  
    ##  [177] "BenjaminKangLim18363newsML.txt"   
    ##  [178] "BenjaminKangLim186173newsML.txt"  
    ##  [179] "BenjaminKangLim186199newsML.txt"  
    ##  [180] "BenjaminKangLim187423newsML.txt"  
    ##  [181] "BenjaminKangLim188955newsML.txt"  
    ##  [182] "BenjaminKangLim188988newsML.txt"  
    ##  [183] "BenjaminKangLim189031newsML.txt"  
    ##  [184] "BenjaminKangLim190439newsML.txt"  
    ##  [185] "BenjaminKangLim192394newsML.txt"  
    ##  [186] "BenjaminKangLim195350newsML.txt"  
    ##  [187] "BenjaminKangLim198232newsML.txt"  
    ##  [188] "BenjaminKangLim201374newsML.txt"  
    ##  [189] "BenjaminKangLim204845newsML.txt"  
    ##  [190] "BenjaminKangLim211064newsML.txt"  
    ##  [191] "BenjaminKangLim214183newsML.txt"  
    ##  [192] "BenjaminKangLim21575newsML.txt"   
    ##  [193] "BenjaminKangLim221670newsML.txt"  
    ##  [194] "BenjaminKangLim222816newsML.txt"  
    ##  [195] "BenjaminKangLim231106newsML.txt"  
    ##  [196] "BenjaminKangLim232863newsML.txt"  
    ##  [197] "BenjaminKangLim235646newsML.txt"  
    ##  [198] "BenjaminKangLim241295newsML.txt"  
    ##  [199] "BenjaminKangLim241719newsML.txt"  
    ##  [200] "BenjaminKangLim24300newsML.txt"   
    ##  [201] "BernardHickey103816newsML.txt"    
    ##  [202] "BernardHickey110697newsML.txt"    
    ##  [203] "BernardHickey111971newsML.txt"    
    ##  [204] "BernardHickey111981newsML.txt"    
    ##  [205] "BernardHickey117740newsML.txt"    
    ##  [206] "BernardHickey118434newsML.txt"    
    ##  [207] "BernardHickey118448newsML.txt"    
    ##  [208] "BernardHickey118470newsML.txt"    
    ##  [209] "BernardHickey118472newsML.txt"    
    ##  [210] "BernardHickey118901newsML.txt"    
    ##  [211] "BernardHickey148778newsML.txt"    
    ##  [212] "BernardHickey148786newsML.txt"    
    ##  [213] "BernardHickey151764newsML.txt"    
    ##  [214] "BernardHickey154997newsML.txt"    
    ##  [215] "BernardHickey155005newsML.txt"    
    ##  [216] "BernardHickey164986newsML.txt"    
    ##  [217] "BernardHickey165020newsML.txt"    
    ##  [218] "BernardHickey165334newsML.txt"    
    ##  [219] "BernardHickey174426newsML.txt"    
    ##  [220] "BernardHickey176080newsML.txt"    
    ##  [221] "BernardHickey181034newsML.txt"    
    ##  [222] "BernardHickey18328newsML.txt"     
    ##  [223] "BernardHickey184099newsML.txt"    
    ##  [224] "BernardHickey184111newsML.txt"    
    ##  [225] "BernardHickey198061newsML.txt"    
    ##  [226] "BernardHickey201116newsML.txt"    
    ##  [227] "BernardHickey218591newsML.txt"    
    ##  [228] "BernardHickey223842newsML.txt"    
    ##  [229] "BernardHickey223856newsML.txt"    
    ##  [230] "BernardHickey223857newsML.txt"    
    ##  [231] "BernardHickey242252newsML.txt"    
    ##  [232] "BernardHickey250680newsML.txt"    
    ##  [233] "BernardHickey25315newsML.txt"     
    ##  [234] "BernardHickey25320newsML.txt"     
    ##  [235] "BernardHickey25344newsML.txt"     
    ##  [236] "BernardHickey294933newsML.txt"    
    ##  [237] "BernardHickey294952newsML.txt"    
    ##  [238] "BernardHickey30677newsML.txt"     
    ##  [239] "BernardHickey31892newsML.txt"     
    ##  [240] "BernardHickey322990newsML.txt"    
    ##  [241] "BernardHickey322996newsML.txt"    
    ##  [242] "BernardHickey323527newsML.txt"    
    ##  [243] "BernardHickey33299newsML.txt"     
    ##  [244] "BernardHickey33301newsML.txt"     
    ##  [245] "BernardHickey33315newsML.txt"     
    ##  [246] "BernardHickey354020newsML.txt"    
    ##  [247] "BernardHickey355589newsML.txt"    
    ##  [248] "BernardHickey361792newsML.txt"    
    ##  [249] "BernardHickey361793newsML.txt"    
    ##  [250] "BernardHickey361820newsML.txt"    
    ##  [251] "BradDorfman102760newsML.txt"      
    ##  [252] "BradDorfman117102newsML.txt"      
    ##  [253] "BradDorfman142661newsML.txt"      
    ##  [254] "BradDorfman144035newsML.txt"      
    ##  [255] "BradDorfman146644newsML.txt"      
    ##  [256] "BradDorfman146870newsML.txt"      
    ##  [257] "BradDorfman156808newsML.txt"      
    ##  [258] "BradDorfman15944newsML.txt"       
    ##  [259] "BradDorfman160089newsML.txt"      
    ##  [260] "BradDorfman16033newsML.txt"       
    ##  [261] "BradDorfman179121newsML.txt"      
    ##  [262] "BradDorfman196808newsML.txt"      
    ##  [263] "BradDorfman199931newsML.txt"      
    ##  [264] "BradDorfman200749newsML.txt"      
    ##  [265] "BradDorfman206764newsML.txt"      
    ##  [266] "BradDorfman206841newsML.txt"      
    ##  [267] "BradDorfman209880newsML.txt"      
    ##  [268] "BradDorfman225133newsML.txt"      
    ##  [269] "BradDorfman226801newsML.txt"      
    ##  [270] "BradDorfman231427newsML.txt"      
    ##  [271] "BradDorfman231868newsML.txt"      
    ##  [272] "BradDorfman234712newsML.txt"      
    ##  [273] "BradDorfman240124newsML.txt"      
    ##  [274] "BradDorfman240151newsML.txt"      
    ##  [275] "BradDorfman24388newsML.txt"       
    ##  [276] "BradDorfman255717newsML.txt"      
    ##  [277] "BradDorfman272513newsML.txt"      
    ##  [278] "BradDorfman273212newsML.txt"      
    ##  [279] "BradDorfman274879newsML.txt"      
    ##  [280] "BradDorfman276360newsML.txt"      
    ##  [281] "BradDorfman28462newsML.txt"       
    ##  [282] "BradDorfman285403newsML.txt"      
    ##  [283] "BradDorfman28928newsML.txt"       
    ##  [284] "BradDorfman291066newsML.txt"      
    ##  [285] "BradDorfman291619newsML.txt"      
    ##  [286] "BradDorfman293661newsML.txt"      
    ##  [287] "BradDorfman296368newsML.txt"      
    ##  [288] "BradDorfman309066newsML.txt"      
    ##  [289] "BradDorfman314102newsML.txt"      
    ##  [290] "BradDorfman321585newsML.txt"      
    ##  [291] "BradDorfman321667newsML.txt"      
    ##  [292] "BradDorfman321736newsML.txt"      
    ##  [293] "BradDorfman32215newsML.txt"       
    ##  [294] "BradDorfman331819newsML.txt"      
    ##  [295] "BradDorfman33758newsML.txt"       
    ##  [296] "BradDorfman340946newsML.txt"      
    ##  [297] "BradDorfman343945newsML.txt"      
    ##  [298] "BradDorfman344162newsML.txt"      
    ##  [299] "BradDorfman344834newsML.txt"      
    ##  [300] "BradDorfman346831newsML.txt"      
    ##  [301] "DarrenSchuettler104405newsML.txt" 
    ##  [302] "DarrenSchuettler10663newsML.txt"  
    ##  [303] "DarrenSchuettler111941newsML.txt" 
    ##  [304] "DarrenSchuettler12897newsML.txt"  
    ##  [305] "DarrenSchuettler128985newsML.txt" 
    ##  [306] "DarrenSchuettler129448newsML.txt" 
    ##  [307] "DarrenSchuettler130075newsML.txt" 
    ##  [308] "DarrenSchuettler139755newsML.txt" 
    ##  [309] "DarrenSchuettler143081newsML.txt" 
    ##  [310] "DarrenSchuettler144044newsML.txt" 
    ##  [311] "DarrenSchuettler144056newsML.txt" 
    ##  [312] "DarrenSchuettler144792newsML.txt" 
    ##  [313] "DarrenSchuettler156181newsML.txt" 
    ##  [314] "DarrenSchuettler159399newsML.txt" 
    ##  [315] "DarrenSchuettler159589newsML.txt" 
    ##  [316] "DarrenSchuettler159595newsML.txt" 
    ##  [317] "DarrenSchuettler161718newsML.txt" 
    ##  [318] "DarrenSchuettler162018newsML.txt" 
    ##  [319] "DarrenSchuettler168489newsML.txt" 
    ##  [320] "DarrenSchuettler172235newsML.txt" 
    ##  [321] "DarrenSchuettler174369newsML.txt" 
    ##  [322] "DarrenSchuettler175068newsML.txt" 
    ##  [323] "DarrenSchuettler181864newsML.txt" 
    ##  [324] "DarrenSchuettler202141newsML.txt" 
    ##  [325] "DarrenSchuettler205458newsML.txt" 
    ##  [326] "DarrenSchuettler208913newsML.txt" 
    ##  [327] "DarrenSchuettler213544newsML.txt" 
    ##  [328] "DarrenSchuettler213721newsML.txt" 
    ##  [329] "DarrenSchuettler216814newsML.txt" 
    ##  [330] "DarrenSchuettler219645newsML.txt" 
    ##  [331] "DarrenSchuettler222130newsML.txt" 
    ##  [332] "DarrenSchuettler224985newsML.txt" 
    ##  [333] "DarrenSchuettler233587newsML.txt" 
    ##  [334] "DarrenSchuettler234713newsML.txt" 
    ##  [335] "DarrenSchuettler236266newsML.txt" 
    ##  [336] "DarrenSchuettler236470newsML.txt" 
    ##  [337] "DarrenSchuettler237697newsML.txt" 
    ##  [338] "DarrenSchuettler245782newsML.txt" 
    ##  [339] "DarrenSchuettler245783newsML.txt" 
    ##  [340] "DarrenSchuettler248687newsML.txt" 
    ##  [341] "DarrenSchuettler290521newsML.txt" 
    ##  [342] "DarrenSchuettler294932newsML.txt" 
    ##  [343] "DarrenSchuettler34117newsML.txt"  
    ##  [344] "DarrenSchuettler348501newsML.txt" 
    ##  [345] "DarrenSchuettler358675newsML.txt" 
    ##  [346] "DarrenSchuettler364638newsML.txt" 
    ##  [347] "DarrenSchuettler364643newsML.txt" 
    ##  [348] "DarrenSchuettler377885newsML.txt" 
    ##  [349] "DarrenSchuettler377897newsML.txt" 
    ##  [350] "DarrenSchuettler383585newsML.txt" 
    ##  [351] "DavidLawder102529newsML.txt"      
    ##  [352] "DavidLawder105327newsML.txt"      
    ##  [353] "DavidLawder105756newsML.txt"      
    ##  [354] "DavidLawder105834newsML.txt"      
    ##  [355] "DavidLawder108359newsML.txt"      
    ##  [356] "DavidLawder110108newsML.txt"      
    ##  [357] "DavidLawder110883newsML.txt"      
    ##  [358] "DavidLawder113212newsML.txt"      
    ##  [359] "DavidLawder113338newsML.txt"      
    ##  [360] "DavidLawder114353newsML.txt"      
    ##  [361] "DavidLawder117148newsML.txt"      
    ##  [362] "DavidLawder126028newsML.txt"      
    ##  [363] "DavidLawder126792newsML.txt"      
    ##  [364] "DavidLawder127143newsML.txt"      
    ##  [365] "DavidLawder13547newsML.txt"       
    ##  [366] "DavidLawder136026newsML.txt"      
    ##  [367] "DavidLawder140545newsML.txt"      
    ##  [368] "DavidLawder142715newsML.txt"      
    ##  [369] "DavidLawder144031newsML.txt"      
    ##  [370] "DavidLawder144314newsML.txt"      
    ##  [371] "DavidLawder145801newsML.txt"      
    ##  [372] "DavidLawder146321newsML.txt"      
    ##  [373] "DavidLawder146470newsML.txt"      
    ##  [374] "DavidLawder148055newsML.txt"      
    ##  [375] "DavidLawder148087newsML.txt"      
    ##  [376] "DavidLawder148215newsML.txt"      
    ##  [377] "DavidLawder148624newsML.txt"      
    ##  [378] "DavidLawder150717newsML.txt"      
    ##  [379] "DavidLawder153794newsML.txt"      
    ##  [380] "DavidLawder154150newsML.txt"      
    ##  [381] "DavidLawder154151newsML.txt"      
    ##  [382] "DavidLawder15588newsML.txt"       
    ##  [383] "DavidLawder156769newsML.txt"      
    ##  [384] "DavidLawder156823newsML.txt"      
    ##  [385] "DavidLawder157447newsML.txt"      
    ##  [386] "DavidLawder157860newsML.txt"      
    ##  [387] "DavidLawder159135newsML.txt"      
    ##  [388] "DavidLawder160026newsML.txt"      
    ##  [389] "DavidLawder160411newsML.txt"      
    ##  [390] "DavidLawder160765newsML.txt"      
    ##  [391] "DavidLawder161720newsML.txt"      
    ##  [392] "DavidLawder161907newsML.txt"      
    ##  [393] "DavidLawder161920newsML.txt"      
    ##  [394] "DavidLawder162027newsML.txt"      
    ##  [395] "DavidLawder162152newsML.txt"      
    ##  [396] "DavidLawder162531newsML.txt"      
    ##  [397] "DavidLawder163873newsML.txt"      
    ##  [398] "DavidLawder164658newsML.txt"      
    ##  [399] "DavidLawder170305newsML.txt"      
    ##  [400] "DavidLawder173935newsML.txt"      
    ##  [401] "EdnaFernandes10193newsML.txt"     
    ##  [402] "EdnaFernandes10264newsML.txt"     
    ##  [403] "EdnaFernandes116448newsML.txt"    
    ##  [404] "EdnaFernandes117960newsML.txt"    
    ##  [405] "EdnaFernandes119734newsML.txt"    
    ##  [406] "EdnaFernandes121318newsML.txt"    
    ##  [407] "EdnaFernandes121324newsML.txt"    
    ##  [408] "EdnaFernandes122988newsML.txt"    
    ##  [409] "EdnaFernandes126166newsML.txt"    
    ##  [410] "EdnaFernandes126175newsML.txt"    
    ##  [411] "EdnaFernandes126181newsML.txt"    
    ##  [412] "EdnaFernandes127410newsML.txt"    
    ##  [413] "EdnaFernandes134642newsML.txt"    
    ##  [414] "EdnaFernandes134683newsML.txt"    
    ##  [415] "EdnaFernandes138342newsML.txt"    
    ##  [416] "EdnaFernandes141414newsML.txt"    
    ##  [417] "EdnaFernandes15633newsML.txt"     
    ##  [418] "EdnaFernandes15644newsML.txt"     
    ##  [419] "EdnaFernandes17343newsML.txt"     
    ##  [420] "EdnaFernandes17663newsML.txt"     
    ##  [421] "EdnaFernandes178869newsML.txt"    
    ##  [422] "EdnaFernandes179688newsML.txt"    
    ##  [423] "EdnaFernandes18224newsML.txt"     
    ##  [424] "EdnaFernandes183674newsML.txt"    
    ##  [425] "EdnaFernandes183684newsML.txt"    
    ##  [426] "EdnaFernandes191766newsML.txt"    
    ##  [427] "EdnaFernandes191925newsML.txt"    
    ##  [428] "EdnaFernandes195876newsML.txt"    
    ##  [429] "EdnaFernandes199220newsML.txt"    
    ##  [430] "EdnaFernandes199226newsML.txt"    
    ##  [431] "EdnaFernandes202011newsML.txt"    
    ##  [432] "EdnaFernandes202020newsML.txt"    
    ##  [433] "EdnaFernandes203933newsML.txt"    
    ##  [434] "EdnaFernandes206049newsML.txt"    
    ##  [435] "EdnaFernandes207536newsML.txt"    
    ##  [436] "EdnaFernandes214915newsML.txt"    
    ##  [437] "EdnaFernandes214995newsML.txt"    
    ##  [438] "EdnaFernandes216658newsML.txt"    
    ##  [439] "EdnaFernandes219541newsML.txt"    
    ##  [440] "EdnaFernandes220686newsML.txt"    
    ##  [441] "EdnaFernandes222272newsML.txt"    
    ##  [442] "EdnaFernandes223143newsML.txt"    
    ##  [443] "EdnaFernandes223214newsML.txt"    
    ##  [444] "EdnaFernandes223786newsML.txt"    
    ##  [445] "EdnaFernandes225499newsML.txt"    
    ##  [446] "EdnaFernandes225507newsML.txt"    
    ##  [447] "EdnaFernandes225609newsML.txt"    
    ##  [448] "EdnaFernandes225917newsML.txt"    
    ##  [449] "EdnaFernandes230515newsML.txt"    
    ##  [450] "EdnaFernandes230517newsML.txt"    
    ##  [451] "EricAuchard100385newsML.txt"      
    ##  [452] "EricAuchard101678newsML.txt"      
    ##  [453] "EricAuchard111437newsML.txt"      
    ##  [454] "EricAuchard115060newsML.txt"      
    ##  [455] "EricAuchard116212newsML.txt"      
    ##  [456] "EricAuchard117213newsML.txt"      
    ##  [457] "EricAuchard119233newsML.txt"      
    ##  [458] "EricAuchard120484newsML.txt"      
    ##  [459] "EricAuchard121521newsML.txt"      
    ##  [460] "EricAuchard12877newsML.txt"       
    ##  [461] "EricAuchard13068newsML.txt"       
    ##  [462] "EricAuchard13069newsML.txt"       
    ##  [463] "EricAuchard133874newsML.txt"      
    ##  [464] "EricAuchard13415newsML.txt"       
    ##  [465] "EricAuchard13583newsML.txt"       
    ##  [466] "EricAuchard139624newsML.txt"      
    ##  [467] "EricAuchard14573newsML.txt"       
    ##  [468] "EricAuchard147596newsML.txt"      
    ##  [469] "EricAuchard149671newsML.txt"      
    ##  [470] "EricAuchard151452newsML.txt"      
    ##  [471] "EricAuchard152722newsML.txt"      
    ##  [472] "EricAuchard157435newsML.txt"      
    ##  [473] "EricAuchard157855newsML.txt"      
    ##  [474] "EricAuchard162159newsML.txt"      
    ##  [475] "EricAuchard172851newsML.txt"      
    ##  [476] "EricAuchard175415newsML.txt"      
    ##  [477] "EricAuchard18223newsML.txt"       
    ##  [478] "EricAuchard183344newsML.txt"      
    ##  [479] "EricAuchard183534newsML.txt"      
    ##  [480] "EricAuchard183828newsML.txt"      
    ##  [481] "EricAuchard186282newsML.txt"      
    ##  [482] "EricAuchard18802newsML.txt"       
    ##  [483] "EricAuchard191274newsML.txt"      
    ##  [484] "EricAuchard203838newsML.txt"      
    ##  [485] "EricAuchard205742newsML.txt"      
    ##  [486] "EricAuchard209979newsML.txt"      
    ##  [487] "EricAuchard210069newsML.txt"      
    ##  [488] "EricAuchard210631newsML.txt"      
    ##  [489] "EricAuchard216338newsML.txt"      
    ##  [490] "EricAuchard217358newsML.txt"      
    ##  [491] "EricAuchard227670newsML.txt"      
    ##  [492] "EricAuchard230274newsML.txt"      
    ##  [493] "EricAuchard231241newsML.txt"      
    ##  [494] "EricAuchard233319newsML.txt"      
    ##  [495] "EricAuchard260570newsML.txt"      
    ##  [496] "EricAuchard261502newsML.txt"      
    ##  [497] "EricAuchard263257newsML.txt"      
    ##  [498] "EricAuchard264271newsML.txt"      
    ##  [499] "EricAuchard288069newsML.txt"      
    ##  [500] "EricAuchard292697newsML.txt"      
    ##  [501] "FumikoFujisaki10028newsML.txt"    
    ##  [502] "FumikoFujisaki103993newsML.txt"   
    ##  [503] "FumikoFujisaki114292newsML.txt"   
    ##  [504] "FumikoFujisaki123528newsML.txt"   
    ##  [505] "FumikoFujisaki126656newsML.txt"   
    ##  [506] "FumikoFujisaki132085newsML.txt"   
    ##  [507] "FumikoFujisaki133528newsML.txt"   
    ##  [508] "FumikoFujisaki134268newsML.txt"   
    ##  [509] "FumikoFujisaki13494newsML.txt"    
    ##  [510] "FumikoFujisaki140372newsML.txt"   
    ##  [511] "FumikoFujisaki142116newsML.txt"   
    ##  [512] "FumikoFujisaki142460newsML.txt"   
    ##  [513] "FumikoFujisaki155075newsML.txt"   
    ##  [514] "FumikoFujisaki162426newsML.txt"   
    ##  [515] "FumikoFujisaki167727newsML.txt"   
    ##  [516] "FumikoFujisaki167905newsML.txt"   
    ##  [517] "FumikoFujisaki168224newsML.txt"   
    ##  [518] "FumikoFujisaki168234newsML.txt"   
    ##  [519] "FumikoFujisaki178863newsML.txt"   
    ##  [520] "FumikoFujisaki184337newsML.txt"   
    ##  [521] "FumikoFujisaki189025newsML.txt"   
    ##  [522] "FumikoFujisaki192472newsML.txt"   
    ##  [523] "FumikoFujisaki208614newsML.txt"   
    ##  [524] "FumikoFujisaki209817newsML.txt"   
    ##  [525] "FumikoFujisaki214194newsML.txt"   
    ##  [526] "FumikoFujisaki215310newsML.txt"   
    ##  [527] "FumikoFujisaki221583newsML.txt"   
    ##  [528] "FumikoFujisaki232740newsML.txt"   
    ##  [529] "FumikoFujisaki23340newsML.txt"    
    ##  [530] "FumikoFujisaki247828newsML.txt"   
    ##  [531] "FumikoFujisaki25542newsML.txt"    
    ##  [532] "FumikoFujisaki257487newsML.txt"   
    ##  [533] "FumikoFujisaki257754newsML.txt"   
    ##  [534] "FumikoFujisaki257776newsML.txt"   
    ##  [535] "FumikoFujisaki265694newsML.txt"   
    ##  [536] "FumikoFujisaki271169newsML.txt"   
    ##  [537] "FumikoFujisaki28301newsML.txt"    
    ##  [538] "FumikoFujisaki313599newsML.txt"   
    ##  [539] "FumikoFujisaki320052newsML.txt"   
    ##  [540] "FumikoFujisaki326601newsML.txt"   
    ##  [541] "FumikoFujisaki330736newsML.txt"   
    ##  [542] "FumikoFujisaki34837newsML.txt"    
    ##  [543] "FumikoFujisaki348688newsML.txt"   
    ##  [544] "FumikoFujisaki348849newsML.txt"   
    ##  [545] "FumikoFujisaki357074newsML.txt"   
    ##  [546] "FumikoFujisaki358900newsML.txt"   
    ##  [547] "FumikoFujisaki383379newsML.txt"   
    ##  [548] "FumikoFujisaki392201newsML.txt"   
    ##  [549] "FumikoFujisaki395779newsML.txt"   
    ##  [550] "FumikoFujisaki402531newsML.txt"   
    ##  [551] "GrahamEarnshaw103906newsML.txt"   
    ##  [552] "GrahamEarnshaw113070newsML.txt"   
    ##  [553] "GrahamEarnshaw113102newsML.txt"   
    ##  [554] "GrahamEarnshaw114251newsML.txt"   
    ##  [555] "GrahamEarnshaw128171newsML.txt"   
    ##  [556] "GrahamEarnshaw134309newsML.txt"   
    ##  [557] "GrahamEarnshaw13495newsML.txt"    
    ##  [558] "GrahamEarnshaw152163newsML.txt"   
    ##  [559] "GrahamEarnshaw167951newsML.txt"   
    ##  [560] "GrahamEarnshaw169618newsML.txt"   
    ##  [561] "GrahamEarnshaw172788newsML.txt"   
    ##  [562] "GrahamEarnshaw181135newsML.txt"   
    ##  [563] "GrahamEarnshaw187405newsML.txt"   
    ##  [564] "GrahamEarnshaw190410newsML.txt"   
    ##  [565] "GrahamEarnshaw198148newsML.txt"   
    ##  [566] "GrahamEarnshaw201228newsML.txt"   
    ##  [567] "GrahamEarnshaw204791newsML.txt"   
    ##  [568] "GrahamEarnshaw205033newsML.txt"   
    ##  [569] "GrahamEarnshaw217296newsML.txt"   
    ##  [570] "GrahamEarnshaw220126newsML.txt"   
    ##  [571] "GrahamEarnshaw226150newsML.txt"   
    ##  [572] "GrahamEarnshaw229903newsML.txt"   
    ##  [573] "GrahamEarnshaw231076newsML.txt"   
    ##  [574] "GrahamEarnshaw231139newsML.txt"   
    ##  [575] "GrahamEarnshaw232537newsML.txt"   
    ##  [576] "GrahamEarnshaw232804newsML.txt"   
    ##  [577] "GrahamEarnshaw232939newsML.txt"   
    ##  [578] "GrahamEarnshaw238773newsML.txt"   
    ##  [579] "GrahamEarnshaw241254newsML.txt"   
    ##  [580] "GrahamEarnshaw244929newsML.txt"   
    ##  [581] "GrahamEarnshaw247765newsML.txt"   
    ##  [582] "GrahamEarnshaw247793newsML.txt"   
    ##  [583] "GrahamEarnshaw249198newsML.txt"   
    ##  [584] "GrahamEarnshaw250860newsML.txt"   
    ##  [585] "GrahamEarnshaw252118newsML.txt"   
    ##  [586] "GrahamEarnshaw252156newsML.txt"   
    ##  [587] "GrahamEarnshaw254834newsML.txt"   
    ##  [588] "GrahamEarnshaw258604newsML.txt"   
    ##  [589] "GrahamEarnshaw262722newsML.txt"   
    ##  [590] "GrahamEarnshaw262829newsML.txt"   
    ##  [591] "GrahamEarnshaw265664newsML.txt"   
    ##  [592] "GrahamEarnshaw267059newsML.txt"   
    ##  [593] "GrahamEarnshaw270340newsML.txt"   
    ##  [594] "GrahamEarnshaw271134newsML.txt"   
    ##  [595] "GrahamEarnshaw271168newsML.txt"   
    ##  [596] "GrahamEarnshaw273087newsML.txt"   
    ##  [597] "GrahamEarnshaw274219newsML.txt"   
    ##  [598] "GrahamEarnshaw275716newsML.txt"   
    ##  [599] "GrahamEarnshaw277010newsML.txt"   
    ##  [600] "GrahamEarnshaw280504newsML.txt"   
    ##  [601] "HeatherScoffield101997newsML.txt" 
    ##  [602] "HeatherScoffield104778newsML.txt" 
    ##  [603] "HeatherScoffield110341newsML.txt" 
    ##  [604] "HeatherScoffield119818newsML.txt" 
    ##  [605] "HeatherScoffield125085newsML.txt" 
    ##  [606] "HeatherScoffield129447newsML.txt" 
    ##  [607] "HeatherScoffield133016newsML.txt" 
    ##  [608] "HeatherScoffield133017newsML.txt" 
    ##  [609] "HeatherScoffield147086newsML.txt" 
    ##  [610] "HeatherScoffield152972newsML.txt" 
    ##  [611] "HeatherScoffield15895newsML.txt"  
    ##  [612] "HeatherScoffield163214newsML.txt" 
    ##  [613] "HeatherScoffield165906newsML.txt" 
    ##  [614] "HeatherScoffield168501newsML.txt" 
    ##  [615] "HeatherScoffield175078newsML.txt" 
    ##  [616] "HeatherScoffield175721newsML.txt" 
    ##  [617] "HeatherScoffield184769newsML.txt" 
    ##  [618] "HeatherScoffield187990newsML.txt" 
    ##  [619] "HeatherScoffield191069newsML.txt" 
    ##  [620] "HeatherScoffield195125newsML.txt" 
    ##  [621] "HeatherScoffield204659newsML.txt" 
    ##  [622] "HeatherScoffield21156newsML.txt"  
    ##  [623] "HeatherScoffield216816newsML.txt" 
    ##  [624] "HeatherScoffield217889newsML.txt" 
    ##  [625] "HeatherScoffield219197newsML.txt" 
    ##  [626] "HeatherScoffield219630newsML.txt" 
    ##  [627] "HeatherScoffield222129newsML.txt" 
    ##  [628] "HeatherScoffield224980newsML.txt" 
    ##  [629] "HeatherScoffield227481newsML.txt" 
    ##  [630] "HeatherScoffield230639newsML.txt" 
    ##  [631] "HeatherScoffield231856newsML.txt" 
    ##  [632] "HeatherScoffield233579newsML.txt" 
    ##  [633] "HeatherScoffield233588newsML.txt" 
    ##  [634] "HeatherScoffield238607newsML.txt" 
    ##  [635] "HeatherScoffield243004newsML.txt" 
    ##  [636] "HeatherScoffield248680newsML.txt" 
    ##  [637] "HeatherScoffield248690newsML.txt" 
    ##  [638] "HeatherScoffield250167newsML.txt" 
    ##  [639] "HeatherScoffield251147newsML.txt" 
    ##  [640] "HeatherScoffield251643newsML.txt" 
    ##  [641] "HeatherScoffield252855newsML.txt" 
    ##  [642] "HeatherScoffield258231newsML.txt" 
    ##  [643] "HeatherScoffield260834newsML.txt" 
    ##  [644] "HeatherScoffield263612newsML.txt" 
    ##  [645] "HeatherScoffield266002newsML.txt" 
    ##  [646] "HeatherScoffield266003newsML.txt" 
    ##  [647] "HeatherScoffield2751newsML.txt"   
    ##  [648] "HeatherScoffield295589newsML.txt" 
    ##  [649] "HeatherScoffield298748newsML.txt" 
    ##  [650] "HeatherScoffield301524newsML.txt" 
    ##  [651] "JaneMacartney101337newsML.txt"    
    ##  [652] "JaneMacartney102414newsML.txt"    
    ##  [653] "JaneMacartney104067newsML.txt"    
    ##  [654] "JaneMacartney105207newsML.txt"    
    ##  [655] "JaneMacartney105215newsML.txt"    
    ##  [656] "JaneMacartney106671newsML.txt"    
    ##  [657] "JaneMacartney106841newsML.txt"    
    ##  [658] "JaneMacartney108069newsML.txt"    
    ##  [659] "JaneMacartney108114newsML.txt"    
    ##  [660] "JaneMacartney109736newsML.txt"    
    ##  [661] "JaneMacartney112616newsML.txt"    
    ##  [662] "JaneMacartney112619newsML.txt"    
    ##  [663] "JaneMacartney113072newsML.txt"    
    ##  [664] "JaneMacartney113073newsML.txt"    
    ##  [665] "JaneMacartney115502newsML.txt"    
    ##  [666] "JaneMacartney121957newsML.txt"    
    ##  [667] "JaneMacartney125313newsML.txt"    
    ##  [668] "JaneMacartney127969newsML.txt"    
    ##  [669] "JaneMacartney150364newsML.txt"    
    ##  [670] "JaneMacartney153470newsML.txt"    
    ##  [671] "JaneMacartney156668newsML.txt"    
    ##  [672] "JaneMacartney158581newsML.txt"    
    ##  [673] "JaneMacartney162404newsML.txt"    
    ##  [674] "JaneMacartney165077newsML.txt"    
    ##  [675] "JaneMacartney242319newsML.txt"    
    ##  [676] "JaneMacartney242519newsML.txt"    
    ##  [677] "JaneMacartney244948newsML.txt"    
    ##  [678] "JaneMacartney246277newsML.txt"    
    ##  [679] "JaneMacartney247009newsML.txt"    
    ##  [680] "JaneMacartney249226newsML.txt"    
    ##  [681] "JaneMacartney253835newsML.txt"    
    ##  [682] "JaneMacartney264064newsML.txt"    
    ##  [683] "JaneMacartney269865newsML.txt"    
    ##  [684] "JaneMacartney272173newsML.txt"    
    ##  [685] "JaneMacartney275597newsML.txt"    
    ##  [686] "JaneMacartney276094newsML.txt"    
    ##  [687] "JaneMacartney277885newsML.txt"    
    ##  [688] "JaneMacartney279102newsML.txt"    
    ##  [689] "JaneMacartney279442newsML.txt"    
    ##  [690] "JaneMacartney279474newsML.txt"    
    ##  [691] "JaneMacartney279477newsML.txt"    
    ##  [692] "JaneMacartney280467newsML.txt"    
    ##  [693] "JaneMacartney280485newsML.txt"    
    ##  [694] "JaneMacartney281200newsML.txt"    
    ##  [695] "JaneMacartney281220newsML.txt"    
    ##  [696] "JaneMacartney284741newsML.txt"    
    ##  [697] "JaneMacartney296010newsML.txt"    
    ##  [698] "JaneMacartney297895newsML.txt"    
    ##  [699] "JaneMacartney301144newsML.txt"    
    ##  [700] "JaneMacartney301175newsML.txt"    
    ##  [701] "JanLopatka101886newsML.txt"       
    ##  [702] "JanLopatka102211newsML.txt"       
    ##  [703] "JanLopatka10224newsML.txt"        
    ##  [704] "JanLopatka104879newsML.txt"       
    ##  [705] "JanLopatka10626newsML.txt"        
    ##  [706] "JanLopatka123170newsML.txt"       
    ##  [707] "JanLopatka126345newsML.txt"       
    ##  [708] "JanLopatka156044newsML.txt"       
    ##  [709] "JanLopatka15669newsML.txt"        
    ##  [710] "JanLopatka159487newsML.txt"       
    ##  [711] "JanLopatka159744newsML.txt"       
    ##  [712] "JanLopatka163094newsML.txt"       
    ##  [713] "JanLopatka163409newsML.txt"       
    ##  [714] "JanLopatka169004newsML.txt"       
    ##  [715] "JanLopatka172514newsML.txt"       
    ##  [716] "JanLopatka175835newsML.txt"       
    ##  [717] "JanLopatka175940newsML.txt"       
    ##  [718] "JanLopatka177552newsML.txt"       
    ##  [719] "JanLopatka182102newsML.txt"       
    ##  [720] "JanLopatka182167newsML.txt"       
    ##  [721] "JanLopatka185855newsML.txt"       
    ##  [722] "JanLopatka185875newsML.txt"       
    ##  [723] "JanLopatka192053newsML.txt"       
    ##  [724] "JanLopatka192103newsML.txt"       
    ##  [725] "JanLopatka194352newsML.txt"       
    ##  [726] "JanLopatka194663newsML.txt"       
    ##  [727] "JanLopatka194664newsML.txt"       
    ##  [728] "JanLopatka196127newsML.txt"       
    ##  [729] "JanLopatka202372newsML.txt"       
    ##  [730] "JanLopatka220837newsML.txt"       
    ##  [731] "JanLopatka222246newsML.txt"       
    ##  [732] "JanLopatka222478newsML.txt"       
    ##  [733] "JanLopatka224921newsML.txt"       
    ##  [734] "JanLopatka226489newsML.txt"       
    ##  [735] "JanLopatka228103newsML.txt"       
    ##  [736] "JanLopatka228154newsML.txt"       
    ##  [737] "JanLopatka230755newsML.txt"       
    ##  [738] "JanLopatka254538newsML.txt"       
    ##  [739] "JanLopatka260914newsML.txt"       
    ##  [740] "JanLopatka260918newsML.txt"       
    ##  [741] "JanLopatka263451newsML.txt"       
    ##  [742] "JanLopatka263741newsML.txt"       
    ##  [743] "JanLopatka26976newsML.txt"        
    ##  [744] "JanLopatka27569newsML.txt"        
    ##  [745] "JanLopatka288397newsML.txt"       
    ##  [746] "JanLopatka290685newsML.txt"       
    ##  [747] "JanLopatka293006newsML.txt"       
    ##  [748] "JanLopatka293018newsML.txt"       
    ##  [749] "JanLopatka293216newsML.txt"       
    ##  [750] "JanLopatka295717newsML.txt"       
    ##  [751] "JimGilchrist103904newsML.txt"     
    ##  [752] "JimGilchrist105222newsML.txt"     
    ##  [753] "JimGilchrist108091newsML.txt"     
    ##  [754] "JimGilchrist110715newsML.txt"     
    ##  [755] "JimGilchrist110767newsML.txt"     
    ##  [756] "JimGilchrist11269newsML.txt"      
    ##  [757] "JimGilchrist11270newsML.txt"      
    ##  [758] "JimGilchrist114262newsML.txt"     
    ##  [759] "JimGilchrist117000newsML.txt"     
    ##  [760] "JimGilchrist120242newsML.txt"     
    ##  [761] "JimGilchrist120244newsML.txt"     
    ##  [762] "JimGilchrist121948newsML.txt"     
    ##  [763] "JimGilchrist123461newsML.txt"     
    ##  [764] "JimGilchrist123517newsML.txt"     
    ##  [765] "JimGilchrist123524newsML.txt"     
    ##  [766] "JimGilchrist126597newsML.txt"     
    ##  [767] "JimGilchrist126622newsML.txt"     
    ##  [768] "JimGilchrist130525newsML.txt"     
    ##  [769] "JimGilchrist130529newsML.txt"     
    ##  [770] "JimGilchrist130535newsML.txt"     
    ##  [771] "JimGilchrist130537newsML.txt"     
    ##  [772] "JimGilchrist133484newsML.txt"     
    ##  [773] "JimGilchrist133504newsML.txt"     
    ##  [774] "JimGilchrist138486newsML.txt"     
    ##  [775] "JimGilchrist138497newsML.txt"     
    ##  [776] "JimGilchrist140355newsML.txt"     
    ##  [777] "JimGilchrist143463newsML.txt"     
    ##  [778] "JimGilchrist147518newsML.txt"     
    ##  [779] "JimGilchrist147521newsML.txt"     
    ##  [780] "JimGilchrist147528newsML.txt"     
    ##  [781] "JimGilchrist147557newsML.txt"     
    ##  [782] "JimGilchrist148847newsML.txt"     
    ##  [783] "JimGilchrist156681newsML.txt"     
    ##  [784] "JimGilchrist156695newsML.txt"     
    ##  [785] "JimGilchrist159926newsML.txt"     
    ##  [786] "JimGilchrist166432newsML.txt"     
    ##  [787] "JimGilchrist166456newsML.txt"     
    ##  [788] "JimGilchrist171185newsML.txt"     
    ##  [789] "JimGilchrist172811newsML.txt"     
    ##  [790] "JimGilchrist176224newsML.txt"     
    ##  [791] "JimGilchrist180176newsML.txt"     
    ##  [792] "JimGilchrist182456newsML.txt"     
    ##  [793] "JimGilchrist186188newsML.txt"     
    ##  [794] "JimGilchrist186190newsML.txt"     
    ##  [795] "JimGilchrist19345newsML.txt"      
    ##  [796] "JimGilchrist196712newsML.txt"     
    ##  [797] "JimGilchrist196713newsML.txt"     
    ##  [798] "JimGilchrist206557newsML.txt"     
    ##  [799] "JimGilchrist206567newsML.txt"     
    ##  [800] "JimGilchrist21567newsML.txt"      
    ##  [801] "JoeOrtiz100554newsML.txt"         
    ##  [802] "JoeOrtiz100593newsML.txt"         
    ##  [803] "JoeOrtiz100618newsML.txt"         
    ##  [804] "JoeOrtiz130040newsML.txt"         
    ##  [805] "JoeOrtiz137871newsML.txt"         
    ##  [806] "JoeOrtiz148240newsML.txt"         
    ##  [807] "JoeOrtiz160682newsML.txt"         
    ##  [808] "JoeOrtiz167342newsML.txt"         
    ##  [809] "JoeOrtiz167802newsML.txt"         
    ##  [810] "JoeOrtiz172150newsML.txt"         
    ##  [811] "JoeOrtiz180577newsML.txt"         
    ##  [812] "JoeOrtiz185661newsML.txt"         
    ##  [813] "JoeOrtiz186895newsML.txt"         
    ##  [814] "JoeOrtiz189829newsML.txt"         
    ##  [815] "JoeOrtiz197549newsML.txt"         
    ##  [816] "JoeOrtiz202086newsML.txt"         
    ##  [817] "JoeOrtiz207570newsML.txt"         
    ##  [818] "JoeOrtiz210554newsML.txt"         
    ##  [819] "JoeOrtiz216670newsML.txt"         
    ##  [820] "JoeOrtiz222292newsML.txt"         
    ##  [821] "JoeOrtiz223148newsML.txt"         
    ##  [822] "JoeOrtiz223783newsML.txt"         
    ##  [823] "JoeOrtiz232082newsML.txt"         
    ##  [824] "JoeOrtiz242939newsML.txt"         
    ##  [825] "JoeOrtiz245686newsML.txt"         
    ##  [826] "JoeOrtiz247024newsML.txt"         
    ##  [827] "JoeOrtiz263500newsML.txt"         
    ##  [828] "JoeOrtiz263503newsML.txt"         
    ##  [829] "JoeOrtiz289108newsML.txt"         
    ##  [830] "JoeOrtiz290471newsML.txt"         
    ##  [831] "JoeOrtiz29051newsML.txt"          
    ##  [832] "JoeOrtiz294330newsML.txt"         
    ##  [833] "JoeOrtiz294823newsML.txt"         
    ##  [834] "JoeOrtiz297151newsML.txt"         
    ##  [835] "JoeOrtiz299862newsML.txt"         
    ##  [836] "JoeOrtiz299957newsML.txt"         
    ##  [837] "JoeOrtiz300358newsML.txt"         
    ##  [838] "JoeOrtiz300369newsML.txt"         
    ##  [839] "JoeOrtiz309597newsML.txt"         
    ##  [840] "JoeOrtiz320925newsML.txt"         
    ##  [841] "JoeOrtiz324101newsML.txt"         
    ##  [842] "JoeOrtiz34037newsML.txt"          
    ##  [843] "JoeOrtiz341761newsML.txt"         
    ##  [844] "JoeOrtiz342301newsML.txt"         
    ##  [845] "JoeOrtiz343271newsML.txt"         
    ##  [846] "JoeOrtiz343304newsML.txt"         
    ##  [847] "JoeOrtiz344889newsML.txt"         
    ##  [848] "JoeOrtiz344993newsML.txt"         
    ##  [849] "JoeOrtiz364126newsML.txt"         
    ##  [850] "JoeOrtiz364594newsML.txt"         
    ##  [851] "JohnMastrini105045newsML.txt"     
    ##  [852] "JohnMastrini123158newsML.txt"     
    ##  [853] "JohnMastrini133171newsML.txt"     
    ##  [854] "JohnMastrini13806newsML.txt"      
    ##  [855] "JohnMastrini153158newsML.txt"     
    ##  [856] "JohnMastrini156033newsML.txt"     
    ##  [857] "JohnMastrini156054newsML.txt"     
    ##  [858] "JohnMastrini156301newsML.txt"     
    ##  [859] "JohnMastrini159754newsML.txt"     
    ##  [860] "JohnMastrini161715newsML.txt"     
    ##  [861] "JohnMastrini177540newsML.txt"     
    ##  [862] "JohnMastrini179858newsML.txt"     
    ##  [863] "JohnMastrini18297newsML.txt"      
    ##  [864] "JohnMastrini18320newsML.txt"      
    ##  [865] "JohnMastrini18519newsML.txt"      
    ##  [866] "JohnMastrini18529newsML.txt"      
    ##  [867] "JohnMastrini192052newsML.txt"     
    ##  [868] "JohnMastrini192116newsML.txt"     
    ##  [869] "JohnMastrini194494newsML.txt"     
    ##  [870] "JohnMastrini194608newsML.txt"     
    ##  [871] "JohnMastrini195069newsML.txt"     
    ##  [872] "JohnMastrini195181newsML.txt"     
    ##  [873] "JohnMastrini195805newsML.txt"     
    ##  [874] "JohnMastrini202307newsML.txt"     
    ##  [875] "JohnMastrini205953newsML.txt"     
    ##  [876] "JohnMastrini207961newsML.txt"     
    ##  [877] "JohnMastrini212289newsML.txt"     
    ##  [878] "JohnMastrini21246newsML.txt"      
    ##  [879] "JohnMastrini212832newsML.txt"     
    ##  [880] "JohnMastrini216927newsML.txt"     
    ##  [881] "JohnMastrini22903newsML.txt"      
    ##  [882] "JohnMastrini230504newsML.txt"     
    ##  [883] "JohnMastrini236594newsML.txt"     
    ##  [884] "JohnMastrini239149newsML.txt"     
    ##  [885] "JohnMastrini242192newsML.txt"     
    ##  [886] "JohnMastrini242893newsML.txt"     
    ##  [887] "JohnMastrini258334newsML.txt"     
    ##  [888] "JohnMastrini258383newsML.txt"     
    ##  [889] "JohnMastrini260915newsML.txt"     
    ##  [890] "JohnMastrini260917newsML.txt"     
    ##  [891] "JohnMastrini26311newsML.txt"      
    ##  [892] "JohnMastrini263486newsML.txt"     
    ##  [893] "JohnMastrini263693newsML.txt"     
    ##  [894] "JohnMastrini266394newsML.txt"     
    ##  [895] "JohnMastrini266601newsML.txt"     
    ##  [896] "JohnMastrini266669newsML.txt"     
    ##  [897] "JohnMastrini269377newsML.txt"     
    ##  [898] "JohnMastrini27007newsML.txt"      
    ##  [899] "JohnMastrini272887newsML.txt"     
    ##  [900] "JohnMastrini281893newsML.txt"     
    ##  [901] "JonathanBirt100677newsML.txt"     
    ##  [902] "JonathanBirt106076newsML.txt"     
    ##  [903] "JonathanBirt107612newsML.txt"     
    ##  [904] "JonathanBirt116412newsML.txt"     
    ##  [905] "JonathanBirt116415newsML.txt"     
    ##  [906] "JonathanBirt123004newsML.txt"     
    ##  [907] "JonathanBirt124585newsML.txt"     
    ##  [908] "JonathanBirt134647newsML.txt"     
    ##  [909] "JonathanBirt137920newsML.txt"     
    ##  [910] "JonathanBirt159532newsML.txt"     
    ##  [911] "JonathanBirt160360newsML.txt"     
    ##  [912] "JonathanBirt163111newsML.txt"     
    ##  [913] "JonathanBirt163135newsML.txt"     
    ##  [914] "JonathanBirt164502newsML.txt"     
    ##  [915] "JonathanBirt165827newsML.txt"     
    ##  [916] "JonathanBirt167309newsML.txt"     
    ##  [917] "JonathanBirt169040newsML.txt"     
    ##  [918] "JonathanBirt172185newsML.txt"     
    ##  [919] "JonathanBirt174287newsML.txt"     
    ##  [920] "JonathanBirt200640newsML.txt"     
    ##  [921] "JonathanBirt200679newsML.txt"     
    ##  [922] "JonathanBirt201053newsML.txt"     
    ##  [923] "JonathanBirt219548newsML.txt"     
    ##  [924] "JonathanBirt225509newsML.txt"     
    ##  [925] "JonathanBirt230577newsML.txt"     
    ##  [926] "JonathanBirt232113newsML.txt"     
    ##  [927] "JonathanBirt250638newsML.txt"     
    ##  [928] "JonathanBirt26877newsML.txt"      
    ##  [929] "JonathanBirt27668newsML.txt"      
    ##  [930] "JonathanBirt28480newsML.txt"      
    ##  [931] "JonathanBirt289165newsML.txt"     
    ##  [932] "JonathanBirt29057newsML.txt"      
    ##  [933] "JonathanBirt293044newsML.txt"     
    ##  [934] "JonathanBirt294343newsML.txt"     
    ##  [935] "JonathanBirt300439newsML.txt"     
    ##  [936] "JonathanBirt31091newsML.txt"      
    ##  [937] "JonathanBirt314382newsML.txt"     
    ##  [938] "JonathanBirt315525newsML.txt"     
    ##  [939] "JonathanBirt319856newsML.txt"     
    ##  [940] "JonathanBirt325858newsML.txt"     
    ##  [941] "JonathanBirt327653newsML.txt"     
    ##  [942] "JonathanBirt329893newsML.txt"     
    ##  [943] "JonathanBirt331603newsML.txt"     
    ##  [944] "JonathanBirt331672newsML.txt"     
    ##  [945] "JonathanBirt334243newsML.txt"     
    ##  [946] "JonathanBirt338594newsML.txt"     
    ##  [947] "JonathanBirt338600newsML.txt"     
    ##  [948] "JonathanBirt339075newsML.txt"     
    ##  [949] "JonathanBirt339711newsML.txt"     
    ##  [950] "JonathanBirt340119newsML.txt"     
    ##  [951] "JoWinterbottom103362newsML.txt"   
    ##  [952] "JoWinterbottom103380newsML.txt"   
    ##  [953] "JoWinterbottom105972newsML.txt"   
    ##  [954] "JoWinterbottom115389newsML.txt"   
    ##  [955] "JoWinterbottom117971newsML.txt"   
    ##  [956] "JoWinterbottom126147newsML.txt"   
    ##  [957] "JoWinterbottom130021newsML.txt"   
    ##  [958] "JoWinterbottom131132newsML.txt"   
    ##  [959] "JoWinterbottom131763newsML.txt"   
    ##  [960] "JoWinterbottom132968newsML.txt"   
    ##  [961] "JoWinterbottom136300newsML.txt"   
    ##  [962] "JoWinterbottom138243newsML.txt"   
    ##  [963] "JoWinterbottom141413newsML.txt"   
    ##  [964] "JoWinterbottom142994newsML.txt"   
    ##  [965] "JoWinterbottom142995newsML.txt"   
    ##  [966] "JoWinterbottom142997newsML.txt"   
    ##  [967] "JoWinterbottom143011newsML.txt"   
    ##  [968] "JoWinterbottom143033newsML.txt"   
    ##  [969] "JoWinterbottom143036newsML.txt"   
    ##  [970] "JoWinterbottom144390newsML.txt"   
    ##  [971] "JoWinterbottom147012newsML.txt"   
    ##  [972] "JoWinterbottom148026newsML.txt"   
    ##  [973] "JoWinterbottom148227newsML.txt"   
    ##  [974] "JoWinterbottom149908newsML.txt"   
    ##  [975] "JoWinterbottom151249newsML.txt"   
    ##  [976] "JoWinterbottom152843newsML.txt"   
    ##  [977] "JoWinterbottom154415newsML.txt"   
    ##  [978] "JoWinterbottom159560newsML.txt"   
    ##  [979] "JoWinterbottom183638newsML.txt"   
    ##  [980] "JoWinterbottom183716newsML.txt"   
    ##  [981] "JoWinterbottom184783newsML.txt"   
    ##  [982] "JoWinterbottom185611newsML.txt"   
    ##  [983] "JoWinterbottom185674newsML.txt"   
    ##  [984] "JoWinterbottom186888newsML.txt"   
    ##  [985] "JoWinterbottom186890newsML.txt"   
    ##  [986] "JoWinterbottom187979newsML.txt"   
    ##  [987] "JoWinterbottom190268newsML.txt"   
    ##  [988] "JoWinterbottom191718newsML.txt"   
    ##  [989] "JoWinterbottom195814newsML.txt"   
    ##  [990] "JoWinterbottom197541newsML.txt"   
    ##  [991] "JoWinterbottom199265newsML.txt"   
    ##  [992] "JoWinterbottom200394newsML.txt"   
    ##  [993] "JoWinterbottom200668newsML.txt"   
    ##  [994] "JoWinterbottom203923newsML.txt"   
    ##  [995] "JoWinterbottom203926newsML.txt"   
    ##  [996] "JoWinterbottom206040newsML.txt"   
    ##  [997] "JoWinterbottom207515newsML.txt"   
    ##  [998] "JoWinterbottom210506newsML.txt"   
    ##  [999] "JoWinterbottom210519newsML.txt"   
    ## [1000] "JoWinterbottom220918newsML.txt"   
    ## [1001] "KarlPenhaul110696newsML.txt"      
    ## [1002] "KarlPenhaul123422newsML.txt"      
    ## [1003] "KarlPenhaul126553newsML.txt"      
    ## [1004] "KarlPenhaul129041newsML.txt"      
    ## [1005] "KarlPenhaul130351newsML.txt"      
    ## [1006] "KarlPenhaul133336newsML.txt"      
    ## [1007] "KarlPenhaul136654newsML.txt"      
    ## [1008] "KarlPenhaul140181newsML.txt"      
    ## [1009] "KarlPenhaul143439newsML.txt"      
    ## [1010] "KarlPenhaul156487newsML.txt"      
    ## [1011] "KarlPenhaul156602newsML.txt"      
    ## [1012] "KarlPenhaul179986newsML.txt"      
    ## [1013] "KarlPenhaul186082newsML.txt"      
    ## [1014] "KarlPenhaul195207newsML.txt"      
    ## [1015] "KarlPenhaul202500newsML.txt"      
    ## [1016] "KarlPenhaul217081newsML.txt"      
    ## [1017] "KarlPenhaul217140newsML.txt"      
    ## [1018] "KarlPenhaul227159newsML.txt"      
    ## [1019] "KarlPenhaul228172newsML.txt"      
    ## [1020] "KarlPenhaul228182newsML.txt"      
    ## [1021] "KarlPenhaul236734newsML.txt"      
    ## [1022] "KarlPenhaul236854newsML.txt"      
    ## [1023] "KarlPenhaul242202newsML.txt"      
    ## [1024] "KarlPenhaul243366newsML.txt"      
    ## [1025] "KarlPenhaul251933newsML.txt"      
    ## [1026] "KarlPenhaul254655newsML.txt"      
    ## [1027] "KarlPenhaul254777newsML.txt"      
    ## [1028] "KarlPenhaul258467newsML.txt"      
    ## [1029] "KarlPenhaul261073newsML.txt"      
    ## [1030] "KarlPenhaul261076newsML.txt"      
    ## [1031] "KarlPenhaul263877newsML.txt"      
    ## [1032] "KarlPenhaul263931newsML.txt"      
    ## [1033] "KarlPenhaul266933newsML.txt"      
    ## [1034] "KarlPenhaul266934newsML.txt"      
    ## [1035] "KarlPenhaul269494newsML.txt"      
    ## [1036] "KarlPenhaul281959newsML.txt"      
    ## [1037] "KarlPenhaul286977newsML.txt"      
    ## [1038] "KarlPenhaul293363newsML.txt"      
    ## [1039] "KarlPenhaul293377newsML.txt"      
    ## [1040] "KarlPenhaul293437newsML.txt"      
    ## [1041] "KarlPenhaul295872newsML.txt"      
    ## [1042] "KarlPenhaul305571newsML.txt"      
    ## [1043] "KarlPenhaul305637newsML.txt"      
    ## [1044] "KarlPenhaul311686newsML.txt"      
    ## [1045] "KarlPenhaul314740newsML.txt"      
    ## [1046] "KarlPenhaul314809newsML.txt"      
    ## [1047] "KarlPenhaul333463newsML.txt"      
    ## [1048] "KarlPenhaul334589newsML.txt"      
    ## [1049] "KarlPenhaul343706newsML.txt"      
    ## [1050] "KarlPenhaul349781newsML.txt"      
    ## [1051] "KeithWeir107610newsML.txt"        
    ## [1052] "KeithWeir110316newsML.txt"        
    ## [1053] "KeithWeir113806newsML.txt"        
    ## [1054] "KeithWeir114919newsML.txt"        
    ## [1055] "KeithWeir114962newsML.txt"        
    ## [1056] "KeithWeir117941newsML.txt"        
    ## [1057] "KeithWeir117955newsML.txt"        
    ## [1058] "KeithWeir117958newsML.txt"        
    ## [1059] "KeithWeir122945newsML.txt"        
    ## [1060] "KeithWeir124269newsML.txt"        
    ## [1061] "KeithWeir125542newsML.txt"        
    ## [1062] "KeithWeir12639newsML.txt"         
    ## [1063] "KeithWeir13441newsML.txt"         
    ## [1064] "KeithWeir13442newsML.txt"         
    ## [1065] "KeithWeir138347newsML.txt"        
    ## [1066] "KeithWeir152889newsML.txt"        
    ## [1067] "KeithWeir157702newsML.txt"        
    ## [1068] "KeithWeir158161newsML.txt"        
    ## [1069] "KeithWeir161602newsML.txt"        
    ## [1070] "KeithWeir161738newsML.txt"        
    ## [1071] "KeithWeir161747newsML.txt"        
    ## [1072] "KeithWeir161753newsML.txt"        
    ## [1073] "KeithWeir162021newsML.txt"        
    ## [1074] "KeithWeir162208newsML.txt"        
    ## [1075] "KeithWeir163109newsML.txt"        
    ## [1076] "KeithWeir164490newsML.txt"        
    ## [1077] "KeithWeir172133newsML.txt"        
    ## [1078] "KeithWeir181588newsML.txt"        
    ## [1079] "KeithWeir199225newsML.txt"        
    ## [1080] "KeithWeir200592newsML.txt"        
    ## [1081] "KeithWeir201067newsML.txt"        
    ## [1082] "KeithWeir202015newsML.txt"        
    ## [1083] "KeithWeir203924newsML.txt"        
    ## [1084] "KeithWeir203930newsML.txt"        
    ## [1085] "KeithWeir21070newsML.txt"         
    ## [1086] "KeithWeir213689newsML.txt"        
    ## [1087] "KeithWeir223176newsML.txt"        
    ## [1088] "KeithWeir22411newsML.txt"         
    ## [1089] "KeithWeir229606newsML.txt"        
    ## [1090] "KeithWeir232566newsML.txt"        
    ## [1091] "KeithWeir232616newsML.txt"        
    ## [1092] "KeithWeir2344newsML.txt"          
    ## [1093] "KeithWeir234892newsML.txt"        
    ## [1094] "KeithWeir241521newsML.txt"        
    ## [1095] "KeithWeir259522newsML.txt"        
    ## [1096] "KeithWeir262296newsML.txt"        
    ## [1097] "KeithWeir265008newsML.txt"        
    ## [1098] "KeithWeir265140newsML.txt"        
    ## [1099] "KeithWeir275481newsML.txt"        
    ## [1100] "KeithWeir286107newsML.txt"        
    ## [1101] "KevinDrawbaugh100163newsML.txt"   
    ## [1102] "KevinDrawbaugh100485newsML.txt"   
    ## [1103] "KevinDrawbaugh102702newsML.txt"   
    ## [1104] "KevinDrawbaugh105399newsML.txt"   
    ## [1105] "KevinDrawbaugh10771newsML.txt"    
    ## [1106] "KevinDrawbaugh108321newsML.txt"   
    ## [1107] "KevinDrawbaugh110894newsML.txt"   
    ## [1108] "KevinDrawbaugh110981newsML.txt"   
    ## [1109] "KevinDrawbaugh113464newsML.txt"   
    ## [1110] "KevinDrawbaugh11690newsML.txt"    
    ## [1111] "KevinDrawbaugh11692newsML.txt"    
    ## [1112] "KevinDrawbaugh120486newsML.txt"   
    ## [1113] "KevinDrawbaugh126026newsML.txt"   
    ## [1114] "KevinDrawbaugh130811newsML.txt"   
    ## [1115] "KevinDrawbaugh133777newsML.txt"   
    ## [1116] "KevinDrawbaugh137042newsML.txt"   
    ## [1117] "KevinDrawbaugh137481newsML.txt"   
    ## [1118] "KevinDrawbaugh137616newsML.txt"   
    ## [1119] "KevinDrawbaugh13838newsML.txt"    
    ## [1120] "KevinDrawbaugh143586newsML.txt"   
    ## [1121] "KevinDrawbaugh143738newsML.txt"   
    ## [1122] "KevinDrawbaugh154378newsML.txt"   
    ## [1123] "KevinDrawbaugh154388newsML.txt"   
    ## [1124] "KevinDrawbaugh16057newsML.txt"    
    ## [1125] "KevinDrawbaugh173045newsML.txt"   
    ## [1126] "KevinDrawbaugh178086newsML.txt"   
    ## [1127] "KevinDrawbaugh191657newsML.txt"   
    ## [1128] "KevinDrawbaugh233324newsML.txt"   
    ## [1129] "KevinDrawbaugh243558newsML.txt"   
    ## [1130] "KevinDrawbaugh248401newsML.txt"   
    ## [1131] "KevinDrawbaugh249305newsML.txt"   
    ## [1132] "KevinDrawbaugh249404newsML.txt"   
    ## [1133] "KevinDrawbaugh259009newsML.txt"   
    ## [1134] "KevinDrawbaugh269637newsML.txt"   
    ## [1135] "KevinDrawbaugh270070newsML.txt"   
    ## [1136] "KevinDrawbaugh283396newsML.txt"   
    ## [1137] "KevinDrawbaugh283914newsML.txt"   
    ## [1138] "KevinDrawbaugh284178newsML.txt"   
    ## [1139] "KevinDrawbaugh290199newsML.txt"   
    ## [1140] "KevinDrawbaugh2906newsML.txt"     
    ## [1141] "KevinDrawbaugh290996newsML.txt"   
    ## [1142] "KevinDrawbaugh291190newsML.txt"   
    ## [1143] "KevinDrawbaugh291432newsML.txt"   
    ## [1144] "KevinDrawbaugh293659newsML.txt"   
    ## [1145] "KevinDrawbaugh293808newsML.txt"   
    ## [1146] "KevinDrawbaugh299416newsML.txt"   
    ## [1147] "KevinDrawbaugh305957newsML.txt"   
    ## [1148] "KevinDrawbaugh311940newsML.txt"   
    ## [1149] "KevinDrawbaugh317846newsML.txt"   
    ## [1150] "KevinDrawbaugh323618newsML.txt"   
    ## [1151] "KevinMorrison105191newsML.txt"    
    ## [1152] "KevinMorrison109875newsML.txt"    
    ## [1153] "KevinMorrison109876newsML.txt"    
    ## [1154] "KevinMorrison110703newsML.txt"    
    ## [1155] "KevinMorrison114198newsML.txt"    
    ## [1156] "KevinMorrison115436newsML.txt"    
    ## [1157] "KevinMorrison116890newsML.txt"    
    ## [1158] "KevinMorrison125098newsML.txt"    
    ## [1159] "KevinMorrison125545newsML.txt"    
    ## [1160] "KevinMorrison133461newsML.txt"    
    ## [1161] "KevinMorrison138954newsML.txt"    
    ## [1162] "KevinMorrison15226newsML.txt"     
    ## [1163] "KevinMorrison171096newsML.txt"    
    ## [1164] "KevinMorrison176105newsML.txt"    
    ## [1165] "KevinMorrison18329newsML.txt"     
    ## [1166] "KevinMorrison184096newsML.txt"    
    ## [1167] "KevinMorrison184143newsML.txt"    
    ## [1168] "KevinMorrison184150newsML.txt"    
    ## [1169] "KevinMorrison198071newsML.txt"    
    ## [1170] "KevinMorrison21536newsML.txt"     
    ## [1171] "KevinMorrison218602newsML.txt"    
    ## [1172] "KevinMorrison218625newsML.txt"    
    ## [1173] "KevinMorrison221499newsML.txt"    
    ## [1174] "KevinMorrison221509newsML.txt"    
    ## [1175] "KevinMorrison221518newsML.txt"    
    ## [1176] "KevinMorrison223858newsML.txt"    
    ## [1177] "KevinMorrison223897newsML.txt"    
    ## [1178] "KevinMorrison229778newsML.txt"    
    ## [1179] "KevinMorrison235532newsML.txt"    
    ## [1180] "KevinMorrison241149newsML.txt"    
    ## [1181] "KevinMorrison244696newsML.txt"    
    ## [1182] "KevinMorrison244698newsML.txt"    
    ## [1183] "KevinMorrison244734newsML.txt"    
    ## [1184] "KevinMorrison247635newsML.txt"    
    ## [1185] "KevinMorrison247636newsML.txt"    
    ## [1186] "KevinMorrison247652newsML.txt"    
    ## [1187] "KevinMorrison247668newsML.txt"    
    ## [1188] "KevinMorrison247671newsML.txt"    
    ## [1189] "KevinMorrison255906newsML.txt"    
    ## [1190] "KevinMorrison256319newsML.txt"    
    ## [1191] "KevinMorrison256320newsML.txt"    
    ## [1192] "KevinMorrison256349newsML.txt"    
    ## [1193] "KevinMorrison256350newsML.txt"    
    ## [1194] "KevinMorrison2730newsML.txt"      
    ## [1195] "KevinMorrison278563newsML.txt"    
    ## [1196] "KevinMorrison278908newsML.txt"    
    ## [1197] "KevinMorrison292246newsML.txt"    
    ## [1198] "KevinMorrison292666newsML.txt"    
    ## [1199] "KevinMorrison297804newsML.txt"    
    ## [1200] "KevinMorrison304259newsML.txt"    
    ## [1201] "KirstinRidley103790newsML.txt"    
    ## [1202] "KirstinRidley156083newsML.txt"    
    ## [1203] "KirstinRidley157761newsML.txt"    
    ## [1204] "KirstinRidley162181newsML.txt"    
    ## [1205] "KirstinRidley162189newsML.txt"    
    ## [1206] "KirstinRidley162191newsML.txt"    
    ## [1207] "KirstinRidley162193newsML.txt"    
    ## [1208] "KirstinRidley162195newsML.txt"    
    ## [1209] "KirstinRidley162644newsML.txt"    
    ## [1210] "KirstinRidley163112newsML.txt"    
    ## [1211] "KirstinRidley163119newsML.txt"    
    ## [1212] "KirstinRidley164526newsML.txt"    
    ## [1213] "KirstinRidley165832newsML.txt"    
    ## [1214] "KirstinRidley167814newsML.txt"    
    ## [1215] "KirstinRidley181858newsML.txt"    
    ## [1216] "KirstinRidley181907newsML.txt"    
    ## [1217] "KirstinRidley187985newsML.txt"    
    ## [1218] "KirstinRidley188419newsML.txt"    
    ## [1219] "KirstinRidley189882newsML.txt"    
    ## [1220] "KirstinRidley189900newsML.txt"    
    ## [1221] "KirstinRidley199262newsML.txt"    
    ## [1222] "KirstinRidley219536newsML.txt"    
    ## [1223] "KirstinRidley23873newsML.txt"     
    ## [1224] "KirstinRidley239217newsML.txt"    
    ## [1225] "KirstinRidley250064newsML.txt"    
    ## [1226] "KirstinRidley254351newsML.txt"    
    ## [1227] "KirstinRidley267983newsML.txt"    
    ## [1228] "KirstinRidley27422newsML.txt"     
    ## [1229] "KirstinRidley278254newsML.txt"    
    ## [1230] "KirstinRidley294355newsML.txt"    
    ## [1231] "KirstinRidley297203newsML.txt"    
    ## [1232] "KirstinRidley314384newsML.txt"    
    ## [1233] "KirstinRidley314406newsML.txt"    
    ## [1234] "KirstinRidley315603newsML.txt"    
    ## [1235] "KirstinRidley316041newsML.txt"    
    ## [1236] "KirstinRidley322479newsML.txt"    
    ## [1237] "KirstinRidley322955newsML.txt"    
    ## [1238] "KirstinRidley327620newsML.txt"    
    ## [1239] "KirstinRidley329130newsML.txt"    
    ## [1240] "KirstinRidley340175newsML.txt"    
    ## [1241] "KirstinRidley341840newsML.txt"    
    ## [1242] "KirstinRidley341864newsML.txt"    
    ## [1243] "KirstinRidley347828newsML.txt"    
    ## [1244] "KirstinRidley359687newsML.txt"    
    ## [1245] "KirstinRidley36556newsML.txt"     
    ## [1246] "KirstinRidley371805newsML.txt"    
    ## [1247] "KirstinRidley374916newsML.txt"    
    ## [1248] "KirstinRidley393388newsML.txt"    
    ## [1249] "KirstinRidley394816newsML.txt"    
    ## [1250] "KirstinRidley402055newsML.txt"    
    ## [1251] "KouroshKarimkhany101520newsML.txt"
    ## [1252] "KouroshKarimkhany104417newsML.txt"
    ## [1253] "KouroshKarimkhany108366newsML.txt"
    ## [1254] "KouroshKarimkhany113206newsML.txt"
    ## [1255] "KouroshKarimkhany117158newsML.txt"
    ## [1256] "KouroshKarimkhany120392newsML.txt"
    ## [1257] "KouroshKarimkhany121032newsML.txt"
    ## [1258] "KouroshKarimkhany123800newsML.txt"
    ## [1259] "KouroshKarimkhany124768newsML.txt"
    ## [1260] "KouroshKarimkhany128940newsML.txt"
    ## [1261] "KouroshKarimkhany129309newsML.txt"
    ## [1262] "KouroshKarimkhany146032newsML.txt"
    ## [1263] "KouroshKarimkhany150572newsML.txt"
    ## [1264] "KouroshKarimkhany157930newsML.txt"
    ## [1265] "KouroshKarimkhany160129newsML.txt"
    ## [1266] "KouroshKarimkhany160557newsML.txt"
    ## [1267] "KouroshKarimkhany167199newsML.txt"
    ## [1268] "KouroshKarimkhany170654newsML.txt"
    ## [1269] "KouroshKarimkhany172988newsML.txt"
    ## [1270] "KouroshKarimkhany173933newsML.txt"
    ## [1271] "KouroshKarimkhany181666newsML.txt"
    ## [1272] "KouroshKarimkhany183208newsML.txt"
    ## [1273] "KouroshKarimkhany183605newsML.txt"
    ## [1274] "KouroshKarimkhany185419newsML.txt"
    ## [1275] "KouroshKarimkhany189198newsML.txt"
    ## [1276] "KouroshKarimkhany200795newsML.txt"
    ## [1277] "KouroshKarimkhany202658newsML.txt"
    ## [1278] "KouroshKarimkhany203674newsML.txt"
    ## [1279] "KouroshKarimkhany218300newsML.txt"
    ## [1280] "KouroshKarimkhany264379newsML.txt"
    ## [1281] "KouroshKarimkhany271793newsML.txt"
    ## [1282] "KouroshKarimkhany271795newsML.txt"
    ## [1283] "KouroshKarimkhany271894newsML.txt"
    ## [1284] "KouroshKarimkhany272490newsML.txt"
    ## [1285] "KouroshKarimkhany283247newsML.txt"
    ## [1286] "KouroshKarimkhany285535newsML.txt"
    ## [1287] "KouroshKarimkhany287103newsML.txt"
    ## [1288] "KouroshKarimkhany287796newsML.txt"
    ## [1289] "KouroshKarimkhany290979newsML.txt"
    ## [1290] "KouroshKarimkhany298368newsML.txt"
    ## [1291] "KouroshKarimkhany299663newsML.txt"
    ## [1292] "KouroshKarimkhany307652newsML.txt"
    ## [1293] "KouroshKarimkhany309316newsML.txt"
    ## [1294] "KouroshKarimkhany311931newsML.txt"
    ## [1295] "KouroshKarimkhany311972newsML.txt"
    ## [1296] "KouroshKarimkhany313088newsML.txt"
    ## [1297] "KouroshKarimkhany315358newsML.txt"
    ## [1298] "KouroshKarimkhany330773newsML.txt"
    ## [1299] "KouroshKarimkhany331262newsML.txt"
    ## [1300] "KouroshKarimkhany331500newsML.txt"
    ## [1301] "LydiaZajc10274newsML.txt"         
    ## [1302] "LydiaZajc105793newsML.txt"        
    ## [1303] "LydiaZajc130081newsML.txt"        
    ## [1304] "LydiaZajc13810newsML.txt"         
    ## [1305] "LydiaZajc139760newsML.txt"        
    ## [1306] "LydiaZajc15592newsML.txt"         
    ## [1307] "LydiaZajc162222newsML.txt"        
    ## [1308] "LydiaZajc163197newsML.txt"        
    ## [1309] "LydiaZajc164298newsML.txt"        
    ## [1310] "LydiaZajc165902newsML.txt"        
    ## [1311] "LydiaZajc172241newsML.txt"        
    ## [1312] "LydiaZajc174373newsML.txt"        
    ## [1313] "LydiaZajc185720newsML.txt"        
    ## [1314] "LydiaZajc188535newsML.txt"        
    ## [1315] "LydiaZajc202140newsML.txt"        
    ## [1316] "LydiaZajc207278newsML.txt"        
    ## [1317] "LydiaZajc212440newsML.txt"        
    ## [1318] "LydiaZajc213719newsML.txt"        
    ## [1319] "LydiaZajc216822newsML.txt"        
    ## [1320] "LydiaZajc219628newsML.txt"        
    ## [1321] "LydiaZajc224725newsML.txt"        
    ## [1322] "LydiaZajc227933newsML.txt"        
    ## [1323] "LydiaZajc230638newsML.txt"        
    ## [1324] "LydiaZajc233590newsML.txt"        
    ## [1325] "LydiaZajc236474newsML.txt"        
    ## [1326] "LydiaZajc2387newsML.txt"          
    ## [1327] "LydiaZajc239275newsML.txt"        
    ## [1328] "LydiaZajc239280newsML.txt"        
    ## [1329] "LydiaZajc254426newsML.txt"        
    ## [1330] "LydiaZajc257337newsML.txt"        
    ## [1331] "LydiaZajc302437newsML.txt"        
    ## [1332] "LydiaZajc305250newsML.txt"        
    ## [1333] "LydiaZajc308198newsML.txt"        
    ## [1334] "LydiaZajc311319newsML.txt"        
    ## [1335] "LydiaZajc316724newsML.txt"        
    ## [1336] "LydiaZajc317743newsML.txt"        
    ## [1337] "LydiaZajc319107newsML.txt"        
    ## [1338] "LydiaZajc319113newsML.txt"        
    ## [1339] "LydiaZajc326409newsML.txt"        
    ## [1340] "LydiaZajc329147newsML.txt"        
    ## [1341] "LydiaZajc332252newsML.txt"        
    ## [1342] "LydiaZajc334277newsML.txt"        
    ## [1343] "LydiaZajc339037newsML.txt"        
    ## [1344] "LydiaZajc345128newsML.txt"        
    ## [1345] "LydiaZajc346330newsML.txt"        
    ## [1346] "LydiaZajc353567newsML.txt"        
    ## [1347] "LydiaZajc356565newsML.txt"        
    ## [1348] "LydiaZajc362785newsML.txt"        
    ## [1349] "LydiaZajc364632newsML.txt"        
    ## [1350] "LydiaZajc366471newsML.txt"        
    ## [1351] "LynneO'Donnell102480newsML.txt"   
    ## [1352] "LynneO'Donnell10327newsML.txt"    
    ## [1353] "LynneO'Donnell106719newsML.txt"   
    ## [1354] "LynneO'Donnell107032newsML.txt"   
    ## [1355] "LynneO'Donnell110718newsML.txt"   
    ## [1356] "LynneO'Donnell111855newsML.txt"   
    ## [1357] "LynneO'Donnell112106newsML.txt"   
    ## [1358] "LynneO'Donnell114299newsML.txt"   
    ## [1359] "LynneO'Donnell115594newsML.txt"   
    ## [1360] "LynneO'Donnell116963newsML.txt"   
    ## [1361] "LynneO'Donnell120232newsML.txt"   
    ## [1362] "LynneO'Donnell121886newsML.txt"   
    ## [1363] "LynneO'Donnell121931newsML.txt"   
    ## [1364] "LynneO'Donnell12381newsML.txt"    
    ## [1365] "LynneO'Donnell125205newsML.txt"   
    ## [1366] "LynneO'Donnell125560newsML.txt"   
    ## [1367] "LynneO'Donnell133490newsML.txt"   
    ## [1368] "LynneO'Donnell135223newsML.txt"   
    ## [1369] "LynneO'Donnell146245newsML.txt"   
    ## [1370] "LynneO'Donnell146472newsML.txt"   
    ## [1371] "LynneO'Donnell148890newsML.txt"   
    ## [1372] "LynneO'Donnell151846newsML.txt"   
    ## [1373] "LynneO'Donnell15736newsML.txt"    
    ## [1374] "LynneO'Donnell158405newsML.txt"   
    ## [1375] "LynneO'Donnell158416newsML.txt"   
    ## [1376] "LynneO'Donnell159976newsML.txt"   
    ## [1377] "LynneO'Donnell166518newsML.txt"   
    ## [1378] "LynneO'Donnell169600newsML.txt"   
    ## [1379] "LynneO'Donnell171253newsML.txt"   
    ## [1380] "LynneO'Donnell171633newsML.txt"   
    ## [1381] "LynneO'Donnell17858newsML.txt"    
    ## [1382] "LynneO'Donnell180191newsML.txt"   
    ## [1383] "LynneO'Donnell187362newsML.txt"   
    ## [1384] "LynneO'Donnell198178newsML.txt"   
    ## [1385] "LynneO'Donnell204874newsML.txt"   
    ## [1386] "LynneO'Donnell20855newsML.txt"    
    ## [1387] "LynneO'Donnell229894newsML.txt"   
    ## [1388] "LynneO'Donnell23184newsML.txt"    
    ## [1389] "LynneO'Donnell238772newsML.txt"   
    ## [1390] "LynneO'Donnell2439newsML.txt"     
    ## [1391] "LynneO'Donnell244800newsML.txt"   
    ## [1392] "LynneO'Donnell2462newsML.txt"     
    ## [1393] "LynneO'Donnell247724newsML.txt"   
    ## [1394] "LynneO'Donnell248156newsML.txt"   
    ## [1395] "LynneO'Donnell250844newsML.txt"   
    ## [1396] "LynneO'Donnell258739newsML.txt"   
    ## [1397] "LynneO'Donnell265681newsML.txt"   
    ## [1398] "LynneO'Donnell27316newsML.txt"    
    ## [1399] "LynneO'Donnell275670newsML.txt"   
    ## [1400] "LynneO'Donnell29593newsML.txt"    
    ## [1401] "LynnleyBrowning102195newsML.txt"  
    ## [1402] "LynnleyBrowning103703newsML.txt"  
    ## [1403] "LynnleyBrowning105018newsML.txt"  
    ## [1404] "LynnleyBrowning107881newsML.txt"  
    ## [1405] "LynnleyBrowning114005newsML.txt"  
    ## [1406] "LynnleyBrowning116673newsML.txt"  
    ## [1407] "LynnleyBrowning123245newsML.txt"  
    ## [1408] "LynnleyBrowning130255newsML.txt"  
    ## [1409] "LynnleyBrowning147283newsML.txt"  
    ## [1410] "LynnleyBrowning153131newsML.txt"  
    ## [1411] "LynnleyBrowning156303newsML.txt"  
    ## [1412] "LynnleyBrowning159726newsML.txt"  
    ## [1413] "LynnleyBrowning163468newsML.txt"  
    ## [1414] "LynnleyBrowning166151newsML.txt"  
    ## [1415] "LynnleyBrowning166214newsML.txt"  
    ## [1416] "LynnleyBrowning169353newsML.txt"  
    ## [1417] "LynnleyBrowning182130newsML.txt"  
    ## [1418] "LynnleyBrowning185549newsML.txt"  
    ## [1419] "LynnleyBrowning199480newsML.txt"  
    ## [1420] "LynnleyBrowning202315newsML.txt"  
    ## [1421] "LynnleyBrowning206315newsML.txt"  
    ## [1422] "LynnleyBrowning209593newsML.txt"  
    ## [1423] "LynnleyBrowning219830newsML.txt"  
    ## [1424] "LynnleyBrowning224891newsML.txt"  
    ## [1425] "LynnleyBrowning230857newsML.txt"  
    ## [1426] "LynnleyBrowning236661newsML.txt"  
    ## [1427] "LynnleyBrowning239652newsML.txt"  
    ## [1428] "LynnleyBrowning239689newsML.txt"  
    ## [1429] "LynnleyBrowning241692newsML.txt"  
    ## [1430] "LynnleyBrowning245918newsML.txt"  
    ## [1431] "LynnleyBrowning248885newsML.txt"  
    ## [1432] "LynnleyBrowning260916newsML.txt"  
    ## [1433] "LynnleyBrowning262566newsML.txt"  
    ## [1434] "LynnleyBrowning263722newsML.txt"  
    ## [1435] "LynnleyBrowning263781newsML.txt"  
    ## [1436] "LynnleyBrowning266599newsML.txt"  
    ## [1437] "LynnleyBrowning266625newsML.txt"  
    ## [1438] "LynnleyBrowning269345newsML.txt"  
    ## [1439] "LynnleyBrowning269386newsML.txt"  
    ## [1440] "LynnleyBrowning27013newsML.txt"   
    ## [1441] "LynnleyBrowning293211newsML.txt"  
    ## [1442] "LynnleyBrowning294148newsML.txt"  
    ## [1443] "LynnleyBrowning295731newsML.txt"  
    ## [1444] "LynnleyBrowning298904newsML.txt"  
    ## [1445] "LynnleyBrowning305426newsML.txt"  
    ## [1446] "LynnleyBrowning308394newsML.txt"  
    ## [1447] "LynnleyBrowning314644newsML.txt"  
    ## [1448] "LynnleyBrowning318404newsML.txt"  
    ## [1449] "LynnleyBrowning318451newsML.txt"  
    ## [1450] "LynnleyBrowning324280newsML.txt"  
    ## [1451] "MarcelMichelson108487newsML.txt"  
    ## [1452] "MarcelMichelson115160newsML.txt"  
    ## [1453] "MarcelMichelson115199newsML.txt"  
    ## [1454] "MarcelMichelson121654newsML.txt"  
    ## [1455] "MarcelMichelson121701newsML.txt"  
    ## [1456] "MarcelMichelson128737newsML.txt"  
    ## [1457] "MarcelMichelson129351newsML.txt"  
    ## [1458] "MarcelMichelson129426newsML.txt"  
    ## [1459] "MarcelMichelson134055newsML.txt"  
    ## [1460] "MarcelMichelson134919newsML.txt"  
    ## [1461] "MarcelMichelson134931newsML.txt"  
    ## [1462] "MarcelMichelson137247newsML.txt"  
    ## [1463] "MarcelMichelson137362newsML.txt"  
    ## [1464] "MarcelMichelson137480newsML.txt"  
    ## [1465] "MarcelMichelson140842newsML.txt"  
    ## [1466] "MarcelMichelson140849newsML.txt"  
    ## [1467] "MarcelMichelson141667newsML.txt"  
    ## [1468] "MarcelMichelson141744newsML.txt"  
    ## [1469] "MarcelMichelson141746newsML.txt"  
    ## [1470] "MarcelMichelson142386newsML.txt"  
    ## [1471] "MarcelMichelson143829newsML.txt"  
    ## [1472] "MarcelMichelson144618newsML.txt"  
    ## [1473] "MarcelMichelson144673newsML.txt"  
    ## [1474] "MarcelMichelson147835newsML.txt"  
    ## [1475] "MarcelMichelson148534newsML.txt"  
    ## [1476] "MarcelMichelson148549newsML.txt"  
    ## [1477] "MarcelMichelson150780newsML.txt"  
    ## [1478] "MarcelMichelson158085newsML.txt"  
    ## [1479] "MarcelMichelson16949newsML.txt"   
    ## [1480] "MarcelMichelson171152newsML.txt"  
    ## [1481] "MarcelMichelson171548newsML.txt"  
    ## [1482] "MarcelMichelson174150newsML.txt"  
    ## [1483] "MarcelMichelson184864newsML.txt"  
    ## [1484] "MarcelMichelson18555newsML.txt"   
    ## [1485] "MarcelMichelson187829newsML.txt"  
    ## [1486] "MarcelMichelson197891newsML.txt"  
    ## [1487] "MarcelMichelson200130newsML.txt"  
    ## [1488] "MarcelMichelson200818newsML.txt"  
    ## [1489] "MarcelMichelson200991newsML.txt"  
    ## [1490] "MarcelMichelson203406newsML.txt"  
    ## [1491] "MarcelMichelson20886newsML.txt"   
    ## [1492] "MarcelMichelson210745newsML.txt"  
    ## [1493] "MarcelMichelson217645newsML.txt"  
    ## [1494] "MarcelMichelson217650newsML.txt"  
    ## [1495] "MarcelMichelson218344newsML.txt"  
    ## [1496] "MarcelMichelson221119newsML.txt"  
    ## [1497] "MarcelMichelson221246newsML.txt"  
    ## [1498] "MarcelMichelson222937newsML.txt"  
    ## [1499] "MarcelMichelson22884newsML.txt"   
    ## [1500] "MarcelMichelson229499newsML.txt"  
    ## [1501] "MarkBendeich101093newsML.txt"     
    ## [1502] "MarkBendeich101098newsML.txt"     
    ## [1503] "MarkBendeich103818newsML.txt"     
    ## [1504] "MarkBendeich103826newsML.txt"     
    ## [1505] "MarkBendeich114722newsML.txt"     
    ## [1506] "MarkBendeich115420newsML.txt"     
    ## [1507] "MarkBendeich115431newsML.txt"     
    ## [1508] "MarkBendeich118468newsML.txt"     
    ## [1509] "MarkBendeich121864newsML.txt"     
    ## [1510] "MarkBendeich125094newsML.txt"     
    ## [1511] "MarkBendeich125147newsML.txt"     
    ## [1512] "MarkBendeich135098newsML.txt"     
    ## [1513] "MarkBendeich151787newsML.txt"     
    ## [1514] "MarkBendeich155007newsML.txt"     
    ## [1515] "MarkBendeich164991newsML.txt"     
    ## [1516] "MarkBendeich192328newsML.txt"     
    ## [1517] "MarkBendeich21528newsML.txt"      
    ## [1518] "MarkBendeich258563newsML.txt"     
    ## [1519] "MarkBendeich269583newsML.txt"     
    ## [1520] "MarkBendeich271058newsML.txt"     
    ## [1521] "MarkBendeich271077newsML.txt"     
    ## [1522] "MarkBendeich271088newsML.txt"     
    ## [1523] "MarkBendeich27264newsML.txt"      
    ## [1524] "MarkBendeich286524newsML.txt"     
    ## [1525] "MarkBendeich286542newsML.txt"     
    ## [1526] "MarkBendeich289652newsML.txt"     
    ## [1527] "MarkBendeich294948newsML.txt"     
    ## [1528] "MarkBendeich294969newsML.txt"     
    ## [1529] "MarkBendeich30673newsML.txt"      
    ## [1530] "MarkBendeich30686newsML.txt"      
    ## [1531] "MarkBendeich33304newsML.txt"      
    ## [1532] "MarkBendeich33308newsML.txt"      
    ## [1533] "MarkBendeich358695newsML.txt"     
    ## [1534] "MarkBendeich358701newsML.txt"     
    ## [1535] "MarkBendeich364654newsML.txt"     
    ## [1536] "MarkBendeich368102newsML.txt"     
    ## [1537] "MarkBendeich368145newsML.txt"     
    ## [1538] "MarkBendeich370908newsML.txt"     
    ## [1539] "MarkBendeich373924newsML.txt"     
    ## [1540] "MarkBendeich379952newsML.txt"     
    ## [1541] "MarkBendeich383098newsML.txt"     
    ## [1542] "MarkBendeich383103newsML.txt"     
    ## [1543] "MarkBendeich391048newsML.txt"     
    ## [1544] "MarkBendeich395441newsML.txt"     
    ## [1545] "MarkBendeich398974newsML.txt"     
    ## [1546] "MarkBendeich399002newsML.txt"     
    ## [1547] "MarkBendeich399895newsML.txt"     
    ## [1548] "MarkBendeich402409newsML.txt"     
    ## [1549] "MarkBendeich402412newsML.txt"     
    ## [1550] "MarkBendeich402444newsML.txt"     
    ## [1551] "MartinWolk100375newsML.txt"       
    ## [1552] "MartinWolk101547newsML.txt"       
    ## [1553] "MartinWolk108184newsML.txt"       
    ## [1554] "MartinWolk108730newsML.txt"       
    ## [1555] "MartinWolk11750newsML.txt"        
    ## [1556] "MartinWolk127151newsML.txt"       
    ## [1557] "MartinWolk127318newsML.txt"       
    ## [1558] "MartinWolk130607newsML.txt"       
    ## [1559] "MartinWolk130622newsML.txt"       
    ## [1560] "MartinWolk130730newsML.txt"       
    ## [1561] "MartinWolk132308newsML.txt"       
    ## [1562] "MartinWolk132566newsML.txt"       
    ## [1563] "MartinWolk13568newsML.txt"        
    ## [1564] "MartinWolk140462newsML.txt"       
    ## [1565] "MartinWolk143574newsML.txt"       
    ## [1566] "MartinWolk15583newsML.txt"        
    ## [1567] "MartinWolk157448newsML.txt"       
    ## [1568] "MartinWolk158943newsML.txt"       
    ## [1569] "MartinWolk162155newsML.txt"       
    ## [1570] "MartinWolk16678newsML.txt"        
    ## [1571] "MartinWolk174033newsML.txt"       
    ## [1572] "MartinWolk182849newsML.txt"       
    ## [1573] "MartinWolk183836newsML.txt"       
    ## [1574] "MartinWolk202582newsML.txt"       
    ## [1575] "MartinWolk203663newsML.txt"       
    ## [1576] "MartinWolk20801newsML.txt"        
    ## [1577] "MartinWolk22286newsML.txt"        
    ## [1578] "MartinWolk232346newsML.txt"       
    ## [1579] "MartinWolk2538newsML.txt"         
    ## [1580] "MartinWolk257342newsML.txt"       
    ## [1581] "MartinWolk257622newsML.txt"       
    ## [1582] "MartinWolk257926newsML.txt"       
    ## [1583] "MartinWolk258793newsML.txt"       
    ## [1584] "MartinWolk259308newsML.txt"       
    ## [1585] "MartinWolk259796newsML.txt"       
    ## [1586] "MartinWolk274876newsML.txt"       
    ## [1587] "MartinWolk275458newsML.txt"       
    ## [1588] "MartinWolk276214newsML.txt"       
    ## [1589] "MartinWolk285485newsML.txt"       
    ## [1590] "MartinWolk290078newsML.txt"       
    ## [1591] "MartinWolk295347newsML.txt"       
    ## [1592] "MartinWolk302120newsML.txt"       
    ## [1593] "MartinWolk309552newsML.txt"       
    ## [1594] "MartinWolk311937newsML.txt"       
    ## [1595] "MartinWolk312192newsML.txt"       
    ## [1596] "MartinWolk315688newsML.txt"       
    ## [1597] "MartinWolk316912newsML.txt"       
    ## [1598] "MartinWolk317025newsML.txt"       
    ## [1599] "MartinWolk318853newsML.txt"       
    ## [1600] "MartinWolk319110newsML.txt"       
    ## [1601] "MatthewBunce102033newsML.txt"     
    ## [1602] "MatthewBunce107670newsML.txt"     
    ## [1603] "MatthewBunce130106newsML.txt"     
    ## [1604] "MatthewBunce139789newsML.txt"     
    ## [1605] "MatthewBunce153002newsML.txt"     
    ## [1606] "MatthewBunce156243newsML.txt"     
    ## [1607] "MatthewBunce165943newsML.txt"     
    ## [1608] "MatthewBunce165976newsML.txt"     
    ## [1609] "MatthewBunce169180newsML.txt"     
    ## [1610] "MatthewBunce169229newsML.txt"     
    ## [1611] "MatthewBunce175759newsML.txt"     
    ## [1612] "MatthewBunce224752newsML.txt"     
    ## [1613] "MatthewBunce224769newsML.txt"     
    ## [1614] "MatthewBunce224786newsML.txt"     
    ## [1615] "MatthewBunce233655newsML.txt"     
    ## [1616] "MatthewBunce236509newsML.txt"     
    ## [1617] "MatthewBunce248714newsML.txt"     
    ## [1618] "MatthewBunce251685newsML.txt"     
    ## [1619] "MatthewBunce258294newsML.txt"     
    ## [1620] "MatthewBunce263657newsML.txt"     
    ## [1621] "MatthewBunce266534newsML.txt"     
    ## [1622] "MatthewBunce280201newsML.txt"     
    ## [1623] "MatthewBunce280207newsML.txt"     
    ## [1624] "MatthewBunce283605newsML.txt"     
    ## [1625] "MatthewBunce286922newsML.txt"     
    ## [1626] "MatthewBunce287384newsML.txt"     
    ## [1627] "MatthewBunce288284newsML.txt"     
    ## [1628] "MatthewBunce293127newsML.txt"     
    ## [1629] "MatthewBunce298758newsML.txt"     
    ## [1630] "MatthewBunce301536newsML.txt"     
    ## [1631] "MatthewBunce302468newsML.txt"     
    ## [1632] "MatthewBunce302478newsML.txt"     
    ## [1633] "MatthewBunce305276newsML.txt"     
    ## [1634] "MatthewBunce308257newsML.txt"     
    ## [1635] "MatthewBunce311372newsML.txt"     
    ## [1636] "MatthewBunce318283newsML.txt"     
    ## [1637] "MatthewBunce327686newsML.txt"     
    ## [1638] "MatthewBunce333417newsML.txt"     
    ## [1639] "MatthewBunce334354newsML.txt"     
    ## [1640] "MatthewBunce343391newsML.txt"     
    ## [1641] "MatthewBunce343415newsML.txt"     
    ## [1642] "MatthewBunce343437newsML.txt"     
    ## [1643] "MatthewBunce346379newsML.txt"     
    ## [1644] "MatthewBunce350671newsML.txt"     
    ## [1645] "MatthewBunce356601newsML.txt"     
    ## [1646] "MatthewBunce359837newsML.txt"     
    ## [1647] "MatthewBunce359857newsML.txt"     
    ## [1648] "MatthewBunce362649newsML.txt"     
    ## [1649] "MatthewBunce362811newsML.txt"     
    ## [1650] "MatthewBunce362814newsML.txt"     
    ## [1651] "MichaelConnor102530newsML.txt"    
    ## [1652] "MichaelConnor105491newsML.txt"    
    ## [1653] "MichaelConnor119156newsML.txt"    
    ## [1654] "MichaelConnor120404newsML.txt"    
    ## [1655] "MichaelConnor122415newsML.txt"    
    ## [1656] "MichaelConnor124763newsML.txt"    
    ## [1657] "MichaelConnor13406newsML.txt"     
    ## [1658] "MichaelConnor13865newsML.txt"     
    ## [1659] "MichaelConnor13958newsML.txt"     
    ## [1660] "MichaelConnor152528newsML.txt"    
    ## [1661] "MichaelConnor183243newsML.txt"    
    ## [1662] "MichaelConnor184787newsML.txt"    
    ## [1663] "MichaelConnor186387newsML.txt"    
    ## [1664] "MichaelConnor186990newsML.txt"    
    ## [1665] "MichaelConnor22201newsML.txt"     
    ## [1666] "MichaelConnor233199newsML.txt"    
    ## [1667] "MichaelConnor234177newsML.txt"    
    ## [1668] "MichaelConnor237057newsML.txt"    
    ## [1669] "MichaelConnor239435newsML.txt"    
    ## [1670] "MichaelConnor239967newsML.txt"    
    ## [1671] "MichaelConnor245471newsML.txt"    
    ## [1672] "MichaelConnor257827newsML.txt"    
    ## [1673] "MichaelConnor261401newsML.txt"    
    ## [1674] "MichaelConnor269791newsML.txt"    
    ## [1675] "MichaelConnor269817newsML.txt"    
    ## [1676] "MichaelConnor269990newsML.txt"    
    ## [1677] "MichaelConnor270352newsML.txt"    
    ## [1678] "MichaelConnor285409newsML.txt"    
    ## [1679] "MichaelConnor287614newsML.txt"    
    ## [1680] "MichaelConnor288623newsML.txt"    
    ## [1681] "MichaelConnor288745newsML.txt"    
    ## [1682] "MichaelConnor298475newsML.txt"    
    ## [1683] "MichaelConnor299336newsML.txt"    
    ## [1684] "MichaelConnor305809newsML.txt"    
    ## [1685] "MichaelConnor307864newsML.txt"    
    ## [1686] "MichaelConnor309322newsML.txt"    
    ## [1687] "MichaelConnor320765newsML.txt"    
    ## [1688] "MichaelConnor323784newsML.txt"    
    ## [1689] "MichaelConnor324861newsML.txt"    
    ## [1690] "MichaelConnor328867newsML.txt"    
    ## [1691] "MichaelConnor353261newsML.txt"    
    ## [1692] "MichaelConnor357313newsML.txt"    
    ## [1693] "MichaelConnor360346newsML.txt"    
    ## [1694] "MichaelConnor363969newsML.txt"    
    ## [1695] "MichaelConnor372558newsML.txt"    
    ## [1696] "MichaelConnor375668newsML.txt"    
    ## [1697] "MichaelConnor379600newsML.txt"    
    ## [1698] "MichaelConnor40606newsML.txt"     
    ## [1699] "MichaelConnor406455newsML.txt"    
    ## [1700] "MichaelConnor418479newsML.txt"    
    ## [1701] "MureDickie102467newsML.txt"       
    ## [1702] "MureDickie105255newsML.txt"       
    ## [1703] "MureDickie108143newsML.txt"       
    ## [1704] "MureDickie110742newsML.txt"       
    ## [1705] "MureDickie11265newsML.txt"        
    ## [1706] "MureDickie11273newsML.txt"        
    ## [1707] "MureDickie117025newsML.txt"       
    ## [1708] "MureDickie118737newsML.txt"       
    ## [1709] "MureDickie120273newsML.txt"       
    ## [1710] "MureDickie125212newsML.txt"       
    ## [1711] "MureDickie126632newsML.txt"       
    ## [1712] "MureDickie129103newsML.txt"       
    ## [1713] "MureDickie130515newsML.txt"       
    ## [1714] "MureDickie133488newsML.txt"       
    ## [1715] "MureDickie137508newsML.txt"       
    ## [1716] "MureDickie138551newsML.txt"       
    ## [1717] "MureDickie140299newsML.txt"       
    ## [1718] "MureDickie142229newsML.txt"       
    ## [1719] "MureDickie144988newsML.txt"       
    ## [1720] "MureDickie153427newsML.txt"       
    ## [1721] "MureDickie155055newsML.txt"       
    ## [1722] "MureDickie156653newsML.txt"       
    ## [1723] "MureDickie15700newsML.txt"        
    ## [1724] "MureDickie158417newsML.txt"       
    ## [1725] "MureDickie159932newsML.txt"       
    ## [1726] "MureDickie159960newsML.txt"       
    ## [1727] "MureDickie161084newsML.txt"       
    ## [1728] "MureDickie161833newsML.txt"       
    ## [1729] "MureDickie163678newsML.txt"       
    ## [1730] "MureDickie166433newsML.txt"       
    ## [1731] "MureDickie167928newsML.txt"       
    ## [1732] "MureDickie169588newsML.txt"       
    ## [1733] "MureDickie169608newsML.txt"       
    ## [1734] "MureDickie174493newsML.txt"       
    ## [1735] "MureDickie176131newsML.txt"       
    ## [1736] "MureDickie182489newsML.txt"       
    ## [1737] "MureDickie186174newsML.txt"       
    ## [1738] "MureDickie187394newsML.txt"       
    ## [1739] "MureDickie188953newsML.txt"       
    ## [1740] "MureDickie192419newsML.txt"       
    ## [1741] "MureDickie192434newsML.txt"       
    ## [1742] "MureDickie196752newsML.txt"       
    ## [1743] "MureDickie199837newsML.txt"       
    ## [1744] "MureDickie204883newsML.txt"       
    ## [1745] "MureDickie209808newsML.txt"       
    ## [1746] "MureDickie209826newsML.txt"       
    ## [1747] "MureDickie211093newsML.txt"       
    ## [1748] "MureDickie214186newsML.txt"       
    ## [1749] "MureDickie222803newsML.txt"       
    ## [1750] "MureDickie225075newsML.txt"       
    ## [1751] "NickLouth105567newsML.txt"        
    ## [1752] "NickLouth10799newsML.txt"         
    ## [1753] "NickLouth108449newsML.txt"        
    ## [1754] "NickLouth110904newsML.txt"        
    ## [1755] "NickLouth112673newsML.txt"        
    ## [1756] "NickLouth116073newsML.txt"        
    ## [1757] "NickLouth116176newsML.txt"        
    ## [1758] "NickLouth117081newsML.txt"        
    ## [1759] "NickLouth119266newsML.txt"        
    ## [1760] "NickLouth120416newsML.txt"        
    ## [1761] "NickLouth120591newsML.txt"        
    ## [1762] "NickLouth121030newsML.txt"        
    ## [1763] "NickLouth123754newsML.txt"        
    ## [1764] "NickLouth123878newsML.txt"        
    ## [1765] "NickLouth130621newsML.txt"        
    ## [1766] "NickLouth133736newsML.txt"        
    ## [1767] "NickLouth133837newsML.txt"        
    ## [1768] "NickLouth13391newsML.txt"         
    ## [1769] "NickLouth135866newsML.txt"        
    ## [1770] "NickLouth136039newsML.txt"        
    ## [1771] "NickLouth136074newsML.txt"        
    ## [1772] "NickLouth136895newsML.txt"        
    ## [1773] "NickLouth137483newsML.txt"        
    ## [1774] "NickLouth137486newsML.txt"        
    ## [1775] "NickLouth13825newsML.txt"         
    ## [1776] "NickLouth13930newsML.txt"         
    ## [1777] "NickLouth13951newsML.txt"         
    ## [1778] "NickLouth140553newsML.txt"        
    ## [1779] "NickLouth154642newsML.txt"        
    ## [1780] "NickLouth155834newsML.txt"        
    ## [1781] "NickLouth159095newsML.txt"        
    ## [1782] "NickLouth159997newsML.txt"        
    ## [1783] "NickLouth160344newsML.txt"        
    ## [1784] "NickLouth160349newsML.txt"        
    ## [1785] "NickLouth16038newsML.txt"         
    ## [1786] "NickLouth161927newsML.txt"        
    ## [1787] "NickLouth162020newsML.txt"        
    ## [1788] "NickLouth162173newsML.txt"        
    ## [1789] "NickLouth162177newsML.txt"        
    ## [1790] "NickLouth162521newsML.txt"        
    ## [1791] "NickLouth162540newsML.txt"        
    ## [1792] "NickLouth162682newsML.txt"        
    ## [1793] "NickLouth162894newsML.txt"        
    ## [1794] "NickLouth163897newsML.txt"        
    ## [1795] "NickLouth164455newsML.txt"        
    ## [1796] "NickLouth164659newsML.txt"        
    ## [1797] "NickLouth165338newsML.txt"        
    ## [1798] "NickLouth180293newsML.txt"        
    ## [1799] "NickLouth182667newsML.txt"        
    ## [1800] "NickLouth18798newsML.txt"         
    ## [1801] "PatriciaCommins102551newsML.txt"  
    ## [1802] "PatriciaCommins102719newsML.txt"  
    ## [1803] "PatriciaCommins102793newsML.txt"  
    ## [1804] "PatriciaCommins108222newsML.txt"  
    ## [1805] "PatriciaCommins108443newsML.txt"  
    ## [1806] "PatriciaCommins114356newsML.txt"  
    ## [1807] "PatriciaCommins114428newsML.txt"  
    ## [1808] "PatriciaCommins114859newsML.txt"  
    ## [1809] "PatriciaCommins117099newsML.txt"  
    ## [1810] "PatriciaCommins120569newsML.txt"  
    ## [1811] "PatriciaCommins120588newsML.txt"  
    ## [1812] "PatriciaCommins123595newsML.txt"  
    ## [1813] "PatriciaCommins123628newsML.txt"  
    ## [1814] "PatriciaCommins123987newsML.txt"  
    ## [1815] "PatriciaCommins125965newsML.txt"  
    ## [1816] "PatriciaCommins126714newsML.txt"  
    ## [1817] "PatriciaCommins132671newsML.txt"  
    ## [1818] "PatriciaCommins140446newsML.txt"  
    ## [1819] "PatriciaCommins142683newsML.txt"  
    ## [1820] "PatriciaCommins142736newsML.txt"  
    ## [1821] "PatriciaCommins143591newsML.txt"  
    ## [1822] "PatriciaCommins143765newsML.txt"  
    ## [1823] "PatriciaCommins153684newsML.txt"  
    ## [1824] "PatriciaCommins156759newsML.txt"  
    ## [1825] "PatriciaCommins15778newsML.txt"   
    ## [1826] "PatriciaCommins178068newsML.txt"  
    ## [1827] "PatriciaCommins178089newsML.txt"  
    ## [1828] "PatriciaCommins18217newsML.txt"   
    ## [1829] "PatriciaCommins18739newsML.txt"   
    ## [1830] "PatriciaCommins203296newsML.txt"  
    ## [1831] "PatriciaCommins205566newsML.txt"  
    ## [1832] "PatriciaCommins209925newsML.txt"  
    ## [1833] "PatriciaCommins209945newsML.txt"  
    ## [1834] "PatriciaCommins213454newsML.txt"  
    ## [1835] "PatriciaCommins217368newsML.txt"  
    ## [1836] "PatriciaCommins2296newsML.txt"    
    ## [1837] "PatriciaCommins231178newsML.txt"  
    ## [1838] "PatriciaCommins237159newsML.txt"  
    ## [1839] "PatriciaCommins251195newsML.txt"  
    ## [1840] "PatriciaCommins251347newsML.txt"  
    ## [1841] "PatriciaCommins255134newsML.txt"  
    ## [1842] "PatriciaCommins255733newsML.txt"  
    ## [1843] "PatriciaCommins258801newsML.txt"  
    ## [1844] "PatriciaCommins259000newsML.txt"  
    ## [1845] "PatriciaCommins264335newsML.txt"  
    ## [1846] "PatriciaCommins267178newsML.txt"  
    ## [1847] "PatriciaCommins270137newsML.txt"  
    ## [1848] "PatriciaCommins283991newsML.txt"  
    ## [1849] "PatriciaCommins288774newsML.txt"  
    ## [1850] "PatriciaCommins291177newsML.txt"  
    ## [1851] "PeterHumphrey108105newsML.txt"    
    ## [1852] "PeterHumphrey115524newsML.txt"    
    ## [1853] "PeterHumphrey120288newsML.txt"    
    ## [1854] "PeterHumphrey129185newsML.txt"    
    ## [1855] "PeterHumphrey151867newsML.txt"    
    ## [1856] "PeterHumphrey158626newsML.txt"    
    ## [1857] "PeterHumphrey161373newsML.txt"    
    ## [1858] "PeterHumphrey166468newsML.txt"    
    ## [1859] "PeterHumphrey174559newsML.txt"    
    ## [1860] "PeterHumphrey184622newsML.txt"    
    ## [1861] "PeterHumphrey190619newsML.txt"    
    ## [1862] "PeterHumphrey192387newsML.txt"    
    ## [1863] "PeterHumphrey194769newsML.txt"    
    ## [1864] "PeterHumphrey195348newsML.txt"    
    ## [1865] "PeterHumphrey202909newsML.txt"    
    ## [1866] "PeterHumphrey205146newsML.txt"    
    ## [1867] "PeterHumphrey208161newsML.txt"    
    ## [1868] "PeterHumphrey215906newsML.txt"    
    ## [1869] "PeterHumphrey217232newsML.txt"    
    ## [1870] "PeterHumphrey221800newsML.txt"    
    ## [1871] "PeterHumphrey222778newsML.txt"    
    ## [1872] "PeterHumphrey224140newsML.txt"    
    ## [1873] "PeterHumphrey226225newsML.txt"    
    ## [1874] "PeterHumphrey227262newsML.txt"    
    ## [1875] "PeterHumphrey232735newsML.txt"    
    ## [1876] "PeterHumphrey232850newsML.txt"    
    ## [1877] "PeterHumphrey235615newsML.txt"    
    ## [1878] "PeterHumphrey235690newsML.txt"    
    ## [1879] "PeterHumphrey238764newsML.txt"    
    ## [1880] "PeterHumphrey242322newsML.txt"    
    ## [1881] "PeterHumphrey243415newsML.txt"    
    ## [1882] "PeterHumphrey244883newsML.txt"    
    ## [1883] "PeterHumphrey246251newsML.txt"    
    ## [1884] "PeterHumphrey247781newsML.txt"    
    ## [1885] "PeterHumphrey247855newsML.txt"    
    ## [1886] "PeterHumphrey250733newsML.txt"    
    ## [1887] "PeterHumphrey250825newsML.txt"    
    ## [1888] "PeterHumphrey252085newsML.txt"    
    ## [1889] "PeterHumphrey25384newsML.txt"     
    ## [1890] "PeterHumphrey256526newsML.txt"    
    ## [1891] "PeterHumphrey262875newsML.txt"    
    ## [1892] "PeterHumphrey262888newsML.txt"    
    ## [1893] "PeterHumphrey26410newsML.txt"     
    ## [1894] "PeterHumphrey269850newsML.txt"    
    ## [1895] "PeterHumphrey269864newsML.txt"    
    ## [1896] "PeterHumphrey271287newsML.txt"    
    ## [1897] "PeterHumphrey271684newsML.txt"    
    ## [1898] "PeterHumphrey276107newsML.txt"    
    ## [1899] "PeterHumphrey278596newsML.txt"    
    ## [1900] "PeterHumphrey302864newsML.txt"    
    ## [1901] "PierreTran100796newsML.txt"       
    ## [1902] "PierreTran103156newsML.txt"       
    ## [1903] "PierreTran103612newsML.txt"       
    ## [1904] "PierreTran111120newsML.txt"       
    ## [1905] "PierreTran114507newsML.txt"       
    ## [1906] "PierreTran114726newsML.txt"       
    ## [1907] "PierreTran117510newsML.txt"       
    ## [1908] "PierreTran120892newsML.txt"       
    ## [1909] "PierreTran120914newsML.txt"       
    ## [1910] "PierreTran121047newsML.txt"       
    ## [1911] "PierreTran121060newsML.txt"       
    ## [1912] "PierreTran131651newsML.txt"       
    ## [1913] "PierreTran134012newsML.txt"       
    ## [1914] "PierreTran134014newsML.txt"       
    ## [1915] "PierreTran135536newsML.txt"       
    ## [1916] "PierreTran137348newsML.txt"       
    ## [1917] "PierreTran166855newsML.txt"       
    ## [1918] "PierreTran169998newsML.txt"       
    ## [1919] "PierreTran187064newsML.txt"       
    ## [1920] "PierreTran193894newsML.txt"       
    ## [1921] "PierreTran200399newsML.txt"       
    ## [1922] "PierreTran203401newsML.txt"       
    ## [1923] "PierreTran203527newsML.txt"       
    ## [1924] "PierreTran204524newsML.txt"       
    ## [1925] "PierreTran220430newsML.txt"       
    ## [1926] "PierreTran221117newsML.txt"       
    ## [1927] "PierreTran231585newsML.txt"       
    ## [1928] "PierreTran240967newsML.txt"       
    ## [1929] "PierreTran243837newsML.txt"       
    ## [1930] "PierreTran244133newsML.txt"       
    ## [1931] "PierreTran257666newsML.txt"       
    ## [1932] "PierreTran257759newsML.txt"       
    ## [1933] "PierreTran257760newsML.txt"       
    ## [1934] "PierreTran259821newsML.txt"       
    ## [1935] "PierreTran261933newsML.txt"       
    ## [1936] "PierreTran270873newsML.txt"       
    ## [1937] "PierreTran286787newsML.txt"       
    ## [1938] "PierreTran29116newsML.txt"        
    ## [1939] "PierreTran303195newsML.txt"       
    ## [1940] "PierreTran304051newsML.txt"       
    ## [1941] "PierreTran315959newsML.txt"       
    ## [1942] "PierreTran319304newsML.txt"       
    ## [1943] "PierreTran322828newsML.txt"       
    ## [1944] "PierreTran322947newsML.txt"       
    ## [1945] "PierreTran335078newsML.txt"       
    ## [1946] "PierreTran335319newsML.txt"       
    ## [1947] "PierreTran37690newsML.txt"        
    ## [1948] "PierreTran37710newsML.txt"        
    ## [1949] "PierreTran37762newsML.txt"        
    ## [1950] "PierreTran379739newsML.txt"       
    ## [1951] "RobinSidel104639newsML.txt"       
    ## [1952] "RobinSidel105455newsML.txt"       
    ## [1953] "RobinSidel10726newsML.txt"        
    ## [1954] "RobinSidel110827newsML.txt"       
    ## [1955] "RobinSidel110986newsML.txt"       
    ## [1956] "RobinSidel113551newsML.txt"       
    ## [1957] "RobinSidel122605newsML.txt"       
    ## [1958] "RobinSidel129289newsML.txt"       
    ## [1959] "RobinSidel129717newsML.txt"       
    ## [1960] "RobinSidel137061newsML.txt"       
    ## [1961] "RobinSidel140433newsML.txt"       
    ## [1962] "RobinSidel142837newsML.txt"       
    ## [1963] "RobinSidel143798newsML.txt"       
    ## [1964] "RobinSidel144598newsML.txt"       
    ## [1965] "RobinSidel147604newsML.txt"       
    ## [1966] "RobinSidel147744newsML.txt"       
    ## [1967] "RobinSidel153574newsML.txt"       
    ## [1968] "RobinSidel156766newsML.txt"       
    ## [1969] "RobinSidel156978newsML.txt"       
    ## [1970] "RobinSidel163749newsML.txt"       
    ## [1971] "RobinSidel163771newsML.txt"       
    ## [1972] "RobinSidel163815newsML.txt"       
    ## [1973] "RobinSidel164273newsML.txt"       
    ## [1974] "RobinSidel166722newsML.txt"       
    ## [1975] "RobinSidel168531newsML.txt"       
    ## [1976] "RobinSidel168654newsML.txt"       
    ## [1977] "RobinSidel168806newsML.txt"       
    ## [1978] "RobinSidel172865newsML.txt"       
    ## [1979] "RobinSidel173005newsML.txt"       
    ## [1980] "RobinSidel173947newsML.txt"       
    ## [1981] "RobinSidel175201newsML.txt"       
    ## [1982] "RobinSidel177958newsML.txt"       
    ## [1983] "RobinSidel186416newsML.txt"       
    ## [1984] "RobinSidel196693newsML.txt"       
    ## [1985] "RobinSidel196812newsML.txt"       
    ## [1986] "RobinSidel196990newsML.txt"       
    ## [1987] "RobinSidel198999newsML.txt"       
    ## [1988] "RobinSidel206638newsML.txt"       
    ## [1989] "RobinSidel207287newsML.txt"       
    ## [1990] "RobinSidel207288newsML.txt"       
    ## [1991] "RobinSidel216335newsML.txt"       
    ## [1992] "RobinSidel219316newsML.txt"       
    ## [1993] "RobinSidel220676newsML.txt"       
    ## [1994] "RobinSidel243641newsML.txt"       
    ## [1995] "RobinSidel243684newsML.txt"       
    ## [1996] "RobinSidel251225newsML.txt"       
    ## [1997] "RobinSidel255995newsML.txt"       
    ## [1998] "RobinSidel257938newsML.txt"       
    ## [1999] "RobinSidel259306newsML.txt"       
    ## [2000] "RobinSidel264190newsML.txt"       
    ## [2001] "RogerFillion121036newsML.txt"     
    ## [2002] "RogerFillion135924newsML.txt"     
    ## [2003] "RogerFillion137485newsML.txt"     
    ## [2004] "RogerFillion139224newsML.txt"     
    ## [2005] "RogerFillion140752newsML.txt"     
    ## [2006] "RogerFillion146861newsML.txt"     
    ## [2007] "RogerFillion150665newsML.txt"     
    ## [2008] "RogerFillion154675newsML.txt"     
    ## [2009] "RogerFillion155654newsML.txt"     
    ## [2010] "RogerFillion168572newsML.txt"     
    ## [2011] "RogerFillion171742newsML.txt"     
    ## [2012] "RogerFillion172846newsML.txt"     
    ## [2013] "RogerFillion173423newsML.txt"     
    ## [2014] "RogerFillion174058newsML.txt"     
    ## [2015] "RogerFillion175177newsML.txt"     
    ## [2016] "RogerFillion177017newsML.txt"     
    ## [2017] "RogerFillion181874newsML.txt"     
    ## [2018] "RogerFillion186425newsML.txt"     
    ## [2019] "RogerFillion186437newsML.txt"     
    ## [2020] "RogerFillion206783newsML.txt"     
    ## [2021] "RogerFillion214814newsML.txt"     
    ## [2022] "RogerFillion215305newsML.txt"     
    ## [2023] "RogerFillion215313newsML.txt"     
    ## [2024] "RogerFillion217357newsML.txt"     
    ## [2025] "RogerFillion217899newsML.txt"     
    ## [2026] "RogerFillion218296newsML.txt"     
    ## [2027] "RogerFillion231347newsML.txt"     
    ## [2028] "RogerFillion237323newsML.txt"     
    ## [2029] "RogerFillion252404newsML.txt"     
    ## [2030] "RogerFillion255709newsML.txt"     
    ## [2031] "RogerFillion264342newsML.txt"     
    ## [2032] "RogerFillion267134newsML.txt"     
    ## [2033] "RogerFillion277122newsML.txt"     
    ## [2034] "RogerFillion282184newsML.txt"     
    ## [2035] "RogerFillion283280newsML.txt"     
    ## [2036] "RogerFillion296312newsML.txt"     
    ## [2037] "RogerFillion305787newsML.txt"     
    ## [2038] "RogerFillion306326newsML.txt"     
    ## [2039] "RogerFillion312253newsML.txt"     
    ## [2040] "RogerFillion317583newsML.txt"     
    ## [2041] "RogerFillion32727newsML.txt"      
    ## [2042] "RogerFillion351344newsML.txt"     
    ## [2043] "RogerFillion352215newsML.txt"     
    ## [2044] "RogerFillion357213newsML.txt"     
    ## [2045] "RogerFillion357386newsML.txt"     
    ## [2046] "RogerFillion360479newsML.txt"     
    ## [2047] "RogerFillion361475newsML.txt"     
    ## [2048] "RogerFillion369492newsML.txt"     
    ## [2049] "RogerFillion379555newsML.txt"     
    ## [2050] "RogerFillion379567newsML.txt"     
    ## [2051] "SamuelPerry105557newsML.txt"      
    ## [2052] "SamuelPerry10781newsML.txt"       
    ## [2053] "SamuelPerry120398newsML.txt"      
    ## [2054] "SamuelPerry134566newsML.txt"      
    ## [2055] "SamuelPerry137035newsML.txt"      
    ## [2056] "SamuelPerry137812newsML.txt"      
    ## [2057] "SamuelPerry138944newsML.txt"      
    ## [2058] "SamuelPerry142698newsML.txt"      
    ## [2059] "SamuelPerry144030newsML.txt"      
    ## [2060] "SamuelPerry150672newsML.txt"      
    ## [2061] "SamuelPerry151392newsML.txt"      
    ## [2062] "SamuelPerry157604newsML.txt"      
    ## [2063] "SamuelPerry158945newsML.txt"      
    ## [2064] "SamuelPerry160066newsML.txt"      
    ## [2065] "SamuelPerry168565newsML.txt"      
    ## [2066] "SamuelPerry175043newsML.txt"      
    ## [2067] "SamuelPerry177032newsML.txt"      
    ## [2068] "SamuelPerry178271newsML.txt"      
    ## [2069] "SamuelPerry178647newsML.txt"      
    ## [2070] "SamuelPerry180753newsML.txt"      
    ## [2071] "SamuelPerry181711newsML.txt"      
    ## [2072] "SamuelPerry189155newsML.txt"      
    ## [2073] "SamuelPerry193298newsML.txt"      
    ## [2074] "SamuelPerry195461newsML.txt"      
    ## [2075] "SamuelPerry195465newsML.txt"      
    ## [2076] "SamuelPerry196664newsML.txt"      
    ## [2077] "SamuelPerry196870newsML.txt"      
    ## [2078] "SamuelPerry197518newsML.txt"      
    ## [2079] "SamuelPerry199947newsML.txt"      
    ## [2080] "SamuelPerry206811newsML.txt"      
    ## [2081] "SamuelPerry207693newsML.txt"      
    ## [2082] "SamuelPerry207747newsML.txt"      
    ## [2083] "SamuelPerry212471newsML.txt"      
    ## [2084] "SamuelPerry213090newsML.txt"      
    ## [2085] "SamuelPerry218057newsML.txt"      
    ## [2086] "SamuelPerry242117newsML.txt"      
    ## [2087] "SamuelPerry242528newsML.txt"      
    ## [2088] "SamuelPerry247268newsML.txt"      
    ## [2089] "SamuelPerry247516newsML.txt"      
    ## [2090] "SamuelPerry25052newsML.txt"       
    ## [2091] "SamuelPerry26550newsML.txt"       
    ## [2092] "SamuelPerry268174newsML.txt"      
    ## [2093] "SamuelPerry273216newsML.txt"      
    ## [2094] "SamuelPerry293806newsML.txt"      
    ## [2095] "SamuelPerry297101newsML.txt"      
    ## [2096] "SamuelPerry309801newsML.txt"      
    ## [2097] "SamuelPerry310667newsML.txt"      
    ## [2098] "SamuelPerry312805newsML.txt"      
    ## [2099] "SamuelPerry31339newsML.txt"       
    ## [2100] "SamuelPerry313966newsML.txt"      
    ## [2101] "SarahDavison108084newsML.txt"     
    ## [2102] "SarahDavison113117newsML.txt"     
    ## [2103] "SarahDavison140274newsML.txt"     
    ## [2104] "SarahDavison147534newsML.txt"     
    ## [2105] "SarahDavison148891newsML.txt"     
    ## [2106] "SarahDavison150435newsML.txt"     
    ## [2107] "SarahDavison155177newsML.txt"     
    ## [2108] "SarahDavison162424newsML.txt"     
    ## [2109] "SarahDavison165189newsML.txt"     
    ## [2110] "SarahDavison166481newsML.txt"     
    ## [2111] "SarahDavison169592newsML.txt"     
    ## [2112] "SarahDavison190734newsML.txt"     
    ## [2113] "SarahDavison196732newsML.txt"     
    ## [2114] "SarahDavison213002newsML.txt"     
    ## [2115] "SarahDavison21569newsML.txt"      
    ## [2116] "SarahDavison24260newsML.txt"      
    ## [2117] "SarahDavison26419newsML.txt"      
    ## [2118] "SarahDavison27290newsML.txt"      
    ## [2119] "SarahDavison310313newsML.txt"     
    ## [2120] "SarahDavison310654newsML.txt"     
    ## [2121] "SarahDavison317452newsML.txt"     
    ## [2122] "SarahDavison317706newsML.txt"     
    ## [2123] "SarahDavison31981newsML.txt"      
    ## [2124] "SarahDavison323333newsML.txt"     
    ## [2125] "SarahDavison324597newsML.txt"     
    ## [2126] "SarahDavison342562newsML.txt"     
    ## [2127] "SarahDavison352851newsML.txt"     
    ## [2128] "SarahDavison360226newsML.txt"     
    ## [2129] "SarahDavison361890newsML.txt"     
    ## [2130] "SarahDavison365768newsML.txt"     
    ## [2131] "SarahDavison369465newsML.txt"     
    ## [2132] "SarahDavison383224newsML.txt"     
    ## [2133] "SarahDavison386644newsML.txt"     
    ## [2134] "SarahDavison387955newsML.txt"     
    ## [2135] "SarahDavison387956newsML.txt"     
    ## [2136] "SarahDavison392316newsML.txt"     
    ## [2137] "SarahDavison396739newsML.txt"     
    ## [2138] "SarahDavison396740newsML.txt"     
    ## [2139] "SarahDavison396743newsML.txt"     
    ## [2140] "SarahDavison405565newsML.txt"     
    ## [2141] "SarahDavison409023newsML.txt"     
    ## [2142] "SarahDavison414690newsML.txt"     
    ## [2143] "SarahDavison419462newsML.txt"     
    ## [2144] "SarahDavison421047newsML.txt"     
    ## [2145] "SarahDavison426559newsML.txt"     
    ## [2146] "SarahDavison426661newsML.txt"     
    ## [2147] "SarahDavison428346newsML.txt"     
    ## [2148] "SarahDavison429223newsML.txt"     
    ## [2149] "SarahDavison429225newsML.txt"     
    ## [2150] "SarahDavison436334newsML.txt"     
    ## [2151] "ScottHillis116929newsML.txt"      
    ## [2152] "ScottHillis123483newsML.txt"      
    ## [2153] "ScottHillis123507newsML.txt"      
    ## [2154] "ScottHillis125213newsML.txt"      
    ## [2155] "ScottHillis126593newsML.txt"      
    ## [2156] "ScottHillis130513newsML.txt"      
    ## [2157] "ScottHillis133498newsML.txt"      
    ## [2158] "ScottHillis136771newsML.txt"      
    ## [2159] "ScottHillis136805newsML.txt"      
    ## [2160] "ScottHillis140273newsML.txt"      
    ## [2161] "ScottHillis140340newsML.txt"      
    ## [2162] "ScottHillis142465newsML.txt"      
    ## [2163] "ScottHillis143454newsML.txt"      
    ## [2164] "ScottHillis145712newsML.txt"      
    ## [2165] "ScottHillis158507newsML.txt"      
    ## [2166] "ScottHillis159412newsML.txt"      
    ## [2167] "ScottHillis161138newsML.txt"      
    ## [2168] "ScottHillis165123newsML.txt"      
    ## [2169] "ScottHillis184219newsML.txt"      
    ## [2170] "ScottHillis194787newsML.txt"      
    ## [2171] "ScottHillis195353newsML.txt"      
    ## [2172] "ScottHillis195357newsML.txt"      
    ## [2173] "ScottHillis198216newsML.txt"      
    ## [2174] "ScottHillis199747newsML.txt"      
    ## [2175] "ScottHillis208266newsML.txt"      
    ## [2176] "ScottHillis209835newsML.txt"      
    ## [2177] "ScottHillis232758newsML.txt"      
    ## [2178] "ScottHillis244999newsML.txt"      
    ## [2179] "ScottHillis250880newsML.txt"      
    ## [2180] "ScottHillis253868newsML.txt"      
    ## [2181] "ScottHillis254803newsML.txt"      
    ## [2182] "ScottHillis256974newsML.txt"      
    ## [2183] "ScottHillis282007newsML.txt"      
    ## [2184] "ScottHillis287502newsML.txt"      
    ## [2185] "ScottHillis289837newsML.txt"      
    ## [2186] "ScottHillis292263newsML.txt"      
    ## [2187] "ScottHillis295436newsML.txt"      
    ## [2188] "ScottHillis295992newsML.txt"      
    ## [2189] "ScottHillis299129newsML.txt"      
    ## [2190] "ScottHillis301673newsML.txt"      
    ## [2191] "ScottHillis305692newsML.txt"      
    ## [2192] "ScottHillis310466newsML.txt"      
    ## [2193] "ScottHillis314875newsML.txt"      
    ## [2194] "ScottHillis318653newsML.txt"      
    ## [2195] "ScottHillis318742newsML.txt"      
    ## [2196] "ScottHillis321513newsML.txt"      
    ## [2197] "ScottHillis334739newsML.txt"      
    ## [2198] "ScottHillis340709newsML.txt"      
    ## [2199] "ScottHillis340736newsML.txt"      
    ## [2200] "ScottHillis345375newsML.txt"      
    ## [2201] "SimonCowell15627newsML.txt"       
    ## [2202] "SimonCowell242983newsML.txt"      
    ## [2203] "SimonCowell245741newsML.txt"      
    ## [2204] "SimonCowell248599newsML.txt"      
    ## [2205] "SimonCowell253060newsML.txt"      
    ## [2206] "SimonCowell255730newsML.txt"      
    ## [2207] "SimonCowell258206newsML.txt"      
    ## [2208] "SimonCowell259617newsML.txt"      
    ## [2209] "SimonCowell27675newsML.txt"       
    ## [2210] "SimonCowell302376newsML.txt"      
    ## [2211] "SimonCowell303707newsML.txt"      
    ## [2212] "SimonCowell311253newsML.txt"      
    ## [2213] "SimonCowell312858newsML.txt"      
    ## [2214] "SimonCowell312983newsML.txt"      
    ## [2215] "SimonCowell324051newsML.txt"      
    ## [2216] "SimonCowell347888newsML.txt"      
    ## [2217] "SimonCowell347894newsML.txt"      
    ## [2218] "SimonCowell350608newsML.txt"      
    ## [2219] "SimonCowell353509newsML.txt"      
    ## [2220] "SimonCowell356525newsML.txt"      
    ## [2221] "SimonCowell361250newsML.txt"      
    ## [2222] "SimonCowell376405newsML.txt"      
    ## [2223] "SimonCowell376414newsML.txt"      
    ## [2224] "SimonCowell377851newsML.txt"      
    ## [2225] "SimonCowell384133newsML.txt"      
    ## [2226] "SimonCowell385473newsML.txt"      
    ## [2227] "SimonCowell385542newsML.txt"      
    ## [2228] "SimonCowell390568newsML.txt"      
    ## [2229] "SimonCowell390574newsML.txt"      
    ## [2230] "SimonCowell400494newsML.txt"      
    ## [2231] "SimonCowell402074newsML.txt"      
    ## [2232] "SimonCowell403689newsML.txt"      
    ## [2233] "SimonCowell405057newsML.txt"      
    ## [2234] "SimonCowell405124newsML.txt"      
    ## [2235] "SimonCowell405237newsML.txt"      
    ## [2236] "SimonCowell406879newsML.txt"      
    ## [2237] "SimonCowell409884newsML.txt"      
    ## [2238] "SimonCowell413323newsML.txt"      
    ## [2239] "SimonCowell417061newsML.txt"      
    ## [2240] "SimonCowell418734newsML.txt"      
    ## [2241] "SimonCowell420566newsML.txt"      
    ## [2242] "SimonCowell423379newsML.txt"      
    ## [2243] "SimonCowell424503newsML.txt"      
    ## [2244] "SimonCowell425943newsML.txt"      
    ## [2245] "SimonCowell440535newsML.txt"      
    ## [2246] "SimonCowell442065newsML.txt"      
    ## [2247] "SimonCowell447824newsML.txt"      
    ## [2248] "SimonCowell448236newsML.txt"      
    ## [2249] "SimonCowell450839newsML.txt"      
    ## [2250] "SimonCowell455543newsML.txt"      
    ## [2251] "TanEeLyn108092newsML.txt"         
    ## [2252] "TanEeLyn110751newsML.txt"         
    ## [2253] "TanEeLyn113062newsML.txt"         
    ## [2254] "TanEeLyn113074newsML.txt"         
    ## [2255] "TanEeLyn123487newsML.txt"         
    ## [2256] "TanEeLyn127958newsML.txt"         
    ## [2257] "TanEeLyn153426newsML.txt"         
    ## [2258] "TanEeLyn155113newsML.txt"         
    ## [2259] "TanEeLyn163670newsML.txt"         
    ## [2260] "TanEeLyn169589newsML.txt"         
    ## [2261] "TanEeLyn169609newsML.txt"         
    ## [2262] "TanEeLyn186184newsML.txt"         
    ## [2263] "TanEeLyn190418newsML.txt"         
    ## [2264] "TanEeLyn192393newsML.txt"         
    ## [2265] "TanEeLyn192410newsML.txt"         
    ## [2266] "TanEeLyn195093newsML.txt"         
    ## [2267] "TanEeLyn241238newsML.txt"         
    ## [2268] "TanEeLyn242294newsML.txt"         
    ## [2269] "TanEeLyn243445newsML.txt"         
    ## [2270] "TanEeLyn246306newsML.txt"         
    ## [2271] "TanEeLyn250726newsML.txt"         
    ## [2272] "TanEeLyn250732newsML.txt"         
    ## [2273] "TanEeLyn253732newsML.txt"         
    ## [2274] "TanEeLyn253869newsML.txt"         
    ## [2275] "TanEeLyn264108newsML.txt"         
    ## [2276] "TanEeLyn266981newsML.txt"         
    ## [2277] "TanEeLyn271710newsML.txt"         
    ## [2278] "TanEeLyn27284newsML.txt"          
    ## [2279] "TanEeLyn273117newsML.txt"         
    ## [2280] "TanEeLyn281155newsML.txt"         
    ## [2281] "TanEeLyn283835newsML.txt"         
    ## [2282] "TanEeLyn301674newsML.txt"         
    ## [2283] "TanEeLyn31955newsML.txt"          
    ## [2284] "TanEeLyn336357newsML.txt"         
    ## [2285] "TanEeLyn380721newsML.txt"         
    ## [2286] "TanEeLyn380733newsML.txt"         
    ## [2287] "TanEeLyn381221newsML.txt"         
    ## [2288] "TanEeLyn381228newsML.txt"         
    ## [2289] "TanEeLyn392182newsML.txt"         
    ## [2290] "TanEeLyn392471newsML.txt"         
    ## [2291] "TanEeLyn395666newsML.txt"         
    ## [2292] "TanEeLyn409157newsML.txt"         
    ## [2293] "TanEeLyn41230newsML.txt"          
    ## [2294] "TanEeLyn417677newsML.txt"         
    ## [2295] "TanEeLyn422392newsML.txt"         
    ## [2296] "TanEeLyn426779newsML.txt"         
    ## [2297] "TanEeLyn446979newsML.txt"         
    ## [2298] "TanEeLyn456180newsML.txt"         
    ## [2299] "TanEeLyn462146newsML.txt"         
    ## [2300] "TanEeLyn462149newsML.txt"         
    ## [2301] "TheresePoletti101565newsML.txt"   
    ## [2302] "TheresePoletti104608newsML.txt"   
    ## [2303] "TheresePoletti113481newsML.txt"   
    ## [2304] "TheresePoletti115852newsML.txt"   
    ## [2305] "TheresePoletti120387newsML.txt"   
    ## [2306] "TheresePoletti120410newsML.txt"   
    ## [2307] "TheresePoletti126708newsML.txt"   
    ## [2308] "TheresePoletti126897newsML.txt"   
    ## [2309] "TheresePoletti129783newsML.txt"   
    ## [2310] "TheresePoletti130612newsML.txt"   
    ## [2311] "TheresePoletti136917newsML.txt"   
    ## [2312] "TheresePoletti140583newsML.txt"   
    ## [2313] "TheresePoletti144326newsML.txt"   
    ## [2314] "TheresePoletti146012newsML.txt"   
    ## [2315] "TheresePoletti149309newsML.txt"   
    ## [2316] "TheresePoletti151061newsML.txt"   
    ## [2317] "TheresePoletti154598newsML.txt"   
    ## [2318] "TheresePoletti157001newsML.txt"   
    ## [2319] "TheresePoletti16054newsML.txt"    
    ## [2320] "TheresePoletti176991newsML.txt"   
    ## [2321] "TheresePoletti179117newsML.txt"   
    ## [2322] "TheresePoletti18158newsML.txt"    
    ## [2323] "TheresePoletti181674newsML.txt"   
    ## [2324] "TheresePoletti18215newsML.txt"    
    ## [2325] "TheresePoletti182700newsML.txt"   
    ## [2326] "TheresePoletti186457newsML.txt"   
    ## [2327] "TheresePoletti187047newsML.txt"   
    ## [2328] "TheresePoletti189987newsML.txt"   
    ## [2329] "TheresePoletti200003newsML.txt"   
    ## [2330] "TheresePoletti203028newsML.txt"   
    ## [2331] "TheresePoletti203129newsML.txt"   
    ## [2332] "TheresePoletti232280newsML.txt"   
    ## [2333] "TheresePoletti237345newsML.txt"   
    ## [2334] "TheresePoletti240007newsML.txt"   
    ## [2335] "TheresePoletti242113newsML.txt"   
    ## [2336] "TheresePoletti242590newsML.txt"   
    ## [2337] "TheresePoletti243514newsML.txt"   
    ## [2338] "TheresePoletti246358newsML.txt"   
    ## [2339] "TheresePoletti246460newsML.txt"   
    ## [2340] "TheresePoletti249362newsML.txt"   
    ## [2341] "TheresePoletti252289newsML.txt"   
    ## [2342] "TheresePoletti257205newsML.txt"   
    ## [2343] "TheresePoletti272016newsML.txt"   
    ## [2344] "TheresePoletti27551newsML.txt"    
    ## [2345] "TheresePoletti283077newsML.txt"   
    ## [2346] "TheresePoletti285748newsML.txt"   
    ## [2347] "TheresePoletti287618newsML.txt"   
    ## [2348] "TheresePoletti301796newsML.txt"   
    ## [2349] "TheresePoletti305885newsML.txt"   
    ## [2350] "TheresePoletti309865newsML.txt"   
    ## [2351] "TimFarrand10265newsML.txt"        
    ## [2352] "TimFarrand114953newsML.txt"       
    ## [2353] "TimFarrand127403newsML.txt"       
    ## [2354] "TimFarrand127856newsML.txt"       
    ## [2355] "TimFarrand143038newsML.txt"       
    ## [2356] "TimFarrand149844newsML.txt"       
    ## [2357] "TimFarrand152851newsML.txt"       
    ## [2358] "TimFarrand154492newsML.txt"       
    ## [2359] "TimFarrand15631newsML.txt"        
    ## [2360] "TimFarrand159528newsML.txt"       
    ## [2361] "TimFarrand159531newsML.txt"       
    ## [2362] "TimFarrand160644newsML.txt"       
    ## [2363] "TimFarrand164516newsML.txt"       
    ## [2364] "TimFarrand164877newsML.txt"       
    ## [2365] "TimFarrand164890newsML.txt"       
    ## [2366] "TimFarrand165801newsML.txt"       
    ## [2367] "TimFarrand165864newsML.txt"       
    ## [2368] "TimFarrand167779newsML.txt"       
    ## [2369] "TimFarrand167803newsML.txt"       
    ## [2370] "TimFarrand172132newsML.txt"       
    ## [2371] "TimFarrand173770newsML.txt"       
    ## [2372] "TimFarrand173850newsML.txt"       
    ## [2373] "TimFarrand175500newsML.txt"       
    ## [2374] "TimFarrand175594newsML.txt"       
    ## [2375] "TimFarrand177684newsML.txt"       
    ## [2376] "TimFarrand199222newsML.txt"       
    ## [2377] "TimFarrand201017newsML.txt"       
    ## [2378] "TimFarrand202017newsML.txt"       
    ## [2379] "TimFarrand204606newsML.txt"       
    ## [2380] "TimFarrand207557newsML.txt"       
    ## [2381] "TimFarrand210560newsML.txt"       
    ## [2382] "TimFarrand220852newsML.txt"       
    ## [2383] "TimFarrand220857newsML.txt"       
    ## [2384] "TimFarrand220938newsML.txt"       
    ## [2385] "TimFarrand221452newsML.txt"       
    ## [2386] "TimFarrand224668newsML.txt"       
    ## [2387] "TimFarrand224684newsML.txt"       
    ## [2388] "TimFarrand225561newsML.txt"       
    ## [2389] "TimFarrand229159newsML.txt"       
    ## [2390] "TimFarrand229685newsML.txt"       
    ## [2391] "TimFarrand230604newsML.txt"       
    ## [2392] "TimFarrand234885newsML.txt"       
    ## [2393] "TimFarrand235003newsML.txt"       
    ## [2394] "TimFarrand236371newsML.txt"       
    ## [2395] "TimFarrand237957newsML.txt"       
    ## [2396] "TimFarrand237980newsML.txt"       
    ## [2397] "TimFarrand238095newsML.txt"       
    ## [2398] "TimFarrand240674newsML.txt"       
    ## [2399] "TimFarrand242910newsML.txt"       
    ## [2400] "TimFarrand242992newsML.txt"       
    ## [2401] "ToddNissen107276newsML.txt"       
    ## [2402] "ToddNissen108216newsML.txt"       
    ## [2403] "ToddNissen110957newsML.txt"       
    ## [2404] "ToddNissen111247newsML.txt"       
    ## [2405] "ToddNissen117089newsML.txt"       
    ## [2406] "ToddNissen120415newsML.txt"       
    ## [2407] "ToddNissen121051newsML.txt"       
    ## [2408] "ToddNissen133717newsML.txt"       
    ## [2409] "ToddNissen140567newsML.txt"       
    ## [2410] "ToddNissen144319newsML.txt"       
    ## [2411] "ToddNissen146468newsML.txt"       
    ## [2412] "ToddNissen146741newsML.txt"       
    ## [2413] "ToddNissen148408newsML.txt"       
    ## [2414] "ToddNissen149287newsML.txt"       
    ## [2415] "ToddNissen154381newsML.txt"       
    ## [2416] "ToddNissen155659newsML.txt"       
    ## [2417] "ToddNissen158140newsML.txt"       
    ## [2418] "ToddNissen160059newsML.txt"       
    ## [2419] "ToddNissen18151newsML.txt"        
    ## [2420] "ToddNissen18393newsML.txt"        
    ## [2421] "ToddNissen18414newsML.txt"        
    ## [2422] "ToddNissen189161newsML.txt"       
    ## [2423] "ToddNissen189259newsML.txt"       
    ## [2424] "ToddNissen191372newsML.txt"       
    ## [2425] "ToddNissen193383newsML.txt"       
    ## [2426] "ToddNissen193662newsML.txt"       
    ## [2427] "ToddNissen199894newsML.txt"       
    ## [2428] "ToddNissen200426newsML.txt"       
    ## [2429] "ToddNissen216410newsML.txt"       
    ## [2430] "ToddNissen217362newsML.txt"       
    ## [2431] "ToddNissen217419newsML.txt"       
    ## [2432] "ToddNissen217896newsML.txt"       
    ## [2433] "ToddNissen222096newsML.txt"       
    ## [2434] "ToddNissen227529newsML.txt"       
    ## [2435] "ToddNissen237066newsML.txt"       
    ## [2436] "ToddNissen245562newsML.txt"       
    ## [2437] "ToddNissen246995newsML.txt"       
    ## [2438] "ToddNissen259007newsML.txt"       
    ## [2439] "ToddNissen261367newsML.txt"       
    ## [2440] "ToddNissen264956newsML.txt"       
    ## [2441] "ToddNissen267365newsML.txt"       
    ## [2442] "ToddNissen270703newsML.txt"       
    ## [2443] "ToddNissen270751newsML.txt"       
    ## [2444] "ToddNissen271791newsML.txt"       
    ## [2445] "ToddNissen280639newsML.txt"       
    ## [2446] "ToddNissen284034newsML.txt"       
    ## [2447] "ToddNissen284597newsML.txt"       
    ## [2448] "ToddNissen28509newsML.txt"        
    ## [2449] "ToddNissen286232newsML.txt"       
    ## [2450] "ToddNissen291856newsML.txt"       
    ## [2451] "WilliamKazer101226newsML.txt"     
    ## [2452] "WilliamKazer10321newsML.txt"      
    ## [2453] "WilliamKazer105226newsML.txt"     
    ## [2454] "WilliamKazer106731newsML.txt"     
    ## [2455] "WilliamKazer108094newsML.txt"     
    ## [2456] "WilliamKazer113095newsML.txt"     
    ## [2457] "WilliamKazer113097newsML.txt"     
    ## [2458] "WilliamKazer115519newsML.txt"     
    ## [2459] "WilliamKazer118912newsML.txt"     
    ## [2460] "WilliamKazer123484newsML.txt"     
    ## [2461] "WilliamKazer125333newsML.txt"     
    ## [2462] "WilliamKazer126643newsML.txt"     
    ## [2463] "WilliamKazer12909newsML.txt"      
    ## [2464] "WilliamKazer129188newsML.txt"     
    ## [2465] "WilliamKazer143490newsML.txt"     
    ## [2466] "WilliamKazer147523newsML.txt"     
    ## [2467] "WilliamKazer148885newsML.txt"     
    ## [2468] "WilliamKazer153448newsML.txt"     
    ## [2469] "WilliamKazer165109newsML.txt"     
    ## [2470] "WilliamKazer169626newsML.txt"     
    ## [2471] "WilliamKazer178505newsML.txt"     
    ## [2472] "WilliamKazer178884newsML.txt"     
    ## [2473] "WilliamKazer180188newsML.txt"     
    ## [2474] "WilliamKazer188958newsML.txt"     
    ## [2475] "WilliamKazer195340newsML.txt"     
    ## [2476] "WilliamKazer196734newsML.txt"     
    ## [2477] "WilliamKazer202888newsML.txt"     
    ## [2478] "WilliamKazer206559newsML.txt"     
    ## [2479] "WilliamKazer208244newsML.txt"     
    ## [2480] "WilliamKazer209813newsML.txt"     
    ## [2481] "WilliamKazer214207newsML.txt"     
    ## [2482] "WilliamKazer222819newsML.txt"     
    ## [2483] "WilliamKazer235763newsML.txt"     
    ## [2484] "WilliamKazer239878newsML.txt"     
    ## [2485] "WilliamKazer2430newsML.txt"       
    ## [2486] "WilliamKazer246276newsML.txt"     
    ## [2487] "WilliamKazer247720newsML.txt"     
    ## [2488] "WilliamKazer247829newsML.txt"     
    ## [2489] "WilliamKazer247909newsML.txt"     
    ## [2490] "WilliamKazer257526newsML.txt"     
    ## [2491] "WilliamKazer258689newsML.txt"     
    ## [2492] "WilliamKazer264132newsML.txt"     
    ## [2493] "WilliamKazer268647newsML.txt"     
    ## [2494] "WilliamKazer278687newsML.txt"     
    ## [2495] "WilliamKazer281216newsML.txt"     
    ## [2496] "WilliamKazer28223newsML.txt"      
    ## [2497] "WilliamKazer282935newsML.txt"     
    ## [2498] "WilliamKazer287736newsML.txt"     
    ## [2499] "WilliamKazer289747newsML.txt"     
    ## [2500] "WilliamKazer304402newsML.txt"

    names(all.authors) = mynames

Set up document term matrix

    # create a text mining corpus with the plain docs 
    documents_raw = Corpus(VectorSource(all.authors))

    # Pre-processing/tokenization step
    my_documents = documents_raw
    my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
    my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
    my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
    my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) # remove excess white-space
    my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en")) # Remove stop words

    # create a doc-term-matrix
    DTM_all.authors = DocumentTermMatrix(my_documents)
    # DTM_all.authors # some basic summary statistics; ~#32.6k terms
    DTM_all.authors = removeSparseTerms(DTM_all.authors, 0.95) # remove words not used in > 5% of docs
    DTM_all.authors # now 801 terms

    ## <<DocumentTermMatrix (documents: 2500, terms: 801)>>
    ## Non-/sparse entries: 280686/1721814
    ## Sparsity           : 86%
    ## Maximal term length: 18
    ## Weighting          : term frequency (tf)

Construct TF-IDF weights

    tfidf_all.authors = weightTfIdf(DTM_all.authors)

Train a Random Forest model

    library(stringr)
    library(klaR)

    ## Warning: package 'klaR' was built under R version 3.6.1

    ## Loading required package: MASS

    ## Warning: package 'MASS' was built under R version 3.6.1

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    library(caret)

    ## Warning: package 'caret' was built under R version 3.6.1

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

    ## The following object is masked from 'package:mosaic':
    ## 
    ##     dotPlot

    library(randomForest)

    ## Warning: package 'randomForest' was built under R version 3.6.1

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    adtm.df<-as.data.frame(as.matrix(tfidf_all.authors))
    adtm.df$authors=authors # add the response variable to the df
    names(adtm.df) <- make.names(names(adtm.df))
    set.seed(10)
    model2 <- randomForest(as.factor(authors)~., data = adtm.df, ntree = 1000, mtry = 10, importance = TRUE)

Set up the test set - same steps as for the training set

    dir.list = list.files('ReutersC50/C50test/')
    file_list3 = c()
    authors2 = c()
    for (x in dir.list){
      files = Sys.glob(paste('ReutersC50/C50test/', x,'/*.txt', sep=''))
      file_list3 = c(file_list3, files)
      authors2 = c(authors2, rep(x,50))
    }

    all.authors2 = lapply(file_list3, readerPlain)
    mynames = file_list3 %>%
      { strsplit(., '/', fixed=TRUE) } %>%
      { lapply(., tail, n=2) } %>%
      { lapply(., paste0, collapse = '') } %>%
      unlist

    # Rename the articles
    names(all.authors2) = mynames

    # Create a text mining corpus with the plain docs
    documents_raw = Corpus(VectorSource(all.authors2))

    # Pre-processing/tokenization steps
    my_documents = documents_raw
    my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
    my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
    my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
    my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) # remove excess white-space
    my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en")) # remove stop words

Create a doc-term-matrix for the test set

    DTM_all.authors2 = DocumentTermMatrix(my_documents)
    # DTM_all.authors2 # ~33.4K terms
    DTM_all.authors2 = removeSparseTerms(DTM_all.authors2, 0.95) # remove words not used in > 5% of docs
    DTM_all.authors2 # now ~ 816 terms

    ## <<DocumentTermMatrix (documents: 2500, terms: 816)>>
    ## Non-/sparse entries: 285048/1754952
    ## Sparsity           : 86%
    ## Maximal term length: 18
    ## Weighting          : term frequency (tf)

**Note:** unlike Naive Bayes, Random Forest does not assume independence
and compound probabilities. Therefore, terms not in the training set are
simply not considered in the model and do not affect the existing terms’
predictive power. As such, we did not set up a pseudo-word count for our
model.

Construct TF-IDF weights

    tfidf_all.authors2 = weightTfIdf(DTM_all.authors2)
    tfidf_all.authors2

    ## <<DocumentTermMatrix (documents: 2500, terms: 816)>>
    ## Non-/sparse entries: 245048/1794952
    ## Sparsity           : 88%
    ## Maximal term length: 18
    ## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)

Set up test set data frame and predict

    adtm.df_test2<-as.data.frame(as.matrix(tfidf_all.authors2))
    rf.pred = predict(model2, data=adtm.df_test2)
    mean(rf.pred==authors2) #80.32% accuracy

    ## [1] 0.8032

Deeper dive into model performance

    rf.confusion.matrix = confusionMatrix(table(rf.pred,authors2))
    rf.confusion.matrix$overall # again, 80.32% accuracy

    ##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
    ##      0.8032000      0.7991837      0.7870638      0.8186227      0.0200000 
    ## AccuracyPValue  McnemarPValue 
    ##      0.0000000            NaN

    rf.confusion.matrix.df = as.data.frame(rf.confusion.matrix$byClass)
    rf.confusion.matrix.df[order(-rf.confusion.matrix.df$Sensitivity),1:2]

    ##                          Sensitivity Specificity
    ## Class: JimGilchrist             1.00   0.9938776
    ## Class: RogerFillion             1.00   0.9979592
    ## Class: FumikoFujisaki           0.98   0.9987755
    ## Class: LynneO'Donnell           0.98   0.9963265
    ## Class: LynnleyBrowning          0.98   0.9987755
    ## Class: AaronPressman            0.96   0.9975510
    ## Class: MatthewBunce             0.96   0.9971429
    ## Class: DarrenSchuettler         0.94   0.9975510
    ## Class: JoWinterbottom           0.94   0.9946939
    ## Class: MarcelMichelson          0.92   0.9971429
    ## Class: NickLouth                0.92   0.9971429
    ## Class: PeterHumphrey            0.92   0.9893878
    ## Class: DavidLawder              0.90   0.9963265
    ## Class: HeatherScoffield         0.90   0.9995918
    ## Class: KouroshKarimkhany        0.90   0.9914286
    ## Class: LydiaZajc                0.90   0.9983673
    ## Class: RobinSidel               0.90   0.9967347
    ## Class: JanLopatka               0.88   0.9942857
    ## Class: KarlPenhaul              0.88   0.9955102
    ## Class: MarkBendeich             0.88   0.9963265
    ## Class: AlanCrosby               0.86   0.9987755
    ## Class: GrahamEarnshaw           0.84   0.9955102
    ## Class: KevinDrawbaugh           0.82   0.9926531
    ## Class: KeithWeir                0.80   0.9983673
    ## Class: KirstinRidley            0.80   0.9975510
    ## Class: PierreTran               0.80   0.9979592
    ## Class: SarahDavison             0.80   0.9967347
    ## Class: SimonCowell              0.80   0.9951020
    ## Class: TimFarrand               0.80   0.9946939
    ## Class: JoeOrtiz                 0.78   0.9967347
    ## Class: JohnMastrini             0.78   0.9967347
    ## Class: JonathanBirt             0.78   0.9946939
    ## Class: KevinMorrison            0.78   0.9967347
    ## Class: ToddNissen               0.78   0.9963265
    ## Class: AlexanderSmith           0.74   0.9979592
    ## Class: BernardHickey            0.74   0.9971429
    ## Class: EricAuchard              0.74   0.9975510
    ## Class: TheresePoletti           0.74   0.9930612
    ## Class: BenjaminKangLim          0.72   0.9828571
    ## Class: MartinWolk               0.72   0.9979592
    ## Class: MichaelConnor            0.72   0.9987755
    ## Class: PatriciaCommins          0.70   0.9967347
    ## Class: SamuelPerry              0.70   0.9951020
    ## Class: BradDorfman              0.68   0.9967347
    ## Class: EdnaFernandes            0.68   0.9967347
    ## Class: TanEeLyn                 0.54   0.9975510
    ## Class: WilliamKazer             0.54   0.9930612
    ## Class: JaneMacartney            0.52   0.9975510
    ## Class: MureDickie               0.50   0.9914286
    ## Class: ScottHillis              0.32   0.9955102

**Report:** To predict authorship based on an article’s textual content,
we relied on term TF-IDFs as the predictors and tested several Random
Forest models. We reached the best accuracy of 80% with 1000 trees and
10 variables. Our data pre-processing and analysis pipeline is as
followed (the same steps were followed for both training and test data):

1.  Read the file names and author names in as two vectors
2.  Clean up each file’s name by splitting the file on “/,” keeping only
    the last two parts of the name (Author Name and unique identifier),
    and concatenate those two parts
3.  Read the files with the readPlain function and name each file the
    cleaned version of its name (from Step 2)
4.  Turn the plain documents into a vector then a corpus for text mining
5.  Tokenize the symbols in the corpus by
    1.  Making everything lowercase
    2.  Removing numbers
    3.  Removing punctuations
    4.  Removing excess white spaces
    5.  Removing stop words
6.  Create a document term matrix (DTM) and remove words that are not
    used in over 5% of the documents in the corpus
7.  Calculate the TF-IDF for each term in each document of the DTM with
    the weightTfIdf function – the TF-IDF weights would serve as the
    features for our Random Forest models. We use the training set’s
    TF-IDF weights to train the model. Then use the model on the test
    set’s TF-IDF weights to predict authorship.

As mentioned above, our best Random Forest model had 1000 trees and
sampled 10 variables at each split. The model reached an overall
accuracy of 80%. Additionally, when predicting for a specific author,
our model’s precision (specificity) is generally over 99%. That said,
some authors’ recalls (sensitivity, or true positives out of all
positives) were especially bad. For example, the model only identified
about a third of all Scott Hillis’ articles.

\#6. Association rule mining  
We first read in the data and create the appropriate model

    rm(list=ls())
    library(tidyverse)
    library(arules)

    ## Warning: package 'arules' was built under R version 3.6.1

    library(arulesViz)

    ## Warning: package 'arulesViz' was built under R version 3.6.1

    groceries <- read.transactions('groceries.txt', sep=',')
    groceries_trans = as(groceries, "transactions")
    grocery_rules = apriori(groceries, 
                         parameter=list(support=.005, confidence=.1, maxlen=3))

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.1    0.1    1 none FALSE            TRUE       5   0.005      1
    ##  maxlen target   ext
    ##       3  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 49 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [120 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3

    ## Warning in apriori(groceries, parameter = list(support = 0.005, confidence
    ## = 0.1, : Mining stopped (maxlen reached). Only patterns up to a length of 3
    ## returned!

    ##  done [0.00s].
    ## writing ... [1534 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

We can then run various imspections to get a clearer idea of the
associations. We also look at different subsets, controlling for lift,
confidence, and both lift and confidence.

    inspect(grocery_rules)

    ##        lhs                           rhs                            support confidence      lift count
    ## [1]    {}                         => {bottled water}            0.110523640  0.1105236 1.0000000  1087
    ## [2]    {}                         => {tropical fruit}           0.104931368  0.1049314 1.0000000  1032
    ## [3]    {}                         => {root vegetables}          0.108998475  0.1089985 1.0000000  1072
    ## [4]    {}                         => {soda}                     0.174377224  0.1743772 1.0000000  1715
    ## [5]    {}                         => {yogurt}                   0.139501779  0.1395018 1.0000000  1372
    ## [6]    {}                         => {rolls/buns}               0.183934926  0.1839349 1.0000000  1809
    ## [7]    {}                         => {other vegetables}         0.193492628  0.1934926 1.0000000  1903
    ## [8]    {}                         => {whole milk}               0.255516014  0.2555160 1.0000000  2513
    ## [9]    {cake bar}                 => {whole milk}               0.005592272  0.4230769 1.6557746    55
    ## [10]   {dishes}                   => {other vegetables}         0.005998983  0.3410405 1.7625502    59
    ## [11]   {dishes}                   => {whole milk}               0.005287239  0.3005780 1.1763569    52
    ## [12]   {mustard}                  => {whole milk}               0.005185562  0.4322034 1.6914924    51
    ## [13]   {pot plants}               => {whole milk}               0.006914082  0.4000000 1.5654596    68
    ## [14]   {chewing gum}              => {soda}                     0.005388917  0.2560386 1.4683033    53
    ## [15]   {chewing gum}              => {whole milk}               0.005083884  0.2415459 0.9453259    50
    ## [16]   {canned fish}              => {other vegetables}         0.005083884  0.3378378 1.7459985    50
    ## [17]   {pasta}                    => {whole milk}               0.006100661  0.4054054 1.5866145    60
    ## [18]   {herbs}                    => {root vegetables}          0.007015760  0.4312500 3.9564774    69
    ## [19]   {herbs}                    => {other vegetables}         0.007727504  0.4750000 2.4548739    76
    ## [20]   {herbs}                    => {whole milk}               0.007727504  0.4750000 1.8589833    76
    ## [21]   {processed cheese}         => {soda}                     0.005287239  0.3190184 1.8294729    52
    ## [22]   {processed cheese}         => {other vegetables}         0.005490595  0.3312883 1.7121497    54
    ## [23]   {processed cheese}         => {whole milk}               0.007015760  0.4233129 1.6566981    69
    ## [24]   {semi-finished bread}      => {other vegetables}         0.005185562  0.2931034 1.5148042    51
    ## [25]   {semi-finished bread}      => {whole milk}               0.007117438  0.4022989 1.5744565    70
    ## [26]   {beverages}                => {yogurt}                   0.005490595  0.2109375 1.5120775    54
    ## [27]   {beverages}                => {rolls/buns}               0.005388917  0.2070312 1.1255679    53
    ## [28]   {beverages}                => {other vegetables}         0.005185562  0.1992188 1.0295935    51
    ## [29]   {beverages}                => {whole milk}               0.006812405  0.2617188 1.0242753    67
    ## [30]   {ice cream}                => {soda}                     0.006100661  0.2439024 1.3987058    60
    ## [31]   {ice cream}                => {other vegetables}         0.005083884  0.2032520 1.0504381    50
    ## [32]   {ice cream}                => {whole milk}               0.005897306  0.2357724 0.9227303    58
    ## [33]   {detergent}                => {other vegetables}         0.006405694  0.3333333 1.7227185    63
    ## [34]   {detergent}                => {whole milk}               0.008947636  0.4656085 1.8222281    88
    ## [35]   {pickled vegetables}       => {other vegetables}         0.006405694  0.3579545 1.8499648    63
    ## [36]   {pickled vegetables}       => {whole milk}               0.007117438  0.3977273 1.5565650    70
    ## [37]   {baking powder}            => {other vegetables}         0.007320793  0.4137931 2.1385471    72
    ## [38]   {baking powder}            => {whole milk}               0.009252669  0.5229885 2.0467935    91
    ## [39]   {flour}                    => {other vegetables}         0.006304016  0.3625731 1.8738342    62
    ## [40]   {flour}                    => {whole milk}               0.008439248  0.4853801 1.8996074    83
    ## [41]   {soft cheese}              => {yogurt}                   0.005998983  0.3511905 2.5174623    59
    ## [42]   {soft cheese}              => {rolls/buns}               0.005388917  0.3154762 1.7151511    53
    ## [43]   {soft cheese}              => {other vegetables}         0.007117438  0.4166667 2.1533981    70
    ## [44]   {soft cheese}              => {whole milk}               0.007524148  0.4404762 1.7238692    74
    ## [45]   {specialty bar}            => {soda}                     0.007219115  0.2639405 1.5136181    71
    ## [46]   {specialty bar}            => {rolls/buns}               0.005592272  0.2044610 1.1115940    55
    ## [47]   {specialty bar}            => {other vegetables}         0.005592272  0.2044610 1.0566861    55
    ## [48]   {specialty bar}            => {whole milk}               0.006507372  0.2379182 0.9311284    64
    ## [49]   {misc. beverages}          => {bottled water}            0.005287239  0.1863799 1.6863354    52
    ## [50]   {misc. beverages}          => {soda}                     0.007320793  0.2580645 1.4799210    72
    ## [51]   {misc. beverages}          => {other vegetables}         0.005592272  0.1971326 1.0188120    55
    ## [52]   {misc. beverages}          => {whole milk}               0.007015760  0.2473118 0.9678917    69
    ## [53]   {grapes}                   => {tropical fruit}           0.006100661  0.2727273 2.5991015    60
    ## [54]   {grapes}                   => {other vegetables}         0.009049314  0.4045455 2.0907538    89
    ## [55]   {grapes}                   => {whole milk}               0.007320793  0.3272727 1.2808306    72
    ## [56]   {cat food}                 => {yogurt}                   0.006202339  0.2663755 1.9094778    61
    ## [57]   {cat food}                 => {other vegetables}         0.006507372  0.2794760 1.4443753    64
    ## [58]   {cat food}                 => {whole milk}               0.008845958  0.3799127 1.4868448    87
    ## [59]   {specialty chocolate}      => {soda}                     0.006304016  0.2073579 1.1891338    62
    ## [60]   {specialty chocolate}      => {rolls/buns}               0.005592272  0.1839465 1.0000629    55
    ## [61]   {specialty chocolate}      => {other vegetables}         0.006100661  0.2006689 1.0370881    60
    ## [62]   {specialty chocolate}      => {whole milk}               0.008032537  0.2642140 1.0340410    79
    ## [63]   {meat}                     => {sausage}                  0.005287239  0.2047244 2.1790742    52
    ## [64]   {meat}                     => {root vegetables}          0.005083884  0.1968504 1.8059922    50
    ## [65]   {meat}                     => {soda}                     0.005490595  0.2125984 1.2191869    54
    ## [66]   {meat}                     => {yogurt}                   0.005287239  0.2047244 1.4675398    52
    ## [67]   {meat}                     => {rolls/buns}               0.006914082  0.2677165 1.4554959    68
    ## [68]   {meat}                     => {other vegetables}         0.009964413  0.3858268 1.9940128    98
    ## [69]   {meat}                     => {whole milk}               0.009964413  0.3858268 1.5099906    98
    ## [70]   {frozen meals}             => {tropical fruit}           0.005490595  0.1935484 1.8445236    54
    ## [71]   {frozen meals}             => {soda}                     0.006202339  0.2186380 1.2538220    61
    ## [72]   {frozen meals}             => {yogurt}                   0.006202339  0.2186380 1.5672774    61
    ## [73]   {frozen meals}             => {other vegetables}         0.007524148  0.2652330 1.3707653    74
    ## [74]   {frozen meals}             => {whole milk}               0.009862735  0.3476703 1.3606593    97
    ## [75]   {hard cheese}              => {sausage}                  0.005185562  0.2116183 2.2524519    51
    ## [76]   {hard cheese}              => {root vegetables}          0.005592272  0.2282158 2.0937519    55
    ## [77]   {hard cheese}              => {yogurt}                   0.006405694  0.2614108 1.8738886    63
    ## [78]   {hard cheese}              => {rolls/buns}               0.005897306  0.2406639 1.3084187    58
    ## [79]   {hard cheese}              => {other vegetables}         0.009456024  0.3858921 1.9943505    93
    ## [80]   {hard cheese}              => {whole milk}               0.010066090  0.4107884 1.6076815    99
    ## [81]   {butter milk}              => {pip fruit}                0.005083884  0.1818182 2.4034702    50
    ## [82]   {butter milk}              => {tropical fruit}           0.005490595  0.1963636 1.8713531    54
    ## [83]   {butter milk}              => {root vegetables}          0.005083884  0.1818182 1.6680801    50
    ## [84]   {butter milk}              => {yogurt}                   0.008540925  0.3054545 2.1896104    84
    ## [85]   {butter milk}              => {rolls/buns}               0.007625826  0.2727273 1.4827378    75
    ## [86]   {butter milk}              => {other vegetables}         0.010371124  0.3709091 1.9169159   102
    ## [87]   {butter milk}              => {whole milk}               0.011591256  0.4145455 1.6223854   114
    ## [88]   {candy}                    => {tropical fruit}           0.005388917  0.1802721 1.7180002    53
    ## [89]   {candy}                    => {soda}                     0.008642603  0.2891156 1.6579897    85
    ## [90]   {candy}                    => {yogurt}                   0.005490595  0.1836735 1.3166389    54
    ## [91]   {candy}                    => {rolls/buns}               0.007117438  0.2380952 1.2944537    70
    ## [92]   {candy}                    => {other vegetables}         0.006914082  0.2312925 1.1953557    68
    ## [93]   {candy}                    => {whole milk}               0.008235892  0.2755102 1.0782502    81
    ## [94]   {ham}                      => {white bread}              0.005083884  0.1953125 4.6398513    50
    ## [95]   {white bread}              => {ham}                      0.005083884  0.1207729 4.6398513    50
    ## [96]   {ham}                      => {tropical fruit}           0.005388917  0.2070312 1.9730158    53
    ## [97]   {ham}                      => {yogurt}                   0.006710727  0.2578125 1.8480947    66
    ## [98]   {ham}                      => {rolls/buns}               0.006914082  0.2656250 1.4441249    68
    ## [99]   {ham}                      => {other vegetables}         0.009150991  0.3515625 1.8169297    90
    ## [100]  {ham}                      => {whole milk}               0.011489578  0.4414062 1.7275091   113
    ## [101]  {sliced cheese}            => {sausage}                  0.007015760  0.2863071 3.0474349    69
    ## [102]  {sliced cheese}            => {tropical fruit}           0.005287239  0.2157676 2.0562739    52
    ## [103]  {sliced cheese}            => {root vegetables}          0.005592272  0.2282158 2.0937519    55
    ## [104]  {sliced cheese}            => {soda}                     0.005083884  0.2074689 1.1897705    50
    ## [105]  {sliced cheese}            => {yogurt}                   0.008032537  0.3278008 2.3497968    79
    ## [106]  {sliced cheese}            => {rolls/buns}               0.007625826  0.3112033 1.6919208    75
    ## [107]  {sliced cheese}            => {other vegetables}         0.009049314  0.3692946 1.9085720    89
    ## [108]  {sliced cheese}            => {whole milk}               0.010777834  0.4398340 1.7213560   106
    ## [109]  {UHT-milk}                 => {bottled water}            0.007320793  0.2188450 1.9800740    72
    ## [110]  {UHT-milk}                 => {soda}                     0.007625826  0.2279635 1.3073010    75
    ## [111]  {UHT-milk}                 => {yogurt}                   0.007422471  0.2218845 1.5905496    73
    ## [112]  {UHT-milk}                 => {rolls/buns}               0.006405694  0.1914894 1.0410712    63
    ## [113]  {UHT-milk}                 => {other vegetables}         0.008134215  0.2431611 1.2566944    80
    ## [114]  {oil}                      => {root vegetables}          0.007015760  0.2500000 2.2936101    69
    ## [115]  {oil}                      => {yogurt}                   0.005287239  0.1884058 1.3505620    52
    ## [116]  {oil}                      => {rolls/buns}               0.005083884  0.1811594 0.9849104    50
    ## [117]  {oil}                      => {other vegetables}         0.009964413  0.3550725 1.8350697    98
    ## [118]  {oil}                      => {whole milk}               0.011286223  0.4021739 1.5739675   111
    ## [119]  {onions}                   => {whipped/sour cream}       0.005083884  0.1639344 2.2869434    50
    ## [120]  {onions}                   => {citrus fruit}             0.005592272  0.1803279 2.1787771    55
    ## [121]  {onions}                   => {bottled water}            0.005897306  0.1901639 1.7205725    58
    ## [122]  {onions}                   => {tropical fruit}           0.005693950  0.1836066 1.7497776    56
    ## [123]  {onions}                   => {root vegetables}          0.009456024  0.3049180 2.7974523    93
    ## [124]  {onions}                   => {soda}                     0.005287239  0.1704918 0.9777183    52
    ## [125]  {onions}                   => {yogurt}                   0.007219115  0.2327869 1.6687019    71
    ## [126]  {onions}                   => {rolls/buns}               0.006812405  0.2196721 1.1942927    67
    ## [127]  {onions}                   => {other vegetables}         0.014234875  0.4590164 2.3722681   140
    ## [128]  {onions}                   => {whole milk}               0.012099644  0.3901639 1.5269647   119
    ## [129]  {berries}                  => {whipped/sour cream}       0.009049314  0.2721713 3.7968855    89
    ## [130]  {whipped/sour cream}       => {berries}                  0.009049314  0.1262411 3.7968855    89
    ## [131]  {berries}                  => {citrus fruit}             0.005388917  0.1620795 1.9582948    53
    ## [132]  {berries}                  => {tropical fruit}           0.006710727  0.2018349 1.9234941    66
    ## [133]  {berries}                  => {root vegetables}          0.006609049  0.1987768 1.8236655    65
    ## [134]  {berries}                  => {soda}                     0.007320793  0.2201835 1.2626849    72
    ## [135]  {berries}                  => {yogurt}                   0.010574479  0.3180428 2.2798477   104
    ## [136]  {berries}                  => {rolls/buns}               0.006609049  0.1987768 1.0806907    65
    ## [137]  {berries}                  => {other vegetables}         0.010269446  0.3088685 1.5962805   101
    ## [138]  {berries}                  => {whole milk}               0.011794611  0.3547401 1.3883281   116
    ## [139]  {hamburger meat}           => {sausage}                  0.005185562  0.1559633 1.6600639    51
    ## [140]  {hamburger meat}           => {root vegetables}          0.006202339  0.1865443 1.7114399    61
    ## [141]  {hamburger meat}           => {soda}                     0.005795628  0.1743119 0.9996255    57
    ## [142]  {hamburger meat}           => {yogurt}                   0.006507372  0.1957187 1.4029832    64
    ## [143]  {hamburger meat}           => {rolls/buns}               0.008642603  0.2599388 1.4132109    85
    ## [144]  {hamburger meat}           => {other vegetables}         0.013828165  0.4159021 2.1494470   136
    ## [145]  {hamburger meat}           => {whole milk}               0.014743264  0.4434251 1.7354101   145
    ## [146]  {hygiene articles}         => {napkins}                  0.006100661  0.1851852 3.5364977    60
    ## [147]  {napkins}                  => {hygiene articles}         0.006100661  0.1165049 3.5364977    60
    ## [148]  {hygiene articles}         => {citrus fruit}             0.005287239  0.1604938 1.9391361    52
    ## [149]  {hygiene articles}         => {shopping bags}            0.005185562  0.1574074 1.5976283    51
    ## [150]  {hygiene articles}         => {bottled water}            0.005693950  0.1728395 1.5638239    56
    ## [151]  {hygiene articles}         => {tropical fruit}           0.006710727  0.2037037 1.9413042    66
    ## [152]  {hygiene articles}         => {root vegetables}          0.005388917  0.1635802 1.5007572    53
    ## [153]  {hygiene articles}         => {soda}                     0.007015760  0.2129630 1.2212774    69
    ## [154]  {hygiene articles}         => {yogurt}                   0.007320793  0.2222222 1.5929705    72
    ## [155]  {hygiene articles}         => {rolls/buns}               0.005897306  0.1790123 0.9732374    58
    ## [156]  {hygiene articles}         => {other vegetables}         0.009557702  0.2901235 1.4994032    94
    ## [157]  {hygiene articles}         => {whole milk}               0.012811388  0.3888889 1.5219746   126
    ## [158]  {salty snack}              => {fruit/vegetable juice}    0.005998983  0.1586022 2.1938849    59
    ## [159]  {salty snack}              => {whipped/sour cream}       0.005185562  0.1370968 1.9125486    51
    ## [160]  {salty snack}              => {pastry}                   0.005185562  0.1370968 1.5409677    51
    ## [161]  {salty snack}              => {shopping bags}            0.005998983  0.1586022 1.6097545    59
    ## [162]  {salty snack}              => {sausage}                  0.005287239  0.1397849 1.4878625    52
    ## [163]  {salty snack}              => {tropical fruit}           0.005592272  0.1478495 1.4090111    55
    ## [164]  {salty snack}              => {soda}                     0.009354347  0.2473118 1.4182576    92
    ## [165]  {salty snack}              => {yogurt}                   0.006202339  0.1639785 1.1754581    61
    ## [166]  {salty snack}              => {other vegetables}         0.010777834  0.2849462 1.4726465   106
    ## [167]  {salty snack}              => {whole milk}               0.011184545  0.2956989 1.1572618   110
    ## [168]  {sugar}                    => {margarine}                0.005490595  0.1621622 2.7688626    54
    ## [169]  {sugar}                    => {pastry}                   0.005185562  0.1531532 1.7214414    51
    ## [170]  {sugar}                    => {root vegetables}          0.006405694  0.1891892 1.7357049    63
    ## [171]  {sugar}                    => {soda}                     0.007320793  0.2162162 1.2399338    72
    ## [172]  {sugar}                    => {yogurt}                   0.006914082  0.2042042 1.4638107    68
    ## [173]  {sugar}                    => {rolls/buns}               0.007015760  0.2072072 1.1265245    69
    ## [174]  {sugar}                    => {other vegetables}         0.010777834  0.3183183 1.6451186   106
    ## [175]  {sugar}                    => {whole milk}               0.015048297  0.4444444 1.7393996   148
    ## [176]  {waffles}                  => {chocolate}                0.005795628  0.1507937 3.0390483    57
    ## [177]  {chocolate}                => {waffles}                  0.005795628  0.1168033 3.0390483    57
    ## [178]  {waffles}                  => {whipped/sour cream}       0.005083884  0.1322751 1.8452850    50
    ## [179]  {waffles}                  => {pastry}                   0.007015760  0.1825397 2.0517460    69
    ## [180]  {waffles}                  => {shopping bags}            0.005490595  0.1428571 1.4499484    54
    ## [181]  {waffles}                  => {tropical fruit}           0.006100661  0.1587302 1.5127046    60
    ## [182]  {waffles}                  => {root vegetables}          0.006609049  0.1719577 1.5776154    65
    ## [183]  {waffles}                  => {soda}                     0.009557702  0.2486772 1.4260879    94
    ## [184]  {waffles}                  => {yogurt}                   0.007524148  0.1957672 1.4033312    74
    ## [185]  {waffles}                  => {rolls/buns}               0.009150991  0.2380952 1.2944537    90
    ## [186]  {waffles}                  => {other vegetables}         0.010066090  0.2619048 1.3535645    99
    ## [187]  {waffles}                  => {whole milk}               0.012709710  0.3306878 1.2941961   125
    ## [188]  {long life bakery product} => {chocolate}                0.005287239  0.1413043 2.8478038    52
    ## [189]  {chocolate}                => {long life bakery product} 0.005287239  0.1065574 2.8478038    52
    ## [190]  {long life bakery product} => {fruit/vegetable juice}    0.006202339  0.1657609 2.2929088    61
    ## [191]  {long life bakery product} => {whipped/sour cream}       0.005795628  0.1548913 2.1607886    57
    ## [192]  {long life bakery product} => {pastry}                   0.005897306  0.1576087 1.7715217    58
    ## [193]  {long life bakery product} => {shopping bags}            0.005388917  0.1440217 1.4617686    53
    ## [194]  {long life bakery product} => {sausage}                  0.005388917  0.1440217 1.5329587    53
    ## [195]  {long life bakery product} => {tropical fruit}           0.006304016  0.1684783 1.6056044    62
    ## [196]  {long life bakery product} => {root vegetables}          0.005287239  0.1413043 1.2963883    52
    ## [197]  {long life bakery product} => {soda}                     0.007625826  0.2038043 1.1687555    75
    ## [198]  {long life bakery product} => {yogurt}                   0.008744281  0.2336957 1.6752163    86
    ## [199]  {long life bakery product} => {rolls/buns}               0.007930859  0.2119565 1.1523452    78
    ## [200]  {long life bakery product} => {other vegetables}         0.010676157  0.2853261 1.4746096   105
    ## [201]  {long life bakery product} => {whole milk}               0.013523132  0.3614130 1.4144438   133
    ## [202]  {dessert}                  => {curd}                     0.005185562  0.1397260 2.6225295    51
    ## [203]  {dessert}                  => {fruit/vegetable juice}    0.005998983  0.1616438 2.2359594    59
    ## [204]  {dessert}                  => {pastry}                   0.005388917  0.1452055 1.6321096    53
    ## [205]  {dessert}                  => {shopping bags}            0.006202339  0.1671233 1.6962410    61
    ## [206]  {dessert}                  => {sausage}                  0.005897306  0.1589041 1.6913657    58
    ## [207]  {dessert}                  => {bottled water}            0.005185562  0.1397260 1.2642185    51
    ## [208]  {dessert}                  => {tropical fruit}           0.006304016  0.1698630 1.6188011    62
    ## [209]  {dessert}                  => {root vegetables}          0.005795628  0.1561644 1.4327208    57
    ## [210]  {dessert}                  => {soda}                     0.009862735  0.2657534 1.5240145    97
    ## [211]  {dessert}                  => {yogurt}                   0.009862735  0.2657534 1.9050182    97
    ## [212]  {dessert}                  => {rolls/buns}               0.006812405  0.1835616 0.9979706    67
    ## [213]  {dessert}                  => {other vegetables}         0.011591256  0.3123288 1.6141636   114
    ## [214]  {dessert}                  => {whole milk}               0.013726487  0.3698630 1.4475140   135
    ## [215]  {canned beer}              => {shopping bags}            0.011387900  0.1465969 1.4879052   112
    ## [216]  {shopping bags}            => {canned beer}              0.011387900  0.1155831 1.4879052   112
    ## [217]  {canned beer}              => {bottled water}            0.008032537  0.1034031 0.9355749    79
    ## [218]  {canned beer}              => {soda}                     0.013828165  0.1780105 1.0208356   136
    ## [219]  {canned beer}              => {rolls/buns}               0.011286223  0.1452880 0.7898878   111
    ## [220]  {canned beer}              => {other vegetables}         0.009049314  0.1164921 0.6020495    89
    ## [221]  {canned beer}              => {whole milk}               0.008845958  0.1138743 0.4456642    87
    ## [222]  {cream cheese}             => {curd}                     0.005083884  0.1282051 2.4062928    50
    ## [223]  {cream cheese}             => {domestic eggs}            0.005083884  0.1282051 2.0206690    50
    ## [224]  {cream cheese}             => {fruit/vegetable juice}    0.005693950  0.1435897 1.9862238    56
    ## [225]  {cream cheese}             => {whipped/sour cream}       0.006405694  0.1615385 2.2535188    63
    ## [226]  {cream cheese}             => {pip fruit}                0.006100661  0.1538462 2.0337055    60
    ## [227]  {cream cheese}             => {citrus fruit}             0.005693950  0.1435897 1.7348957    56
    ## [228]  {cream cheese}             => {shopping bags}            0.005592272  0.1410256 1.4313593    55
    ## [229]  {cream cheese}             => {sausage}                  0.005592272  0.1410256 1.5010684    55
    ## [230]  {cream cheese}             => {bottled water}            0.005897306  0.1487179 1.3455759    58
    ## [231]  {cream cheese}             => {tropical fruit}           0.007219115  0.1820513 1.7349558    71
    ## [232]  {cream cheese}             => {root vegetables}          0.007524148  0.1897436 1.7407912    74
    ## [233]  {cream cheese}             => {soda}                     0.006812405  0.1717949 0.9851910    67
    ## [234]  {cream cheese}             => {yogurt}                   0.012404677  0.3128205 2.2424123   122
    ## [235]  {cream cheese}             => {rolls/buns}               0.009964413  0.2512821 1.3661465    98
    ## [236]  {cream cheese}             => {other vegetables}         0.013726487  0.3461538 1.7889769   135
    ## [237]  {cream cheese}             => {whole milk}               0.016471784  0.4153846 1.6256696   162
    ## [238]  {chicken}                  => {frozen vegetables}        0.006710727  0.1563981 3.2519564    66
    ## [239]  {frozen vegetables}        => {chicken}                  0.006710727  0.1395349 3.2519564    66
    ## [240]  {chicken}                  => {pork}                     0.005795628  0.1350711 2.3428998    57
    ## [241]  {pork}                     => {chicken}                  0.005795628  0.1005291 2.3428998    57
    ## [242]  {chicken}                  => {butter}                   0.005795628  0.1350711 2.4374755    57
    ## [243]  {butter}                   => {chicken}                  0.005795628  0.1045872 2.4374755    57
    ## [244]  {chicken}                  => {newspapers}               0.005185562  0.1208531 1.5141274    51
    ## [245]  {chicken}                  => {domestic eggs}            0.006202339  0.1445498 2.2782803    61
    ## [246]  {chicken}                  => {whipped/sour cream}       0.007219115  0.1682464 2.3470976    71
    ## [247]  {whipped/sour cream}       => {chicken}                  0.007219115  0.1007092 2.3470976    71
    ## [248]  {chicken}                  => {citrus fruit}             0.006914082  0.1611374 1.9469124    68
    ## [249]  {chicken}                  => {sausage}                  0.005287239  0.1232227 1.3115755    52
    ## [250]  {chicken}                  => {bottled water}            0.005287239  0.1232227 1.1148995    52
    ## [251]  {chicken}                  => {tropical fruit}           0.006405694  0.1492891 1.4227309    63
    ## [252]  {chicken}                  => {root vegetables}          0.010879512  0.2535545 2.3262206   107
    ## [253]  {chicken}                  => {soda}                     0.008337570  0.1943128 1.1143244    82
    ## [254]  {chicken}                  => {yogurt}                   0.008337570  0.1943128 1.3929055    82
    ## [255]  {chicken}                  => {rolls/buns}               0.009659380  0.2251185 1.2239029    95
    ## [256]  {chicken}                  => {other vegetables}         0.017895272  0.4170616 2.1554393   176
    ## [257]  {chicken}                  => {whole milk}               0.017590239  0.4099526 1.6044106   173
    ## [258]  {white bread}              => {frankfurter}              0.005185562  0.1231884 2.0888931    51
    ## [259]  {white bread}              => {domestic eggs}            0.005795628  0.1376812 2.1700228    57
    ## [260]  {white bread}              => {fruit/vegetable juice}    0.007422471  0.1763285 2.4390869    73
    ## [261]  {fruit/vegetable juice}    => {white bread}              0.007422471  0.1026723 2.4390869    73
    ## [262]  {white bread}              => {whipped/sour cream}       0.005490595  0.1304348 1.8196115    54
    ## [263]  {white bread}              => {pip fruit}                0.006609049  0.1570048 2.0754604    65
    ## [264]  {white bread}              => {pastry}                   0.005592272  0.1328502 1.4932367    55
    ## [265]  {white bread}              => {shopping bags}            0.007422471  0.1763285 1.7896706    73
    ## [266]  {white bread}              => {sausage}                  0.007219115  0.1714976 1.8254099    71
    ## [267]  {white bread}              => {tropical fruit}           0.008744281  0.2077295 1.9796699    86
    ## [268]  {white bread}              => {root vegetables}          0.007930859  0.1884058 1.7285177    78
    ## [269]  {white bread}              => {soda}                     0.010269446  0.2439614 1.3990437   101
    ## [270]  {white bread}              => {yogurt}                   0.009049314  0.2149758 1.5410258    89
    ## [271]  {white bread}              => {rolls/buns}               0.006507372  0.1545894 0.8404569    64
    ## [272]  {white bread}              => {other vegetables}         0.013726487  0.3260870 1.6852681   135
    ## [273]  {white bread}              => {whole milk}               0.017081851  0.4057971 1.5881474   168
    ## [274]  {chocolate}                => {butter}                   0.006202339  0.1250000 2.2557339    61
    ## [275]  {butter}                   => {chocolate}                0.006202339  0.1119266 2.2557339    61
    ## [276]  {chocolate}                => {newspapers}               0.005490595  0.1106557 1.3863684    54
    ## [277]  {chocolate}                => {fruit/vegetable juice}    0.006812405  0.1372951 1.8991521    67
    ## [278]  {chocolate}                => {pip fruit}                0.006100661  0.1229508 1.6252975    60
    ## [279]  {chocolate}                => {pastry}                   0.008032537  0.1618852 1.8195902    79
    ## [280]  {chocolate}                => {citrus fruit}             0.006405694  0.1290984 1.5598064    63
    ## [281]  {chocolate}                => {shopping bags}            0.008134215  0.1639344 1.6638752    80
    ## [282]  {chocolate}                => {sausage}                  0.006609049  0.1331967 1.4177378    65
    ## [283]  {chocolate}                => {bottled water}            0.005795628  0.1168033 1.0568172    57
    ## [284]  {chocolate}                => {tropical fruit}           0.008134215  0.1639344 1.5623014    80
    ## [285]  {chocolate}                => {root vegetables}          0.006405694  0.1290984 1.1844052    63
    ## [286]  {chocolate}                => {soda}                     0.013523132  0.2725410 1.5629391   133
    ## [287]  {chocolate}                => {yogurt}                   0.009252669  0.1864754 1.3367242    91
    ## [288]  {chocolate}                => {rolls/buns}               0.011794611  0.2377049 1.2923316   116
    ## [289]  {chocolate}                => {other vegetables}         0.012709710  0.2561475 1.3238103   125
    ## [290]  {chocolate}                => {whole milk}               0.016675140  0.3360656 1.3152427   164
    ## [291]  {coffee}                   => {fruit/vegetable juice}    0.005998983  0.1033275 1.4292910    59
    ## [292]  {coffee}                   => {whipped/sour cream}       0.006100661  0.1050788 1.4658866    60
    ## [293]  {coffee}                   => {pip fruit}                0.006914082  0.1190893 1.5742519    68
    ## [294]  {coffee}                   => {pastry}                   0.006914082  0.1190893 1.3385639    68
    ## [295]  {coffee}                   => {citrus fruit}             0.006405694  0.1103327 1.3330744    63
    ## [296]  {coffee}                   => {shopping bags}            0.009354347  0.1611208 1.6353183    92
    ## [297]  {coffee}                   => {sausage}                  0.006914082  0.1190893 1.2675795    68
    ## [298]  {coffee}                   => {bottled water}            0.007320793  0.1260946 1.1408833    72
    ## [299]  {coffee}                   => {tropical fruit}           0.007117438  0.1225919 1.1683060    70
    ## [300]  {coffee}                   => {root vegetables}          0.007320793  0.1260946 1.1568471    72
    ## [301]  {coffee}                   => {soda}                     0.009964413  0.1716287 0.9842382    98
    ## [302]  {coffee}                   => {yogurt}                   0.009761057  0.1681261 1.2051896    96
    ## [303]  {coffee}                   => {rolls/buns}               0.010981190  0.1891419 1.0283085   108
    ## [304]  {coffee}                   => {other vegetables}         0.013421454  0.2311734 1.1947400   132
    ## [305]  {coffee}                   => {whole milk}               0.018708693  0.3222417 1.2611408   184
    ## [306]  {frozen vegetables}        => {pork}                     0.006405694  0.1331924 2.3103124    63
    ## [307]  {pork}                     => {frozen vegetables}        0.006405694  0.1111111 2.3103124    63
    ## [308]  {frozen vegetables}        => {frankfurter}              0.005083884  0.1057082 1.7924838    50
    ## [309]  {frozen vegetables}        => {margarine}                0.005083884  0.1057082 1.8049316    50
    ## [310]  {frozen vegetables}        => {butter}                   0.005795628  0.1205074 2.1746611    57
    ## [311]  {butter}                   => {frozen vegetables}        0.005795628  0.1045872 2.1746611    57
    ## [312]  {frozen vegetables}        => {domestic eggs}            0.005185562  0.1078224 1.6994125    51
    ## [313]  {frozen vegetables}        => {fruit/vegetable juice}    0.007829181  0.1627907 2.2518235    77
    ## [314]  {fruit/vegetable juice}    => {frozen vegetables}        0.007829181  0.1082982 2.2518235    77
    ## [315]  {frozen vegetables}        => {whipped/sour cream}       0.007930859  0.1649049 2.3004813    78
    ## [316]  {whipped/sour cream}       => {frozen vegetables}        0.007930859  0.1106383 2.3004813    78
    ## [317]  {frozen vegetables}        => {pip fruit}                0.007320793  0.1522199 2.0122076    72
    ## [318]  {frozen vegetables}        => {citrus fruit}             0.006609049  0.1374207 1.6603597    65
    ## [319]  {frozen vegetables}        => {sausage}                  0.005998983  0.1247357 1.3276795    59
    ## [320]  {frozen vegetables}        => {bottled water}            0.006202339  0.1289641 1.1668459    61
    ## [321]  {frozen vegetables}        => {tropical fruit}           0.008744281  0.1818182 1.7327343    86
    ## [322]  {frozen vegetables}        => {root vegetables}          0.011591256  0.2410148 2.2111759   114
    ## [323]  {root vegetables}          => {frozen vegetables}        0.011591256  0.1063433 2.2111759   114
    ## [324]  {frozen vegetables}        => {soda}                     0.008642603  0.1797040 1.0305475    85
    ## [325]  {frozen vegetables}        => {yogurt}                   0.012404677  0.2579281 1.8489235   122
    ## [326]  {frozen vegetables}        => {rolls/buns}               0.010167768  0.2114165 1.1494092   100
    ## [327]  {frozen vegetables}        => {other vegetables}         0.017793594  0.3699789 1.9121083   175
    ## [328]  {frozen vegetables}        => {whole milk}               0.020437214  0.4249471 1.6630940   201
    ## [329]  {beef}                     => {pork}                     0.007625826  0.1453488 2.5211743    75
    ## [330]  {pork}                     => {beef}                     0.007625826  0.1322751 2.5211743    75
    ## [331]  {beef}                     => {margarine}                0.006202339  0.1182171 2.0185152    61
    ## [332]  {margarine}                => {beef}                     0.006202339  0.1059028 2.0185152    61
    ## [333]  {beef}                     => {butter}                   0.005795628  0.1104651 1.9934393    57
    ## [334]  {butter}                   => {beef}                     0.005795628  0.1045872 1.9934393    57
    ## [335]  {beef}                     => {newspapers}               0.006405694  0.1220930 1.5296623    63
    ## [336]  {beef}                     => {domestic eggs}            0.005998983  0.1143411 1.8021548    59
    ## [337]  {beef}                     => {whipped/sour cream}       0.006710727  0.1279070 1.7843477    66
    ## [338]  {beef}                     => {pastry}                   0.006304016  0.1201550 1.3505426    62
    ## [339]  {beef}                     => {citrus fruit}             0.008439248  0.1608527 1.9434723    83
    ## [340]  {citrus fruit}             => {beef}                     0.008439248  0.1019656 1.9434723    83
    ## [341]  {beef}                     => {sausage}                  0.005592272  0.1065891 1.1345284    55
    ## [342]  {beef}                     => {bottled water}            0.006202339  0.1182171 1.0696088    61
    ## [343]  {beef}                     => {tropical fruit}           0.007625826  0.1453488 1.3851801    75
    ## [344]  {beef}                     => {root vegetables}          0.017386884  0.3313953 3.0403668   171
    ## [345]  {root vegetables}          => {beef}                     0.017386884  0.1595149 3.0403668   171
    ## [346]  {beef}                     => {soda}                     0.008134215  0.1550388 0.8890998    80
    ## [347]  {beef}                     => {yogurt}                   0.011692933  0.2228682 1.5976012   115
    ## [348]  {beef}                     => {rolls/buns}               0.013624809  0.2596899 1.4118576   134
    ## [349]  {beef}                     => {other vegetables}         0.019725470  0.3759690 1.9430662   194
    ## [350]  {other vegetables}         => {beef}                     0.019725470  0.1019443 1.9430662   194
    ## [351]  {beef}                     => {whole milk}               0.021250635  0.4050388 1.5851795   209
    ## [352]  {curd}                     => {margarine}                0.006304016  0.1183206 2.0202833    62
    ## [353]  {margarine}                => {curd}                     0.006304016  0.1076389 2.0202833    62
    ## [354]  {curd}                     => {butter}                   0.006812405  0.1278626 2.3073920    67
    ## [355]  {butter}                   => {curd}                     0.006812405  0.1229358 2.3073920    67
    ## [356]  {curd}                     => {newspapers}               0.005693950  0.1068702 1.3389410    56
    ## [357]  {curd}                     => {domestic eggs}            0.006507372  0.1221374 1.9250343    64
    ## [358]  {domestic eggs}            => {curd}                     0.006507372  0.1025641 1.9250343    64
    ## [359]  {curd}                     => {whipped/sour cream}       0.010472801  0.1965649 2.7421499   103
    ## [360]  {whipped/sour cream}       => {curd}                     0.010472801  0.1460993 2.7421499   103
    ## [361]  {curd}                     => {pip fruit}                0.007829181  0.1469466 1.9424993    77
    ## [362]  {pip fruit}                => {curd}                     0.007829181  0.1034946 1.9424993    77
    ## [363]  {curd}                     => {pastry}                   0.007524148  0.1412214 1.5873282    74
    ## [364]  {curd}                     => {citrus fruit}             0.007117438  0.1335878 1.6140490    70
    ## [365]  {curd}                     => {shopping bags}            0.005388917  0.1011450 1.0265856    53
    ## [366]  {curd}                     => {sausage}                  0.007625826  0.1431298 1.5234646    75
    ## [367]  {curd}                     => {bottled water}            0.006100661  0.1145038 1.0360120    60
    ## [368]  {curd}                     => {tropical fruit}           0.010269446  0.1927481 1.8368968   101
    ## [369]  {curd}                     => {root vegetables}          0.010879512  0.2041985 1.8734067   107
    ## [370]  {curd}                     => {soda}                     0.008134215  0.1526718 0.8755258    80
    ## [371]  {curd}                     => {yogurt}                   0.017285206  0.3244275 2.3256154   170
    ## [372]  {yogurt}                   => {curd}                     0.017285206  0.1239067 2.3256154   170
    ## [373]  {curd}                     => {rolls/buns}               0.010066090  0.1889313 1.0271638    99
    ## [374]  {curd}                     => {other vegetables}         0.017183528  0.3225191 1.6668288   169
    ## [375]  {curd}                     => {whole milk}               0.026131164  0.4904580 1.9194805   257
    ## [376]  {whole milk}               => {curd}                     0.026131164  0.1022682 1.9194805   257
    ## [377]  {napkins}                  => {newspapers}               0.006202339  0.1184466 1.4839775    61
    ## [378]  {napkins}                  => {domestic eggs}            0.005998983  0.1145631 1.8056541    59
    ## [379]  {napkins}                  => {fruit/vegetable juice}    0.006914082  0.1320388 1.8264444    68
    ## [380]  {napkins}                  => {whipped/sour cream}       0.007219115  0.1378641 1.9232528    71
    ## [381]  {whipped/sour cream}       => {napkins}                  0.007219115  0.1007092 1.9232528    71
    ## [382]  {napkins}                  => {pip fruit}                0.006710727  0.1281553 1.6940965    66
    ## [383]  {napkins}                  => {pastry}                   0.007015760  0.1339806 1.5059417    69
    ## [384]  {napkins}                  => {citrus fruit}             0.007625826  0.1456311 1.7595596    75
    ## [385]  {napkins}                  => {shopping bags}            0.007219115  0.1378641 1.3992706    71
    ## [386]  {napkins}                  => {sausage}                  0.006710727  0.1281553 1.3640777    66
    ## [387]  {napkins}                  => {bottled water}            0.008642603  0.1650485 1.4933325    85
    ## [388]  {napkins}                  => {tropical fruit}           0.010066090  0.1922330 1.8319880    99
    ## [389]  {napkins}                  => {root vegetables}          0.009964413  0.1902913 1.7458158    98
    ## [390]  {napkins}                  => {soda}                     0.011997966  0.2291262 1.3139687   118
    ## [391]  {napkins}                  => {yogurt}                   0.012302999  0.2349515 1.6842183   121
    ## [392]  {napkins}                  => {rolls/buns}               0.011692933  0.2233010 1.2140216   115
    ## [393]  {napkins}                  => {other vegetables}         0.014438231  0.2757282 1.4250060   142
    ## [394]  {napkins}                  => {whole milk}               0.019725470  0.3766990 1.4742678   194
    ## [395]  {pork}                     => {frankfurter}              0.005897306  0.1022928 1.7345679    58
    ## [396]  {frankfurter}              => {pork}                     0.005897306  0.1000000 1.7345679    58
    ## [397]  {pork}                     => {margarine}                0.006405694  0.1111111 1.8971836    63
    ## [398]  {margarine}                => {pork}                     0.006405694  0.1093750 1.8971836    63
    ## [399]  {pork}                     => {newspapers}               0.006609049  0.1146384 1.4362664    65
    ## [400]  {pork}                     => {whipped/sour cream}       0.008235892  0.1428571 1.9929078    81
    ## [401]  {whipped/sour cream}       => {pork}                     0.008235892  0.1148936 1.9929078    81
    ## [402]  {pork}                     => {pip fruit}                0.006100661  0.1058201 1.3988451    60
    ## [403]  {pork}                     => {pastry}                   0.006304016  0.1093474 1.2290653    62
    ## [404]  {pork}                     => {citrus fruit}             0.006507372  0.1128748 1.3637880    64
    ## [405]  {pork}                     => {shopping bags}            0.006405694  0.1111111 1.1277376    63
    ## [406]  {pork}                     => {sausage}                  0.006507372  0.1128748 1.2014323    64
    ## [407]  {pork}                     => {bottled water}            0.007422471  0.1287478 1.1648892    73
    ## [408]  {pork}                     => {tropical fruit}           0.008540925  0.1481481 1.4118576    84
    ## [409]  {pork}                     => {root vegetables}          0.013624809  0.2363316 2.1682099   134
    ## [410]  {root vegetables}          => {pork}                     0.013624809  0.1250000 2.1682099   134
    ## [411]  {pork}                     => {soda}                     0.011896289  0.2063492 1.1833495   117
    ## [412]  {pork}                     => {yogurt}                   0.009557702  0.1657848 1.1884066    94
    ## [413]  {pork}                     => {rolls/buns}               0.011286223  0.1957672 1.0643286   111
    ## [414]  {pork}                     => {other vegetables}         0.021657346  0.3756614 1.9414764   213
    ## [415]  {other vegetables}         => {pork}                     0.021657346  0.1119285 1.9414764   213
    ## [416]  {pork}                     => {whole milk}               0.022165735  0.3844797 1.5047187   218
    ## [417]  {frankfurter}              => {brown bread}              0.007117438  0.1206897 1.8604745    70
    ## [418]  {brown bread}              => {frankfurter}              0.007117438  0.1097179 1.8604745    70
    ## [419]  {frankfurter}              => {margarine}                0.006405694  0.1086207 1.8546606    63
    ## [420]  {margarine}                => {frankfurter}              0.006405694  0.1093750 1.8546606    63
    ## [421]  {frankfurter}              => {domestic eggs}            0.007015760  0.1189655 1.8750414    69
    ## [422]  {domestic eggs}            => {frankfurter}              0.007015760  0.1105769 1.8750414    69
    ## [423]  {frankfurter}              => {whipped/sour cream}       0.006202339  0.1051724 1.4671925    61
    ## [424]  {frankfurter}              => {pip fruit}                0.007219115  0.1224138 1.6181985    71
    ## [425]  {frankfurter}              => {pastry}                   0.008337570  0.1413793 1.5891034    82
    ## [426]  {frankfurter}              => {citrus fruit}             0.006507372  0.1103448 1.3332204    64
    ## [427]  {frankfurter}              => {shopping bags}            0.008235892  0.1396552 1.4174496    81
    ## [428]  {frankfurter}              => {sausage}                  0.010066090  0.1706897 1.8168103    99
    ## [429]  {sausage}                  => {frankfurter}              0.010066090  0.1071429 1.8168103    99
    ## [430]  {frankfurter}              => {bottled water}            0.007320793  0.1241379 1.1231799    72
    ## [431]  {frankfurter}              => {tropical fruit}           0.009456024  0.1603448 1.5280924    93
    ## [432]  {frankfurter}              => {root vegetables}          0.010167768  0.1724138 1.5818001   100
    ## [433]  {frankfurter}              => {soda}                     0.011286223  0.1913793 1.0975018   111
    ## [434]  {frankfurter}              => {yogurt}                   0.011184545  0.1896552 1.3595179   110
    ## [435]  {frankfurter}              => {rolls/buns}               0.019217082  0.3258621 1.7716161   189
    ## [436]  {rolls/buns}               => {frankfurter}              0.019217082  0.1044776 1.7716161   189
    ## [437]  {frankfurter}              => {other vegetables}         0.016471784  0.2793103 1.4435193   162
    ## [438]  {frankfurter}              => {whole milk}               0.020538892  0.3482759 1.3630295   202
    ## [439]  {margarine}                => {bottled beer}             0.006100661  0.1041667 1.2935343    60
    ## [440]  {butter}                   => {bottled beer}             0.005795628  0.1045872 1.2987559    57
    ## [441]  {bottled beer}             => {bottled water}            0.015760041  0.1957071 1.7707259   155
    ## [442]  {bottled water}            => {bottled beer}             0.015760041  0.1425943 1.7707259   155
    ## [443]  {bottled beer}             => {tropical fruit}           0.008235892  0.1022727 0.9746631    81
    ## [444]  {bottled beer}             => {root vegetables}          0.009659380  0.1199495 1.1004695    95
    ## [445]  {bottled beer}             => {soda}                     0.016980173  0.2108586 1.2092094   167
    ## [446]  {bottled beer}             => {yogurt}                   0.009252669  0.1148990 0.8236382    91
    ## [447]  {bottled beer}             => {rolls/buns}               0.013624809  0.1691919 0.9198466   134
    ## [448]  {bottled beer}             => {other vegetables}         0.016166751  0.2007576 1.0375464   159
    ## [449]  {bottled beer}             => {whole milk}               0.020437214  0.2537879 0.9932367   201
    ## [450]  {brown bread}              => {margarine}                0.006507372  0.1003135 1.7128178    64
    ## [451]  {margarine}                => {brown bread}              0.006507372  0.1111111 1.7128178    64
    ## [452]  {butter}                   => {brown bread}              0.005795628  0.1045872 1.6122487    57
    ## [453]  {brown bread}              => {newspapers}               0.007625826  0.1175549 1.4728051    75
    ## [454]  {brown bread}              => {domestic eggs}            0.006812405  0.1050157 1.6551749    67
    ## [455]  {domestic eggs}            => {brown bread}              0.006812405  0.1073718 1.6551749    67
    ## [456]  {brown bread}              => {fruit/vegetable juice}    0.008337570  0.1285266 1.7778615    82
    ## [457]  {fruit/vegetable juice}    => {brown bread}              0.008337570  0.1153305 1.7778615    82
    ## [458]  {brown bread}              => {pip fruit}                0.007625826  0.1175549 1.5539678    75
    ## [459]  {pip fruit}                => {brown bread}              0.007625826  0.1008065 1.5539678    75
    ## [460]  {brown bread}              => {pastry}                   0.009659380  0.1489028 1.6736677    95
    ## [461]  {pastry}                   => {brown bread}              0.009659380  0.1085714 1.6736677    95
    ## [462]  {brown bread}              => {citrus fruit}             0.008337570  0.1285266 1.5528987    82
    ## [463]  {citrus fruit}             => {brown bread}              0.008337570  0.1007371 1.5528987    82
    ## [464]  {brown bread}              => {shopping bags}            0.009252669  0.1426332 1.4476758    91
    ## [465]  {brown bread}              => {sausage}                  0.010676157  0.1645768 1.7517455   105
    ## [466]  {sausage}                  => {brown bread}              0.010676157  0.1136364 1.7517455   105
    ## [467]  {brown bread}              => {bottled water}            0.008235892  0.1269592 1.1487067    81
    ## [468]  {brown bread}              => {tropical fruit}           0.010676157  0.1645768 1.5684233   105
    ## [469]  {tropical fruit}           => {brown bread}              0.010676157  0.1017442 1.5684233   105
    ## [470]  {brown bread}              => {root vegetables}          0.010167768  0.1567398 1.4380000   100
    ## [471]  {brown bread}              => {soda}                     0.012608033  0.1943574 1.1145800   124
    ## [472]  {brown bread}              => {yogurt}                   0.014539908  0.2241379 1.6067030   143
    ## [473]  {yogurt}                   => {brown bread}              0.014539908  0.1042274 1.6067030   143
    ## [474]  {brown bread}              => {rolls/buns}               0.012608033  0.1943574 1.0566637   124
    ## [475]  {brown bread}              => {other vegetables}         0.018708693  0.2884013 1.4905025   184
    ## [476]  {brown bread}              => {whole milk}               0.025216065  0.3887147 1.5212930   248
    ## [477]  {margarine}                => {butter}                   0.006710727  0.1145833 2.0677561    66
    ## [478]  {butter}                   => {margarine}                0.006710727  0.1211009 2.0677561    66
    ## [479]  {margarine}                => {newspapers}               0.007117438  0.1215278 1.5225805    70
    ## [480]  {margarine}                => {domestic eggs}            0.008337570  0.1423611 2.2437845    82
    ## [481]  {domestic eggs}            => {margarine}                0.008337570  0.1314103 2.2437845    82
    ## [482]  {margarine}                => {fruit/vegetable juice}    0.006202339  0.1059028 1.4649140    61
    ## [483]  {margarine}                => {whipped/sour cream}       0.006812405  0.1163194 1.6226975    67
    ## [484]  {margarine}                => {pip fruit}                0.008540925  0.1458333 1.9277834    84
    ## [485]  {pip fruit}                => {margarine}                0.008540925  0.1129032 1.9277834    84
    ## [486]  {margarine}                => {pastry}                   0.006812405  0.1163194 1.3074306    67
    ## [487]  {margarine}                => {citrus fruit}             0.007930859  0.1354167 1.6361461    78
    ## [488]  {margarine}                => {sausage}                  0.007117438  0.1215278 1.2935343    70
    ## [489]  {margarine}                => {bottled water}            0.010269446  0.1753472 1.5865133   101
    ## [490]  {margarine}                => {tropical fruit}           0.009354347  0.1597222 1.5221590    92
    ## [491]  {margarine}                => {root vegetables}          0.011082867  0.1892361 1.7361354   109
    ## [492]  {root vegetables}          => {margarine}                0.011082867  0.1016791 1.7361354   109
    ## [493]  {margarine}                => {soda}                     0.010167768  0.1736111 0.9956066   100
    ## [494]  {margarine}                => {yogurt}                   0.014234875  0.2430556 1.7423115   140
    ## [495]  {yogurt}                   => {margarine}                0.014234875  0.1020408 1.7423115   140
    ## [496]  {margarine}                => {rolls/buns}               0.014743264  0.2517361 1.3686151   145
    ## [497]  {margarine}                => {other vegetables}         0.019725470  0.3368056 1.7406635   194
    ## [498]  {other vegetables}         => {margarine}                0.019725470  0.1019443 1.7406635   194
    ## [499]  {margarine}                => {whole milk}               0.024199288  0.4131944 1.6170980   238
    ## [500]  {butter}                   => {newspapers}               0.005795628  0.1045872 1.3103372    57
    ## [501]  {butter}                   => {domestic eggs}            0.009659380  0.1743119 2.7473683    95
    ## [502]  {domestic eggs}            => {butter}                   0.009659380  0.1522436 2.7473683    95
    ## [503]  {butter}                   => {fruit/vegetable juice}    0.008032537  0.1449541 2.0050968    79
    ## [504]  {fruit/vegetable juice}    => {butter}                   0.008032537  0.1111111 2.0050968    79
    ## [505]  {butter}                   => {whipped/sour cream}       0.010167768  0.1834862 2.5596981   100
    ## [506]  {whipped/sour cream}       => {butter}                   0.010167768  0.1418440 2.5596981   100
    ## [507]  {butter}                   => {pip fruit}                0.007320793  0.1321101 1.7463747    72
    ## [508]  {butter}                   => {pastry}                   0.007625826  0.1376147 1.5467890    75
    ## [509]  {butter}                   => {citrus fruit}             0.009150991  0.1651376 1.9952438    90
    ## [510]  {citrus fruit}             => {butter}                   0.009150991  0.1105651 1.9952438    90
    ## [511]  {butter}                   => {sausage}                  0.008642603  0.1559633 1.6600639    85
    ## [512]  {butter}                   => {bottled water}            0.008947636  0.1614679 1.4609353    88
    ## [513]  {butter}                   => {tropical fruit}           0.009964413  0.1798165 1.7136583    98
    ## [514]  {butter}                   => {root vegetables}          0.012913066  0.2330275 2.1378971   127
    ## [515]  {root vegetables}          => {butter}                   0.012913066  0.1184701 2.1378971   127
    ## [516]  {butter}                   => {soda}                     0.008845958  0.1596330 0.9154465    87
    ## [517]  {butter}                   => {yogurt}                   0.014641586  0.2642202 1.8940273   144
    ## [518]  {yogurt}                   => {butter}                   0.014641586  0.1049563 1.8940273   144
    ## [519]  {butter}                   => {rolls/buns}               0.013421454  0.2422018 1.3167800   132
    ## [520]  {butter}                   => {other vegetables}         0.020030503  0.3614679 1.8681223   197
    ## [521]  {other vegetables}         => {butter}                   0.020030503  0.1035208 1.8681223   197
    ## [522]  {butter}                   => {whole milk}               0.027554652  0.4972477 1.9460530   271
    ## [523]  {whole milk}               => {butter}                   0.027554652  0.1078392 1.9460530   271
    ## [524]  {domestic eggs}            => {newspapers}               0.006914082  0.1089744 1.3653030    68
    ## [525]  {newspapers}               => {fruit/vegetable juice}    0.008235892  0.1031847 1.4273160    81
    ## [526]  {fruit/vegetable juice}    => {newspapers}               0.008235892  0.1139241 1.4273160    81
    ## [527]  {whipped/sour cream}       => {newspapers}               0.007219115  0.1007092 1.2617518    71
    ## [528]  {newspapers}               => {pastry}                   0.008439248  0.1057325 1.1884331    83
    ## [529]  {newspapers}               => {citrus fruit}             0.008337570  0.1044586 1.2621011    82
    ## [530]  {citrus fruit}             => {newspapers}               0.008337570  0.1007371 1.2621011    82
    ## [531]  {newspapers}               => {sausage}                  0.008032537  0.1006369 1.0711735    79
    ## [532]  {newspapers}               => {bottled water}            0.011286223  0.1414013 1.2793758   111
    ## [533]  {bottled water}            => {newspapers}               0.011286223  0.1021159 1.2793758   111
    ## [534]  {newspapers}               => {tropical fruit}           0.011794611  0.1477707 1.4082605   116
    ## [535]  {tropical fruit}           => {newspapers}               0.011794611  0.1124031 1.4082605   116
    ## [536]  {newspapers}               => {root vegetables}          0.011489578  0.1439490 1.3206519   113
    ## [537]  {root vegetables}          => {newspapers}               0.011489578  0.1054104 1.3206519   113
    ## [538]  {newspapers}               => {soda}                     0.014641586  0.1834395 1.0519693   144
    ## [539]  {newspapers}               => {yogurt}                   0.015353330  0.1923567 1.3788834   151
    ## [540]  {yogurt}                   => {newspapers}               0.015353330  0.1100583 1.3788834   151
    ## [541]  {newspapers}               => {rolls/buns}               0.019725470  0.2471338 1.3435934   194
    ## [542]  {rolls/buns}               => {newspapers}               0.019725470  0.1072416 1.3435934   194
    ## [543]  {newspapers}               => {other vegetables}         0.019318760  0.2420382 1.2508912   190
    ## [544]  {newspapers}               => {whole milk}               0.027351296  0.3426752 1.3411103   269
    ## [545]  {whole milk}               => {newspapers}               0.027351296  0.1070434 1.3411103   269
    ## [546]  {domestic eggs}            => {fruit/vegetable juice}    0.008032537  0.1266026 1.7512464    79
    ## [547]  {fruit/vegetable juice}    => {domestic eggs}            0.008032537  0.1111111 1.7512464    79
    ## [548]  {domestic eggs}            => {whipped/sour cream}       0.009964413  0.1570513 2.1909211    98
    ## [549]  {whipped/sour cream}       => {domestic eggs}            0.009964413  0.1390071 2.1909211    98
    ## [550]  {domestic eggs}            => {pip fruit}                0.008642603  0.1362179 1.8006768    85
    ## [551]  {pip fruit}                => {domestic eggs}            0.008642603  0.1142473 1.8006768    85
    ## [552]  {domestic eggs}            => {pastry}                   0.009049314  0.1426282 1.6031410    89
    ## [553]  {pastry}                   => {domestic eggs}            0.009049314  0.1017143 1.6031410    89
    ## [554]  {domestic eggs}            => {citrus fruit}             0.010371124  0.1634615 1.9749929   102
    ## [555]  {citrus fruit}             => {domestic eggs}            0.010371124  0.1253071 1.9749929   102
    ## [556]  {domestic eggs}            => {shopping bags}            0.009049314  0.1426282 1.4476248    89
    ## [557]  {domestic eggs}            => {sausage}                  0.009557702  0.1506410 1.6034139    94
    ## [558]  {sausage}                  => {domestic eggs}            0.009557702  0.1017316 1.6034139    94
    ## [559]  {domestic eggs}            => {bottled water}            0.009150991  0.1442308 1.3049766    90
    ## [560]  {domestic eggs}            => {tropical fruit}           0.011387900  0.1794872 1.7105198   112
    ## [561]  {tropical fruit}           => {domestic eggs}            0.011387900  0.1085271 1.7105198   112
    ## [562]  {domestic eggs}            => {root vegetables}          0.014336553  0.2259615 2.0730706   141
    ## [563]  {root vegetables}          => {domestic eggs}            0.014336553  0.1315299 2.0730706   141
    ## [564]  {domestic eggs}            => {soda}                     0.012404677  0.1955128 1.1212062   122
    ## [565]  {domestic eggs}            => {yogurt}                   0.014336553  0.2259615 1.6197753   141
    ## [566]  {yogurt}                   => {domestic eggs}            0.014336553  0.1027697 1.6197753   141
    ## [567]  {domestic eggs}            => {rolls/buns}               0.015658363  0.2467949 1.3417510   154
    ## [568]  {domestic eggs}            => {other vegetables}         0.022267412  0.3509615 1.8138238   219
    ## [569]  {other vegetables}         => {domestic eggs}            0.022267412  0.1150815 1.8138238   219
    ## [570]  {domestic eggs}            => {whole milk}               0.029994916  0.4727564 1.8502027   295
    ## [571]  {whole milk}               => {domestic eggs}            0.029994916  0.1173896 1.8502027   295
    ## [572]  {fruit/vegetable juice}    => {whipped/sour cream}       0.009049314  0.1251758 1.7462469    89
    ## [573]  {whipped/sour cream}       => {fruit/vegetable juice}    0.009049314  0.1262411 1.7462469    89
    ## [574]  {fruit/vegetable juice}    => {pip fruit}                0.009557702  0.1322082 1.7476710    94
    ## [575]  {pip fruit}                => {fruit/vegetable juice}    0.009557702  0.1263441 1.7476710    94
    ## [576]  {fruit/vegetable juice}    => {pastry}                   0.008540925  0.1181435 1.3279325    84
    ## [577]  {fruit/vegetable juice}    => {citrus fruit}             0.010371124  0.1434599 1.7333271   102
    ## [578]  {citrus fruit}             => {fruit/vegetable juice}    0.010371124  0.1253071 1.7333271   102
    ## [579]  {fruit/vegetable juice}    => {shopping bags}            0.010676157  0.1476793 1.4988918   105
    ## [580]  {shopping bags}            => {fruit/vegetable juice}    0.010676157  0.1083591 1.4988918   105
    ## [581]  {fruit/vegetable juice}    => {sausage}                  0.010066090  0.1392405 1.4820675    99
    ## [582]  {sausage}                  => {fruit/vegetable juice}    0.010066090  0.1071429 1.4820675    99
    ## [583]  {fruit/vegetable juice}    => {bottled water}            0.014234875  0.1969058 1.7815715   140
    ## [584]  {bottled water}            => {fruit/vegetable juice}    0.014234875  0.1287948 1.7815715   140
    ## [585]  {fruit/vegetable juice}    => {tropical fruit}           0.013726487  0.1898734 1.8095010   135
    ## [586]  {tropical fruit}           => {fruit/vegetable juice}    0.013726487  0.1308140 1.8095010   135
    ## [587]  {fruit/vegetable juice}    => {root vegetables}          0.011997966  0.1659634 1.5226216   118
    ## [588]  {root vegetables}          => {fruit/vegetable juice}    0.011997966  0.1100746 1.5226216   118
    ## [589]  {fruit/vegetable juice}    => {soda}                     0.018403660  0.2545710 1.4598869   181
    ## [590]  {soda}                     => {fruit/vegetable juice}    0.018403660  0.1055394 1.4598869   181
    ## [591]  {fruit/vegetable juice}    => {yogurt}                   0.018708693  0.2587904 1.8551049   184
    ## [592]  {yogurt}                   => {fruit/vegetable juice}    0.018708693  0.1341108 1.8551049   184
    ## [593]  {fruit/vegetable juice}    => {rolls/buns}               0.014539908  0.2011252 1.0934583   143
    ## [594]  {fruit/vegetable juice}    => {other vegetables}         0.021047280  0.2911392 1.5046529   207
    ## [595]  {other vegetables}         => {fruit/vegetable juice}    0.021047280  0.1087756 1.5046529   207
    ## [596]  {fruit/vegetable juice}    => {whole milk}               0.026639553  0.3684951 1.4421604   262
    ## [597]  {whole milk}               => {fruit/vegetable juice}    0.026639553  0.1042579 1.4421604   262
    ## [598]  {whipped/sour cream}       => {pip fruit}                0.009252669  0.1290780 1.7062934    91
    ## [599]  {pip fruit}                => {whipped/sour cream}       0.009252669  0.1223118 1.7062934    91
    ## [600]  {whipped/sour cream}       => {pastry}                   0.007524148  0.1049645 1.1798014    74
    ## [601]  {whipped/sour cream}       => {citrus fruit}             0.010879512  0.1517730 1.8337690   107
    ## [602]  {citrus fruit}             => {whipped/sour cream}       0.010879512  0.1314496 1.8337690   107
    ## [603]  {whipped/sour cream}       => {shopping bags}            0.007930859  0.1106383 1.1229388    78
    ## [604]  {whipped/sour cream}       => {sausage}                  0.009049314  0.1262411 1.3437030    89
    ## [605]  {whipped/sour cream}       => {bottled water}            0.008744281  0.1219858 1.1037079    86
    ## [606]  {whipped/sour cream}       => {tropical fruit}           0.013828165  0.1929078 1.8384188   136
    ## [607]  {tropical fruit}           => {whipped/sour cream}       0.013828165  0.1317829 1.8384188   136
    ## [608]  {whipped/sour cream}       => {root vegetables}          0.017081851  0.2382979 2.1862496   168
    ## [609]  {root vegetables}          => {whipped/sour cream}       0.017081851  0.1567164 2.1862496   168
    ## [610]  {whipped/sour cream}       => {soda}                     0.011591256  0.1617021 0.9273122   114
    ## [611]  {whipped/sour cream}       => {yogurt}                   0.020742247  0.2893617 2.0742510   204
    ## [612]  {yogurt}                   => {whipped/sour cream}       0.020742247  0.1486880 2.0742510   204
    ## [613]  {whipped/sour cream}       => {rolls/buns}               0.014641586  0.2042553 1.1104760   144
    ## [614]  {whipped/sour cream}       => {other vegetables}         0.028876462  0.4028369 2.0819237   284
    ## [615]  {other vegetables}         => {whipped/sour cream}       0.028876462  0.1492380 2.0819237   284
    ## [616]  {whipped/sour cream}       => {whole milk}               0.032231825  0.4496454 1.7597542   317
    ## [617]  {whole milk}               => {whipped/sour cream}       0.032231825  0.1261441 1.7597542   317
    ## [618]  {pip fruit}                => {pastry}                   0.010676157  0.1411290 1.5862903   105
    ## [619]  {pastry}                   => {pip fruit}                0.010676157  0.1200000 1.5862903   105
    ## [620]  {pip fruit}                => {citrus fruit}             0.013828165  0.1827957 2.2085942   136
    ## [621]  {citrus fruit}             => {pip fruit}                0.013828165  0.1670762 2.2085942   136
    ## [622]  {pip fruit}                => {shopping bags}            0.009354347  0.1236559 1.2550629    92
    ## [623]  {pip fruit}                => {sausage}                  0.010777834  0.1424731 1.5164752   106
    ## [624]  {sausage}                  => {pip fruit}                0.010777834  0.1147186 1.5164752   106
    ## [625]  {pip fruit}                => {bottled water}            0.010574479  0.1397849 1.2647516   104
    ## [626]  {pip fruit}                => {tropical fruit}           0.020437214  0.2701613 2.5746476   201
    ## [627]  {tropical fruit}           => {pip fruit}                0.020437214  0.1947674 2.5746476   201
    ## [628]  {pip fruit}                => {root vegetables}          0.015556685  0.2056452 1.8866793   153
    ## [629]  {root vegetables}          => {pip fruit}                0.015556685  0.1427239 1.8866793   153
    ## [630]  {pip fruit}                => {soda}                     0.013319776  0.1760753 1.0097378   131
    ## [631]  {pip fruit}                => {yogurt}                   0.017996950  0.2379032 1.7053777   177
    ## [632]  {yogurt}                   => {pip fruit}                0.017996950  0.1290087 1.7053777   177
    ## [633]  {pip fruit}                => {rolls/buns}               0.013929842  0.1841398 1.0011138   137
    ## [634]  {pip fruit}                => {other vegetables}         0.026131164  0.3454301 1.7852365   257
    ## [635]  {other vegetables}         => {pip fruit}                0.026131164  0.1350499 1.7852365   257
    ## [636]  {pip fruit}                => {whole milk}               0.030096594  0.3978495 1.5570432   296
    ## [637]  {whole milk}               => {pip fruit}                0.030096594  0.1177875 1.5570432   296
    ## [638]  {pastry}                   => {citrus fruit}             0.009761057  0.1097143 1.3256020    96
    ## [639]  {citrus fruit}             => {pastry}                   0.009761057  0.1179361 1.3256020    96
    ## [640]  {pastry}                   => {shopping bags}            0.011896289  0.1337143 1.3571517   117
    ## [641]  {shopping bags}            => {pastry}                   0.011896289  0.1207430 1.3571517   117
    ## [642]  {pastry}                   => {sausage}                  0.012506355  0.1405714 1.4962338   123
    ## [643]  {sausage}                  => {pastry}                   0.012506355  0.1331169 1.4962338   123
    ## [644]  {pastry}                   => {bottled water}            0.008947636  0.1005714 0.9099540    88
    ## [645]  {pastry}                   => {tropical fruit}           0.013218099  0.1485714 1.4158915   130
    ## [646]  {tropical fruit}           => {pastry}                   0.013218099  0.1259690 1.4158915   130
    ## [647]  {pastry}                   => {root vegetables}          0.010981190  0.1234286 1.1323881   108
    ## [648]  {root vegetables}          => {pastry}                   0.010981190  0.1007463 1.1323881   108
    ## [649]  {pastry}                   => {soda}                     0.021047280  0.2365714 1.3566647   207
    ## [650]  {soda}                     => {pastry}                   0.021047280  0.1206997 1.3566647   207
    ## [651]  {pastry}                   => {yogurt}                   0.017691917  0.1988571 1.4254810   174
    ## [652]  {yogurt}                   => {pastry}                   0.017691917  0.1268222 1.4254810   174
    ## [653]  {pastry}                   => {rolls/buns}               0.020945602  0.2354286 1.2799558   206
    ## [654]  {rolls/buns}               => {pastry}                   0.020945602  0.1138751 1.2799558   206
    ## [655]  {pastry}                   => {other vegetables}         0.022572445  0.2537143 1.3112349   222
    ## [656]  {other vegetables}         => {pastry}                   0.022572445  0.1166579 1.3112349   222
    ## [657]  {pastry}                   => {whole milk}               0.033248602  0.3737143 1.4625865   327
    ## [658]  {whole milk}               => {pastry}                   0.033248602  0.1301234 1.4625865   327
    ## [659]  {citrus fruit}             => {shopping bags}            0.009761057  0.1179361 1.1970090    96
    ## [660]  {citrus fruit}             => {sausage}                  0.011286223  0.1363636 1.4514463   111
    ## [661]  {sausage}                  => {citrus fruit}             0.011286223  0.1201299 1.4514463   111
    ## [662]  {citrus fruit}             => {bottled water}            0.013523132  0.1633907 1.4783323   133
    ## [663]  {bottled water}            => {citrus fruit}             0.013523132  0.1223551 1.4783323   133
    ## [664]  {citrus fruit}             => {tropical fruit}           0.019928826  0.2407862 2.2947022   196
    ## [665]  {tropical fruit}           => {citrus fruit}             0.019928826  0.1899225 2.2947022   196
    ## [666]  {citrus fruit}             => {root vegetables}          0.017691917  0.2137592 1.9611211   174
    ## [667]  {root vegetables}          => {citrus fruit}             0.017691917  0.1623134 1.9611211   174
    ## [668]  {citrus fruit}             => {soda}                     0.012811388  0.1547912 0.8876799   126
    ## [669]  {citrus fruit}             => {yogurt}                   0.021657346  0.2616708 1.8757521   213
    ## [670]  {yogurt}                   => {citrus fruit}             0.021657346  0.1552478 1.8757521   213
    ## [671]  {citrus fruit}             => {rolls/buns}               0.016776817  0.2027027 1.1020349   165
    ## [672]  {citrus fruit}             => {other vegetables}         0.028876462  0.3488943 1.8031403   284
    ## [673]  {other vegetables}         => {citrus fruit}             0.028876462  0.1492380 1.8031403   284
    ## [674]  {citrus fruit}             => {whole milk}               0.030503305  0.3685504 1.4423768   300
    ## [675]  {whole milk}               => {citrus fruit}             0.030503305  0.1193792 1.4423768   300
    ## [676]  {shopping bags}            => {sausage}                  0.015658363  0.1589267 1.6916065   154
    ## [677]  {sausage}                  => {shopping bags}            0.015658363  0.1666667 1.6916065   154
    ## [678]  {shopping bags}            => {bottled water}            0.010981190  0.1114551 1.0084278   108
    ## [679]  {shopping bags}            => {tropical fruit}           0.013523132  0.1372549 1.3080445   133
    ## [680]  {tropical fruit}           => {shopping bags}            0.013523132  0.1288760 1.3080445   133
    ## [681]  {shopping bags}            => {root vegetables}          0.012811388  0.1300310 1.1929613   126
    ## [682]  {root vegetables}          => {shopping bags}            0.012811388  0.1175373 1.1929613   126
    ## [683]  {shopping bags}            => {soda}                     0.024605999  0.2497420 1.4321939   242
    ## [684]  {soda}                     => {shopping bags}            0.024605999  0.1411079 1.4321939   242
    ## [685]  {shopping bags}            => {yogurt}                   0.015251652  0.1547988 1.1096544   150
    ## [686]  {yogurt}                   => {shopping bags}            0.015251652  0.1093294 1.1096544   150
    ## [687]  {shopping bags}            => {rolls/buns}               0.019522115  0.1981424 1.0772419   192
    ## [688]  {rolls/buns}               => {shopping bags}            0.019522115  0.1061360 1.0772419   192
    ## [689]  {shopping bags}            => {other vegetables}         0.023182511  0.2352941 1.2160366   228
    ## [690]  {other vegetables}         => {shopping bags}            0.023182511  0.1198108 1.2160366   228
    ## [691]  {shopping bags}            => {whole milk}               0.024504321  0.2487100 0.9733637   241
    ## [692]  {sausage}                  => {bottled water}            0.011997966  0.1277056 1.1554598   118
    ## [693]  {bottled water}            => {sausage}                  0.011997966  0.1085557 1.1554598   118
    ## [694]  {sausage}                  => {tropical fruit}           0.013929842  0.1482684 1.4130036   137
    ## [695]  {tropical fruit}           => {sausage}                  0.013929842  0.1327519 1.4130036   137
    ## [696]  {sausage}                  => {root vegetables}          0.014946619  0.1590909 1.4595700   147
    ## [697]  {root vegetables}          => {sausage}                  0.014946619  0.1371269 1.4595700   147
    ## [698]  {sausage}                  => {soda}                     0.024300966  0.2586580 1.4833245   239
    ## [699]  {soda}                     => {sausage}                  0.024300966  0.1393586 1.4833245   239
    ## [700]  {sausage}                  => {yogurt}                   0.019623793  0.2088745 1.4972889   193
    ## [701]  {yogurt}                   => {sausage}                  0.019623793  0.1406706 1.4972889   193
    ## [702]  {sausage}                  => {rolls/buns}               0.030604982  0.3257576 1.7710480   301
    ## [703]  {rolls/buns}               => {sausage}                  0.030604982  0.1663903 1.7710480   301
    ## [704]  {sausage}                  => {other vegetables}         0.026944586  0.2867965 1.4822091   265
    ## [705]  {other vegetables}         => {sausage}                  0.026944586  0.1392538 1.4822091   265
    ## [706]  {sausage}                  => {whole milk}               0.029893238  0.3181818 1.2452520   294
    ## [707]  {whole milk}               => {sausage}                  0.029893238  0.1169916 1.2452520   294
    ## [708]  {bottled water}            => {tropical fruit}           0.018505338  0.1674333 1.5956459   182
    ## [709]  {tropical fruit}           => {bottled water}            0.018505338  0.1763566 1.5956459   182
    ## [710]  {bottled water}            => {root vegetables}          0.015658363  0.1416743 1.2997827   154
    ## [711]  {root vegetables}          => {bottled water}            0.015658363  0.1436567 1.2997827   154
    ## [712]  {bottled water}            => {soda}                     0.028978139  0.2621895 1.5035766   285
    ## [713]  {soda}                     => {bottled water}            0.028978139  0.1661808 1.5035766   285
    ## [714]  {bottled water}            => {yogurt}                   0.022979156  0.2079117 1.4903873   226
    ## [715]  {yogurt}                   => {bottled water}            0.022979156  0.1647230 1.4903873   226
    ## [716]  {bottled water}            => {rolls/buns}               0.024199288  0.2189512 1.1903734   238
    ## [717]  {rolls/buns}               => {bottled water}            0.024199288  0.1315644 1.1903734   238
    ## [718]  {bottled water}            => {other vegetables}         0.024809354  0.2244710 1.1601012   244
    ## [719]  {other vegetables}         => {bottled water}            0.024809354  0.1282186 1.1601012   244
    ## [720]  {bottled water}            => {whole milk}               0.034367056  0.3109476 1.2169396   338
    ## [721]  {whole milk}               => {bottled water}            0.034367056  0.1345006 1.2169396   338
    ## [722]  {tropical fruit}           => {root vegetables}          0.021047280  0.2005814 1.8402220   207
    ## [723]  {root vegetables}          => {tropical fruit}           0.021047280  0.1930970 1.8402220   207
    ## [724]  {tropical fruit}           => {soda}                     0.020843925  0.1986434 1.1391592   205
    ## [725]  {soda}                     => {tropical fruit}           0.020843925  0.1195335 1.1391592   205
    ## [726]  {tropical fruit}           => {yogurt}                   0.029283172  0.2790698 2.0004746   288
    ## [727]  {yogurt}                   => {tropical fruit}           0.029283172  0.2099125 2.0004746   288
    ## [728]  {tropical fruit}           => {rolls/buns}               0.024605999  0.2344961 1.2748863   242
    ## [729]  {rolls/buns}               => {tropical fruit}           0.024605999  0.1337756 1.2748863   242
    ## [730]  {tropical fruit}           => {other vegetables}         0.035892222  0.3420543 1.7677896   353
    ## [731]  {other vegetables}         => {tropical fruit}           0.035892222  0.1854966 1.7677896   353
    ## [732]  {tropical fruit}           => {whole milk}               0.042297916  0.4031008 1.5775950   416
    ## [733]  {whole milk}               => {tropical fruit}           0.042297916  0.1655392 1.5775950   416
    ## [734]  {root vegetables}          => {soda}                     0.018607016  0.1707090 0.9789636   183
    ## [735]  {soda}                     => {root vegetables}          0.018607016  0.1067055 0.9789636   183
    ## [736]  {root vegetables}          => {yogurt}                   0.025826131  0.2369403 1.6984751   254
    ## [737]  {yogurt}                   => {root vegetables}          0.025826131  0.1851312 1.6984751   254
    ## [738]  {root vegetables}          => {rolls/buns}               0.024300966  0.2229478 1.2121013   239
    ## [739]  {rolls/buns}               => {root vegetables}          0.024300966  0.1321172 1.2121013   239
    ## [740]  {root vegetables}          => {other vegetables}         0.047381800  0.4347015 2.2466049   466
    ## [741]  {other vegetables}         => {root vegetables}          0.047381800  0.2448765 2.2466049   466
    ## [742]  {root vegetables}          => {whole milk}               0.048906965  0.4486940 1.7560310   481
    ## [743]  {whole milk}               => {root vegetables}          0.048906965  0.1914047 1.7560310   481
    ## [744]  {soda}                     => {yogurt}                   0.027351296  0.1568513 1.1243678   269
    ## [745]  {yogurt}                   => {soda}                     0.027351296  0.1960641 1.1243678   269
    ## [746]  {soda}                     => {rolls/buns}               0.038332486  0.2198251 1.1951242   377
    ## [747]  {rolls/buns}               => {soda}                     0.038332486  0.2084024 1.1951242   377
    ## [748]  {soda}                     => {other vegetables}         0.032740214  0.1877551 0.9703476   322
    ## [749]  {other vegetables}         => {soda}                     0.032740214  0.1692065 0.9703476   322
    ## [750]  {soda}                     => {whole milk}               0.040061007  0.2297376 0.8991124   394
    ## [751]  {whole milk}               => {soda}                     0.040061007  0.1567847 0.8991124   394
    ## [752]  {yogurt}                   => {rolls/buns}               0.034367056  0.2463557 1.3393633   338
    ## [753]  {rolls/buns}               => {yogurt}                   0.034367056  0.1868436 1.3393633   338
    ## [754]  {yogurt}                   => {other vegetables}         0.043416370  0.3112245 1.6084566   427
    ## [755]  {other vegetables}         => {yogurt}                   0.043416370  0.2243826 1.6084566   427
    ## [756]  {yogurt}                   => {whole milk}               0.056024403  0.4016035 1.5717351   551
    ## [757]  {whole milk}               => {yogurt}                   0.056024403  0.2192598 1.5717351   551
    ## [758]  {rolls/buns}               => {other vegetables}         0.042602949  0.2316197 1.1970465   419
    ## [759]  {other vegetables}         => {rolls/buns}               0.042602949  0.2201787 1.1970465   419
    ## [760]  {rolls/buns}               => {whole milk}               0.056634469  0.3079049 1.2050318   557
    ## [761]  {whole milk}               => {rolls/buns}               0.056634469  0.2216474 1.2050318   557
    ## [762]  {other vegetables}         => {whole milk}               0.074834774  0.3867578 1.5136341   736
    ## [763]  {whole milk}               => {other vegetables}         0.074834774  0.2928770 1.5136341   736
    ## [764]  {oil,                                                                                          
    ##         other vegetables}         => {whole milk}               0.005083884  0.5102041 1.9967597    50
    ## [765]  {oil,                                                                                          
    ##         whole milk}               => {other vegetables}         0.005083884  0.4504505 2.3279980    50
    ## [766]  {onions,                                                                                       
    ##         root vegetables}          => {other vegetables}         0.005693950  0.6021505 3.1120076    56
    ## [767]  {onions,                                                                                       
    ##         other vegetables}         => {root vegetables}          0.005693950  0.4000000 3.6697761    56
    ## [768]  {other vegetables,                                                                             
    ##         root vegetables}          => {onions}                   0.005693950  0.1201717 3.8750440    56
    ## [769]  {onions,                                                                                       
    ##         other vegetables}         => {whole milk}               0.006609049  0.4642857 1.8170513    65
    ## [770]  {onions,                                                                                       
    ##         whole milk}               => {other vegetables}         0.006609049  0.5462185 2.8229421    65
    ## [771]  {hamburger meat,                                                                               
    ##         other vegetables}         => {whole milk}               0.006304016  0.4558824 1.7841635    62
    ## [772]  {hamburger meat,                                                                               
    ##         whole milk}               => {other vegetables}         0.006304016  0.4275862 2.2098320    62
    ## [773]  {hygiene articles,                                                                             
    ##         other vegetables}         => {whole milk}               0.005185562  0.5425532 2.1233628    51
    ## [774]  {hygiene articles,                                                                             
    ##         whole milk}               => {other vegetables}         0.005185562  0.4047619 2.0918725    51
    ## [775]  {other vegetables,                                                                             
    ##         sugar}                    => {whole milk}               0.006304016  0.5849057 2.2891155    62
    ## [776]  {sugar,                                                                                        
    ##         whole milk}               => {other vegetables}         0.006304016  0.4189189 2.1650381    62
    ## [777]  {long life bakery product,                                                                     
    ##         other vegetables}         => {whole milk}               0.005693950  0.5333333 2.0872795    56
    ## [778]  {long life bakery product,                                                                     
    ##         whole milk}               => {other vegetables}         0.005693950  0.4210526 2.1760655    56
    ## [779]  {cream cheese,                                                                                 
    ##         yogurt}                   => {other vegetables}         0.005287239  0.4262295 2.2028204    52
    ## [780]  {cream cheese,                                                                                 
    ##         other vegetables}         => {yogurt}                   0.005287239  0.3851852 2.7611489    52
    ## [781]  {other vegetables,                                                                             
    ##         yogurt}                   => {cream cheese}             0.005287239  0.1217799 3.0710383    52
    ## [782]  {cream cheese,                                                                                 
    ##         yogurt}                   => {whole milk}               0.006609049  0.5327869 2.0851409    65
    ## [783]  {cream cheese,                                                                                 
    ##         whole milk}               => {yogurt}                   0.006609049  0.4012346 2.8761968    65
    ## [784]  {whole milk,                                                                                   
    ##         yogurt}                   => {cream cheese}             0.006609049  0.1179673 2.9748941    65
    ## [785]  {cream cheese,                                                                                 
    ##         other vegetables}         => {whole milk}               0.006710727  0.4888889 1.9133395    66
    ## [786]  {cream cheese,                                                                                 
    ##         whole milk}               => {other vegetables}         0.006710727  0.4074074 2.1055449    66
    ## [787]  {chicken,                                                                                      
    ##         root vegetables}          => {other vegetables}         0.005693950  0.5233645 2.7048291    56
    ## [788]  {chicken,                                                                                      
    ##         other vegetables}         => {root vegetables}          0.005693950  0.3181818 2.9191401    56
    ## [789]  {other vegetables,                                                                             
    ##         root vegetables}          => {chicken}                  0.005693950  0.1201717 2.8006834    56
    ## [790]  {chicken,                                                                                      
    ##         root vegetables}          => {whole milk}               0.005998983  0.5514019 2.1579934    59
    ## [791]  {chicken,                                                                                      
    ##         whole milk}               => {root vegetables}          0.005998983  0.3410405 3.1288554    59
    ## [792]  {root vegetables,                                                                              
    ##         whole milk}               => {chicken}                  0.005998983  0.1226611 2.8587018    59
    ## [793]  {chicken,                                                                                      
    ##         rolls/buns}               => {whole milk}               0.005287239  0.5473684 2.1422079    52
    ## [794]  {chicken,                                                                                      
    ##         whole milk}               => {rolls/buns}               0.005287239  0.3005780 1.6341542    52
    ## [795]  {chicken,                                                                                      
    ##         other vegetables}         => {whole milk}               0.008439248  0.4715909 1.8456413    83
    ## [796]  {chicken,                                                                                      
    ##         whole milk}               => {other vegetables}         0.008439248  0.4797688 2.4795197    83
    ## [797]  {other vegetables,                                                                             
    ##         whole milk}               => {chicken}                  0.008439248  0.1127717 2.6282229    83
    ## [798]  {other vegetables,                                                                             
    ##         white bread}              => {whole milk}               0.005897306  0.4296296 1.6814196    58
    ## [799]  {white bread,                                                                                  
    ##         whole milk}               => {other vegetables}         0.005897306  0.3452381 1.7842442    58
    ## [800]  {chocolate,                                                                                    
    ##         soda}                     => {whole milk}               0.005083884  0.3759398 1.4712966    50
    ## [801]  {chocolate,                                                                                    
    ##         whole milk}               => {soda}                     0.005083884  0.3048780 1.7483823    50
    ## [802]  {soda,                                                                                         
    ##         whole milk}               => {chocolate}                0.005083884  0.1269036 2.5575747    50
    ## [803]  {chocolate,                                                                                    
    ##         other vegetables}         => {whole milk}               0.005490595  0.4320000 1.6906964    54
    ## [804]  {chocolate,                                                                                    
    ##         whole milk}               => {other vegetables}         0.005490595  0.3292683 1.7017098    54
    ## [805]  {coffee,                                                                                       
    ##         yogurt}                   => {whole milk}               0.005083884  0.5208333 2.0383589    50
    ## [806]  {coffee,                                                                                       
    ##         whole milk}               => {yogurt}                   0.005083884  0.2717391 1.9479259    50
    ## [807]  {coffee,                                                                                       
    ##         other vegetables}         => {whole milk}               0.006405694  0.4772727 1.8678779    63
    ## [808]  {coffee,                                                                                       
    ##         whole milk}               => {other vegetables}         0.006405694  0.3423913 1.7695315    63
    ## [809]  {frozen vegetables,                                                                            
    ##         root vegetables}          => {other vegetables}         0.006100661  0.5263158 2.7200819    60
    ## [810]  {frozen vegetables,                                                                            
    ##         other vegetables}         => {root vegetables}          0.006100661  0.3428571 3.1455224    60
    ## [811]  {other vegetables,                                                                             
    ##         root vegetables}          => {frozen vegetables}        0.006100661  0.1287554 2.6771861    60
    ## [812]  {frozen vegetables,                                                                            
    ##         root vegetables}          => {whole milk}               0.006202339  0.5350877 2.0941455    61
    ## [813]  {frozen vegetables,                                                                            
    ##         whole milk}               => {root vegetables}          0.006202339  0.3034826 2.7842829    61
    ## [814]  {root vegetables,                                                                              
    ##         whole milk}               => {frozen vegetables}        0.006202339  0.1268191 2.6369262    61
    ## [815]  {frozen vegetables,                                                                            
    ##         yogurt}                   => {other vegetables}         0.005287239  0.4262295 2.2028204    52
    ## [816]  {frozen vegetables,                                                                            
    ##         other vegetables}         => {yogurt}                   0.005287239  0.2971429 2.1300292    52
    ## [817]  {other vegetables,                                                                             
    ##         yogurt}                   => {frozen vegetables}        0.005287239  0.1217799 2.5321457    52
    ## [818]  {frozen vegetables,                                                                            
    ##         yogurt}                   => {whole milk}               0.006100661  0.4918033 1.9247454    60
    ## [819]  {frozen vegetables,                                                                            
    ##         whole milk}               => {yogurt}                   0.006100661  0.2985075 2.1398111    60
    ## [820]  {whole milk,                                                                                   
    ##         yogurt}                   => {frozen vegetables}        0.006100661  0.1088929 2.2641900    60
    ## [821]  {frozen vegetables,                                                                            
    ##         rolls/buns}               => {whole milk}               0.005083884  0.5000000 1.9568245    50
    ## [822]  {frozen vegetables,                                                                            
    ##         whole milk}               => {rolls/buns}               0.005083884  0.2487562 1.3524143    50
    ## [823]  {frozen vegetables,                                                                            
    ##         other vegetables}         => {whole milk}               0.009659380  0.5428571 2.1245523    95
    ## [824]  {frozen vegetables,                                                                            
    ##         whole milk}               => {other vegetables}         0.009659380  0.4726368 2.4426606    95
    ## [825]  {other vegetables,                                                                             
    ##         whole milk}               => {frozen vegetables}        0.009659380  0.1290761 2.6838548    95
    ## [826]  {beef,                                                                                         
    ##         root vegetables}          => {other vegetables}         0.007930859  0.4561404 2.3574043    78
    ## [827]  {beef,                                                                                         
    ##         other vegetables}         => {root vegetables}          0.007930859  0.4020619 3.6886925    78
    ## [828]  {other vegetables,                                                                             
    ##         root vegetables}          => {beef}                     0.007930859  0.1673820 3.1903134    78
    ## [829]  {beef,                                                                                         
    ##         root vegetables}          => {whole milk}               0.008032537  0.4619883 1.8080601    79
    ## [830]  {beef,                                                                                         
    ##         whole milk}               => {root vegetables}          0.008032537  0.3779904 3.4678506    79
    ## [831]  {root vegetables,                                                                              
    ##         whole milk}               => {beef}                     0.008032537  0.1642412 3.1304493    79
    ## [832]  {beef,                                                                                         
    ##         yogurt}                   => {other vegetables}         0.005185562  0.4434783 2.2919646    51
    ## [833]  {beef,                                                                                         
    ##         other vegetables}         => {yogurt}                   0.005185562  0.2628866 1.8844677    51
    ## [834]  {other vegetables,                                                                             
    ##         yogurt}                   => {beef}                     0.005185562  0.1194379 2.2764964    51
    ## [835]  {beef,                                                                                         
    ##         yogurt}                   => {whole milk}               0.006100661  0.5217391 2.0419038    60
    ## [836]  {beef,                                                                                         
    ##         whole milk}               => {yogurt}                   0.006100661  0.2870813 2.0579045    60
    ## [837]  {whole milk,                                                                                   
    ##         yogurt}                   => {beef}                     0.006100661  0.1088929 2.0755075    60
    ## [838]  {beef,                                                                                         
    ##         rolls/buns}               => {other vegetables}         0.005795628  0.4253731 2.1983945    57
    ## [839]  {beef,                                                                                         
    ##         other vegetables}         => {rolls/buns}               0.005795628  0.2938144 1.5973825    57
    ## [840]  {other vegetables,                                                                             
    ##         rolls/buns}               => {beef}                     0.005795628  0.1360382 2.5928984    57
    ## [841]  {beef,                                                                                         
    ##         rolls/buns}               => {whole milk}               0.006812405  0.5000000 1.9568245    67
    ## [842]  {beef,                                                                                         
    ##         whole milk}               => {rolls/buns}               0.006812405  0.3205742 1.7428673    67
    ## [843]  {rolls/buns,                                                                                   
    ##         whole milk}               => {beef}                     0.006812405  0.1202873 2.2926844    67
    ## [844]  {beef,                                                                                         
    ##         other vegetables}         => {whole milk}               0.009252669  0.4690722 1.8357838    91
    ## [845]  {beef,                                                                                         
    ##         whole milk}               => {other vegetables}         0.009252669  0.4354067 2.2502495    91
    ## [846]  {other vegetables,                                                                             
    ##         whole milk}               => {beef}                     0.009252669  0.1236413 2.3566128    91
    ## [847]  {curd,                                                                                         
    ##         whipped/sour cream}       => {whole milk}               0.005897306  0.5631068 2.2038024    58
    ## [848]  {curd,                                                                                         
    ##         whole milk}               => {whipped/sour cream}       0.005897306  0.2256809 3.1483291    58
    ## [849]  {whipped/sour cream,                                                                           
    ##         whole milk}               => {curd}                     0.005897306  0.1829653 3.4340911    58
    ## [850]  {curd,                                                                                         
    ##         tropical fruit}           => {yogurt}                   0.005287239  0.5148515 3.6906446    52
    ## [851]  {curd,                                                                                         
    ##         yogurt}                   => {tropical fruit}           0.005287239  0.3058824 2.9150707    52
    ## [852]  {tropical fruit,                                                                               
    ##         yogurt}                   => {curd}                     0.005287239  0.1805556 3.3888624    52
    ## [853]  {curd,                                                                                         
    ##         tropical fruit}           => {other vegetables}         0.005287239  0.5148515 2.6608326    52
    ## [854]  {curd,                                                                                         
    ##         other vegetables}         => {tropical fruit}           0.005287239  0.3076923 2.9323196    52
    ## [855]  {other vegetables,                                                                             
    ##         tropical fruit}           => {curd}                     0.005287239  0.1473088 2.7648509    52
    ## [856]  {curd,                                                                                         
    ##         tropical fruit}           => {whole milk}               0.006507372  0.6336634 2.4799360    64
    ## [857]  {curd,                                                                                         
    ##         whole milk}               => {tropical fruit}           0.006507372  0.2490272 2.3732392    64
    ## [858]  {tropical fruit,                                                                               
    ##         whole milk}               => {curd}                     0.006507372  0.1538462 2.8875514    64
    ## [859]  {curd,                                                                                         
    ##         root vegetables}          => {other vegetables}         0.005490595  0.5046729 2.6082280    54
    ## [860]  {curd,                                                                                         
    ##         other vegetables}         => {root vegetables}          0.005490595  0.3195266 2.9314780    54
    ## [861]  {other vegetables,                                                                             
    ##         root vegetables}          => {curd}                     0.005490595  0.1158798 2.1749582    54
    ## [862]  {curd,                                                                                         
    ##         root vegetables}          => {whole milk}               0.006202339  0.5700935 2.2311457    61
    ## [863]  {curd,                                                                                         
    ##         whole milk}               => {root vegetables}          0.006202339  0.2373541 2.1775909    61
    ## [864]  {root vegetables,                                                                              
    ##         whole milk}               => {curd}                     0.006202339  0.1268191 2.3802788    61
    ## [865]  {curd,                                                                                         
    ##         yogurt}                   => {other vegetables}         0.006100661  0.3529412 1.8240549    60
    ## [866]  {curd,                                                                                         
    ##         other vegetables}         => {yogurt}                   0.006100661  0.3550296 2.5449825    60
    ## [867]  {other vegetables,                                                                             
    ##         yogurt}                   => {curd}                     0.006100661  0.1405152 2.6373420    60
    ## [868]  {curd,                                                                                         
    ##         yogurt}                   => {whole milk}               0.010066090  0.5823529 2.2791250    99
    ## [869]  {curd,                                                                                         
    ##         whole milk}               => {yogurt}                   0.010066090  0.3852140 2.7613555    99
    ## [870]  {whole milk,                                                                                   
    ##         yogurt}                   => {curd}                     0.010066090  0.1796733 3.3723037    99
    ## [871]  {curd,                                                                                         
    ##         rolls/buns}               => {whole milk}               0.005897306  0.5858586 2.2928449    58
    ## [872]  {curd,                                                                                         
    ##         whole milk}               => {rolls/buns}               0.005897306  0.2256809 1.2269607    58
    ## [873]  {rolls/buns,                                                                                   
    ##         whole milk}               => {curd}                     0.005897306  0.1041293 1.9544109    58
    ## [874]  {curd,                                                                                         
    ##         other vegetables}         => {whole milk}               0.009862735  0.5739645 2.2462956    97
    ## [875]  {curd,                                                                                         
    ##         whole milk}               => {other vegetables}         0.009862735  0.3774319 1.9506268    97
    ## [876]  {other vegetables,                                                                             
    ##         whole milk}               => {curd}                     0.009862735  0.1317935 2.4736429    97
    ## [877]  {napkins,                                                                                      
    ##         yogurt}                   => {whole milk}               0.006100661  0.4958678 1.9406524    60
    ## [878]  {napkins,                                                                                      
    ##         whole milk}               => {yogurt}                   0.006100661  0.3092784 2.2170208    60
    ## [879]  {whole milk,                                                                                   
    ##         yogurt}                   => {napkins}                  0.006100661  0.1088929 2.0795376    60
    ## [880]  {napkins,                                                                                      
    ##         rolls/buns}               => {whole milk}               0.005287239  0.4521739 1.7696500    52
    ## [881]  {napkins,                                                                                      
    ##         whole milk}               => {rolls/buns}               0.005287239  0.2680412 1.4572612    52
    ## [882]  {napkins,                                                                                      
    ##         other vegetables}         => {whole milk}               0.006812405  0.4718310 1.8465809    67
    ## [883]  {napkins,                                                                                      
    ##         whole milk}               => {other vegetables}         0.006812405  0.3453608 1.7848785    67
    ## [884]  {pork,                                                                                         
    ##         root vegetables}          => {other vegetables}         0.007015760  0.5149254 2.6612144    69
    ## [885]  {other vegetables,                                                                             
    ##         pork}                     => {root vegetables}          0.007015760  0.3239437 2.9720018    69
    ## [886]  {other vegetables,                                                                             
    ##         root vegetables}          => {pork}                     0.007015760  0.1480687 2.5683516    69
    ## [887]  {pork,                                                                                         
    ##         root vegetables}          => {whole milk}               0.006812405  0.5000000 1.9568245    67
    ## [888]  {pork,                                                                                         
    ##         whole milk}               => {root vegetables}          0.006812405  0.3073394 2.8196674    67
    ## [889]  {root vegetables,                                                                              
    ##         whole milk}               => {pork}                     0.006812405  0.1392931 2.4161341    67
    ## [890]  {pork,                                                                                         
    ##         rolls/buns}               => {other vegetables}         0.005592272  0.4954955 2.5607978    55
    ## [891]  {other vegetables,                                                                             
    ##         pork}                     => {rolls/buns}               0.005592272  0.2582160 1.4038441    55
    ## [892]  {other vegetables,                                                                             
    ##         rolls/buns}               => {pork}                     0.005592272  0.1312649 2.2768791    55
    ## [893]  {pork,                                                                                         
    ##         rolls/buns}               => {whole milk}               0.006202339  0.5495495 2.1507441    61
    ## [894]  {pork,                                                                                         
    ##         whole milk}               => {rolls/buns}               0.006202339  0.2798165 1.5212799    61
    ## [895]  {rolls/buns,                                                                                   
    ##         whole milk}               => {pork}                     0.006202339  0.1095153 1.8996166    61
    ## [896]  {other vegetables,                                                                             
    ##         pork}                     => {whole milk}               0.010167768  0.4694836 1.8373939   100
    ## [897]  {pork,                                                                                         
    ##         whole milk}               => {other vegetables}         0.010167768  0.4587156 2.3707136   100
    ## [898]  {other vegetables,                                                                             
    ##         whole milk}               => {pork}                     0.010167768  0.1358696 2.3567499   100
    ## [899]  {frankfurter,                                                                                  
    ##         tropical fruit}           => {whole milk}               0.005185562  0.5483871 2.1461946    51
    ## [900]  {frankfurter,                                                                                  
    ##         whole milk}               => {tropical fruit}           0.005185562  0.2524752 2.4060989    51
    ## [901]  {tropical fruit,                                                                               
    ##         whole milk}               => {frankfurter}              0.005185562  0.1225962 2.0788503    51
    ## [902]  {frankfurter,                                                                                  
    ##         root vegetables}          => {whole milk}               0.005083884  0.5000000 1.9568245    50
    ## [903]  {frankfurter,                                                                                  
    ##         whole milk}               => {root vegetables}          0.005083884  0.2475248 2.2709011    50
    ## [904]  {root vegetables,                                                                              
    ##         whole milk}               => {frankfurter}              0.005083884  0.1039501 1.7626712    50
    ## [905]  {frankfurter,                                                                                  
    ##         yogurt}                   => {whole milk}               0.006202339  0.5545455 2.1702963    61
    ## [906]  {frankfurter,                                                                                  
    ##         whole milk}               => {yogurt}                   0.006202339  0.3019802 2.1647050    61
    ## [907]  {whole milk,                                                                                   
    ##         yogurt}                   => {frankfurter}              0.006202339  0.1107078 1.8772608    61
    ## [908]  {frankfurter,                                                                                  
    ##         rolls/buns}               => {other vegetables}         0.005592272  0.2910053 1.5039606    55
    ## [909]  {frankfurter,                                                                                  
    ##         other vegetables}         => {rolls/buns}               0.005592272  0.3395062 1.8457950    55
    ## [910]  {other vegetables,                                                                             
    ##         rolls/buns}               => {frankfurter}              0.005592272  0.1312649 2.2258456    55
    ## [911]  {frankfurter,                                                                                  
    ##         rolls/buns}               => {whole milk}               0.005998983  0.3121693 1.2217211    59
    ## [912]  {frankfurter,                                                                                  
    ##         whole milk}               => {rolls/buns}               0.005998983  0.2920792 1.5879486    59
    ## [913]  {rolls/buns,                                                                                   
    ##         whole milk}               => {frankfurter}              0.005998983  0.1059246 1.7961524    59
    ## [914]  {frankfurter,                                                                                  
    ##         other vegetables}         => {whole milk}               0.007625826  0.4629630 1.8118745    75
    ## [915]  {frankfurter,                                                                                  
    ##         whole milk}               => {other vegetables}         0.007625826  0.3712871 1.9188696    75
    ## [916]  {other vegetables,                                                                             
    ##         whole milk}               => {frankfurter}              0.007625826  0.1019022 1.7279446    75
    ## [917]  {bottled beer,                                                                                 
    ##         bottled water}            => {soda}                     0.005083884  0.3225806 1.8499013    50
    ## [918]  {bottled beer,                                                                                 
    ##         soda}                     => {bottled water}            0.005083884  0.2994012 2.7089336    50
    ## [919]  {bottled water,                                                                                
    ##         soda}                     => {bottled beer}             0.005083884  0.1754386 2.1785841    50
    ## [920]  {bottled beer,                                                                                 
    ##         bottled water}            => {whole milk}               0.006100661  0.3870968 1.5149609    60
    ## [921]  {bottled beer,                                                                                 
    ##         whole milk}               => {bottled water}            0.006100661  0.2985075 2.7008472    60
    ## [922]  {bottled water,                                                                                
    ##         whole milk}               => {bottled beer}             0.006100661  0.1775148 2.2043661    60
    ## [923]  {bottled beer,                                                                                 
    ##         yogurt}                   => {whole milk}               0.005185562  0.5604396 2.1933637    51
    ## [924]  {bottled beer,                                                                                 
    ##         whole milk}               => {yogurt}                   0.005185562  0.2537313 1.8188395    51
    ## [925]  {bottled beer,                                                                                 
    ##         rolls/buns}               => {whole milk}               0.005388917  0.3955224 1.5479358    53
    ## [926]  {bottled beer,                                                                                 
    ##         whole milk}               => {rolls/buns}               0.005388917  0.2636816 1.4335591    53
    ## [927]  {bottled beer,                                                                                 
    ##         other vegetables}         => {whole milk}               0.007625826  0.4716981 1.8460609    75
    ## [928]  {bottled beer,                                                                                 
    ##         whole milk}               => {other vegetables}         0.007625826  0.3731343 1.9284162    75
    ## [929]  {other vegetables,                                                                             
    ##         whole milk}               => {bottled beer}             0.007625826  0.1019022 1.2654140    75
    ## [930]  {brown bread,                                                                                  
    ##         tropical fruit}           => {whole milk}               0.005693950  0.5333333 2.0872795    56
    ## [931]  {brown bread,                                                                                  
    ##         whole milk}               => {tropical fruit}           0.005693950  0.2258065 2.1519442    56
    ## [932]  {tropical fruit,                                                                               
    ##         whole milk}               => {brown bread}              0.005693950  0.1346154 2.0751447    56
    ## [933]  {brown bread,                                                                                  
    ##         root vegetables}          => {whole milk}               0.005693950  0.5600000 2.1916435    56
    ## [934]  {brown bread,                                                                                  
    ##         whole milk}               => {root vegetables}          0.005693950  0.2258065 2.0716478    56
    ## [935]  {root vegetables,                                                                              
    ##         whole milk}               => {brown bread}              0.005693950  0.1164241 1.7947197    56
    ## [936]  {brown bread,                                                                                  
    ##         soda}                     => {whole milk}               0.005083884  0.4032258 1.5780843    50
    ## [937]  {brown bread,                                                                                  
    ##         whole milk}               => {soda}                     0.005083884  0.2016129 1.1561883    50
    ## [938]  {soda,                                                                                         
    ##         whole milk}               => {brown bread}              0.005083884  0.1269036 1.9562640    50
    ## [939]  {brown bread,                                                                                  
    ##         yogurt}                   => {other vegetables}         0.005185562  0.3566434 1.8431883    51
    ## [940]  {brown bread,                                                                                  
    ##         other vegetables}         => {yogurt}                   0.005185562  0.2771739 1.9868844    51
    ## [941]  {other vegetables,                                                                             
    ##         yogurt}                   => {brown bread}              0.005185562  0.1194379 1.8411789    51
    ## [942]  {brown bread,                                                                                  
    ##         yogurt}                   => {whole milk}               0.007117438  0.4895105 1.9157723    70
    ## [943]  {brown bread,                                                                                  
    ##         whole milk}               => {yogurt}                   0.007117438  0.2822581 2.0233295    70
    ## [944]  {whole milk,                                                                                   
    ##         yogurt}                   => {brown bread}              0.007117438  0.1270417 1.9583943    70
    ## [945]  {brown bread,                                                                                  
    ##         rolls/buns}               => {whole milk}               0.005287239  0.4193548 1.6412077    52
    ## [946]  {brown bread,                                                                                  
    ##         whole milk}               => {rolls/buns}               0.005287239  0.2096774 1.1399544    52
    ## [947]  {brown bread,                                                                                  
    ##         other vegetables}         => {whole milk}               0.009354347  0.5000000 1.9568245    92
    ## [948]  {brown bread,                                                                                  
    ##         whole milk}               => {other vegetables}         0.009354347  0.3709677 1.9172190    92
    ## [949]  {other vegetables,                                                                             
    ##         whole milk}               => {brown bread}              0.009354347  0.1250000 1.9269201    92
    ## [950]  {domestic eggs,                                                                                
    ##         margarine}                => {whole milk}               0.005185562  0.6219512 2.4340988    51
    ## [951]  {margarine,                                                                                    
    ##         whole milk}               => {domestic eggs}            0.005185562  0.2142857 3.3774038    51
    ## [952]  {domestic eggs,                                                                                
    ##         whole milk}               => {margarine}                0.005185562  0.1728814 2.9518891    51
    ## [953]  {margarine,                                                                                    
    ##         root vegetables}          => {other vegetables}         0.005897306  0.5321101 2.7500277    58
    ## [954]  {margarine,                                                                                    
    ##         other vegetables}         => {root vegetables}          0.005897306  0.2989691 2.7428739    58
    ## [955]  {other vegetables,                                                                             
    ##         root vegetables}          => {margarine}                0.005897306  0.1244635 2.1251714    58
    ## [956]  {margarine,                                                                                    
    ##         yogurt}                   => {other vegetables}         0.005693950  0.4000000 2.0672622    56
    ## [957]  {margarine,                                                                                    
    ##         other vegetables}         => {yogurt}                   0.005693950  0.2886598 2.0692194    56
    ## [958]  {other vegetables,                                                                             
    ##         yogurt}                   => {margarine}                0.005693950  0.1311475 2.2392987    56
    ## [959]  {margarine,                                                                                    
    ##         yogurt}                   => {whole milk}               0.007015760  0.4928571 1.9288699    69
    ## [960]  {margarine,                                                                                    
    ##         whole milk}               => {yogurt}                   0.007015760  0.2899160 2.0782241    69
    ## [961]  {whole milk,                                                                                   
    ##         yogurt}                   => {margarine}                0.007015760  0.1252269 2.1382052    69
    ## [962]  {margarine,                                                                                    
    ##         rolls/buns}               => {other vegetables}         0.005185562  0.3517241 1.8177651    51
    ## [963]  {margarine,                                                                                    
    ##         other vegetables}         => {rolls/buns}               0.005185562  0.2628866 1.4292370    51
    ## [964]  {other vegetables,                                                                             
    ##         rolls/buns}               => {margarine}                0.005185562  0.1217184 2.0782990    51
    ## [965]  {margarine,                                                                                    
    ##         rolls/buns}               => {whole milk}               0.007930859  0.5379310 2.1052733    78
    ## [966]  {margarine,                                                                                    
    ##         whole milk}               => {rolls/buns}               0.007930859  0.3277311 1.7817774    78
    ## [967]  {rolls/buns,                                                                                   
    ##         whole milk}               => {margarine}                0.007930859  0.1400359 2.3910645    78
    ## [968]  {margarine,                                                                                    
    ##         other vegetables}         => {whole milk}               0.009252669  0.4690722 1.8357838    91
    ## [969]  {margarine,                                                                                    
    ##         whole milk}               => {other vegetables}         0.009252669  0.3823529 1.9760595    91
    ## [970]  {other vegetables,                                                                             
    ##         whole milk}               => {margarine}                0.009252669  0.1236413 2.1111323    91
    ## [971]  {butter,                                                                                       
    ##         domestic eggs}            => {whole milk}               0.005998983  0.6210526 2.4305820    59
    ## [972]  {butter,                                                                                       
    ##         whole milk}               => {domestic eggs}            0.005998983  0.2177122 3.4314091    59
    ## [973]  {domestic eggs,                                                                                
    ##         whole milk}               => {butter}                   0.005998983  0.2000000 3.6091743    59
    ## [974]  {butter,                                                                                       
    ##         whipped/sour cream}       => {other vegetables}         0.005795628  0.5700000 2.9458487    57
    ## [975]  {butter,                                                                                       
    ##         other vegetables}         => {whipped/sour cream}       0.005795628  0.2893401 4.0363970    57
    ## [976]  {other vegetables,                                                                             
    ##         whipped/sour cream}       => {butter}                   0.005795628  0.2007042 3.6218827    57
    ## [977]  {butter,                                                                                       
    ##         whipped/sour cream}       => {whole milk}               0.006710727  0.6600000 2.5830084    66
    ## [978]  {butter,                                                                                       
    ##         whole milk}               => {whipped/sour cream}       0.006710727  0.2435424 3.3975033    66
    ## [979]  {whipped/sour cream,                                                                           
    ##         whole milk}               => {butter}                   0.006710727  0.2082019 3.7571846    66
    ## [980]  {butter,                                                                                       
    ##         citrus fruit}             => {whole milk}               0.005083884  0.5555556 2.1742495    50
    ## [981]  {butter,                                                                                       
    ##         whole milk}               => {citrus fruit}             0.005083884  0.1845018 2.2292084    50
    ## [982]  {citrus fruit,                                                                                 
    ##         whole milk}               => {butter}                   0.005083884  0.1666667 3.0076453    50
    ## [983]  {bottled water,                                                                                
    ##         butter}                   => {whole milk}               0.005388917  0.6022727 2.3570841    53
    ## [984]  {butter,                                                                                       
    ##         whole milk}               => {bottled water}            0.005388917  0.1955720 1.7695034    53
    ## [985]  {bottled water,                                                                                
    ##         whole milk}               => {butter}                   0.005388917  0.1568047 2.8296781    53
    ## [986]  {butter,                                                                                       
    ##         tropical fruit}           => {other vegetables}         0.005490595  0.5510204 2.8477592    54
    ## [987]  {butter,                                                                                       
    ##         other vegetables}         => {tropical fruit}           0.005490595  0.2741117 2.6122949    54
    ## [988]  {other vegetables,                                                                             
    ##         tropical fruit}           => {butter}                   0.005490595  0.1529745 2.7605583    54
    ## [989]  {butter,                                                                                       
    ##         tropical fruit}           => {whole milk}               0.006202339  0.6224490 2.4360468    61
    ## [990]  {butter,                                                                                       
    ##         whole milk}               => {tropical fruit}           0.006202339  0.2250923 2.1451379    61
    ## [991]  {tropical fruit,                                                                               
    ##         whole milk}               => {butter}                   0.006202339  0.1466346 2.6461494    61
    ## [992]  {butter,                                                                                       
    ##         root vegetables}          => {other vegetables}         0.006609049  0.5118110 2.6451190    65
    ## [993]  {butter,                                                                                       
    ##         other vegetables}         => {root vegetables}          0.006609049  0.3299492 3.0270996    65
    ## [994]  {other vegetables,                                                                             
    ##         root vegetables}          => {butter}                   0.006609049  0.1394850 2.5171280    65
    ## [995]  {butter,                                                                                       
    ##         root vegetables}          => {whole milk}               0.008235892  0.6377953 2.4961069    81
    ## [996]  {butter,                                                                                       
    ##         whole milk}               => {root vegetables}          0.008235892  0.2988930 2.7421759    81
    ## [997]  {root vegetables,                                                                              
    ##         whole milk}               => {butter}                   0.008235892  0.1683992 3.0389098    81
    ## [998]  {butter,                                                                                       
    ##         yogurt}                   => {other vegetables}         0.006405694  0.4375000 2.2610681    63
    ## [999]  {butter,                                                                                       
    ##         other vegetables}         => {yogurt}                   0.006405694  0.3197970 2.2924220    63
    ## [1000] {other vegetables,                                                                             
    ##         yogurt}                   => {butter}                   0.006405694  0.1475410 2.6625056    63
    ## [1001] {butter,                                                                                       
    ##         yogurt}                   => {whole milk}               0.009354347  0.6388889 2.5003869    92
    ## [1002] {butter,                                                                                       
    ##         whole milk}               => {yogurt}                   0.009354347  0.3394834 2.4335417    92
    ## [1003] {whole milk,                                                                                   
    ##         yogurt}                   => {butter}                   0.009354347  0.1669691 3.0131038    92
    ## [1004] {butter,                                                                                       
    ##         rolls/buns}               => {other vegetables}         0.005693950  0.4242424 2.1925508    56
    ## [1005] {butter,                                                                                       
    ##         other vegetables}         => {rolls/buns}               0.005693950  0.2842640 1.5454594    56
    ## [1006] {other vegetables,                                                                             
    ##         rolls/buns}               => {butter}                   0.005693950  0.1336516 2.4118587    56
    ## [1007] {butter,                                                                                       
    ##         rolls/buns}               => {whole milk}               0.006609049  0.4924242 1.9271757    65
    ## [1008] {butter,                                                                                       
    ##         whole milk}               => {rolls/buns}               0.006609049  0.2398524 1.3040068    65
    ## [1009] {rolls/buns,                                                                                   
    ##         whole milk}               => {butter}                   0.006609049  0.1166966 2.1058917    65
    ## [1010] {butter,                                                                                       
    ##         other vegetables}         => {whole milk}               0.011489578  0.5736041 2.2448850   113
    ## [1011] {butter,                                                                                       
    ##         whole milk}               => {other vegetables}         0.011489578  0.4169742 2.1549874   113
    ## [1012] {other vegetables,                                                                             
    ##         whole milk}               => {butter}                   0.011489578  0.1535326 2.7706297   113
    ## [1013] {newspapers,                                                                                   
    ##         tropical fruit}           => {whole milk}               0.005083884  0.4310345 1.6869177    50
    ## [1014] {newspapers,                                                                                   
    ##         whole milk}               => {tropical fruit}           0.005083884  0.1858736 1.7713827    50
    ## [1015] {tropical fruit,                                                                               
    ##         whole milk}               => {newspapers}               0.005083884  0.1201923 1.5058488    50
    ## [1016] {newspapers,                                                                                   
    ##         root vegetables}          => {other vegetables}         0.005998983  0.5221239 2.6984175    59
    ## [1017] {newspapers,                                                                                   
    ##         other vegetables}         => {root vegetables}          0.005998983  0.3105263 2.8489051    59
    ## [1018] {other vegetables,                                                                             
    ##         root vegetables}          => {newspapers}               0.005998983  0.1266094 1.5862470    59
    ## [1019] {newspapers,                                                                                   
    ##         root vegetables}          => {whole milk}               0.005795628  0.5044248 1.9741415    57
    ## [1020] {newspapers,                                                                                   
    ##         whole milk}               => {root vegetables}          0.005795628  0.2118959 1.9440264    57
    ## [1021] {root vegetables,                                                                              
    ##         whole milk}               => {newspapers}               0.005795628  0.1185031 1.4846856    57
    ## [1022] {newspapers,                                                                                   
    ##         yogurt}                   => {rolls/buns}               0.005083884  0.3311258 1.8002336    50
    ## [1023] {newspapers,                                                                                   
    ##         rolls/buns}               => {yogurt}                   0.005083884  0.2577320 1.8475174    50
    ## [1024] {rolls/buns,                                                                                   
    ##         yogurt}                   => {newspapers}               0.005083884  0.1479290 1.8533524    50
    ## [1025] {newspapers,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.005592272  0.3642384 1.8824408    55
    ## [1026] {newspapers,                                                                                   
    ##         other vegetables}         => {yogurt}                   0.005592272  0.2894737 2.0750537    55
    ## [1027] {other vegetables,                                                                             
    ##         yogurt}                   => {newspapers}               0.005592272  0.1288056 1.6137621    55
    ## [1028] {newspapers,                                                                                   
    ##         yogurt}                   => {whole milk}               0.006609049  0.4304636 1.6846834    65
    ## [1029] {newspapers,                                                                                   
    ##         whole milk}               => {yogurt}                   0.006609049  0.2416357 1.7321334    65
    ## [1030] {whole milk,                                                                                   
    ##         yogurt}                   => {newspapers}               0.006609049  0.1179673 1.4779729    65
    ## [1031] {newspapers,                                                                                   
    ##         rolls/buns}               => {other vegetables}         0.005490595  0.2783505 1.4385588    54
    ## [1032] {newspapers,                                                                                   
    ##         other vegetables}         => {rolls/buns}               0.005490595  0.2842105 1.5451689    54
    ## [1033] {other vegetables,                                                                             
    ##         rolls/buns}               => {newspapers}               0.005490595  0.1288783 1.6146725    54
    ## [1034] {newspapers,                                                                                   
    ##         rolls/buns}               => {whole milk}               0.007625826  0.3865979 1.5130086    75
    ## [1035] {newspapers,                                                                                   
    ##         whole milk}               => {rolls/buns}               0.007625826  0.2788104 1.5158100    75
    ## [1036] {rolls/buns,                                                                                   
    ##         whole milk}               => {newspapers}               0.007625826  0.1346499 1.6869833    75
    ## [1037] {newspapers,                                                                                   
    ##         other vegetables}         => {whole milk}               0.008337570  0.4315789 1.6890485    82
    ## [1038] {newspapers,                                                                                   
    ##         whole milk}               => {other vegetables}         0.008337570  0.3048327 1.5754229    82
    ## [1039] {other vegetables,                                                                             
    ##         whole milk}               => {newspapers}               0.008337570  0.1114130 1.3958564    82
    ## [1040] {domestic eggs,                                                                                
    ##         whipped/sour cream}       => {other vegetables}         0.005083884  0.5102041 2.6368141    50
    ## [1041] {domestic eggs,                                                                                
    ##         other vegetables}         => {whipped/sour cream}       0.005083884  0.2283105 3.1850125    50
    ## [1042] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {domestic eggs}            0.005083884  0.1760563 2.7748623    50
    ## [1043] {domestic eggs,                                                                                
    ##         whipped/sour cream}       => {whole milk}               0.005693950  0.5714286 2.2363709    56
    ## [1044] {domestic eggs,                                                                                
    ##         whole milk}               => {whipped/sour cream}       0.005693950  0.1898305 2.6482029    56
    ## [1045] {whipped/sour cream,                                                                           
    ##         whole milk}               => {domestic eggs}            0.005693950  0.1766562 2.7843161    56
    ## [1046] {domestic eggs,                                                                                
    ##         pip fruit}                => {whole milk}               0.005388917  0.6235294 2.4402753    53
    ## [1047] {domestic eggs,                                                                                
    ##         whole milk}               => {pip fruit}                0.005388917  0.1796610 2.3749544    53
    ## [1048] {pip fruit,                                                                                    
    ##         whole milk}               => {domestic eggs}            0.005388917  0.1790541 2.8221100    53
    ## [1049] {citrus fruit,                                                                                 
    ##         domestic eggs}            => {whole milk}               0.005693950  0.5490196 2.1486701    56
    ## [1050] {domestic eggs,                                                                                
    ##         whole milk}               => {citrus fruit}             0.005693950  0.1898305 2.2935910    56
    ## [1051] {citrus fruit,                                                                                 
    ##         whole milk}               => {domestic eggs}            0.005693950  0.1866667 2.9420940    56
    ## [1052] {domestic eggs,                                                                                
    ##         tropical fruit}           => {whole milk}               0.006914082  0.6071429 2.3761441    68
    ## [1053] {domestic eggs,                                                                                
    ##         whole milk}               => {tropical fruit}           0.006914082  0.2305085 2.1967547    68
    ## [1054] {tropical fruit,                                                                               
    ##         whole milk}               => {domestic eggs}            0.006914082  0.1634615 2.5763529    68
    ## [1055] {domestic eggs,                                                                                
    ##         root vegetables}          => {other vegetables}         0.007320793  0.5106383 2.6390582    72
    ## [1056] {domestic eggs,                                                                                
    ##         other vegetables}         => {root vegetables}          0.007320793  0.3287671 3.0162543    72
    ## [1057] {other vegetables,                                                                             
    ##         root vegetables}          => {domestic eggs}            0.007320793  0.1545064 2.4352096    72
    ## [1058] {domestic eggs,                                                                                
    ##         root vegetables}          => {whole milk}               0.008540925  0.5957447 2.3315356    84
    ## [1059] {domestic eggs,                                                                                
    ##         whole milk}               => {root vegetables}          0.008540925  0.2847458 2.6123830    84
    ## [1060] {root vegetables,                                                                              
    ##         whole milk}               => {domestic eggs}            0.008540925  0.1746362 2.7524788    84
    ## [1061] {domestic eggs,                                                                                
    ##         soda}                     => {other vegetables}         0.005083884  0.4098361 2.1180965    50
    ## [1062] {domestic eggs,                                                                                
    ##         other vegetables}         => {soda}                     0.005083884  0.2283105 1.3092908    50
    ## [1063] {other vegetables,                                                                             
    ##         soda}                     => {domestic eggs}            0.005083884  0.1552795 2.4473941    50
    ## [1064] {domestic eggs,                                                                                
    ##         soda}                     => {whole milk}               0.005185562  0.4180328 1.6360336    51
    ## [1065] {domestic eggs,                                                                                
    ##         whole milk}               => {soda}                     0.005185562  0.1728814 0.9914217    51
    ## [1066] {soda,                                                                                         
    ##         whole milk}               => {domestic eggs}            0.005185562  0.1294416 2.0401577    51
    ## [1067] {domestic eggs,                                                                                
    ##         yogurt}                   => {other vegetables}         0.005795628  0.4042553 2.0892544    57
    ## [1068] {domestic eggs,                                                                                
    ##         other vegetables}         => {yogurt}                   0.005795628  0.2602740 1.8657394    57
    ## [1069] {other vegetables,                                                                             
    ##         yogurt}                   => {domestic eggs}            0.005795628  0.1334895 2.1039565    57
    ## [1070] {domestic eggs,                                                                                
    ##         yogurt}                   => {whole milk}               0.007727504  0.5390071 2.1094846    76
    ## [1071] {domestic eggs,                                                                                
    ##         whole milk}               => {yogurt}                   0.007727504  0.2576271 1.8467658    76
    ## [1072] {whole milk,                                                                                   
    ##         yogurt}                   => {domestic eggs}            0.007727504  0.1379310 2.1739611    76
    ## [1073] {domestic eggs,                                                                                
    ##         rolls/buns}               => {other vegetables}         0.005897306  0.3766234 1.9464482    58
    ## [1074] {domestic eggs,                                                                                
    ##         other vegetables}         => {rolls/buns}               0.005897306  0.2648402 1.4398580    58
    ## [1075] {other vegetables,                                                                             
    ##         rolls/buns}               => {domestic eggs}            0.005897306  0.1384248 2.1817438    58
    ## [1076] {domestic eggs,                                                                                
    ##         rolls/buns}               => {whole milk}               0.006609049  0.4220779 1.6518648    65
    ## [1077] {domestic eggs,                                                                                
    ##         whole milk}               => {rolls/buns}               0.006609049  0.2203390 1.1979181    65
    ## [1078] {rolls/buns,                                                                                   
    ##         whole milk}               => {domestic eggs}            0.006609049  0.1166966 1.8392804    65
    ## [1079] {domestic eggs,                                                                                
    ##         other vegetables}         => {whole milk}               0.012302999  0.5525114 2.1623358   121
    ## [1080] {domestic eggs,                                                                                
    ##         whole milk}               => {other vegetables}         0.012302999  0.4101695 2.1198197   121
    ## [1081] {other vegetables,                                                                             
    ##         whole milk}               => {domestic eggs}            0.012302999  0.1644022 2.5911785   121
    ## [1082] {bottled water,                                                                                
    ##         fruit/vegetable juice}    => {soda}                     0.005185562  0.3642857 2.0890671    51
    ## [1083] {fruit/vegetable juice,                                                                        
    ##         soda}                     => {bottled water}            0.005185562  0.2817680 2.5493908    51
    ## [1084] {bottled water,                                                                                
    ##         soda}                     => {fruit/vegetable juice}    0.005185562  0.1789474 2.4753128    51
    ## [1085] {bottled water,                                                                                
    ##         fruit/vegetable juice}    => {whole milk}               0.005795628  0.4071429 1.5934142    57
    ## [1086] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {bottled water}            0.005795628  0.2175573 1.9684228    57
    ## [1087] {bottled water,                                                                                
    ##         whole milk}               => {fruit/vegetable juice}    0.005795628  0.1686391 2.3327216    57
    ## [1088] {fruit/vegetable juice,                                                                        
    ##         tropical fruit}           => {other vegetables}         0.006609049  0.4814815 2.4883712    65
    ## [1089] {fruit/vegetable juice,                                                                        
    ##         other vegetables}         => {tropical fruit}           0.006609049  0.3140097 2.9925242    65
    ## [1090] {other vegetables,                                                                             
    ##         tropical fruit}           => {fruit/vegetable juice}    0.006609049  0.1841360 2.5470849    65
    ## [1091] {fruit/vegetable juice,                                                                        
    ##         tropical fruit}           => {whole milk}               0.005998983  0.4370370 1.7104096    59
    ## [1092] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {tropical fruit}           0.005998983  0.2251908 2.1460774    59
    ## [1093] {tropical fruit,                                                                               
    ##         whole milk}               => {fruit/vegetable juice}    0.005998983  0.1418269 1.9618394    59
    ## [1094] {fruit/vegetable juice,                                                                        
    ##         root vegetables}          => {other vegetables}         0.006609049  0.5508475 2.8468653    65
    ## [1095] {fruit/vegetable juice,                                                                        
    ##         other vegetables}         => {root vegetables}          0.006609049  0.3140097 2.8808629    65
    ## [1096] {other vegetables,                                                                             
    ##         root vegetables}          => {fruit/vegetable juice}    0.006609049  0.1394850 1.9294441    65
    ## [1097] {fruit/vegetable juice,                                                                        
    ##         root vegetables}          => {whole milk}               0.006507372  0.5423729 2.1226571    64
    ## [1098] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {root vegetables}          0.006507372  0.2442748 2.2410847    64
    ## [1099] {root vegetables,                                                                              
    ##         whole milk}               => {fruit/vegetable juice}    0.006507372  0.1330561 1.8405163    64
    ## [1100] {fruit/vegetable juice,                                                                        
    ##         soda}                     => {yogurt}                   0.005083884  0.2762431 1.9802120    50
    ## [1101] {fruit/vegetable juice,                                                                        
    ##         yogurt}                   => {soda}                     0.005083884  0.2717391 1.5583407    50
    ## [1102] {soda,                                                                                         
    ##         yogurt}                   => {fruit/vegetable juice}    0.005083884  0.1858736 2.5711208    50
    ## [1103] {fruit/vegetable juice,                                                                        
    ##         soda}                     => {whole milk}               0.006100661  0.3314917 1.2973422    60
    ## [1104] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {soda}                     0.006100661  0.2290076 1.3132887    60
    ## [1105] {soda,                                                                                         
    ##         whole milk}               => {fruit/vegetable juice}    0.006100661  0.1522843 2.1064919    60
    ## [1106] {fruit/vegetable juice,                                                                        
    ##         yogurt}                   => {other vegetables}         0.008235892  0.4402174 2.2751120    81
    ## [1107] {fruit/vegetable juice,                                                                        
    ##         other vegetables}         => {yogurt}                   0.008235892  0.3913043 2.8050133    81
    ## [1108] {other vegetables,                                                                             
    ##         yogurt}                   => {fruit/vegetable juice}    0.008235892  0.1896956 2.6239884    81
    ## [1109] {fruit/vegetable juice,                                                                        
    ##         yogurt}                   => {whole milk}               0.009456024  0.5054348 1.9780943    93
    ## [1110] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {yogurt}                   0.009456024  0.3549618 2.5444968    93
    ## [1111] {whole milk,                                                                                   
    ##         yogurt}                   => {fruit/vegetable juice}    0.009456024  0.1687840 2.3347270    93
    ## [1112] {fruit/vegetable juice,                                                                        
    ##         rolls/buns}               => {whole milk}               0.005592272  0.3846154 1.5052496    55
    ## [1113] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {rolls/buns}               0.005592272  0.2099237 1.1412931    55
    ## [1114] {fruit/vegetable juice,                                                                        
    ##         other vegetables}         => {whole milk}               0.010472801  0.4975845 1.9473713   103
    ## [1115] {fruit/vegetable juice,                                                                        
    ##         whole milk}               => {other vegetables}         0.010472801  0.3931298 2.0317558   103
    ## [1116] {other vegetables,                                                                             
    ##         whole milk}               => {fruit/vegetable juice}    0.010472801  0.1399457 1.9358164   103
    ## [1117] {pip fruit,                                                                                    
    ##         whipped/sour cream}       => {other vegetables}         0.005592272  0.6043956 3.1236105    55
    ## [1118] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {pip fruit}                0.005592272  0.1936620 2.5600343    55
    ## [1119] {other vegetables,                                                                             
    ##         pip fruit}                => {whipped/sour cream}       0.005592272  0.2140078 2.9854844    55
    ## [1120] {pip fruit,                                                                                    
    ##         whipped/sour cream}       => {whole milk}               0.005998983  0.6483516 2.5374208    59
    ## [1121] {whipped/sour cream,                                                                           
    ##         whole milk}               => {pip fruit}                0.005998983  0.1861199 2.4603346    59
    ## [1122] {pip fruit,                                                                                    
    ##         whole milk}               => {whipped/sour cream}       0.005998983  0.1993243 2.7806450    59
    ## [1123] {citrus fruit,                                                                                 
    ##         whipped/sour cream}       => {other vegetables}         0.005693950  0.5233645 2.7048291    56
    ## [1124] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {citrus fruit}             0.005693950  0.1971831 2.3824272    56
    ## [1125] {citrus fruit,                                                                                 
    ##         other vegetables}         => {whipped/sour cream}       0.005693950  0.1971831 2.7507741    56
    ## [1126] {citrus fruit,                                                                                 
    ##         whipped/sour cream}       => {whole milk}               0.006304016  0.5794393 2.2677219    62
    ## [1127] {whipped/sour cream,                                                                           
    ##         whole milk}               => {citrus fruit}             0.006304016  0.1955836 2.3631016    62
    ## [1128] {citrus fruit,                                                                                 
    ##         whole milk}               => {whipped/sour cream}       0.006304016  0.2066667 2.8830733    62
    ## [1129] {sausage,                                                                                      
    ##         whipped/sour cream}       => {whole milk}               0.005083884  0.5617978 2.1986792    50
    ## [1130] {whipped/sour cream,                                                                           
    ##         whole milk}               => {sausage}                  0.005083884  0.1577287 1.6788548    50
    ## [1131] {sausage,                                                                                      
    ##         whole milk}               => {whipped/sour cream}       0.005083884  0.1700680 2.3725093    50
    ## [1132] {tropical fruit,                                                                               
    ##         whipped/sour cream}       => {yogurt}                   0.006202339  0.4485294 3.2152236    61
    ## [1133] {whipped/sour cream,                                                                           
    ##         yogurt}                   => {tropical fruit}           0.006202339  0.2990196 2.8496685    61
    ## [1134] {tropical fruit,                                                                               
    ##         yogurt}                   => {whipped/sour cream}       0.006202339  0.2118056 2.9547626    61
    ## [1135] {tropical fruit,                                                                               
    ##         whipped/sour cream}       => {other vegetables}         0.007829181  0.5661765 2.9260881    77
    ## [1136] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {tropical fruit}           0.007829181  0.2711268 2.5838485    77
    ## [1137] {other vegetables,                                                                             
    ##         tropical fruit}           => {whipped/sour cream}       0.007829181  0.2181303 3.0429952    77
    ## [1138] {tropical fruit,                                                                               
    ##         whipped/sour cream}       => {whole milk}               0.007930859  0.5735294 2.2445928    78
    ## [1139] {whipped/sour cream,                                                                           
    ##         whole milk}               => {tropical fruit}           0.007930859  0.2460568 2.3449307    78
    ## [1140] {tropical fruit,                                                                               
    ##         whole milk}               => {whipped/sour cream}       0.007930859  0.1875000 2.6156915    78
    ## [1141] {root vegetables,                                                                              
    ##         whipped/sour cream}       => {yogurt}                   0.006405694  0.3750000 2.6881378    63
    ## [1142] {whipped/sour cream,                                                                           
    ##         yogurt}                   => {root vegetables}          0.006405694  0.3088235 2.8332830    63
    ## [1143] {root vegetables,                                                                              
    ##         yogurt}                   => {whipped/sour cream}       0.006405694  0.2480315 3.4601273    63
    ## [1144] {root vegetables,                                                                              
    ##         whipped/sour cream}       => {other vegetables}         0.008540925  0.5000000 2.5840778    84
    ## [1145] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {root vegetables}          0.008540925  0.2957746 2.7135668    84
    ## [1146] {other vegetables,                                                                             
    ##         root vegetables}          => {whipped/sour cream}       0.008540925  0.1802575 2.5146562    84
    ## [1147] {root vegetables,                                                                              
    ##         whipped/sour cream}       => {whole milk}               0.009456024  0.5535714 2.1664843    93
    ## [1148] {whipped/sour cream,                                                                           
    ##         whole milk}               => {root vegetables}          0.009456024  0.2933754 2.6915550    93
    ## [1149] {root vegetables,                                                                              
    ##         whole milk}               => {whipped/sour cream}       0.009456024  0.1933472 2.6972619    93
    ## [1150] {soda,                                                                                         
    ##         whipped/sour cream}       => {whole milk}               0.005490595  0.4736842 1.8538337    54
    ## [1151] {whipped/sour cream,                                                                           
    ##         whole milk}               => {soda}                     0.005490595  0.1703470 0.9768879    54
    ## [1152] {soda,                                                                                         
    ##         whole milk}               => {whipped/sour cream}       0.005490595  0.1370558 1.9119775    54
    ## [1153] {whipped/sour cream,                                                                           
    ##         yogurt}                   => {other vegetables}         0.010167768  0.4901961 2.5334096   100
    ## [1154] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {yogurt}                   0.010167768  0.3521127 2.5240730   100
    ## [1155] {other vegetables,                                                                             
    ##         yogurt}                   => {whipped/sour cream}       0.010167768  0.2341920 3.2670620   100
    ## [1156] {whipped/sour cream,                                                                           
    ##         yogurt}                   => {whole milk}               0.010879512  0.5245098 2.0527473   107
    ## [1157] {whipped/sour cream,                                                                           
    ##         whole milk}               => {yogurt}                   0.010879512  0.3375394 2.4196066   107
    ## [1158] {whole milk,                                                                                   
    ##         yogurt}                   => {whipped/sour cream}       0.010879512  0.1941924 2.7090525   107
    ## [1159] {rolls/buns,                                                                                   
    ##         whipped/sour cream}       => {other vegetables}         0.006710727  0.4583333 2.3687380    66
    ## [1160] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {rolls/buns}               0.006710727  0.2323944 1.2634597    66
    ## [1161] {other vegetables,                                                                             
    ##         rolls/buns}               => {whipped/sour cream}       0.006710727  0.1575179 2.1974306    66
    ## [1162] {rolls/buns,                                                                                   
    ##         whipped/sour cream}       => {whole milk}               0.007829181  0.5347222 2.0927151    77
    ## [1163] {whipped/sour cream,                                                                           
    ##         whole milk}               => {rolls/buns}               0.007829181  0.2429022 1.3205877    77
    ## [1164] {rolls/buns,                                                                                   
    ##         whole milk}               => {whipped/sour cream}       0.007829181  0.1382406 1.9285050    77
    ## [1165] {other vegetables,                                                                             
    ##         whipped/sour cream}       => {whole milk}               0.014641586  0.5070423 1.9843854   144
    ## [1166] {whipped/sour cream,                                                                           
    ##         whole milk}               => {other vegetables}         0.014641586  0.4542587 2.3476795   144
    ## [1167] {other vegetables,                                                                             
    ##         whole milk}               => {whipped/sour cream}       0.014641586  0.1956522 2.7294172   144
    ## [1168] {pastry,                                                                                       
    ##         pip fruit}                => {whole milk}               0.005083884  0.4761905 1.8636424    50
    ## [1169] {pip fruit,                                                                                    
    ##         whole milk}               => {pastry}                   0.005083884  0.1689189 1.8986486    50
    ## [1170] {pastry,                                                                                       
    ##         whole milk}               => {pip fruit}                0.005083884  0.1529052 2.0212670    50
    ## [1171] {citrus fruit,                                                                                 
    ##         pip fruit}                => {tropical fruit}           0.005592272  0.4044118 3.8540598    55
    ## [1172] {pip fruit,                                                                                    
    ##         tropical fruit}           => {citrus fruit}             0.005592272  0.2736318 3.3061046    55
    ## [1173] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {pip fruit}                0.005592272  0.2806122 3.7094374    55
    ## [1174] {citrus fruit,                                                                                 
    ##         pip fruit}                => {other vegetables}         0.005897306  0.4264706 2.2040663    58
    ## [1175] {other vegetables,                                                                             
    ##         pip fruit}                => {citrus fruit}             0.005897306  0.2256809 2.7267469    58
    ## [1176] {citrus fruit,                                                                                 
    ##         other vegetables}         => {pip fruit}                0.005897306  0.2042254 2.6996725    58
    ## [1177] {citrus fruit,                                                                                 
    ##         pip fruit}                => {whole milk}               0.005185562  0.3750000 1.4676184    51
    ## [1178] {pip fruit,                                                                                    
    ##         whole milk}               => {citrus fruit}             0.005185562  0.1722973 2.0817493    51
    ## [1179] {citrus fruit,                                                                                 
    ##         whole milk}               => {pip fruit}                0.005185562  0.1700000 2.2472446    51
    ## [1180] {pip fruit,                                                                                    
    ##         sausage}                  => {whole milk}               0.005592272  0.5188679 2.0306669    55
    ## [1181] {pip fruit,                                                                                    
    ##         whole milk}               => {sausage}                  0.005592272  0.1858108 1.9777590    55
    ## [1182] {sausage,                                                                                      
    ##         whole milk}               => {pip fruit}                0.005592272  0.1870748 2.4729583    55
    ## [1183] {pip fruit,                                                                                    
    ##         tropical fruit}           => {root vegetables}          0.005287239  0.2587065 2.3734870    52
    ## [1184] {pip fruit,                                                                                    
    ##         root vegetables}          => {tropical fruit}           0.005287239  0.3398693 3.2389674    52
    ## [1185] {root vegetables,                                                                              
    ##         tropical fruit}           => {pip fruit}                0.005287239  0.2512077 3.3207366    52
    ## [1186] {pip fruit,                                                                                    
    ##         tropical fruit}           => {yogurt}                   0.006405694  0.3134328 2.2468017    63
    ## [1187] {pip fruit,                                                                                    
    ##         yogurt}                   => {tropical fruit}           0.006405694  0.3559322 3.3920477    63
    ## [1188] {tropical fruit,                                                                               
    ##         yogurt}                   => {pip fruit}                0.006405694  0.2187500 2.8916751    63
    ## [1189] {pip fruit,                                                                                    
    ##         tropical fruit}           => {other vegetables}         0.009456024  0.4626866 2.3912361    93
    ## [1190] {other vegetables,                                                                             
    ##         pip fruit}                => {tropical fruit}           0.009456024  0.3618677 3.4486132    93
    ## [1191] {other vegetables,                                                                             
    ##         tropical fruit}           => {pip fruit}                0.009456024  0.2634561 3.4826487    93
    ## [1192] {pip fruit,                                                                                    
    ##         tropical fruit}           => {whole milk}               0.008439248  0.4129353 1.6160839    83
    ## [1193] {pip fruit,                                                                                    
    ##         whole milk}               => {tropical fruit}           0.008439248  0.2804054 2.6722744    83
    ## [1194] {tropical fruit,                                                                               
    ##         whole milk}               => {pip fruit}                0.008439248  0.1995192 2.6374619    83
    ## [1195] {pip fruit,                                                                                    
    ##         root vegetables}          => {yogurt}                   0.005287239  0.3398693 2.4363079    52
    ## [1196] {pip fruit,                                                                                    
    ##         yogurt}                   => {root vegetables}          0.005287239  0.2937853 2.6953158    52
    ## [1197] {root vegetables,                                                                              
    ##         yogurt}                   => {pip fruit}                0.005287239  0.2047244 2.7062696    52
    ## [1198] {pip fruit,                                                                                    
    ##         root vegetables}          => {other vegetables}         0.008134215  0.5228758 2.7023036    80
    ## [1199] {other vegetables,                                                                             
    ##         pip fruit}                => {root vegetables}          0.008134215  0.3112840 2.8558569    80
    ## [1200] {other vegetables,                                                                             
    ##         root vegetables}          => {pip fruit}                0.008134215  0.1716738 2.2693710    80
    ## [1201] {pip fruit,                                                                                    
    ##         root vegetables}          => {whole milk}               0.008947636  0.5751634 2.2509877    88
    ## [1202] {pip fruit,                                                                                    
    ##         whole milk}               => {root vegetables}          0.008947636  0.2972973 2.7275363    88
    ## [1203] {root vegetables,                                                                              
    ##         whole milk}               => {pip fruit}                0.008947636  0.1829522 2.4184606    88
    ## [1204] {pip fruit,                                                                                    
    ##         yogurt}                   => {other vegetables}         0.008134215  0.4519774 2.3358895    80
    ## [1205] {other vegetables,                                                                             
    ##         pip fruit}                => {yogurt}                   0.008134215  0.3112840 2.2313984    80
    ## [1206] {other vegetables,                                                                             
    ##         yogurt}                   => {pip fruit}                0.008134215  0.1873536 2.4766438    80
    ## [1207] {pip fruit,                                                                                    
    ##         yogurt}                   => {whole milk}               0.009557702  0.5310734 2.0784351    94
    ## [1208] {pip fruit,                                                                                    
    ##         whole milk}               => {yogurt}                   0.009557702  0.3175676 2.2764410    94
    ## [1209] {whole milk,                                                                                   
    ##         yogurt}                   => {pip fruit}                0.009557702  0.1705989 2.2551617    94
    ## [1210] {pip fruit,                                                                                    
    ##         rolls/buns}               => {other vegetables}         0.005083884  0.3649635 1.8861882    50
    ## [1211] {other vegetables,                                                                             
    ##         pip fruit}                => {rolls/buns}               0.005083884  0.1945525 1.0577248    50
    ## [1212] {other vegetables,                                                                             
    ##         rolls/buns}               => {pip fruit}                0.005083884  0.1193317 1.5774566    50
    ## [1213] {pip fruit,                                                                                    
    ##         rolls/buns}               => {whole milk}               0.006202339  0.4452555 1.7425737    61
    ## [1214] {pip fruit,                                                                                    
    ##         whole milk}               => {rolls/buns}               0.006202339  0.2060811 1.1204021    61
    ## [1215] {rolls/buns,                                                                                   
    ##         whole milk}               => {pip fruit}                0.006202339  0.1095153 1.4476916    61
    ## [1216] {other vegetables,                                                                             
    ##         pip fruit}                => {whole milk}               0.013523132  0.5175097 2.0253514   133
    ## [1217] {pip fruit,                                                                                    
    ##         whole milk}               => {other vegetables}         0.013523132  0.4493243 2.3221780   133
    ## [1218] {other vegetables,                                                                             
    ##         whole milk}               => {pip fruit}                0.013523132  0.1807065 2.3887751   133
    ## [1219] {pastry,                                                                                       
    ##         sausage}                  => {whole milk}               0.005693950  0.4552846 1.7818239    56
    ## [1220] {pastry,                                                                                       
    ##         whole milk}               => {sausage}                  0.005693950  0.1712538 1.8228153    56
    ## [1221] {sausage,                                                                                      
    ##         whole milk}               => {pastry}                   0.005693950  0.1904762 2.1409524    56
    ## [1222] {pastry,                                                                                       
    ##         tropical fruit}           => {other vegetables}         0.005083884  0.3846154 1.9877521    50
    ## [1223] {other vegetables,                                                                             
    ##         pastry}                   => {tropical fruit}           0.005083884  0.2252252 2.1464051    50
    ## [1224] {other vegetables,                                                                             
    ##         tropical fruit}           => {pastry}                   0.005083884  0.1416431 1.5920680    50
    ## [1225] {pastry,                                                                                       
    ##         tropical fruit}           => {whole milk}               0.006710727  0.5076923 1.9869295    66
    ## [1226] {pastry,                                                                                       
    ##         whole milk}               => {tropical fruit}           0.006710727  0.2018349 1.9234941    66
    ## [1227] {tropical fruit,                                                                               
    ##         whole milk}               => {pastry}                   0.006710727  0.1586538 1.7832692    66
    ## [1228] {pastry,                                                                                       
    ##         root vegetables}          => {other vegetables}         0.005897306  0.5370370 2.7754909    58
    ## [1229] {other vegetables,                                                                             
    ##         pastry}                   => {root vegetables}          0.005897306  0.2612613 2.3969258    58
    ## [1230] {other vegetables,                                                                             
    ##         root vegetables}          => {pastry}                   0.005897306  0.1244635 1.3989700    58
    ## [1231] {pastry,                                                                                       
    ##         root vegetables}          => {whole milk}               0.005693950  0.5185185 2.0292995    56
    ## [1232] {pastry,                                                                                       
    ##         whole milk}               => {root vegetables}          0.005693950  0.1712538 1.5711580    56
    ## [1233] {root vegetables,                                                                              
    ##         whole milk}               => {pastry}                   0.005693950  0.1164241 1.3086071    56
    ## [1234] {pastry,                                                                                       
    ##         soda}                     => {rolls/buns}               0.005388917  0.2560386 1.3920067    53
    ## [1235] {pastry,                                                                                       
    ##         rolls/buns}               => {soda}                     0.005388917  0.2572816 1.4754309    53
    ## [1236] {rolls/buns,                                                                                   
    ##         soda}                     => {pastry}                   0.005388917  0.1405836 1.5801592    53
    ## [1237] {pastry,                                                                                       
    ##         soda}                     => {other vegetables}         0.005490595  0.2608696 1.3482145    54
    ## [1238] {other vegetables,                                                                             
    ##         pastry}                   => {soda}                     0.005490595  0.2432432 1.3949255    54
    ## [1239] {other vegetables,                                                                             
    ##         soda}                     => {pastry}                   0.005490595  0.1677019 1.8849689    54
    ## [1240] {pastry,                                                                                       
    ##         soda}                     => {whole milk}               0.008235892  0.3913043 1.5314279    81
    ## [1241] {pastry,                                                                                       
    ##         whole milk}               => {soda}                     0.008235892  0.2477064 1.4205205    81
    ## [1242] {soda,                                                                                         
    ##         whole milk}               => {pastry}                   0.008235892  0.2055838 2.3107614    81
    ## [1243] {pastry,                                                                                       
    ##         yogurt}                   => {rolls/buns}               0.005795628  0.3275862 1.7809897    57
    ## [1244] {pastry,                                                                                       
    ##         rolls/buns}               => {yogurt}                   0.005795628  0.2766990 1.9834803    57
    ## [1245] {rolls/buns,                                                                                   
    ##         yogurt}                   => {pastry}                   0.005795628  0.1686391 1.8955030    57
    ## [1246] {pastry,                                                                                       
    ##         yogurt}                   => {other vegetables}         0.006609049  0.3735632 1.9306328    65
    ## [1247] {other vegetables,                                                                             
    ##         pastry}                   => {yogurt}                   0.006609049  0.2927928 2.0988463    65
    ## [1248] {other vegetables,                                                                             
    ##         yogurt}                   => {pastry}                   0.006609049  0.1522248 1.7110070    65
    ## [1249] {pastry,                                                                                       
    ##         yogurt}                   => {whole milk}               0.009150991  0.5172414 2.0243012    90
    ## [1250] {pastry,                                                                                       
    ##         whole milk}               => {yogurt}                   0.009150991  0.2752294 1.9729451    90
    ## [1251] {whole milk,                                                                                   
    ##         yogurt}                   => {pastry}                   0.009150991  0.1633394 1.8359347    90
    ## [1252] {pastry,                                                                                       
    ##         rolls/buns}               => {other vegetables}         0.006100661  0.2912621 1.5052880    60
    ## [1253] {other vegetables,                                                                             
    ##         pastry}                   => {rolls/buns}               0.006100661  0.2702703 1.4693798    60
    ## [1254] {other vegetables,                                                                             
    ##         rolls/buns}               => {pastry}                   0.006100661  0.1431981 1.6095465    60
    ## [1255] {pastry,                                                                                       
    ##         rolls/buns}               => {whole milk}               0.008540925  0.4077670 1.5958569    84
    ## [1256] {pastry,                                                                                       
    ##         whole milk}               => {rolls/buns}               0.008540925  0.2568807 1.3965849    84
    ## [1257] {rolls/buns,                                                                                   
    ##         whole milk}               => {pastry}                   0.008540925  0.1508079 1.6950808    84
    ## [1258] {other vegetables,                                                                             
    ##         pastry}                   => {whole milk}               0.010574479  0.4684685 1.8334212   104
    ## [1259] {pastry,                                                                                       
    ##         whole milk}               => {other vegetables}         0.010574479  0.3180428 1.6436947   104
    ## [1260] {other vegetables,                                                                             
    ##         whole milk}               => {pastry}                   0.010574479  0.1413043 1.5882609   104
    ## [1261] {bottled water,                                                                                
    ##         citrus fruit}             => {other vegetables}         0.005083884  0.3759398 1.9429156    50
    ## [1262] {citrus fruit,                                                                                 
    ##         other vegetables}         => {bottled water}            0.005083884  0.1760563 1.5929292    50
    ## [1263] {bottled water,                                                                                
    ##         other vegetables}         => {citrus fruit}             0.005083884  0.2049180 2.4758831    50
    ## [1264] {bottled water,                                                                                
    ##         citrus fruit}             => {whole milk}               0.005897306  0.4360902 1.7067041    58
    ## [1265] {citrus fruit,                                                                                 
    ##         whole milk}               => {bottled water}            0.005897306  0.1933333 1.7492487    58
    ## [1266] {bottled water,                                                                                
    ##         whole milk}               => {citrus fruit}             0.005897306  0.1715976 2.0732957    58
    ## [1267] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {root vegetables}          0.005693950  0.2857143 2.6212687    56
    ## [1268] {citrus fruit,                                                                                 
    ##         root vegetables}          => {tropical fruit}           0.005693950  0.3218391 3.0671389    56
    ## [1269] {root vegetables,                                                                              
    ##         tropical fruit}           => {citrus fruit}             0.005693950  0.2705314 3.2686441    56
    ## [1270] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {yogurt}                   0.006304016  0.3163265 2.2675448    62
    ## [1271] {citrus fruit,                                                                                 
    ##         yogurt}                   => {tropical fruit}           0.006304016  0.2910798 2.7740019    62
    ## [1272] {tropical fruit,                                                                               
    ##         yogurt}                   => {citrus fruit}             0.006304016  0.2152778 2.6010528    62
    ## [1273] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {other vegetables}         0.009049314  0.4540816 2.3467645    89
    ## [1274] {citrus fruit,                                                                                 
    ##         other vegetables}         => {tropical fruit}           0.009049314  0.3133803 2.9865262    89
    ## [1275] {other vegetables,                                                                             
    ##         tropical fruit}           => {citrus fruit}             0.009049314  0.2521246 3.0462480    89
    ## [1276] {citrus fruit,                                                                                 
    ##         tropical fruit}           => {whole milk}               0.009049314  0.4540816 1.7771161    89
    ## [1277] {citrus fruit,                                                                                 
    ##         whole milk}               => {tropical fruit}           0.009049314  0.2966667 2.8272448    89
    ## [1278] {tropical fruit,                                                                               
    ##         whole milk}               => {citrus fruit}             0.009049314  0.2139423 2.5849172    89
    ## [1279] {citrus fruit,                                                                                 
    ##         root vegetables}          => {other vegetables}         0.010371124  0.5862069 3.0296084   102
    ## [1280] {citrus fruit,                                                                                 
    ##         other vegetables}         => {root vegetables}          0.010371124  0.3591549 3.2950455   102
    ## [1281] {other vegetables,                                                                             
    ##         root vegetables}          => {citrus fruit}             0.010371124  0.2188841 2.6446257   102
    ## [1282] {citrus fruit,                                                                                 
    ##         root vegetables}          => {whole milk}               0.009150991  0.5172414 2.0243012    90
    ## [1283] {citrus fruit,                                                                                 
    ##         whole milk}               => {root vegetables}          0.009150991  0.3000000 2.7523321    90
    ## [1284] {root vegetables,                                                                              
    ##         whole milk}               => {citrus fruit}             0.009150991  0.1871102 2.2607232    90
    ## [1285] {citrus fruit,                                                                                 
    ##         yogurt}                   => {rolls/buns}               0.005795628  0.2676056 1.4548930    57
    ## [1286] {citrus fruit,                                                                                 
    ##         rolls/buns}               => {yogurt}                   0.005795628  0.3454545 2.4763451    57
    ## [1287] {rolls/buns,                                                                                   
    ##         yogurt}                   => {citrus fruit}             0.005795628  0.1686391 2.0375492    57
    ## [1288] {citrus fruit,                                                                                 
    ##         yogurt}                   => {other vegetables}         0.007625826  0.3521127 1.8197731    75
    ## [1289] {citrus fruit,                                                                                 
    ##         other vegetables}         => {yogurt}                   0.007625826  0.2640845 1.8930548    75
    ## [1290] {other vegetables,                                                                             
    ##         yogurt}                   => {citrus fruit}             0.007625826  0.1756440 2.1221855    75
    ## [1291] {citrus fruit,                                                                                 
    ##         yogurt}                   => {whole milk}               0.010269446  0.4741784 1.8557678   101
    ## [1292] {citrus fruit,                                                                                 
    ##         whole milk}               => {yogurt}                   0.010269446  0.3366667 2.4133503   101
    ## [1293] {whole milk,                                                                                   
    ##         yogurt}                   => {citrus fruit}             0.010269446  0.1833031 2.2147246   101
    ## [1294] {citrus fruit,                                                                                 
    ##         rolls/buns}               => {other vegetables}         0.005998983  0.3575758 1.8480071    59
    ## [1295] {citrus fruit,                                                                                 
    ##         other vegetables}         => {rolls/buns}               0.005998983  0.2077465 1.1294564    59
    ## [1296] {other vegetables,                                                                             
    ##         rolls/buns}               => {citrus fruit}             0.005998983  0.1408115 1.7013276    59
    ## [1297] {citrus fruit,                                                                                 
    ##         rolls/buns}               => {whole milk}               0.007219115  0.4303030 1.6840550    71
    ## [1298] {citrus fruit,                                                                                 
    ##         whole milk}               => {rolls/buns}               0.007219115  0.2366667 1.2866869    71
    ## [1299] {rolls/buns,                                                                                   
    ##         whole milk}               => {citrus fruit}             0.007219115  0.1274686 1.5401149    71
    ## [1300] {citrus fruit,                                                                                 
    ##         other vegetables}         => {whole milk}               0.013014743  0.4507042 1.7638982   128
    ## [1301] {citrus fruit,                                                                                 
    ##         whole milk}               => {other vegetables}         0.013014743  0.4266667 2.2050797   128
    ## [1302] {other vegetables,                                                                             
    ##         whole milk}               => {citrus fruit}             0.013014743  0.1739130 2.1012712   128
    ## [1303] {sausage,                                                                                      
    ##         shopping bags}            => {soda}                     0.005693950  0.3636364 2.0853432    56
    ## [1304] {shopping bags,                                                                                
    ##         soda}                     => {sausage}                  0.005693950  0.2314050 2.4630604    56
    ## [1305] {sausage,                                                                                      
    ##         soda}                     => {shopping bags}            0.005693950  0.2343096 2.3781580    56
    ## [1306] {sausage,                                                                                      
    ##         shopping bags}            => {rolls/buns}               0.005998983  0.3831169 2.0828936    59
    ## [1307] {rolls/buns,                                                                                   
    ##         shopping bags}            => {sausage}                  0.005998983  0.3072917 3.2707939    59
    ## [1308] {rolls/buns,                                                                                   
    ##         sausage}                  => {shopping bags}            0.005998983  0.1960133 1.9894641    59
    ## [1309] {sausage,                                                                                      
    ##         shopping bags}            => {other vegetables}         0.005388917  0.3441558 1.7786509    53
    ## [1310] {other vegetables,                                                                             
    ##         shopping bags}            => {sausage}                  0.005388917  0.2324561 2.4742491    53
    ## [1311] {other vegetables,                                                                             
    ##         sausage}                  => {shopping bags}            0.005388917  0.2000000 2.0299278    53
    ## [1312] {root vegetables,                                                                              
    ##         shopping bags}            => {other vegetables}         0.006609049  0.5158730 2.6661120    65
    ## [1313] {other vegetables,                                                                             
    ##         shopping bags}            => {root vegetables}          0.006609049  0.2850877 2.6155203    65
    ## [1314] {other vegetables,                                                                             
    ##         root vegetables}          => {shopping bags}            0.006609049  0.1394850 1.4157222    65
    ## [1315] {root vegetables,                                                                              
    ##         shopping bags}            => {whole milk}               0.005287239  0.4126984 1.6151567    52
    ## [1316] {shopping bags,                                                                                
    ##         whole milk}               => {root vegetables}          0.005287239  0.2157676 1.9795473    52
    ## [1317] {root vegetables,                                                                              
    ##         whole milk}               => {shopping bags}            0.005287239  0.1081081 1.0972582    52
    ## [1318] {shopping bags,                                                                                
    ##         soda}                     => {rolls/buns}               0.006304016  0.2561983 1.3928749    62
    ## [1319] {rolls/buns,                                                                                   
    ##         shopping bags}            => {soda}                     0.006304016  0.3229167 1.8518282    62
    ## [1320] {rolls/buns,                                                                                   
    ##         soda}                     => {shopping bags}            0.006304016  0.1644562 1.6691714    62
    ## [1321] {shopping bags,                                                                                
    ##         soda}                     => {other vegetables}         0.005388917  0.2190083 1.1318688    53
    ## [1322] {other vegetables,                                                                             
    ##         shopping bags}            => {soda}                     0.005388917  0.2324561 1.3330648    53
    ## [1323] {other vegetables,                                                                             
    ##         soda}                     => {shopping bags}            0.005388917  0.1645963 1.6705927    53
    ## [1324] {shopping bags,                                                                                
    ##         soda}                     => {whole milk}               0.006812405  0.2768595 1.0835309    67
    ## [1325] {shopping bags,                                                                                
    ##         whole milk}               => {soda}                     0.006812405  0.2780083 1.5942925    67
    ## [1326] {soda,                                                                                         
    ##         whole milk}               => {shopping bags}            0.006812405  0.1700508 1.7259538    67
    ## [1327] {shopping bags,                                                                                
    ##         yogurt}                   => {other vegetables}         0.005388917  0.3533333 1.8260816    53
    ## [1328] {other vegetables,                                                                             
    ##         shopping bags}            => {yogurt}                   0.005388917  0.2324561 1.6663310    53
    ## [1329] {other vegetables,                                                                             
    ##         yogurt}                   => {shopping bags}            0.005388917  0.1241218 1.2597912    53
    ## [1330] {shopping bags,                                                                                
    ##         yogurt}                   => {whole milk}               0.005287239  0.3466667 1.3567317    52
    ## [1331] {shopping bags,                                                                                
    ##         whole milk}               => {yogurt}                   0.005287239  0.2157676 1.5467017    52
    ## [1332] {rolls/buns,                                                                                   
    ##         shopping bags}            => {other vegetables}         0.005287239  0.2708333 1.3997088    52
    ## [1333] {other vegetables,                                                                             
    ##         shopping bags}            => {rolls/buns}               0.005287239  0.2280702 1.2399503    52
    ## [1334] {other vegetables,                                                                             
    ##         rolls/buns}               => {shopping bags}            0.005287239  0.1241050 1.2596210    52
    ## [1335] {rolls/buns,                                                                                   
    ##         shopping bags}            => {whole milk}               0.005287239  0.2708333 1.0599466    52
    ## [1336] {shopping bags,                                                                                
    ##         whole milk}               => {rolls/buns}               0.005287239  0.2157676 1.1730651    52
    ## [1337] {other vegetables,                                                                             
    ##         shopping bags}            => {whole milk}               0.007625826  0.3289474 1.2873845    75
    ## [1338] {shopping bags,                                                                                
    ##         whole milk}               => {other vegetables}         0.007625826  0.3112033 1.6083472    75
    ## [1339] {other vegetables,                                                                             
    ##         whole milk}               => {shopping bags}            0.007625826  0.1019022 1.0342703    75
    ## [1340] {bottled water,                                                                                
    ##         sausage}                  => {other vegetables}         0.005083884  0.4237288 2.1898964    50
    ## [1341] {other vegetables,                                                                             
    ##         sausage}                  => {bottled water}            0.005083884  0.1886792 1.7071393    50
    ## [1342] {bottled water,                                                                                
    ##         other vegetables}         => {sausage}                  0.005083884  0.2049180 2.1811351    50
    ## [1343] {sausage,                                                                                      
    ##         tropical fruit}           => {other vegetables}         0.005998983  0.4306569 2.2257020    59
    ## [1344] {other vegetables,                                                                             
    ##         sausage}                  => {tropical fruit}           0.005998983  0.2226415 2.1217822    59
    ## [1345] {other vegetables,                                                                             
    ##         tropical fruit}           => {sausage}                  0.005998983  0.1671388 1.7790154    59
    ## [1346] {sausage,                                                                                      
    ##         tropical fruit}           => {whole milk}               0.007219115  0.5182482 2.0282415    71
    ## [1347] {sausage,                                                                                      
    ##         whole milk}               => {tropical fruit}           0.007219115  0.2414966 2.3014719    71
    ## [1348] {tropical fruit,                                                                               
    ##         whole milk}               => {sausage}                  0.007219115  0.1706731 1.8166339    71
    ## [1349] {root vegetables,                                                                              
    ##         sausage}                  => {yogurt}                   0.005185562  0.3469388 2.4869846    51
    ## [1350] {sausage,                                                                                      
    ##         yogurt}                   => {root vegetables}          0.005185562  0.2642487 2.4243340    51
    ## [1351] {root vegetables,                                                                              
    ##         yogurt}                   => {sausage}                  0.005185562  0.2007874 2.1371689    51
    ## [1352] {root vegetables,                                                                              
    ##         sausage}                  => {other vegetables}         0.006812405  0.4557823 2.3555539    67
    ## [1353] {other vegetables,                                                                             
    ##         sausage}                  => {root vegetables}          0.006812405  0.2528302 2.3195755    67
    ## [1354] {other vegetables,                                                                             
    ##         root vegetables}          => {sausage}                  0.006812405  0.1437768 1.5303518    67
    ## [1355] {root vegetables,                                                                              
    ##         sausage}                  => {whole milk}               0.007727504  0.5170068 2.0233832    76
    ## [1356] {sausage,                                                                                      
    ##         whole milk}               => {root vegetables}          0.007727504  0.2585034 2.3716240    76
    ## [1357] {root vegetables,                                                                              
    ##         whole milk}               => {sausage}                  0.007727504  0.1580042 1.6817867    76
    ## [1358] {sausage,                                                                                      
    ##         soda}                     => {yogurt}                   0.005592272  0.2301255 1.6496243    55
    ## [1359] {sausage,                                                                                      
    ##         yogurt}                   => {soda}                     0.005592272  0.2849741 1.6342392    55
    ## [1360] {soda,                                                                                         
    ##         yogurt}                   => {sausage}                  0.005592272  0.2044610 2.1762701    55
    ## [1361] {sausage,                                                                                      
    ##         soda}                     => {rolls/buns}               0.009659380  0.3974895 2.1610335    95
    ## [1362] {rolls/buns,                                                                                   
    ##         sausage}                  => {soda}                     0.009659380  0.3156146 1.8099532    95
    ## [1363] {rolls/buns,                                                                                   
    ##         soda}                     => {sausage}                  0.009659380  0.2519894 2.6821598    95
    ## [1364] {sausage,                                                                                      
    ##         soda}                     => {other vegetables}         0.007219115  0.2970711 1.5353098    71
    ## [1365] {other vegetables,                                                                             
    ##         sausage}                  => {soda}                     0.007219115  0.2679245 1.5364652    71
    ## [1366] {other vegetables,                                                                             
    ##         soda}                     => {sausage}                  0.007219115  0.2204969 2.3469556    71
    ## [1367] {sausage,                                                                                      
    ##         soda}                     => {whole milk}               0.006710727  0.2761506 1.0807566    66
    ## [1368] {sausage,                                                                                      
    ##         whole milk}               => {soda}                     0.006710727  0.2244898 1.2873803    66
    ## [1369] {soda,                                                                                         
    ##         whole milk}               => {sausage}                  0.006710727  0.1675127 1.7829949    66
    ## [1370] {sausage,                                                                                      
    ##         yogurt}                   => {rolls/buns}               0.005998983  0.3056995 1.6619980    59
    ## [1371] {rolls/buns,                                                                                   
    ##         sausage}                  => {yogurt}                   0.005998983  0.1960133 1.4050953    59
    ## [1372] {rolls/buns,                                                                                   
    ##         yogurt}                   => {sausage}                  0.005998983  0.1745562 1.8579658    59
    ## [1373] {sausage,                                                                                      
    ##         yogurt}                   => {other vegetables}         0.008134215  0.4145078 2.1422406    80
    ## [1374] {other vegetables,                                                                             
    ##         sausage}                  => {yogurt}                   0.008134215  0.3018868 2.1640354    80
    ## [1375] {other vegetables,                                                                             
    ##         yogurt}                   => {sausage}                  0.008134215  0.1873536 1.9941807    80
    ## [1376] {sausage,                                                                                      
    ##         yogurt}                   => {whole milk}               0.008744281  0.4455959 1.7439058    86
    ## [1377] {sausage,                                                                                      
    ##         whole milk}               => {yogurt}                   0.008744281  0.2925170 2.0968694    86
    ## [1378] {whole milk,                                                                                   
    ##         yogurt}                   => {sausage}                  0.008744281  0.1560799 1.6613045    86
    ## [1379] {rolls/buns,                                                                                   
    ##         sausage}                  => {other vegetables}         0.008845958  0.2890365 1.4937858    87
    ## [1380] {other vegetables,                                                                             
    ##         sausage}                  => {rolls/buns}               0.008845958  0.3283019 1.7848806    87
    ## [1381] {other vegetables,                                                                             
    ##         rolls/buns}               => {sausage}                  0.008845958  0.2076372 2.2100781    87
    ## [1382] {rolls/buns,                                                                                   
    ##         sausage}                  => {whole milk}               0.009354347  0.3056478 1.1961984    92
    ## [1383] {sausage,                                                                                      
    ##         whole milk}               => {rolls/buns}               0.009354347  0.3129252 1.7012820    92
    ## [1384] {rolls/buns,                                                                                   
    ##         whole milk}               => {sausage}                  0.009354347  0.1651706 1.7580654    92
    ## [1385] {other vegetables,                                                                             
    ##         sausage}                  => {whole milk}               0.010167768  0.3773585 1.4768487   100
    ## [1386] {sausage,                                                                                      
    ##         whole milk}               => {other vegetables}         0.010167768  0.3401361 1.7578760   100
    ## [1387] {other vegetables,                                                                             
    ##         whole milk}               => {sausage}                  0.010167768  0.1358696 1.4461874   100
    ## [1388] {bottled water,                                                                                
    ##         tropical fruit}           => {soda}                     0.005185562  0.2802198 1.6069747    51
    ## [1389] {bottled water,                                                                                
    ##         soda}                     => {tropical fruit}           0.005185562  0.1789474 1.7053754    51
    ## [1390] {soda,                                                                                         
    ##         tropical fruit}           => {bottled water}            0.005185562  0.2487805 2.2509256    51
    ## [1391] {bottled water,                                                                                
    ##         tropical fruit}           => {yogurt}                   0.007117438  0.3846154 2.7570644    70
    ## [1392] {bottled water,                                                                                
    ##         yogurt}                   => {tropical fruit}           0.007117438  0.3097345 2.9517819    70
    ## [1393] {tropical fruit,                                                                               
    ##         yogurt}                   => {bottled water}            0.007117438  0.2430556 2.1991273    70
    ## [1394] {bottled water,                                                                                
    ##         tropical fruit}           => {rolls/buns}               0.005388917  0.2912088 1.5832164    53
    ## [1395] {bottled water,                                                                                
    ##         rolls/buns}               => {tropical fruit}           0.005388917  0.2226891 2.1222355    53
    ## [1396] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {bottled water}            0.005388917  0.2190083 1.9815513    53
    ## [1397] {bottled water,                                                                                
    ##         tropical fruit}           => {other vegetables}         0.006202339  0.3351648 1.7321840    61
    ## [1398] {bottled water,                                                                                
    ##         other vegetables}         => {tropical fruit}           0.006202339  0.2500000 2.3825097    61
    ## [1399] {other vegetables,                                                                             
    ##         tropical fruit}           => {bottled water}            0.006202339  0.1728045 1.5635074    61
    ## [1400] {bottled water,                                                                                
    ##         tropical fruit}           => {whole milk}               0.008032537  0.4340659 1.6987817    79
    ## [1401] {bottled water,                                                                                
    ##         whole milk}               => {tropical fruit}           0.008032537  0.2337278 2.2274351    79
    ## [1402] {tropical fruit,                                                                               
    ##         whole milk}               => {bottled water}            0.008032537  0.1899038 1.7182193    79
    ## [1403] {bottled water,                                                                                
    ##         root vegetables}          => {other vegetables}         0.007015760  0.4480519 2.3156022    69
    ## [1404] {bottled water,                                                                                
    ##         other vegetables}         => {root vegetables}          0.007015760  0.2827869 2.5944114    69
    ## [1405] {other vegetables,                                                                             
    ##         root vegetables}          => {bottled water}            0.007015760  0.1480687 1.3397013    69
    ## [1406] {bottled water,                                                                                
    ##         root vegetables}          => {whole milk}               0.007320793  0.4675325 1.8297580    72
    ## [1407] {bottled water,                                                                                
    ##         whole milk}               => {root vegetables}          0.007320793  0.2130178 1.9543186    72
    ## [1408] {root vegetables,                                                                              
    ##         whole milk}               => {bottled water}            0.007320793  0.1496881 1.3543541    72
    ## [1409] {bottled water,                                                                                
    ##         soda}                     => {yogurt}                   0.007422471  0.2561404 1.8361081    73
    ## [1410] {bottled water,                                                                                
    ##         yogurt}                   => {soda}                     0.007422471  0.3230088 1.8523569    73
    ## [1411] {soda,                                                                                         
    ##         yogurt}                   => {bottled water}            0.007422471  0.2713755 2.4553613    73
    ## [1412] {bottled water,                                                                                
    ##         soda}                     => {rolls/buns}               0.006812405  0.2350877 1.2781027    67
    ## [1413] {bottled water,                                                                                
    ##         rolls/buns}               => {soda}                     0.006812405  0.2815126 1.6143886    67
    ## [1414] {rolls/buns,                                                                                   
    ##         soda}                     => {bottled water}            0.006812405  0.1777188 1.6079712    67
    ## [1415] {bottled water,                                                                                
    ##         soda}                     => {other vegetables}         0.005693950  0.1964912 1.0154972    56
    ## [1416] {bottled water,                                                                                
    ##         other vegetables}         => {soda}                     0.005693950  0.2295082 1.3161593    56
    ## [1417] {other vegetables,                                                                             
    ##         soda}                     => {bottled water}            0.005693950  0.1739130 1.5735371    56
    ## [1418] {bottled water,                                                                                
    ##         soda}                     => {whole milk}               0.007524148  0.2596491 1.0161755    74
    ## [1419] {bottled water,                                                                                
    ##         whole milk}               => {soda}                     0.007524148  0.2189349 1.2555247    74
    ## [1420] {soda,                                                                                         
    ##         whole milk}               => {bottled water}            0.007524148  0.1878173 1.6993401    74
    ## [1421] {bottled water,                                                                                
    ##         yogurt}                   => {rolls/buns}               0.007117438  0.3097345 1.6839353    70
    ## [1422] {bottled water,                                                                                
    ##         rolls/buns}               => {yogurt}                   0.007117438  0.2941176 2.1083433    70
    ## [1423] {rolls/buns,                                                                                   
    ##         yogurt}                   => {bottled water}            0.007117438  0.2071006 1.8738126    70
    ## [1424] {bottled water,                                                                                
    ##         yogurt}                   => {other vegetables}         0.008134215  0.3539823 1.8294356    80
    ## [1425] {bottled water,                                                                                
    ##         other vegetables}         => {yogurt}                   0.008134215  0.3278689 2.3502844    80
    ## [1426] {other vegetables,                                                                             
    ##         yogurt}                   => {bottled water}            0.008134215  0.1873536 1.6951453    80
    ## [1427] {bottled water,                                                                                
    ##         yogurt}                   => {whole milk}               0.009659380  0.4203540 1.6451180    95
    ## [1428] {bottled water,                                                                                
    ##         whole milk}               => {yogurt}                   0.009659380  0.2810651 2.0147778    95
    ## [1429] {whole milk,                                                                                   
    ##         yogurt}                   => {bottled water}            0.009659380  0.1724138 1.5599721    95
    ## [1430] {bottled water,                                                                                
    ##         rolls/buns}               => {other vegetables}         0.007320793  0.3025210 1.5634756    72
    ## [1431] {bottled water,                                                                                
    ##         other vegetables}         => {rolls/buns}               0.007320793  0.2950820 1.6042737    72
    ## [1432] {other vegetables,                                                                             
    ##         rolls/buns}               => {bottled water}            0.007320793  0.1718377 1.5547598    72
    ## [1433] {bottled water,                                                                                
    ##         rolls/buns}               => {whole milk}               0.008744281  0.3613445 1.4141757    86
    ## [1434] {bottled water,                                                                                
    ##         whole milk}               => {rolls/buns}               0.008744281  0.2544379 1.3833037    86
    ## [1435] {rolls/buns,                                                                                   
    ##         whole milk}               => {bottled water}            0.008744281  0.1543986 1.3969732    86
    ## [1436] {bottled water,                                                                                
    ##         other vegetables}         => {whole milk}               0.010777834  0.4344262 1.7001918   106
    ## [1437] {bottled water,                                                                                
    ##         whole milk}               => {other vegetables}         0.010777834  0.3136095 1.6207825   106
    ## [1438] {other vegetables,                                                                             
    ##         whole milk}               => {bottled water}            0.010777834  0.1440217 1.3030854   106
    ## [1439] {root vegetables,                                                                              
    ##         tropical fruit}           => {yogurt}                   0.008134215  0.3864734 2.7703835    80
    ## [1440] {tropical fruit,                                                                               
    ##         yogurt}                   => {root vegetables}          0.008134215  0.2777778 2.5484556    80
    ## [1441] {root vegetables,                                                                              
    ##         yogurt}                   => {tropical fruit}           0.008134215  0.3149606 3.0015870    80
    ## [1442] {root vegetables,                                                                              
    ##         tropical fruit}           => {rolls/buns}               0.005897306  0.2801932 1.5233281    58
    ## [1443] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {root vegetables}          0.005897306  0.2396694 2.1988328    58
    ## [1444] {rolls/buns,                                                                                   
    ##         root vegetables}          => {tropical fruit}           0.005897306  0.2426778 2.3127291    58
    ## [1445] {root vegetables,                                                                              
    ##         tropical fruit}           => {other vegetables}         0.012302999  0.5845411 3.0209991   121
    ## [1446] {other vegetables,                                                                             
    ##         tropical fruit}           => {root vegetables}          0.012302999  0.3427762 3.1447798   121
    ## [1447] {other vegetables,                                                                             
    ##         root vegetables}          => {tropical fruit}           0.012302999  0.2596567 2.4745380   121
    ## [1448] {root vegetables,                                                                              
    ##         tropical fruit}           => {whole milk}               0.011997966  0.5700483 2.2309690   118
    ## [1449] {tropical fruit,                                                                               
    ##         whole milk}               => {root vegetables}          0.011997966  0.2836538 2.6023653   118
    ## [1450] {root vegetables,                                                                              
    ##         whole milk}               => {tropical fruit}           0.011997966  0.2453222 2.3379305   118
    ## [1451] {soda,                                                                                         
    ##         tropical fruit}           => {yogurt}                   0.006609049  0.3170732 2.2728970    65
    ## [1452] {tropical fruit,                                                                               
    ##         yogurt}                   => {soda}                     0.006609049  0.2256944 1.2942885    65
    ## [1453] {soda,                                                                                         
    ##         yogurt}                   => {tropical fruit}           0.006609049  0.2416357 2.3027975    65
    ## [1454] {soda,                                                                                         
    ##         tropical fruit}           => {rolls/buns}               0.005388917  0.2585366 1.4055872    53
    ## [1455] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {soda}                     0.005388917  0.2190083 1.2559454    53
    ## [1456] {rolls/buns,                                                                                   
    ##         soda}                     => {tropical fruit}           0.005388917  0.1405836 1.3397667    53
    ## [1457] {soda,                                                                                         
    ##         tropical fruit}           => {other vegetables}         0.007219115  0.3463415 1.7899466    71
    ## [1458] {other vegetables,                                                                             
    ##         tropical fruit}           => {soda}                     0.007219115  0.2011331 1.1534370    71
    ## [1459] {other vegetables,                                                                             
    ##         soda}                     => {tropical fruit}           0.007219115  0.2204969 2.1013440    71
    ## [1460] {soda,                                                                                         
    ##         tropical fruit}           => {whole milk}               0.007829181  0.3756098 1.4700048    77
    ## [1461] {tropical fruit,                                                                               
    ##         whole milk}               => {soda}                     0.007829181  0.1850962 1.0614698    77
    ## [1462] {soda,                                                                                         
    ##         whole milk}               => {tropical fruit}           0.007829181  0.1954315 1.8624695    77
    ## [1463] {tropical fruit,                                                                               
    ##         yogurt}                   => {rolls/buns}               0.008744281  0.2986111 1.6234606    86
    ## [1464] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {yogurt}                   0.008744281  0.3553719 2.5474363    86
    ## [1465] {rolls/buns,                                                                                   
    ##         yogurt}                   => {tropical fruit}           0.008744281  0.2544379 2.4248028    86
    ## [1466] {tropical fruit,                                                                               
    ##         yogurt}                   => {other vegetables}         0.012302999  0.4201389 2.1713431   121
    ## [1467] {other vegetables,                                                                             
    ##         tropical fruit}           => {yogurt}                   0.012302999  0.3427762 2.4571457   121
    ## [1468] {other vegetables,                                                                             
    ##         yogurt}                   => {tropical fruit}           0.012302999  0.2833724 2.7005496   121
    ## [1469] {tropical fruit,                                                                               
    ##         yogurt}                   => {whole milk}               0.015149975  0.5173611 2.0247698   149
    ## [1470] {tropical fruit,                                                                               
    ##         whole milk}               => {yogurt}                   0.015149975  0.3581731 2.5675162   149
    ## [1471] {whole milk,                                                                                   
    ##         yogurt}                   => {tropical fruit}           0.015149975  0.2704174 2.5770885   149
    ## [1472] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {other vegetables}         0.007829181  0.3181818 1.6444131    77
    ## [1473] {other vegetables,                                                                             
    ##         tropical fruit}           => {rolls/buns}               0.007829181  0.2181303 1.1859102    77
    ## [1474] {other vegetables,                                                                             
    ##         rolls/buns}               => {tropical fruit}           0.007829181  0.1837709 1.7513436    77
    ## [1475] {rolls/buns,                                                                                   
    ##         tropical fruit}           => {whole milk}               0.010981190  0.4462810 1.7465872   108
    ## [1476] {tropical fruit,                                                                               
    ##         whole milk}               => {rolls/buns}               0.010981190  0.2596154 1.4114524   108
    ## [1477] {rolls/buns,                                                                                   
    ##         whole milk}               => {tropical fruit}           0.010981190  0.1938959 1.8478352   108
    ## [1478] {other vegetables,                                                                             
    ##         tropical fruit}           => {whole milk}               0.017081851  0.4759207 1.8625865   168
    ## [1479] {tropical fruit,                                                                               
    ##         whole milk}               => {other vegetables}         0.017081851  0.4038462 2.0871397   168
    ## [1480] {other vegetables,                                                                             
    ##         whole milk}               => {tropical fruit}           0.017081851  0.2282609 2.1753349   168
    ## [1481] {root vegetables,                                                                              
    ##         soda}                     => {other vegetables}         0.008235892  0.4426230 2.2875443    81
    ## [1482] {other vegetables,                                                                             
    ##         root vegetables}          => {soda}                     0.008235892  0.1738197 0.9968030    81
    ## [1483] {other vegetables,                                                                             
    ##         soda}                     => {root vegetables}          0.008235892  0.2515528 2.3078561    81
    ## [1484] {root vegetables,                                                                              
    ##         soda}                     => {whole milk}               0.008134215  0.4371585 1.7108848    80
    ## [1485] {root vegetables,                                                                              
    ##         whole milk}               => {soda}                     0.008134215  0.1663202 0.9537952    80
    ## [1486] {soda,                                                                                         
    ##         whole milk}               => {root vegetables}          0.008134215  0.2030457 1.8628305    80
    ## [1487] {root vegetables,                                                                              
    ##         yogurt}                   => {rolls/buns}               0.007219115  0.2795276 1.5197090    71
    ## [1488] {rolls/buns,                                                                                   
    ##         root vegetables}          => {yogurt}                   0.007219115  0.2970711 2.1295150    71
    ## [1489] {rolls/buns,                                                                                   
    ##         yogurt}                   => {root vegetables}          0.007219115  0.2100592 1.9271753    71
    ## [1490] {root vegetables,                                                                              
    ##         yogurt}                   => {other vegetables}         0.012913066  0.5000000 2.5840778   127
    ## [1491] {other vegetables,                                                                             
    ##         root vegetables}          => {yogurt}                   0.012913066  0.2725322 1.9536108   127
    ## [1492] {other vegetables,                                                                             
    ##         yogurt}                   => {root vegetables}          0.012913066  0.2974239 2.7286977   127
    ## [1493] {root vegetables,                                                                              
    ##         yogurt}                   => {whole milk}               0.014539908  0.5629921 2.2033536   143
    ## [1494] {root vegetables,                                                                              
    ##         whole milk}               => {yogurt}                   0.014539908  0.2972973 2.1311362   143
    ## [1495] {whole milk,                                                                                   
    ##         yogurt}                   => {root vegetables}          0.014539908  0.2595281 2.3810253   143
    ## [1496] {rolls/buns,                                                                                   
    ##         root vegetables}          => {other vegetables}         0.012201322  0.5020921 2.5948898   120
    ## [1497] {other vegetables,                                                                             
    ##         root vegetables}          => {rolls/buns}               0.012201322  0.2575107 1.4000100   120
    ## [1498] {other vegetables,                                                                             
    ##         rolls/buns}               => {root vegetables}          0.012201322  0.2863962 2.6275247   120
    ## [1499] {rolls/buns,                                                                                   
    ##         root vegetables}          => {whole milk}               0.012709710  0.5230126 2.0468876   125
    ## [1500] {root vegetables,                                                                              
    ##         whole milk}               => {rolls/buns}               0.012709710  0.2598753 1.4128652   125
    ## [1501] {rolls/buns,                                                                                   
    ##         whole milk}               => {root vegetables}          0.012709710  0.2244165 2.0588959   125
    ## [1502] {other vegetables,                                                                             
    ##         root vegetables}          => {whole milk}               0.023182511  0.4892704 1.9148326   228
    ## [1503] {root vegetables,                                                                              
    ##         whole milk}               => {other vegetables}         0.023182511  0.4740125 2.4497702   228
    ## [1504] {other vegetables,                                                                             
    ##         whole milk}               => {root vegetables}          0.023182511  0.3097826 2.8420820   228
    ## [1505] {soda,                                                                                         
    ##         yogurt}                   => {rolls/buns}               0.008642603  0.3159851 1.7179181    85
    ## [1506] {rolls/buns,                                                                                   
    ##         soda}                     => {yogurt}                   0.008642603  0.2254642 1.6162101    85
    ## [1507] {rolls/buns,                                                                                   
    ##         yogurt}                   => {soda}                     0.008642603  0.2514793 1.4421567    85
    ## [1508] {soda,                                                                                         
    ##         yogurt}                   => {other vegetables}         0.008337570  0.3048327 1.5754229    82
    ## [1509] {other vegetables,                                                                             
    ##         soda}                     => {yogurt}                   0.008337570  0.2546584 1.8254849    82
    ## [1510] {other vegetables,                                                                             
    ##         yogurt}                   => {soda}                     0.008337570  0.1920375 1.1012761    82
    ## [1511] {soda,                                                                                         
    ##         yogurt}                   => {whole milk}               0.010472801  0.3828996 1.4985348   103
    ## [1512] {soda,                                                                                         
    ##         whole milk}               => {yogurt}                   0.010472801  0.2614213 1.8739641   103
    ## [1513] {whole milk,                                                                                   
    ##         yogurt}                   => {soda}                     0.010472801  0.1869328 1.0720027   103
    ## [1514] {rolls/buns,                                                                                   
    ##         soda}                     => {other vegetables}         0.009862735  0.2572944 1.3297376    97
    ## [1515] {other vegetables,                                                                             
    ##         soda}                     => {rolls/buns}               0.009862735  0.3012422 1.6377653    97
    ## [1516] {other vegetables,                                                                             
    ##         rolls/buns}               => {soda}                     0.009862735  0.2315036 1.3276022    97
    ## [1517] {rolls/buns,                                                                                   
    ##         soda}                     => {whole milk}               0.008845958  0.2307692 0.9031498    87
    ## [1518] {soda,                                                                                         
    ##         whole milk}               => {rolls/buns}               0.008845958  0.2208122 1.2004908    87
    ## [1519] {rolls/buns,                                                                                   
    ##         whole milk}               => {soda}                     0.008845958  0.1561939 0.8957242    87
    ## [1520] {other vegetables,                                                                             
    ##         soda}                     => {whole milk}               0.013929842  0.4254658 1.6651240   137
    ## [1521] {soda,                                                                                         
    ##         whole milk}               => {other vegetables}         0.013929842  0.3477157 1.7970490   137
    ## [1522] {other vegetables,                                                                             
    ##         whole milk}               => {soda}                     0.013929842  0.1861413 1.0674634   137
    ## [1523] {rolls/buns,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.011489578  0.3343195 1.7278153   113
    ## [1524] {other vegetables,                                                                             
    ##         yogurt}                   => {rolls/buns}               0.011489578  0.2646370 1.4387534   113
    ## [1525] {other vegetables,                                                                             
    ##         rolls/buns}               => {yogurt}                   0.011489578  0.2696897 1.9332351   113
    ## [1526] {rolls/buns,                                                                                   
    ##         yogurt}                   => {whole milk}               0.015556685  0.4526627 1.7715630   153
    ## [1527] {whole milk,                                                                                   
    ##         yogurt}                   => {rolls/buns}               0.015556685  0.2776770 1.5096478   153
    ## [1528] {rolls/buns,                                                                                   
    ##         whole milk}               => {yogurt}                   0.015556685  0.2746858 1.9690488   153
    ## [1529] {other vegetables,                                                                             
    ##         yogurt}                   => {whole milk}               0.022267412  0.5128806 2.0072345   219
    ## [1530] {whole milk,                                                                                   
    ##         yogurt}                   => {other vegetables}         0.022267412  0.3974592 2.0541308   219
    ## [1531] {other vegetables,                                                                             
    ##         whole milk}               => {yogurt}                   0.022267412  0.2975543 2.1329789   219
    ## [1532] {other vegetables,                                                                             
    ##         rolls/buns}               => {whole milk}               0.017895272  0.4200477 1.6439194   176
    ## [1533] {rolls/buns,                                                                                   
    ##         whole milk}               => {other vegetables}         0.017895272  0.3159785 1.6330258   176
    ## [1534] {other vegetables,                                                                             
    ##         whole milk}               => {rolls/buns}               0.017895272  0.2391304 1.3000817   176

**Exploratory**

    inspect(subset(grocery_rules, subset=lift > 3))

    ##      lhs                     rhs                      support confidence     lift count
    ## [1]  {herbs}              => {root vegetables}    0.007015760  0.4312500 3.956477    69
    ## [2]  {ham}                => {white bread}        0.005083884  0.1953125 4.639851    50
    ## [3]  {white bread}        => {ham}                0.005083884  0.1207729 4.639851    50
    ## [4]  {sliced cheese}      => {sausage}            0.007015760  0.2863071 3.047435    69
    ## [5]  {berries}            => {whipped/sour cream} 0.009049314  0.2721713 3.796886    89
    ## [6]  {whipped/sour cream} => {berries}            0.009049314  0.1262411 3.796886    89
    ## [7]  {hygiene articles}   => {napkins}            0.006100661  0.1851852 3.536498    60
    ## [8]  {napkins}            => {hygiene articles}   0.006100661  0.1165049 3.536498    60
    ## [9]  {waffles}            => {chocolate}          0.005795628  0.1507937 3.039048    57
    ## [10] {chocolate}          => {waffles}            0.005795628  0.1168033 3.039048    57
    ## [11] {chicken}            => {frozen vegetables}  0.006710727  0.1563981 3.251956    66
    ## [12] {frozen vegetables}  => {chicken}            0.006710727  0.1395349 3.251956    66
    ## [13] {beef}               => {root vegetables}    0.017386884  0.3313953 3.040367   171
    ## [14] {root vegetables}    => {beef}               0.017386884  0.1595149 3.040367   171
    ## [15] {onions,                                                                          
    ##       root vegetables}    => {other vegetables}   0.005693950  0.6021505 3.112008    56
    ## [16] {onions,                                                                          
    ##       other vegetables}   => {root vegetables}    0.005693950  0.4000000 3.669776    56
    ## [17] {other vegetables,                                                                
    ##       root vegetables}    => {onions}             0.005693950  0.1201717 3.875044    56
    ## [18] {other vegetables,                                                                
    ##       yogurt}             => {cream cheese}       0.005287239  0.1217799 3.071038    52
    ## [19] {chicken,                                                                         
    ##       whole milk}         => {root vegetables}    0.005998983  0.3410405 3.128855    59
    ## [20] {frozen vegetables,                                                               
    ##       other vegetables}   => {root vegetables}    0.006100661  0.3428571 3.145522    60
    ## [21] {beef,                                                                            
    ##       other vegetables}   => {root vegetables}    0.007930859  0.4020619 3.688692    78
    ## [22] {other vegetables,                                                                
    ##       root vegetables}    => {beef}               0.007930859  0.1673820 3.190313    78
    ## [23] {beef,                                                                            
    ##       whole milk}         => {root vegetables}    0.008032537  0.3779904 3.467851    79
    ## [24] {root vegetables,                                                                 
    ##       whole milk}         => {beef}               0.008032537  0.1642412 3.130449    79
    ## [25] {curd,                                                                            
    ##       whole milk}         => {whipped/sour cream} 0.005897306  0.2256809 3.148329    58
    ## [26] {whipped/sour cream,                                                              
    ##       whole milk}         => {curd}               0.005897306  0.1829653 3.434091    58
    ## [27] {curd,                                                                            
    ##       tropical fruit}     => {yogurt}             0.005287239  0.5148515 3.690645    52
    ## [28] {tropical fruit,                                                                  
    ##       yogurt}             => {curd}               0.005287239  0.1805556 3.388862    52
    ## [29] {whole milk,                                                                      
    ##       yogurt}             => {curd}               0.010066090  0.1796733 3.372304    99
    ## [30] {margarine,                                                                       
    ##       whole milk}         => {domestic eggs}      0.005185562  0.2142857 3.377404    51
    ## [31] {butter,                                                                          
    ##       whole milk}         => {domestic eggs}      0.005998983  0.2177122 3.431409    59
    ## [32] {domestic eggs,                                                                   
    ##       whole milk}         => {butter}             0.005998983  0.2000000 3.609174    59
    ## [33] {butter,                                                                          
    ##       other vegetables}   => {whipped/sour cream} 0.005795628  0.2893401 4.036397    57
    ## [34] {other vegetables,                                                                
    ##       whipped/sour cream} => {butter}             0.005795628  0.2007042 3.621883    57
    ## [35] {butter,                                                                          
    ##       whole milk}         => {whipped/sour cream} 0.006710727  0.2435424 3.397503    66
    ## [36] {whipped/sour cream,                                                              
    ##       whole milk}         => {butter}             0.006710727  0.2082019 3.757185    66
    ## [37] {citrus fruit,                                                                    
    ##       whole milk}         => {butter}             0.005083884  0.1666667 3.007645    50
    ## [38] {butter,                                                                          
    ##       other vegetables}   => {root vegetables}    0.006609049  0.3299492 3.027100    65
    ## [39] {root vegetables,                                                                 
    ##       whole milk}         => {butter}             0.008235892  0.1683992 3.038910    81
    ## [40] {whole milk,                                                                      
    ##       yogurt}             => {butter}             0.009354347  0.1669691 3.013104    92
    ## [41] {domestic eggs,                                                                   
    ##       other vegetables}   => {whipped/sour cream} 0.005083884  0.2283105 3.185012    50
    ## [42] {domestic eggs,                                                                   
    ##       other vegetables}   => {root vegetables}    0.007320793  0.3287671 3.016254    72
    ## [43] {pip fruit,                                                                       
    ##       whipped/sour cream} => {other vegetables}   0.005592272  0.6043956 3.123610    55
    ## [44] {tropical fruit,                                                                  
    ##       whipped/sour cream} => {yogurt}             0.006202339  0.4485294 3.215224    61
    ## [45] {other vegetables,                                                                
    ##       tropical fruit}     => {whipped/sour cream} 0.007829181  0.2181303 3.042995    77
    ## [46] {root vegetables,                                                                 
    ##       yogurt}             => {whipped/sour cream} 0.006405694  0.2480315 3.460127    63
    ## [47] {other vegetables,                                                                
    ##       yogurt}             => {whipped/sour cream} 0.010167768  0.2341920 3.267062   100
    ## [48] {citrus fruit,                                                                    
    ##       pip fruit}          => {tropical fruit}     0.005592272  0.4044118 3.854060    55
    ## [49] {pip fruit,                                                                       
    ##       tropical fruit}     => {citrus fruit}       0.005592272  0.2736318 3.306105    55
    ## [50] {citrus fruit,                                                                    
    ##       tropical fruit}     => {pip fruit}          0.005592272  0.2806122 3.709437    55
    ## [51] {pip fruit,                                                                       
    ##       root vegetables}    => {tropical fruit}     0.005287239  0.3398693 3.238967    52
    ## [52] {root vegetables,                                                                 
    ##       tropical fruit}     => {pip fruit}          0.005287239  0.2512077 3.320737    52
    ## [53] {pip fruit,                                                                       
    ##       yogurt}             => {tropical fruit}     0.006405694  0.3559322 3.392048    63
    ## [54] {other vegetables,                                                                
    ##       pip fruit}          => {tropical fruit}     0.009456024  0.3618677 3.448613    93
    ## [55] {other vegetables,                                                                
    ##       tropical fruit}     => {pip fruit}          0.009456024  0.2634561 3.482649    93
    ## [56] {citrus fruit,                                                                    
    ##       root vegetables}    => {tropical fruit}     0.005693950  0.3218391 3.067139    56
    ## [57] {root vegetables,                                                                 
    ##       tropical fruit}     => {citrus fruit}       0.005693950  0.2705314 3.268644    56
    ## [58] {other vegetables,                                                                
    ##       tropical fruit}     => {citrus fruit}       0.009049314  0.2521246 3.046248    89
    ## [59] {citrus fruit,                                                                    
    ##       root vegetables}    => {other vegetables}   0.010371124  0.5862069 3.029608   102
    ## [60] {citrus fruit,                                                                    
    ##       other vegetables}   => {root vegetables}    0.010371124  0.3591549 3.295045   102
    ## [61] {rolls/buns,                                                                      
    ##       shopping bags}      => {sausage}            0.005998983  0.3072917 3.270794    59
    ## [62] {root vegetables,                                                                 
    ##       yogurt}             => {tropical fruit}     0.008134215  0.3149606 3.001587    80
    ## [63] {root vegetables,                                                                 
    ##       tropical fruit}     => {other vegetables}   0.012302999  0.5845411 3.020999   121
    ## [64] {other vegetables,                                                                
    ##       tropical fruit}     => {root vegetables}    0.012302999  0.3427762 3.144780   121

    inspect(subset(grocery_rules, subset=confidence > 0.4))

    ##       lhs                           rhs                    support confidence     lift count
    ## [1]   {cake bar}                 => {whole milk}       0.005592272  0.4230769 1.655775    55
    ## [2]   {mustard}                  => {whole milk}       0.005185562  0.4322034 1.691492    51
    ## [3]   {pasta}                    => {whole milk}       0.006100661  0.4054054 1.586614    60
    ## [4]   {herbs}                    => {root vegetables}  0.007015760  0.4312500 3.956477    69
    ## [5]   {herbs}                    => {other vegetables} 0.007727504  0.4750000 2.454874    76
    ## [6]   {herbs}                    => {whole milk}       0.007727504  0.4750000 1.858983    76
    ## [7]   {processed cheese}         => {whole milk}       0.007015760  0.4233129 1.656698    69
    ## [8]   {semi-finished bread}      => {whole milk}       0.007117438  0.4022989 1.574457    70
    ## [9]   {detergent}                => {whole milk}       0.008947636  0.4656085 1.822228    88
    ## [10]  {baking powder}            => {other vegetables} 0.007320793  0.4137931 2.138547    72
    ## [11]  {baking powder}            => {whole milk}       0.009252669  0.5229885 2.046793    91
    ## [12]  {flour}                    => {whole milk}       0.008439248  0.4853801 1.899607    83
    ## [13]  {soft cheese}              => {other vegetables} 0.007117438  0.4166667 2.153398    70
    ## [14]  {soft cheese}              => {whole milk}       0.007524148  0.4404762 1.723869    74
    ## [15]  {grapes}                   => {other vegetables} 0.009049314  0.4045455 2.090754    89
    ## [16]  {hard cheese}              => {whole milk}       0.010066090  0.4107884 1.607682    99
    ## [17]  {butter milk}              => {whole milk}       0.011591256  0.4145455 1.622385   114
    ## [18]  {ham}                      => {whole milk}       0.011489578  0.4414062 1.727509   113
    ## [19]  {sliced cheese}            => {whole milk}       0.010777834  0.4398340 1.721356   106
    ## [20]  {oil}                      => {whole milk}       0.011286223  0.4021739 1.573968   111
    ## [21]  {onions}                   => {other vegetables} 0.014234875  0.4590164 2.372268   140
    ## [22]  {hamburger meat}           => {other vegetables} 0.013828165  0.4159021 2.149447   136
    ## [23]  {hamburger meat}           => {whole milk}       0.014743264  0.4434251 1.735410   145
    ## [24]  {sugar}                    => {whole milk}       0.015048297  0.4444444 1.739400   148
    ## [25]  {cream cheese}             => {whole milk}       0.016471784  0.4153846 1.625670   162
    ## [26]  {chicken}                  => {other vegetables} 0.017895272  0.4170616 2.155439   176
    ## [27]  {chicken}                  => {whole milk}       0.017590239  0.4099526 1.604411   173
    ## [28]  {white bread}              => {whole milk}       0.017081851  0.4057971 1.588147   168
    ## [29]  {frozen vegetables}        => {whole milk}       0.020437214  0.4249471 1.663094   201
    ## [30]  {beef}                     => {whole milk}       0.021250635  0.4050388 1.585180   209
    ## [31]  {curd}                     => {whole milk}       0.026131164  0.4904580 1.919481   257
    ## [32]  {margarine}                => {whole milk}       0.024199288  0.4131944 1.617098   238
    ## [33]  {butter}                   => {whole milk}       0.027554652  0.4972477 1.946053   271
    ## [34]  {domestic eggs}            => {whole milk}       0.029994916  0.4727564 1.850203   295
    ## [35]  {whipped/sour cream}       => {other vegetables} 0.028876462  0.4028369 2.081924   284
    ## [36]  {whipped/sour cream}       => {whole milk}       0.032231825  0.4496454 1.759754   317
    ## [37]  {tropical fruit}           => {whole milk}       0.042297916  0.4031008 1.577595   416
    ## [38]  {root vegetables}          => {other vegetables} 0.047381800  0.4347015 2.246605   466
    ## [39]  {root vegetables}          => {whole milk}       0.048906965  0.4486940 1.756031   481
    ## [40]  {yogurt}                   => {whole milk}       0.056024403  0.4016035 1.571735   551
    ## [41]  {oil,                                                                                 
    ##        other vegetables}         => {whole milk}       0.005083884  0.5102041 1.996760    50
    ## [42]  {oil,                                                                                 
    ##        whole milk}               => {other vegetables} 0.005083884  0.4504505 2.327998    50
    ## [43]  {onions,                                                                              
    ##        root vegetables}          => {other vegetables} 0.005693950  0.6021505 3.112008    56
    ## [44]  {onions,                                                                              
    ##        other vegetables}         => {whole milk}       0.006609049  0.4642857 1.817051    65
    ## [45]  {onions,                                                                              
    ##        whole milk}               => {other vegetables} 0.006609049  0.5462185 2.822942    65
    ## [46]  {hamburger meat,                                                                      
    ##        other vegetables}         => {whole milk}       0.006304016  0.4558824 1.784164    62
    ## [47]  {hamburger meat,                                                                      
    ##        whole milk}               => {other vegetables} 0.006304016  0.4275862 2.209832    62
    ## [48]  {hygiene articles,                                                                    
    ##        other vegetables}         => {whole milk}       0.005185562  0.5425532 2.123363    51
    ## [49]  {hygiene articles,                                                                    
    ##        whole milk}               => {other vegetables} 0.005185562  0.4047619 2.091872    51
    ## [50]  {other vegetables,                                                                    
    ##        sugar}                    => {whole milk}       0.006304016  0.5849057 2.289115    62
    ## [51]  {sugar,                                                                               
    ##        whole milk}               => {other vegetables} 0.006304016  0.4189189 2.165038    62
    ## [52]  {long life bakery product,                                                            
    ##        other vegetables}         => {whole milk}       0.005693950  0.5333333 2.087279    56
    ## [53]  {long life bakery product,                                                            
    ##        whole milk}               => {other vegetables} 0.005693950  0.4210526 2.176065    56
    ## [54]  {cream cheese,                                                                        
    ##        yogurt}                   => {other vegetables} 0.005287239  0.4262295 2.202820    52
    ## [55]  {cream cheese,                                                                        
    ##        yogurt}                   => {whole milk}       0.006609049  0.5327869 2.085141    65
    ## [56]  {cream cheese,                                                                        
    ##        whole milk}               => {yogurt}           0.006609049  0.4012346 2.876197    65
    ## [57]  {cream cheese,                                                                        
    ##        other vegetables}         => {whole milk}       0.006710727  0.4888889 1.913340    66
    ## [58]  {cream cheese,                                                                        
    ##        whole milk}               => {other vegetables} 0.006710727  0.4074074 2.105545    66
    ## [59]  {chicken,                                                                             
    ##        root vegetables}          => {other vegetables} 0.005693950  0.5233645 2.704829    56
    ## [60]  {chicken,                                                                             
    ##        root vegetables}          => {whole milk}       0.005998983  0.5514019 2.157993    59
    ## [61]  {chicken,                                                                             
    ##        rolls/buns}               => {whole milk}       0.005287239  0.5473684 2.142208    52
    ## [62]  {chicken,                                                                             
    ##        other vegetables}         => {whole milk}       0.008439248  0.4715909 1.845641    83
    ## [63]  {chicken,                                                                             
    ##        whole milk}               => {other vegetables} 0.008439248  0.4797688 2.479520    83
    ## [64]  {other vegetables,                                                                    
    ##        white bread}              => {whole milk}       0.005897306  0.4296296 1.681420    58
    ## [65]  {chocolate,                                                                           
    ##        other vegetables}         => {whole milk}       0.005490595  0.4320000 1.690696    54
    ## [66]  {coffee,                                                                              
    ##        yogurt}                   => {whole milk}       0.005083884  0.5208333 2.038359    50
    ## [67]  {coffee,                                                                              
    ##        other vegetables}         => {whole milk}       0.006405694  0.4772727 1.867878    63
    ## [68]  {frozen vegetables,                                                                   
    ##        root vegetables}          => {other vegetables} 0.006100661  0.5263158 2.720082    60
    ## [69]  {frozen vegetables,                                                                   
    ##        root vegetables}          => {whole milk}       0.006202339  0.5350877 2.094146    61
    ## [70]  {frozen vegetables,                                                                   
    ##        yogurt}                   => {other vegetables} 0.005287239  0.4262295 2.202820    52
    ## [71]  {frozen vegetables,                                                                   
    ##        yogurt}                   => {whole milk}       0.006100661  0.4918033 1.924745    60
    ## [72]  {frozen vegetables,                                                                   
    ##        rolls/buns}               => {whole milk}       0.005083884  0.5000000 1.956825    50
    ## [73]  {frozen vegetables,                                                                   
    ##        other vegetables}         => {whole milk}       0.009659380  0.5428571 2.124552    95
    ## [74]  {frozen vegetables,                                                                   
    ##        whole milk}               => {other vegetables} 0.009659380  0.4726368 2.442661    95
    ## [75]  {beef,                                                                                
    ##        root vegetables}          => {other vegetables} 0.007930859  0.4561404 2.357404    78
    ## [76]  {beef,                                                                                
    ##        other vegetables}         => {root vegetables}  0.007930859  0.4020619 3.688692    78
    ## [77]  {beef,                                                                                
    ##        root vegetables}          => {whole milk}       0.008032537  0.4619883 1.808060    79
    ## [78]  {beef,                                                                                
    ##        yogurt}                   => {other vegetables} 0.005185562  0.4434783 2.291965    51
    ## [79]  {beef,                                                                                
    ##        yogurt}                   => {whole milk}       0.006100661  0.5217391 2.041904    60
    ## [80]  {beef,                                                                                
    ##        rolls/buns}               => {other vegetables} 0.005795628  0.4253731 2.198395    57
    ## [81]  {beef,                                                                                
    ##        rolls/buns}               => {whole milk}       0.006812405  0.5000000 1.956825    67
    ## [82]  {beef,                                                                                
    ##        other vegetables}         => {whole milk}       0.009252669  0.4690722 1.835784    91
    ## [83]  {beef,                                                                                
    ##        whole milk}               => {other vegetables} 0.009252669  0.4354067 2.250250    91
    ## [84]  {curd,                                                                                
    ##        whipped/sour cream}       => {whole milk}       0.005897306  0.5631068 2.203802    58
    ## [85]  {curd,                                                                                
    ##        tropical fruit}           => {yogurt}           0.005287239  0.5148515 3.690645    52
    ## [86]  {curd,                                                                                
    ##        tropical fruit}           => {other vegetables} 0.005287239  0.5148515 2.660833    52
    ## [87]  {curd,                                                                                
    ##        tropical fruit}           => {whole milk}       0.006507372  0.6336634 2.479936    64
    ## [88]  {curd,                                                                                
    ##        root vegetables}          => {other vegetables} 0.005490595  0.5046729 2.608228    54
    ## [89]  {curd,                                                                                
    ##        root vegetables}          => {whole milk}       0.006202339  0.5700935 2.231146    61
    ## [90]  {curd,                                                                                
    ##        yogurt}                   => {whole milk}       0.010066090  0.5823529 2.279125    99
    ## [91]  {curd,                                                                                
    ##        rolls/buns}               => {whole milk}       0.005897306  0.5858586 2.292845    58
    ## [92]  {curd,                                                                                
    ##        other vegetables}         => {whole milk}       0.009862735  0.5739645 2.246296    97
    ## [93]  {napkins,                                                                             
    ##        yogurt}                   => {whole milk}       0.006100661  0.4958678 1.940652    60
    ## [94]  {napkins,                                                                             
    ##        rolls/buns}               => {whole milk}       0.005287239  0.4521739 1.769650    52
    ## [95]  {napkins,                                                                             
    ##        other vegetables}         => {whole milk}       0.006812405  0.4718310 1.846581    67
    ## [96]  {pork,                                                                                
    ##        root vegetables}          => {other vegetables} 0.007015760  0.5149254 2.661214    69
    ## [97]  {pork,                                                                                
    ##        root vegetables}          => {whole milk}       0.006812405  0.5000000 1.956825    67
    ## [98]  {pork,                                                                                
    ##        rolls/buns}               => {other vegetables} 0.005592272  0.4954955 2.560798    55
    ## [99]  {pork,                                                                                
    ##        rolls/buns}               => {whole milk}       0.006202339  0.5495495 2.150744    61
    ## [100] {other vegetables,                                                                    
    ##        pork}                     => {whole milk}       0.010167768  0.4694836 1.837394   100
    ## [101] {pork,                                                                                
    ##        whole milk}               => {other vegetables} 0.010167768  0.4587156 2.370714   100
    ## [102] {frankfurter,                                                                         
    ##        tropical fruit}           => {whole milk}       0.005185562  0.5483871 2.146195    51
    ## [103] {frankfurter,                                                                         
    ##        root vegetables}          => {whole milk}       0.005083884  0.5000000 1.956825    50
    ## [104] {frankfurter,                                                                         
    ##        yogurt}                   => {whole milk}       0.006202339  0.5545455 2.170296    61
    ## [105] {frankfurter,                                                                         
    ##        other vegetables}         => {whole milk}       0.007625826  0.4629630 1.811875    75
    ## [106] {bottled beer,                                                                        
    ##        yogurt}                   => {whole milk}       0.005185562  0.5604396 2.193364    51
    ## [107] {bottled beer,                                                                        
    ##        other vegetables}         => {whole milk}       0.007625826  0.4716981 1.846061    75
    ## [108] {brown bread,                                                                         
    ##        tropical fruit}           => {whole milk}       0.005693950  0.5333333 2.087279    56
    ## [109] {brown bread,                                                                         
    ##        root vegetables}          => {whole milk}       0.005693950  0.5600000 2.191643    56
    ## [110] {brown bread,                                                                         
    ##        soda}                     => {whole milk}       0.005083884  0.4032258 1.578084    50
    ## [111] {brown bread,                                                                         
    ##        yogurt}                   => {whole milk}       0.007117438  0.4895105 1.915772    70
    ## [112] {brown bread,                                                                         
    ##        rolls/buns}               => {whole milk}       0.005287239  0.4193548 1.641208    52
    ## [113] {brown bread,                                                                         
    ##        other vegetables}         => {whole milk}       0.009354347  0.5000000 1.956825    92
    ## [114] {domestic eggs,                                                                       
    ##        margarine}                => {whole milk}       0.005185562  0.6219512 2.434099    51
    ## [115] {margarine,                                                                           
    ##        root vegetables}          => {other vegetables} 0.005897306  0.5321101 2.750028    58
    ## [116] {margarine,                                                                           
    ##        yogurt}                   => {whole milk}       0.007015760  0.4928571 1.928870    69
    ## [117] {margarine,                                                                           
    ##        rolls/buns}               => {whole milk}       0.007930859  0.5379310 2.105273    78
    ## [118] {margarine,                                                                           
    ##        other vegetables}         => {whole milk}       0.009252669  0.4690722 1.835784    91
    ## [119] {butter,                                                                              
    ##        domestic eggs}            => {whole milk}       0.005998983  0.6210526 2.430582    59
    ## [120] {butter,                                                                              
    ##        whipped/sour cream}       => {other vegetables} 0.005795628  0.5700000 2.945849    57
    ## [121] {butter,                                                                              
    ##        whipped/sour cream}       => {whole milk}       0.006710727  0.6600000 2.583008    66
    ## [122] {butter,                                                                              
    ##        citrus fruit}             => {whole milk}       0.005083884  0.5555556 2.174249    50
    ## [123] {bottled water,                                                                       
    ##        butter}                   => {whole milk}       0.005388917  0.6022727 2.357084    53
    ## [124] {butter,                                                                              
    ##        tropical fruit}           => {other vegetables} 0.005490595  0.5510204 2.847759    54
    ## [125] {butter,                                                                              
    ##        tropical fruit}           => {whole milk}       0.006202339  0.6224490 2.436047    61
    ## [126] {butter,                                                                              
    ##        root vegetables}          => {other vegetables} 0.006609049  0.5118110 2.645119    65
    ## [127] {butter,                                                                              
    ##        root vegetables}          => {whole milk}       0.008235892  0.6377953 2.496107    81
    ## [128] {butter,                                                                              
    ##        yogurt}                   => {other vegetables} 0.006405694  0.4375000 2.261068    63
    ## [129] {butter,                                                                              
    ##        yogurt}                   => {whole milk}       0.009354347  0.6388889 2.500387    92
    ## [130] {butter,                                                                              
    ##        rolls/buns}               => {other vegetables} 0.005693950  0.4242424 2.192551    56
    ## [131] {butter,                                                                              
    ##        rolls/buns}               => {whole milk}       0.006609049  0.4924242 1.927176    65
    ## [132] {butter,                                                                              
    ##        other vegetables}         => {whole milk}       0.011489578  0.5736041 2.244885   113
    ## [133] {butter,                                                                              
    ##        whole milk}               => {other vegetables} 0.011489578  0.4169742 2.154987   113
    ## [134] {newspapers,                                                                          
    ##        tropical fruit}           => {whole milk}       0.005083884  0.4310345 1.686918    50
    ## [135] {newspapers,                                                                          
    ##        root vegetables}          => {other vegetables} 0.005998983  0.5221239 2.698417    59
    ## [136] {newspapers,                                                                          
    ##        root vegetables}          => {whole milk}       0.005795628  0.5044248 1.974142    57
    ## [137] {newspapers,                                                                          
    ##        yogurt}                   => {whole milk}       0.006609049  0.4304636 1.684683    65
    ## [138] {newspapers,                                                                          
    ##        other vegetables}         => {whole milk}       0.008337570  0.4315789 1.689049    82
    ## [139] {domestic eggs,                                                                       
    ##        whipped/sour cream}       => {other vegetables} 0.005083884  0.5102041 2.636814    50
    ## [140] {domestic eggs,                                                                       
    ##        whipped/sour cream}       => {whole milk}       0.005693950  0.5714286 2.236371    56
    ## [141] {domestic eggs,                                                                       
    ##        pip fruit}                => {whole milk}       0.005388917  0.6235294 2.440275    53
    ## [142] {citrus fruit,                                                                        
    ##        domestic eggs}            => {whole milk}       0.005693950  0.5490196 2.148670    56
    ## [143] {domestic eggs,                                                                       
    ##        tropical fruit}           => {whole milk}       0.006914082  0.6071429 2.376144    68
    ## [144] {domestic eggs,                                                                       
    ##        root vegetables}          => {other vegetables} 0.007320793  0.5106383 2.639058    72
    ## [145] {domestic eggs,                                                                       
    ##        root vegetables}          => {whole milk}       0.008540925  0.5957447 2.331536    84
    ## [146] {domestic eggs,                                                                       
    ##        soda}                     => {other vegetables} 0.005083884  0.4098361 2.118097    50
    ## [147] {domestic eggs,                                                                       
    ##        soda}                     => {whole milk}       0.005185562  0.4180328 1.636034    51
    ## [148] {domestic eggs,                                                                       
    ##        yogurt}                   => {other vegetables} 0.005795628  0.4042553 2.089254    57
    ## [149] {domestic eggs,                                                                       
    ##        yogurt}                   => {whole milk}       0.007727504  0.5390071 2.109485    76
    ## [150] {domestic eggs,                                                                       
    ##        rolls/buns}               => {whole milk}       0.006609049  0.4220779 1.651865    65
    ## [151] {domestic eggs,                                                                       
    ##        other vegetables}         => {whole milk}       0.012302999  0.5525114 2.162336   121
    ## [152] {domestic eggs,                                                                       
    ##        whole milk}               => {other vegetables} 0.012302999  0.4101695 2.119820   121
    ## [153] {bottled water,                                                                       
    ##        fruit/vegetable juice}    => {whole milk}       0.005795628  0.4071429 1.593414    57
    ## [154] {fruit/vegetable juice,                                                               
    ##        tropical fruit}           => {other vegetables} 0.006609049  0.4814815 2.488371    65
    ## [155] {fruit/vegetable juice,                                                               
    ##        tropical fruit}           => {whole milk}       0.005998983  0.4370370 1.710410    59
    ## [156] {fruit/vegetable juice,                                                               
    ##        root vegetables}          => {other vegetables} 0.006609049  0.5508475 2.846865    65
    ## [157] {fruit/vegetable juice,                                                               
    ##        root vegetables}          => {whole milk}       0.006507372  0.5423729 2.122657    64
    ## [158] {fruit/vegetable juice,                                                               
    ##        yogurt}                   => {other vegetables} 0.008235892  0.4402174 2.275112    81
    ## [159] {fruit/vegetable juice,                                                               
    ##        yogurt}                   => {whole milk}       0.009456024  0.5054348 1.978094    93
    ## [160] {fruit/vegetable juice,                                                               
    ##        other vegetables}         => {whole milk}       0.010472801  0.4975845 1.947371   103
    ## [161] {pip fruit,                                                                           
    ##        whipped/sour cream}       => {other vegetables} 0.005592272  0.6043956 3.123610    55
    ## [162] {pip fruit,                                                                           
    ##        whipped/sour cream}       => {whole milk}       0.005998983  0.6483516 2.537421    59
    ## [163] {citrus fruit,                                                                        
    ##        whipped/sour cream}       => {other vegetables} 0.005693950  0.5233645 2.704829    56
    ## [164] {citrus fruit,                                                                        
    ##        whipped/sour cream}       => {whole milk}       0.006304016  0.5794393 2.267722    62
    ## [165] {sausage,                                                                             
    ##        whipped/sour cream}       => {whole milk}       0.005083884  0.5617978 2.198679    50
    ## [166] {tropical fruit,                                                                      
    ##        whipped/sour cream}       => {yogurt}           0.006202339  0.4485294 3.215224    61
    ## [167] {tropical fruit,                                                                      
    ##        whipped/sour cream}       => {other vegetables} 0.007829181  0.5661765 2.926088    77
    ## [168] {tropical fruit,                                                                      
    ##        whipped/sour cream}       => {whole milk}       0.007930859  0.5735294 2.244593    78
    ## [169] {root vegetables,                                                                     
    ##        whipped/sour cream}       => {other vegetables} 0.008540925  0.5000000 2.584078    84
    ## [170] {root vegetables,                                                                     
    ##        whipped/sour cream}       => {whole milk}       0.009456024  0.5535714 2.166484    93
    ## [171] {soda,                                                                                
    ##        whipped/sour cream}       => {whole milk}       0.005490595  0.4736842 1.853834    54
    ## [172] {whipped/sour cream,                                                                  
    ##        yogurt}                   => {other vegetables} 0.010167768  0.4901961 2.533410   100
    ## [173] {whipped/sour cream,                                                                  
    ##        yogurt}                   => {whole milk}       0.010879512  0.5245098 2.052747   107
    ## [174] {rolls/buns,                                                                          
    ##        whipped/sour cream}       => {other vegetables} 0.006710727  0.4583333 2.368738    66
    ## [175] {rolls/buns,                                                                          
    ##        whipped/sour cream}       => {whole milk}       0.007829181  0.5347222 2.092715    77
    ## [176] {other vegetables,                                                                    
    ##        whipped/sour cream}       => {whole milk}       0.014641586  0.5070423 1.984385   144
    ## [177] {whipped/sour cream,                                                                  
    ##        whole milk}               => {other vegetables} 0.014641586  0.4542587 2.347679   144
    ## [178] {pastry,                                                                              
    ##        pip fruit}                => {whole milk}       0.005083884  0.4761905 1.863642    50
    ## [179] {citrus fruit,                                                                        
    ##        pip fruit}                => {tropical fruit}   0.005592272  0.4044118 3.854060    55
    ## [180] {citrus fruit,                                                                        
    ##        pip fruit}                => {other vegetables} 0.005897306  0.4264706 2.204066    58
    ## [181] {pip fruit,                                                                           
    ##        sausage}                  => {whole milk}       0.005592272  0.5188679 2.030667    55
    ## [182] {pip fruit,                                                                           
    ##        tropical fruit}           => {other vegetables} 0.009456024  0.4626866 2.391236    93
    ## [183] {pip fruit,                                                                           
    ##        tropical fruit}           => {whole milk}       0.008439248  0.4129353 1.616084    83
    ## [184] {pip fruit,                                                                           
    ##        root vegetables}          => {other vegetables} 0.008134215  0.5228758 2.702304    80
    ## [185] {pip fruit,                                                                           
    ##        root vegetables}          => {whole milk}       0.008947636  0.5751634 2.250988    88
    ## [186] {pip fruit,                                                                           
    ##        yogurt}                   => {other vegetables} 0.008134215  0.4519774 2.335890    80
    ## [187] {pip fruit,                                                                           
    ##        yogurt}                   => {whole milk}       0.009557702  0.5310734 2.078435    94
    ## [188] {pip fruit,                                                                           
    ##        rolls/buns}               => {whole milk}       0.006202339  0.4452555 1.742574    61
    ## [189] {other vegetables,                                                                    
    ##        pip fruit}                => {whole milk}       0.013523132  0.5175097 2.025351   133
    ## [190] {pip fruit,                                                                           
    ##        whole milk}               => {other vegetables} 0.013523132  0.4493243 2.322178   133
    ## [191] {pastry,                                                                              
    ##        sausage}                  => {whole milk}       0.005693950  0.4552846 1.781824    56
    ## [192] {pastry,                                                                              
    ##        tropical fruit}           => {whole milk}       0.006710727  0.5076923 1.986930    66
    ## [193] {pastry,                                                                              
    ##        root vegetables}          => {other vegetables} 0.005897306  0.5370370 2.775491    58
    ## [194] {pastry,                                                                              
    ##        root vegetables}          => {whole milk}       0.005693950  0.5185185 2.029299    56
    ## [195] {pastry,                                                                              
    ##        yogurt}                   => {whole milk}       0.009150991  0.5172414 2.024301    90
    ## [196] {pastry,                                                                              
    ##        rolls/buns}               => {whole milk}       0.008540925  0.4077670 1.595857    84
    ## [197] {other vegetables,                                                                    
    ##        pastry}                   => {whole milk}       0.010574479  0.4684685 1.833421   104
    ## [198] {bottled water,                                                                       
    ##        citrus fruit}             => {whole milk}       0.005897306  0.4360902 1.706704    58
    ## [199] {citrus fruit,                                                                        
    ##        tropical fruit}           => {other vegetables} 0.009049314  0.4540816 2.346765    89
    ## [200] {citrus fruit,                                                                        
    ##        tropical fruit}           => {whole milk}       0.009049314  0.4540816 1.777116    89
    ## [201] {citrus fruit,                                                                        
    ##        root vegetables}          => {other vegetables} 0.010371124  0.5862069 3.029608   102
    ## [202] {citrus fruit,                                                                        
    ##        root vegetables}          => {whole milk}       0.009150991  0.5172414 2.024301    90
    ## [203] {citrus fruit,                                                                        
    ##        yogurt}                   => {whole milk}       0.010269446  0.4741784 1.855768   101
    ## [204] {citrus fruit,                                                                        
    ##        rolls/buns}               => {whole milk}       0.007219115  0.4303030 1.684055    71
    ## [205] {citrus fruit,                                                                        
    ##        other vegetables}         => {whole milk}       0.013014743  0.4507042 1.763898   128
    ## [206] {citrus fruit,                                                                        
    ##        whole milk}               => {other vegetables} 0.013014743  0.4266667 2.205080   128
    ## [207] {root vegetables,                                                                     
    ##        shopping bags}            => {other vegetables} 0.006609049  0.5158730 2.666112    65
    ## [208] {root vegetables,                                                                     
    ##        shopping bags}            => {whole milk}       0.005287239  0.4126984 1.615157    52
    ## [209] {bottled water,                                                                       
    ##        sausage}                  => {other vegetables} 0.005083884  0.4237288 2.189896    50
    ## [210] {sausage,                                                                             
    ##        tropical fruit}           => {other vegetables} 0.005998983  0.4306569 2.225702    59
    ## [211] {sausage,                                                                             
    ##        tropical fruit}           => {whole milk}       0.007219115  0.5182482 2.028241    71
    ## [212] {root vegetables,                                                                     
    ##        sausage}                  => {other vegetables} 0.006812405  0.4557823 2.355554    67
    ## [213] {root vegetables,                                                                     
    ##        sausage}                  => {whole milk}       0.007727504  0.5170068 2.023383    76
    ## [214] {sausage,                                                                             
    ##        yogurt}                   => {other vegetables} 0.008134215  0.4145078 2.142241    80
    ## [215] {sausage,                                                                             
    ##        yogurt}                   => {whole milk}       0.008744281  0.4455959 1.743906    86
    ## [216] {bottled water,                                                                       
    ##        tropical fruit}           => {whole milk}       0.008032537  0.4340659 1.698782    79
    ## [217] {bottled water,                                                                       
    ##        root vegetables}          => {other vegetables} 0.007015760  0.4480519 2.315602    69
    ## [218] {bottled water,                                                                       
    ##        root vegetables}          => {whole milk}       0.007320793  0.4675325 1.829758    72
    ## [219] {bottled water,                                                                       
    ##        yogurt}                   => {whole milk}       0.009659380  0.4203540 1.645118    95
    ## [220] {bottled water,                                                                       
    ##        other vegetables}         => {whole milk}       0.010777834  0.4344262 1.700192   106
    ## [221] {root vegetables,                                                                     
    ##        tropical fruit}           => {other vegetables} 0.012302999  0.5845411 3.020999   121
    ## [222] {root vegetables,                                                                     
    ##        tropical fruit}           => {whole milk}       0.011997966  0.5700483 2.230969   118
    ## [223] {tropical fruit,                                                                      
    ##        yogurt}                   => {other vegetables} 0.012302999  0.4201389 2.171343   121
    ## [224] {tropical fruit,                                                                      
    ##        yogurt}                   => {whole milk}       0.015149975  0.5173611 2.024770   149
    ## [225] {rolls/buns,                                                                          
    ##        tropical fruit}           => {whole milk}       0.010981190  0.4462810 1.746587   108
    ## [226] {other vegetables,                                                                    
    ##        tropical fruit}           => {whole milk}       0.017081851  0.4759207 1.862587   168
    ## [227] {tropical fruit,                                                                      
    ##        whole milk}               => {other vegetables} 0.017081851  0.4038462 2.087140   168
    ## [228] {root vegetables,                                                                     
    ##        soda}                     => {other vegetables} 0.008235892  0.4426230 2.287544    81
    ## [229] {root vegetables,                                                                     
    ##        soda}                     => {whole milk}       0.008134215  0.4371585 1.710885    80
    ## [230] {root vegetables,                                                                     
    ##        yogurt}                   => {other vegetables} 0.012913066  0.5000000 2.584078   127
    ## [231] {root vegetables,                                                                     
    ##        yogurt}                   => {whole milk}       0.014539908  0.5629921 2.203354   143
    ## [232] {rolls/buns,                                                                          
    ##        root vegetables}          => {other vegetables} 0.012201322  0.5020921 2.594890   120
    ## [233] {rolls/buns,                                                                          
    ##        root vegetables}          => {whole milk}       0.012709710  0.5230126 2.046888   125
    ## [234] {other vegetables,                                                                    
    ##        root vegetables}          => {whole milk}       0.023182511  0.4892704 1.914833   228
    ## [235] {root vegetables,                                                                     
    ##        whole milk}               => {other vegetables} 0.023182511  0.4740125 2.449770   228
    ## [236] {other vegetables,                                                                    
    ##        soda}                     => {whole milk}       0.013929842  0.4254658 1.665124   137
    ## [237] {rolls/buns,                                                                          
    ##        yogurt}                   => {whole milk}       0.015556685  0.4526627 1.771563   153
    ## [238] {other vegetables,                                                                    
    ##        yogurt}                   => {whole milk}       0.022267412  0.5128806 2.007235   219
    ## [239] {other vegetables,                                                                    
    ##        rolls/buns}               => {whole milk}       0.017895272  0.4200477 1.643919   176

    inspect(subset(grocery_rules, subset=lift > 2 & confidence > 0.5))

    ##      lhs                           rhs                    support confidence     lift count
    ## [1]  {baking powder}            => {whole milk}       0.009252669  0.5229885 2.046793    91
    ## [2]  {onions,                                                                              
    ##       root vegetables}          => {other vegetables} 0.005693950  0.6021505 3.112008    56
    ## [3]  {onions,                                                                              
    ##       whole milk}               => {other vegetables} 0.006609049  0.5462185 2.822942    65
    ## [4]  {hygiene articles,                                                                    
    ##       other vegetables}         => {whole milk}       0.005185562  0.5425532 2.123363    51
    ## [5]  {other vegetables,                                                                    
    ##       sugar}                    => {whole milk}       0.006304016  0.5849057 2.289115    62
    ## [6]  {long life bakery product,                                                            
    ##       other vegetables}         => {whole milk}       0.005693950  0.5333333 2.087279    56
    ## [7]  {cream cheese,                                                                        
    ##       yogurt}                   => {whole milk}       0.006609049  0.5327869 2.085141    65
    ## [8]  {chicken,                                                                             
    ##       root vegetables}          => {other vegetables} 0.005693950  0.5233645 2.704829    56
    ## [9]  {chicken,                                                                             
    ##       root vegetables}          => {whole milk}       0.005998983  0.5514019 2.157993    59
    ## [10] {chicken,                                                                             
    ##       rolls/buns}               => {whole milk}       0.005287239  0.5473684 2.142208    52
    ## [11] {coffee,                                                                              
    ##       yogurt}                   => {whole milk}       0.005083884  0.5208333 2.038359    50
    ## [12] {frozen vegetables,                                                                   
    ##       root vegetables}          => {other vegetables} 0.006100661  0.5263158 2.720082    60
    ## [13] {frozen vegetables,                                                                   
    ##       root vegetables}          => {whole milk}       0.006202339  0.5350877 2.094146    61
    ## [14] {frozen vegetables,                                                                   
    ##       other vegetables}         => {whole milk}       0.009659380  0.5428571 2.124552    95
    ## [15] {beef,                                                                                
    ##       yogurt}                   => {whole milk}       0.006100661  0.5217391 2.041904    60
    ## [16] {curd,                                                                                
    ##       whipped/sour cream}       => {whole milk}       0.005897306  0.5631068 2.203802    58
    ## [17] {curd,                                                                                
    ##       tropical fruit}           => {yogurt}           0.005287239  0.5148515 3.690645    52
    ## [18] {curd,                                                                                
    ##       tropical fruit}           => {other vegetables} 0.005287239  0.5148515 2.660833    52
    ## [19] {curd,                                                                                
    ##       tropical fruit}           => {whole milk}       0.006507372  0.6336634 2.479936    64
    ## [20] {curd,                                                                                
    ##       root vegetables}          => {other vegetables} 0.005490595  0.5046729 2.608228    54
    ## [21] {curd,                                                                                
    ##       root vegetables}          => {whole milk}       0.006202339  0.5700935 2.231146    61
    ## [22] {curd,                                                                                
    ##       yogurt}                   => {whole milk}       0.010066090  0.5823529 2.279125    99
    ## [23] {curd,                                                                                
    ##       rolls/buns}               => {whole milk}       0.005897306  0.5858586 2.292845    58
    ## [24] {curd,                                                                                
    ##       other vegetables}         => {whole milk}       0.009862735  0.5739645 2.246296    97
    ## [25] {pork,                                                                                
    ##       root vegetables}          => {other vegetables} 0.007015760  0.5149254 2.661214    69
    ## [26] {pork,                                                                                
    ##       rolls/buns}               => {whole milk}       0.006202339  0.5495495 2.150744    61
    ## [27] {frankfurter,                                                                         
    ##       tropical fruit}           => {whole milk}       0.005185562  0.5483871 2.146195    51
    ## [28] {frankfurter,                                                                         
    ##       yogurt}                   => {whole milk}       0.006202339  0.5545455 2.170296    61
    ## [29] {bottled beer,                                                                        
    ##       yogurt}                   => {whole milk}       0.005185562  0.5604396 2.193364    51
    ## [30] {brown bread,                                                                         
    ##       tropical fruit}           => {whole milk}       0.005693950  0.5333333 2.087279    56
    ## [31] {brown bread,                                                                         
    ##       root vegetables}          => {whole milk}       0.005693950  0.5600000 2.191643    56
    ## [32] {domestic eggs,                                                                       
    ##       margarine}                => {whole milk}       0.005185562  0.6219512 2.434099    51
    ## [33] {margarine,                                                                           
    ##       root vegetables}          => {other vegetables} 0.005897306  0.5321101 2.750028    58
    ## [34] {margarine,                                                                           
    ##       rolls/buns}               => {whole milk}       0.007930859  0.5379310 2.105273    78
    ## [35] {butter,                                                                              
    ##       domestic eggs}            => {whole milk}       0.005998983  0.6210526 2.430582    59
    ## [36] {butter,                                                                              
    ##       whipped/sour cream}       => {other vegetables} 0.005795628  0.5700000 2.945849    57
    ## [37] {butter,                                                                              
    ##       whipped/sour cream}       => {whole milk}       0.006710727  0.6600000 2.583008    66
    ## [38] {butter,                                                                              
    ##       citrus fruit}             => {whole milk}       0.005083884  0.5555556 2.174249    50
    ## [39] {bottled water,                                                                       
    ##       butter}                   => {whole milk}       0.005388917  0.6022727 2.357084    53
    ## [40] {butter,                                                                              
    ##       tropical fruit}           => {other vegetables} 0.005490595  0.5510204 2.847759    54
    ## [41] {butter,                                                                              
    ##       tropical fruit}           => {whole milk}       0.006202339  0.6224490 2.436047    61
    ## [42] {butter,                                                                              
    ##       root vegetables}          => {other vegetables} 0.006609049  0.5118110 2.645119    65
    ## [43] {butter,                                                                              
    ##       root vegetables}          => {whole milk}       0.008235892  0.6377953 2.496107    81
    ## [44] {butter,                                                                              
    ##       yogurt}                   => {whole milk}       0.009354347  0.6388889 2.500387    92
    ## [45] {butter,                                                                              
    ##       other vegetables}         => {whole milk}       0.011489578  0.5736041 2.244885   113
    ## [46] {newspapers,                                                                          
    ##       root vegetables}          => {other vegetables} 0.005998983  0.5221239 2.698417    59
    ## [47] {domestic eggs,                                                                       
    ##       whipped/sour cream}       => {other vegetables} 0.005083884  0.5102041 2.636814    50
    ## [48] {domestic eggs,                                                                       
    ##       whipped/sour cream}       => {whole milk}       0.005693950  0.5714286 2.236371    56
    ## [49] {domestic eggs,                                                                       
    ##       pip fruit}                => {whole milk}       0.005388917  0.6235294 2.440275    53
    ## [50] {citrus fruit,                                                                        
    ##       domestic eggs}            => {whole milk}       0.005693950  0.5490196 2.148670    56
    ## [51] {domestic eggs,                                                                       
    ##       tropical fruit}           => {whole milk}       0.006914082  0.6071429 2.376144    68
    ## [52] {domestic eggs,                                                                       
    ##       root vegetables}          => {other vegetables} 0.007320793  0.5106383 2.639058    72
    ## [53] {domestic eggs,                                                                       
    ##       root vegetables}          => {whole milk}       0.008540925  0.5957447 2.331536    84
    ## [54] {domestic eggs,                                                                       
    ##       yogurt}                   => {whole milk}       0.007727504  0.5390071 2.109485    76
    ## [55] {domestic eggs,                                                                       
    ##       other vegetables}         => {whole milk}       0.012302999  0.5525114 2.162336   121
    ## [56] {fruit/vegetable juice,                                                               
    ##       root vegetables}          => {other vegetables} 0.006609049  0.5508475 2.846865    65
    ## [57] {fruit/vegetable juice,                                                               
    ##       root vegetables}          => {whole milk}       0.006507372  0.5423729 2.122657    64
    ## [58] {pip fruit,                                                                           
    ##       whipped/sour cream}       => {other vegetables} 0.005592272  0.6043956 3.123610    55
    ## [59] {pip fruit,                                                                           
    ##       whipped/sour cream}       => {whole milk}       0.005998983  0.6483516 2.537421    59
    ## [60] {citrus fruit,                                                                        
    ##       whipped/sour cream}       => {other vegetables} 0.005693950  0.5233645 2.704829    56
    ## [61] {citrus fruit,                                                                        
    ##       whipped/sour cream}       => {whole milk}       0.006304016  0.5794393 2.267722    62
    ## [62] {sausage,                                                                             
    ##       whipped/sour cream}       => {whole milk}       0.005083884  0.5617978 2.198679    50
    ## [63] {tropical fruit,                                                                      
    ##       whipped/sour cream}       => {other vegetables} 0.007829181  0.5661765 2.926088    77
    ## [64] {tropical fruit,                                                                      
    ##       whipped/sour cream}       => {whole milk}       0.007930859  0.5735294 2.244593    78
    ## [65] {root vegetables,                                                                     
    ##       whipped/sour cream}       => {whole milk}       0.009456024  0.5535714 2.166484    93
    ## [66] {whipped/sour cream,                                                                  
    ##       yogurt}                   => {whole milk}       0.010879512  0.5245098 2.052747   107
    ## [67] {rolls/buns,                                                                          
    ##       whipped/sour cream}       => {whole milk}       0.007829181  0.5347222 2.092715    77
    ## [68] {pip fruit,                                                                           
    ##       sausage}                  => {whole milk}       0.005592272  0.5188679 2.030667    55
    ## [69] {pip fruit,                                                                           
    ##       root vegetables}          => {other vegetables} 0.008134215  0.5228758 2.702304    80
    ## [70] {pip fruit,                                                                           
    ##       root vegetables}          => {whole milk}       0.008947636  0.5751634 2.250988    88
    ## [71] {pip fruit,                                                                           
    ##       yogurt}                   => {whole milk}       0.009557702  0.5310734 2.078435    94
    ## [72] {other vegetables,                                                                    
    ##       pip fruit}                => {whole milk}       0.013523132  0.5175097 2.025351   133
    ## [73] {pastry,                                                                              
    ##       root vegetables}          => {other vegetables} 0.005897306  0.5370370 2.775491    58
    ## [74] {pastry,                                                                              
    ##       root vegetables}          => {whole milk}       0.005693950  0.5185185 2.029299    56
    ## [75] {pastry,                                                                              
    ##       yogurt}                   => {whole milk}       0.009150991  0.5172414 2.024301    90
    ## [76] {citrus fruit,                                                                        
    ##       root vegetables}          => {other vegetables} 0.010371124  0.5862069 3.029608   102
    ## [77] {citrus fruit,                                                                        
    ##       root vegetables}          => {whole milk}       0.009150991  0.5172414 2.024301    90
    ## [78] {root vegetables,                                                                     
    ##       shopping bags}            => {other vegetables} 0.006609049  0.5158730 2.666112    65
    ## [79] {sausage,                                                                             
    ##       tropical fruit}           => {whole milk}       0.007219115  0.5182482 2.028241    71
    ## [80] {root vegetables,                                                                     
    ##       sausage}                  => {whole milk}       0.007727504  0.5170068 2.023383    76
    ## [81] {root vegetables,                                                                     
    ##       tropical fruit}           => {other vegetables} 0.012302999  0.5845411 3.020999   121
    ## [82] {root vegetables,                                                                     
    ##       tropical fruit}           => {whole milk}       0.011997966  0.5700483 2.230969   118
    ## [83] {tropical fruit,                                                                      
    ##       yogurt}                   => {whole milk}       0.015149975  0.5173611 2.024770   149
    ## [84] {root vegetables,                                                                     
    ##       yogurt}                   => {whole milk}       0.014539908  0.5629921 2.203354   143
    ## [85] {rolls/buns,                                                                          
    ##       root vegetables}          => {other vegetables} 0.012201322  0.5020921 2.594890   120
    ## [86] {rolls/buns,                                                                          
    ##       root vegetables}          => {whole milk}       0.012709710  0.5230126 2.046888   125
    ## [87] {other vegetables,                                                                    
    ##       yogurt}                   => {whole milk}       0.022267412  0.5128806 2.007235   219

We then plot the confidence, support, and lift to visualize the spreads
of the metrics and see if there are relationships among them.

    plot(grocery_rules)

    ## To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.

![](20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-56-1.png)

    #plot(grocery_rules, measure = c("support", "lift"), shading = "confidence")

Based on the graph showing the spreads of support, confidence, and lift
above, we decided to

-   not filter by support, as most data points have lower than 0.05
    support
-   filter for &gt; 0.5 in confidence, which should still leave a decent
    sample size of data points, then
-   further filter for &gt; 3 in lift to ensure meaningful rules

We then use the filters and plot the association map below.

    sub1 = subset(grocery_rules, subset=confidence > 0.50 & lift > 2.5)
    inspect(sub1)

    ##      lhs                        rhs                    support confidence     lift count
    ## [1]  {onions,                                                                           
    ##       root vegetables}       => {other vegetables} 0.005693950  0.6021505 3.112008    56
    ## [2]  {onions,                                                                           
    ##       whole milk}            => {other vegetables} 0.006609049  0.5462185 2.822942    65
    ## [3]  {chicken,                                                                          
    ##       root vegetables}       => {other vegetables} 0.005693950  0.5233645 2.704829    56
    ## [4]  {frozen vegetables,                                                                
    ##       root vegetables}       => {other vegetables} 0.006100661  0.5263158 2.720082    60
    ## [5]  {curd,                                                                             
    ##       tropical fruit}        => {yogurt}           0.005287239  0.5148515 3.690645    52
    ## [6]  {curd,                                                                             
    ##       tropical fruit}        => {other vegetables} 0.005287239  0.5148515 2.660833    52
    ## [7]  {curd,                                                                             
    ##       root vegetables}       => {other vegetables} 0.005490595  0.5046729 2.608228    54
    ## [8]  {pork,                                                                             
    ##       root vegetables}       => {other vegetables} 0.007015760  0.5149254 2.661214    69
    ## [9]  {margarine,                                                                        
    ##       root vegetables}       => {other vegetables} 0.005897306  0.5321101 2.750028    58
    ## [10] {butter,                                                                           
    ##       whipped/sour cream}    => {other vegetables} 0.005795628  0.5700000 2.945849    57
    ## [11] {butter,                                                                           
    ##       whipped/sour cream}    => {whole milk}       0.006710727  0.6600000 2.583008    66
    ## [12] {butter,                                                                           
    ##       tropical fruit}        => {other vegetables} 0.005490595  0.5510204 2.847759    54
    ## [13] {butter,                                                                           
    ##       root vegetables}       => {other vegetables} 0.006609049  0.5118110 2.645119    65
    ## [14] {butter,                                                                           
    ##       yogurt}                => {whole milk}       0.009354347  0.6388889 2.500387    92
    ## [15] {newspapers,                                                                       
    ##       root vegetables}       => {other vegetables} 0.005998983  0.5221239 2.698417    59
    ## [16] {domestic eggs,                                                                    
    ##       whipped/sour cream}    => {other vegetables} 0.005083884  0.5102041 2.636814    50
    ## [17] {domestic eggs,                                                                    
    ##       root vegetables}       => {other vegetables} 0.007320793  0.5106383 2.639058    72
    ## [18] {fruit/vegetable juice,                                                            
    ##       root vegetables}       => {other vegetables} 0.006609049  0.5508475 2.846865    65
    ## [19] {pip fruit,                                                                        
    ##       whipped/sour cream}    => {other vegetables} 0.005592272  0.6043956 3.123610    55
    ## [20] {pip fruit,                                                                        
    ##       whipped/sour cream}    => {whole milk}       0.005998983  0.6483516 2.537421    59
    ## [21] {citrus fruit,                                                                     
    ##       whipped/sour cream}    => {other vegetables} 0.005693950  0.5233645 2.704829    56
    ## [22] {tropical fruit,                                                                   
    ##       whipped/sour cream}    => {other vegetables} 0.007829181  0.5661765 2.926088    77
    ## [23] {pip fruit,                                                                        
    ##       root vegetables}       => {other vegetables} 0.008134215  0.5228758 2.702304    80
    ## [24] {pastry,                                                                           
    ##       root vegetables}       => {other vegetables} 0.005897306  0.5370370 2.775491    58
    ## [25] {citrus fruit,                                                                     
    ##       root vegetables}       => {other vegetables} 0.010371124  0.5862069 3.029608   102
    ## [26] {root vegetables,                                                                  
    ##       shopping bags}         => {other vegetables} 0.006609049  0.5158730 2.666112    65
    ## [27] {root vegetables,                                                                  
    ##       tropical fruit}        => {other vegetables} 0.012302999  0.5845411 3.020999   121
    ## [28] {rolls/buns,                                                                       
    ##       root vegetables}       => {other vegetables} 0.012201322  0.5020921 2.594890   120

    plot(sub1, method='graph')

![](20190818_R_TakeHome_2_files/figure-markdown_strict/unnamed-chunk-58-1.png)

    saveAsGraph(head(grocery_rules, n = 1000, by = "lift"), file = "grocery_rules.graphml")

**Rule Interpretation:** Among the 28 rules we generated (i.e., those
we’re confident in and have high lifts), most have “other vegetables” on
the right-hand side, while three have “whole milk” and one has “yogurt.”
As can be seen from the graph aboveL - The “other vegetables” rules make
general sense as the other vegetables are usually connected with
specific vegetables and fruits (additionally, they are generally in the
same section at a grocery story), which are always in the left-hand side
of the rules predicting “other vegetables.” - The “whole milk” rules
also make sense. “Whole milk” is connected to many dairy products in the
graph above, understandably as most people who cook with dairy products
require many types of such products. And as expected, all rules have
some sort of dairy product on the left-hand side. - Lastly, the “yogurt”
rule (predicted from curd and tropical fruit) makes sense as curd is
also a dairy product, connected to yogurt in the graph above, and people
often put fruits with their yogurt.
