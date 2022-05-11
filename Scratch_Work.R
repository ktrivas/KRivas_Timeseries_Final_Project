library(shiny)
library(dashboardthemes)
library(fpp3)
library(ggplot2)
library(ggpubr)
library(fable)


# Path where data is
file_path <- "/Users/keltonrivas/Documents/Classes Spring 2022/BAS 475/BAS 475 Final Project/KRivas_Timeseries_Final_Project/multiTimeline.csv"

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Week", "ApexInterest", "FortniteInterest")
# Convert Month to date. Changing to week instead since data file is in weeks.
g_trends$Week <- yearweek(g_trends$Week)
# Convert to tsibble
g_trends <- tsibble(g_trends)

# Google labels low numbers as "<1"
# Convert those to 0s and ensure the column is numbers
g_trends$ApexInterest <- as.numeric(
  ifelse(g_trends$ApexInterest == "<1", 0, g_trends$ApexInterest)
)
g_trends$FortniteInterest <- as.numeric(
  ifelse(g_trends$FortniteInterest == "<1", 0, g_trends$FortniteInterest)
)





##Full Time Series Graphics
X <- ggplot() + 
  geom_line(data=g_trends, aes(x=Week, y = ApexInterest), color = "blue") +
  geom_line(data=g_trends, aes(x=Week, y = FortniteInterest), color = "red") +
  labs(y = "Search Interest", x = "Week", title = "Search Interest Apex Legends Vs. Fortnite") +
  theme_gray() +
  theme(legend.position = "right") +
  easy_center_title()


ggplotly(X)

#This graphic is the current time-series of both Fortnite and Apex Legends. Fortite is in red and apex is in blue.



#Point 4: User selection of Seasonality, Autocorrelation and Decomposition

#Apex Legends Seasonality

g_trends %>% 
  gg_season(ApexInterest, labels = "both") +
  labs (y = "Search Interest Over Time",
        title = "Seasonal Search Interest  - Apex Legends") + easy_center_title() ->
  ApexSeasonality #storing into variable

#Fortnite Seasonality
g_trends %>% 
  gg_season(FortniteInterest, labels = "both") +
  labs (y = "Search Interest Over Time",
        title = "Seasonal Search Interest  - Fortnite") + easy_center_title() ->
  FortSeasonality #storing into variable

#displaying both plots
ggarrange(ApexSeasonality, FortSeasonality)

#subseries attempt
gg_subseries(g_trends,ApexInterest);gg_subseries(g_trends,FortniteInterest)
ggarrange(ApexSeasonality,FortSeasonality)

#Apex Autocorrelation
g_trends %>% 
  ACF(ApexInterest) %>% autoplot() +
  labs(title = "Apex Search Interest White Noise") + easy_center_title() ->#note white noise!  Trended b/c positive values initially
  #that slowly decrease.
  ApexAuto #storing graph into variable


#Fortnite Autocorrelation
g_trends %>% 
  ACF(FortniteInterest) %>% autoplot() +
  labs(title = "Fortnite Search Interest White Noise") + easy_center_title() ->#Both trended 
  #and seasonal?! NOT white noise!
  FortAuto #storing into variable

ggarrange(ApexAuto,FortAuto) #displaying plots side-by-side
#Decomposition

gg_lag(g_trends, geom = "point")
#Apex Decomp.
g_trends %>% 
  model(
    classical_decomposition(ApexInterest, type = "additive")
  ) %>% 
  components() %>% 
  autoplot() +
  labs(title = "Classical additive decomposition of search interest") +
  easy_center_title()

#Fortnite Decomp.
g_trends %>% 
  model(
    classical_decomposition(FortniteInterest, type = "additive")
  ) %>% 
  components() %>% 
  autoplot() +
  labs(title = "Classical additive decomposition of search interests") +
  easy_center_title()



# Option 1: Use pivot_longer to rearrange data before plotting
# Good option if data is on the same scale
# here Beer ranges from 200-600 and tobacco from 4000-8000
# so we can see much about beer
# Might be a good option for google trends since scale
# is always 0-100
# has ugly legend
my_g_trends_data <- g_trends %>% 
  select(Week,ApexInterest,FortniteInterest)

long_g_trends_data <- my_g_trends_data %>% 
  pivot_longer(-Week)
head(long_g_trends_data)

long_g_trends_data %>% 
  model(classical_decomposition(value)) %>% 
  components() %>% 
  autoplot() +
  easy_center_title() ->
  DECOMP
ggplotly(DECOMP)

#To Do:
#-------------------------------------------------------------------
#New modeling requirements
#Simple Models: (1) Naive, (2) Seasonal Naive, (3) Mean, & (4) Drift
#Exponential smoothing: (1) Holts & (2) Holts/Winters
#ARIMA: (1) Manually selected parameters & (2) Auto selected parameters
#----------------------------------------------------------------------

#Simple Naive Model For Apex 
 ApexNaive <- g_trends %>% 
 filter_index("2017 W29" ~ "2022 W11") %>% 
 select(ApexInterest)
ApexNaive <- ApexNaive[-c(1:80),] #removing the weeks that Apex was not in existence. 

ApexNaive %>% model(
  NAIVE(ApexInterest)
  ) -> fit
#report(fit)
#fit %>% forecast(h = 6) %>%  autoplot(g_trends) 
#gg_tsresiduals(fit)

#Simple Naive Model for Fortnite
FortniteNaive <- g_trends %>% 
  filter_index("2017 W29" ~ "2022 W11") %>% 
  select(FortniteInterest)

FortniteNaive %>% model(
  NAIVE(FortniteInterest)
) -> fit2
fit2 %>% forecast(h = 6) %>% autoplot(g_trends)
#gg_tsresiduals(fit2) #The extreme positive residual is from the map change.


#Seasonal Naive For Apex
ApexNaive %>% model(
  SNAIVE(ApexInterest ~ lag("year"))
) -> fit3
#report(fit3)
fit3 %>% forecast(h = 6) %>%  autoplot(g_trends)
#gg_tsresiduals(fit3)#does not deal with trend, acf looks bad. Terrible autocorrelation
#residuals have sharp dip at 2020 W01.

#Seasonal Naive For Fortnite
FortniteNaive %>% model(
  SNAIVE(FortniteInterest ~ lag("year"))
) -> fit4
fit4 %>% forecast(h = 6) %>% autoplot(g_trends)
gg_tsresiduals(fit4)#Seasonal Naive does not account for Trend in data. Residual plot *not* constant


#Mean Model For Apex
#removing the weeks that apex was not released to equate to Fortnite data.
ApexNaive %>% model(
  MEAN(ApexInterest)
) -> fit5
fit5 %>% forecast(h = 6) %>%  autoplot(g_trends)
#report(fit5)
fit5 %>% forecast(h = 6) %>%  autoplot(g_trends)
gg_tsresiduals(fit5)#Mean of residuals at 0; however, there is high autocorrelation in the series.
#Residual plot also tails heavily to the right. 

#Mean Model For Fortnite
FortniteNaive %>% model(
  MEAN(FortniteInterest)
) -> fit6
report(fit6)
fit6 %>% forecast(h = 6) %>% autoplot(g_trends)
gg_tsresiduals(fit6)#Residual plot not centered at 0, extreme autocorrelation present in ACF.
#residual plot not centered at 0; heavy tail to the right.

#Drift of Apex
ApexNaive %>% model(
  RW(ApexInterest ~ drift())
) -> fit7
report(fit7)
fit7 %>% forecast(h = 6) %>% autoplot(g_trends)
gg_tsresiduals(fit7)#Residual appears centered at 0,  ACF looks like white noise. 
#Residual Histogram approximately normal, with very slight left tail. 

#Dift of Fortnite
FortniteNaive %>% model(
  RW(FortniteInterest ~ drift())
) -> fit8
report(fit8)
fit8 %>% forecast(h = 6) %>% autoplot(g_trends)
gg_tsresiduals(fit8)#residual plot appears to have mean of 0. The outliers are associated with the
#destruction of tilter towers. The dip is the reduced search interest once the event was finished.
#ACF only has 1 point outside of limits, which is acceptable. 
#Additionally, histogram of residuals appears normally distributed. Drift model appears to account
#for all information.

#Holt's Method For Apex
ApexNaive %>% model(
  ETS = ETS(ApexInterest ~ error("A") + trend("A") + season("N"))
) -> fit9
report(fit9)
fit9 %>% forecast( h = 6 ) %>% autoplot(g_trends)
gg_tsresiduals(fit9)#Residual plot looks centered at 0. The data is homo-skedastic after the beginning
#Spike from 2019W05 when came started, slight decrease in search interest, then evens out.
#ACF looks good. Outside point is release of valkyrie. 
#Hist of residual plot looks good. Slight outliers on left.

#Holt's Method For Fortnite
FortniteNaive %>% model(
  ETS = ETS(FortniteInterest ~ error("A") + trend("A") + season("N"))
) -> fit10
report(fit10)
fit10 %>% forecast( h = 6) %>% autoplot(g_trends)#plot looks terrible. Holt not able to deal with 
#seasonality and trend of Fortnite Data.
gg_tsresiduals(fit10)#not quite centered at 0? huge spike at 2018 w10, drake collab. 
#largest spike at 2019 W41 when fortnite search interest reached an all time high due to blackout event

#------------------------------------
#finish the Holt's Winter's for Both!
#------------------------------------
#Comparing Holt's Winter's Additive and Multiplicative for Apex
ApexNaiveMonth <- ApexNaive %>% 
index_by(yearmonth(Week)) %>% 
summarise(ApexInterest = sum(ApexInterest)) #making sum of interest for each month
  
ApexNaiveMonth %>% model(
  additiveApex = ETS(ApexInterest ~ error("A") + trend("A") + season("A")),
  multiplicativeApex = ETS(ApexInterest ~ error("M") + trend("A") +
                         season("M"))
) -> fit11
glance(fit11) %>% arrange(AICc)#multiplicative model best model.
best_HW <- fit11 %>% select(multiplicativeApex) %>% forecast(h = 6)
best_HW  %>% autoplot(ApexNaiveMonth)

#Comparing Holt's Winter's Additive and Multiplicative For Fortnite
FortniteNaiveMonth <- FortniteNaive %>% 
  index_by(yearmonth(Week)) %>% 
  summarise(FortniteInterest = sum(FortniteInterest))

FortniteNaiveMonth %>% model(
  additive = ETS(FortniteInterest ~ error("A") + trend("A") + season("A")),
  multiplicative = ETS(FortniteInterest ~ error("M") + trend("A") +
                         season("M"))
) -> fit12
glance(fit12) %>% arrange(AICc)
best_Fortnite_HW <- fit12 %>% select(multiplicative) %>% forecast(h = 6)
best_Fortnite_HW  %>% autoplot(FortniteNaiveMonth)


#Manual Arima For Apex
ApexNaive %>% 
gg_tsdisplay(difference(ApexInterest),plot_type = "partial")#checking ACF and PACF to identify parameters
#for manual ARIMA
ApexNaive %>% features(ApexInterest, unitroot_ndiffs)#need to difference twice.
ApexNaive %>% features(ApexInterest, unitroot_nsdiffs)#no seasonal differencing required
ApexNaive %>% model(
  arima210 = ARIMA(ApexInterest ~ pdq(2,1,0))
) -> fit13
#report(fit13)
fit13 %>% forecast(h = 6) %>% autoplot(g_trends)

#Auto Arima for Apex
ApexNaive %>% model(
  ARIMA(ApexInterest)
) -> fit14
report(fit14)#best model has ARIMA(4,2,0)(1,0,1)
fit14 %>% forecast(h = 6) %>% autoplot(g_trends)

#manual ARIMA for Fortnite
FortniteNaive %>% 
gg_tsdisplay(difference(FortniteInterest), plot_type = "partial")
FortniteNaive %>% model(
  ARIMA(FortniteInterest ~ pdq(1,1,2))
) -> fit15
report(fit15)
fit15 %>% forecast(h = 6) %>% autoplot(g_trends)

#Auto ARIMA for Fortnite
FortniteNaive %>% model(
  ARIMA(FortniteInterest)
) -> fit16
report(fit16)#best model HAS ARIMA(2,1,2)(1,0,0) w/drift
fit16 %>% forecast(h = 6) %>% autoplot(g_trends)#ARIMA is terrible model for this data.


  
  
  