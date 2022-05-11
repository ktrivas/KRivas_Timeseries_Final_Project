#Final Project Attempt
library(shiny)
library(fpp3)
library(ggeasy)
library(ggpubr)
library(shinydashboard)
library(quantmod)
library(ggplot2)
library(plotly)
library(tidyquant)
library(shinyWidgets)
library(dashboardthemes)
library(regclass)
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

#---------------------------------
#Final Project Expanasion
#---------------------------------
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

#Simple Naive Model For Fortnite
FortniteNaive <- g_trends %>% 
  filter_index("2017 W29" ~ "2022 W11") %>% 
  select(FortniteInterest)

FortniteNaive %>% model(
  NAIVE(FortniteInterest)
) -> fit2

#Seasonal Naive For Apex
ApexNaive %>% model(
  SNAIVE(ApexInterest ~ lag("year"))
) -> fit3

#Seasonal Naive For Fortnite
FortniteNaive %>% model(
  SNAIVE(FortniteInterest ~ lag("year"))
) -> fit4

#Mean Model For Apex
#removing the weeks that apex was not released to equate to Fortnite data.
ApexNaive %>% model(
  MEAN(ApexInterest)
) -> fit5

#Mean Model For Fortnite
FortniteNaive %>% model(
  MEAN(FortniteInterest)
) -> fit6

#Drift of Apex
ApexNaive %>% model(
  RW(ApexInterest ~ drift())
) -> fit7

#Dift of Fortnite
FortniteNaive %>% model(
  RW(FortniteInterest ~ drift())
) -> fit8

#Holt's Method For Apex
ApexNaive %>% model(
  ETS = ETS(ApexInterest ~ error("A") + trend("A") + season("N"))
) -> fit9

#Holt's Method For Fortnite
FortniteNaive %>% model(
  ETS = ETS(FortniteInterest ~ error("A") + trend("A") + season("N"))
) -> fit10

#Comparing Holt's Winter's Additive and Multiplicative for Apex
ApexNaiveMonth <- ApexNaive %>% 
  index_by(yearmonth(Week)) %>% 
  summarise(ApexInterest = sum(ApexInterest)) #making sum of interest for each month

#Comparing Holt's Winter's Additive and Multiplicative for Apex
ApexNaiveMonth <- ApexNaive %>% 
  index_by(yearmonth(Week)) %>% 
  summarise(ApexInterest = sum(ApexInterest)) #making sum of interest for each month

ApexNaiveMonth %>% model(
  additiveApex = ETS(ApexInterest ~ error("A") + trend("A") + season("A")),
  multiplicativeApex = ETS(ApexInterest ~ error("M") + trend("A") +
                             season("M"))
) -> fit11
#glance(fit11) %>% arrange(AICc)#multiplicative model best model.
best_HW <- fit11 %>% select(multiplicativeApex) %>% forecast(h = 6)

#Comparing Holt's Winter's Additive and Multiplicative For Fortnite
FortniteNaiveMonth <- FortniteNaive %>% 
  index_by(yearmonth(Week)) %>% 
  summarise(FortniteInterest = sum(FortniteInterest))

FortniteNaiveMonth %>% model(
  additive = ETS(FortniteInterest ~ error("A") + trend("A") + season("A")),
  multiplicative = ETS(FortniteInterest ~ error("M") + trend("A") +
                         season("M"))
) -> fit12
best_Fortnite_HW <- fit12 %>% select(multiplicative) %>% forecast(h = 6)

#manual ARIMA for Apex
ApexNaive %>% model(
  arima210 = ARIMA(ApexInterest ~ pdq(2,1,0))
) -> fit13

#Auto Arima for Apex
ApexNaive %>% model(
  ARIMA(ApexInterest)
) -> fit14

#manual ARIMA for Fortnite
FortniteNaive %>% 
  gg_tsdisplay(difference(FortniteInterest), plot_type = "partial")
FortniteNaive %>% model(
  ARIMA(FortniteInterest ~ pdq(1,1,2))
) -> fit15

#Auto ARIMA for Fortnite
FortniteNaive %>% model(
  ARIMA(FortniteInterest)
) -> fit16

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Time Series Analysis"),
                    dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  menuItem("Full Time Series", tabName = "tab1"),
                                  menuItem("Seasonality - Apex",tabName = "tab2"),
                                  menuItem("Seasonality - Fortnite",tabName = "tab3"),
                                  menuItem("Autocorrelation - Apex", tabName = "tab4"),
                                  menuItem("Autocorrelation - Fortnite",tabName = "tab5"),
                                  menuItem("Apex & Fortnite Decomposition", tabName = "tab6"),
                                  menuItem("Apex Simple Naive Model", tabName = "tab7"),
                                  menuItem("Fortnite Simple Naive Model", tabName = "tab8"),
                                  menuItem("Apex Seasonal Naive Model", tabName = "tab9"),
                                  menuItem("Fortnite Seasonal Naive Model", tabName = "tab10"),
                                  menuItem("Apex Mean Model", tabName = "tab11"),
                                  menuItem("Fortnite Mean Model", tabName = "tab12"),
                                  menuItem("Apex Drift Model", tabName = "tab13"),
                                  menuItem("Fortnite Drift Model", tabName = "tab14"),
                                  menuItem("Apex Holt's Method Model", tabName = "tab15"),
                                  menuItem("Fortnite Holt's Method Model", tabName = "tab16"),
                                  menuItem("HW Multiplicative Method Apex", tabName = "tab17"),
                                  menuItem("HW Multiplicative Method Fortnite", tabName = "tab18"),
                                  menuItem("Manual ARIMA - Apex", tabName = "tab19"),
                                  menuItem("Auto ARIMA - Apex", tabName = "tab20"),
                                  menuItem("Manual ARIMA - Fortnite", tabName = "tab21"),
                                  menuItem("Auto ARIMA - Fortnite", tabName = "tab22"))
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem("tab1",
                                plotlyOutput("plot1"),
                                textOutput("text1")),
                        tabItem("tab2",
                                plotlyOutput("plot2"),
                                textOutput("text2")),
                        tabItem("tab3",
                                plotlyOutput("plot3"),
                                textOutput("text3")),
                        tabItem("tab4",
                                plotOutput("plot4"),
                                textOutput("text4")),
                        tabItem("tab5",
                                plotOutput("plot5"),
                                textOutput("text5")),
                        tabItem("tab6",
                                plotlyOutput("plot6"),
                                textOutput("text6")),
                        tabItem("tab7",
                                plotOutput("plot7")),
                        tabItem("tab8",
                                plotOutput("plot8")),
                        tabItem("tab9",
                                plotOutput("plot9")),
                        tabItem("tab10",
                                plotOutput("plot10")),
                        tabItem("tab11",
                                plotOutput("plot11")),
                        tabItem("tab12",
                                plotOutput("plot12")),
                        tabItem("tab13",
                                plotOutput("plot13")),
                        tabItem("tab14",
                                plotOutput("plot14")),
                        tabItem("tab15",
                                plotOutput("plot15")),
                        tabItem("tab16",
                                plotOutput("plot16")),
                        tabItem("tab16",
                                plotOutput("plot17")),
                        tabItem("tab18",
                                plotOutput("plot18")),
                        tabItem("tab19",
                                plotOutput("plot19")),
                        tabItem("tab20",
                                plotOutput("plot20")),
                        tabItem("tab21",
                                plotOutput("plot21")),
                        tabItem("tab22",
                                plotOutput("plot22"))
                        
                        
                      )
                      
                      
                    )
                    
)


server <- function(input, output) { 
  
  output$text1 <- renderText(("This infographic is a time-series graph for both Fortnite and Apex Legends. Fortnite is in red and Apex Legends is in blue. Fortnite began with a strong trend upward upon release and experienced consistent growth until the beginning of 2019. This extreme spike was released to the mysterious black-hole event that became a cultural phenomenon. After this event, Fortnite’s search interest has consistently trended downward while also presenting signs of moderate seasonality. The seasonal increases and decreases for Fortnite are associated with a renewed search interest in the game with each new chapter. Apex Legends experienced the same explosive growth in search interest after its initial release, followed by a steadily decreasing trend. However, Apex lacks the apparent seasonality of Fortnite and is only affected by trend."))
  output$plot1 <- renderPlotly({
    
    X <- ggplot() + 
      geom_line(data=g_trends, aes(x=Week, y = ApexInterest), color = "blue") +
      geom_line(data=g_trends, aes(x=Week, y = FortniteInterest), color = "red") +
      labs(y = "Search Interest", x = "Week", title = "Search Interest Apex Legends Vs. Fortnite") +
      theme_gray() +
      theme(legend.position = "right") +
      easy_center_title()
    ggplotly(X)
  })
  output$text2 <- renderText(("Apex Legends demonstrates little seasonality. There is evidence that renewed search interest happens every 2 or 3 months with the release of a new season, but the renewed interest has been steadily decreasing with time, even with the release of a new season."))
  output$plot2 <- renderPlotly({
    
    ggplotly(ApexSeasonality)
  })
  output$text3 <- renderText(("Fortnite has very clear evidence of seasonality. The search interest for almost every year is highest in October, and the lowest point for each year is roughly in November as well—possibly in anticipation of events at the end of the year, where search interest spikes again. "))
  output$plot3 <- renderPlotly({
    ggplotly(FortSeasonality)
  })
  output$text4 <- renderText(("Apex has clear signs of a strong downward trend that cannot be attributed to random chance alone. This is demonstrated from the ACF or autocorrelation plot. The 6 beginning points outside of the line are the closest points attributed to the games release and presumably when ad spending was the highest to help consumers become aware of the game. Apex lacks seasonal effects because there are no spikes or dips that are repeated every two lags, for example. "))
  output$plot4 <- renderPlot({
    plot(ApexAuto)
  })
  output$text5 <- renderText(("Fortnite has very clear signs of both trending and seasonality presented in the ACF plot. The slow decrease in the ACF as the lags increase is due to the trend, while the even, downward slop is due to the seasonality. Additionally, because every lag is outside of the blue bounds, this cannot be attributed to random chance alone. "))
  output$plot5 <- renderPlot({
    plot(FortAuto)
  })
  output$text6 <- renderText(("Apex Legends that shows the three components of forecasting: trend, seasonality and remainder. Apex is most affected by trend because trend has the largest scale.
                              Fortnite is most impacted by seasonality. The seasonal pattern at the beginning is different than the seasonal pattern at the end, demonstrating the decrease in search term interest as the game gets older. "))
  output$plot6 <- renderPlotly({
    ggplotly(DECOMP)
    
  })
  output$plot7 <- renderPlot({
    fit %>% forecast(h = 6) %>%  autoplot(g_trends)
  })
  output$plot8 <- renderPlot({
    fit2 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
  output$plot9 <- renderPlot({
    fit3 %>% forecast(h = 6) %>%  autoplot(g_trends)
  })
  output$plot10 <- renderPlot({
    fit4 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
  output$plot11 <- renderPlot({
    fit5 %>% forecast(h = 6) %>%  autoplot(g_trends)
  })
  output$plot12 <- renderPlot({
    fit6 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
  output$plot13 <- renderPlot({
    fit7 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
  output$plot14 <- renderPlot({
    fit8 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
  output$plot15 <- renderPlot({
    fit9 %>% forecast( h = 6 ) %>% autoplot(g_trends)
  })
  output$plot16 <- renderPlot({
    fit10 %>% forecast( h = 6) %>% autoplot(g_trends)
  })
  output$plot17 <- renderPlot({
    best_HW  %>% autoplot(ApexNaiveMonth)
  })
  output$plot18 <- renderPlot({
    best_Fortnite_HW  %>% autoplot(FortniteNaiveMonth)
  })
  output$plot19 <- renderPlot({
    fit13 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
  output$plot20 <- renderPlot({
    fit14 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
  output$plot21 <- renderPlot({
    fit15 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
  output$plot22 <- renderPlot({
    fit16 %>% forecast(h = 6) %>% autoplot(g_trends)
  })
}

shinyApp(ui, server)