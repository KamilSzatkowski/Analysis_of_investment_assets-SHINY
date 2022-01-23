#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#
#

#---- Libraries ----

library(shiny)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(reshape2)
library(ggforce)

#---- Data Loading ----

data_WIG20d <- read_csv("wig20_d.csv", skip = 1, col_names = c("Date", "OpenWIG", "HighWIG", "LowWIG", "Close/LastWIG", "VolumeWIG"), show_col_types = FALSE)
data_USD <- read_csv("moneypl-1638049960588.csv", skip = 1, col_names = c("Table", "Date", "USD_PLN", "Change"), show_col_types = FALSE)
data_Silver <- read_csv("csv.csv", 
                        skip = 16,col_names = c("Date","Silver_USD"), show_col_types = FALSE)
data_Gold<- read_csv("HistoricalData_1638054335317.csv", 
                       col_types = cols("Date" = col_date(format = "%m/%d/%Y")), show_col_types = FALSE)


#---- Data wrangling ----

#data_WIG20d1 <- data_WIG20d
data_WIG20d$AVG_WIG20 = (data_WIG20d$HighWIG + data_WIG20d$LowWIG)/2
total <-merge(data_WIG20d, data_USD, by="Date")
total1 <-merge(total,data_Silver, by="Date")
total2 <-merge(total1,data_Gold, by="Date") 
total2$Gold_USD = (total2$High+total2$Low)/2
#names(total2)[9] <- "USD_PLN"
total2$Silver_PLN = total2$Silver_USD*total2$USD_PLN
total2$Gold_PLN = total2$Gold_USD*total2$USD_PLN
total3 = select(total2,Date,AVG_WIG20,USD_PLN,Silver_PLN,Gold_PLN)

# Data set for the cumulative plot of WIG20 index  and investment metals prices

total4 <- total2 %>%
select(Date, AVG_WIG20, Gold_PLN, Silver_PLN, Gold_USD, Silver_PLN, USD_PLN) %>%
  melt(id="Date")

#

#

# Data set for gold per silver ratio

total5 <- total3
total5$GSratio = (total5$Gold_PLN/total5$Silver_PLN)

# Residual plot

gold.lm=lm(total5$Gold_PLN~total5$Silver_PLN+total5$USD_PLN)
gold.res=resid(gold.lm)

# Data set for comparison of dollar and WIG20 

total6 <- total2 %>%
  select(Date, AVG_WIG20, USD_PLN) %>%
  melt(id="Date")


#---- UI -----
ui <- dashboardPage(
  dashboardHeader(title="Table of Contents"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Intro",tabName="Intro",icon=icon("bullhorn")),
      menuItem("Gold price",tabName="Gold_price", icon=icon("star")),
      menuItem("Silver price",tabName="Silver_price", icon=icon("star")),
      menuItem("Gold and silver comparison",tabName="GoldSilver_ratio", icon=icon("star")),
      menuItem("Index WIG20",tabName="Index_WIG20",icon=icon("star")),
      menuItem("Corelation",tabName="Corelation",icon=icon("flag")),
      menuItem("Linear Regression",tabName="Linear_Regression",icon=icon("flag")),
      menuItem("Summary",tabName="Summary",icon=icon("asterisk")),
      menuItem("Sources",tabName = "Sources",icon=icon("book")))
  ),
  dashboardBody(
    tabItems(
      tabItem("Intro",
              fluidPage(
                h1("Analysis of 10-year prices of gold, silver and the WIG20 index in order to find the best hedge of the time value of money"),
                h3("It is said that inflation never sleeps. For governments it is not an unambiguously negative factor, as rising inflation causes devaluation of public debt, which is important for state economies."),
                h3("In times of crisis and uncertainty, governments support depressed sectors by printing money and social benefits for disadvantaged groups."),
                h3("For consumers, this causes a price increase due to a decline in the purchasing power of money."),
                h3("Add to this the uncertainty in the world, caused by the Covid pandemic, or the unrest in the Middle East caused by the departure of the American stabilization forces from Afghanistan, we can expect an increase in product prices also caused by factors outside our country's economy."),
                h3("Considering the uncertain geopolitical situation in the world, or the rapidly growing inflation in Poland, it seems obvious to think about how you can protect at least some of your money and try to protect yourself against an uncertain future."),
                h3("For the purposes of this analysis, we will look at the prices of gold and silver, the price of the dollar and try to compare these data with the prices of the WIG20 stock index to answer the question which way of keeping the value over time will be the most advantageous."), 
                h3("The table below summarizes the data that will be analyzed in the subsequent stages of the work."),
                dataTableOutput("total3")
              )
      ),
      
      tabItem("Gold_price",
              fluidPage(
                titlePanel(h1("Gold price analysis")),
                sidebarLayout(
                  sidebarPanel(width=6,
                               h3("Histogram for gold price[PLN] in the last 10 years"),
                               plotOutput("Hist_gold")
                  ),
                  sidebarPanel(width=6,
                               h3("Plot of gold price[PLN] in the last 10 years"),
                               plotOutput("generic_gold")
                  )
                ),
                verbatimTextOutput("summary_Gold"),
                h4("During the 10-year period under study, the price of gold ranged from 3635 to 7692 [PLN / ounce]."),
                h4("The most numerous range is [4400,4600], which means that there is a dominant in this range."),
                h4("The observations are unevenly distributed. The median is 4893 and the average is 5145. Therefore, having relations of the type:"),
                h4("Dominant <Median <Mean,"),
                h4("we can confirm that the distribution of gold prices over the 10-year period is characterized by right-hand asymmetry."),
                h4("We are dealing here with an interesting phenomenon, because from 2020 there was a sudden increase in the price to a level above 7,000 [PLN / ounce]. On the histogram you can see that the price rose very rapidly, which can be seen in less and less numerous ranges from 4800 to 6200.6400. On the basis of the chart, we can also see that the price remains above 7000 [PLN / ounce], which means that we can observe a strong upward trend.")
              )
      ),
      
      tabItem("Silver_price",
              fluidPage(
                titlePanel(h1("Silver price analysis")),
                sidebarLayout(
                  sidebarPanel(width=6,
                               h3("Histogram for silver[PLN] in the last 10 years"),
                               plotOutput("Hist_silver")
                  ),
                  sidebarPanel(width=6,
                               h3("Plot of silver[PLN] in the last 10 years"),
                               plotOutput("generic_silver")
                  )
                ),
                verbatimTextOutput("summary_Silver"),
                h4("During the 10-year period examined, the price of silver ranged from 47.97 to 114.41 [PLN / ounce]."),
                h4("The most numerous interval is [55.60], which means that there is a dominant in this interval."),
                h4("The observations are unevenly distributed. The median is 65.09 and the mean is 72.34. Therefore, having relations of the type:"),
                h4("Dominant <Median <Mean,"),
                h4("we can confirm that the distribution of silver prices in the 10-year period is characterized by right-hand asymmetry."),
                h4("An interesting event on the silver market was undoubtedly the strong decline in the price in 2020, which, however, was followed by a rapid increase in the price to an almost record-breaking level from 10 years ago. The silver market, being a market much smaller than the gold market, fluctuates much more (less capital movement is needed to trigger a large rise or fall)."),
                h4("From the chart, we can observe phenomenon related to both silver and gold (although in the gold chart it is less visible due to the size of the market) - the increase after 2010 could have been due to the housing crisis in the United States, while 2020 brought us fear caused by the pandemic , which resulted in a significant and sudden drop in price, and then a fear of inflation, which caused a sudden and sharp increase in the price).")
              )
      ),
      
      tabItem("GoldSilver_ratio",
              fluidPage(
                titlePanel(h1("Gold and silver price ratio")),
                titlePanel(h4("Based on this factor, we can determine how many ounces of silver can be bought for 1 ounce of gold. We can judge from this level the overall strength of the precious metals market. A sudden increase will mean a large increase in the price of gold and silver. A sudden drop will mean a collapse in the price of gold. Knowing that the gold and silver markets are strongly positively correlated with each other, we know that a moderate upward trend will mean an increase in the value of both gold and silver.")),
                sidebarLayout(
                  sidebarPanel(width=6,
                               h3("Histogram for the amount of silver that can be bought for 1 ounce of gold."),
                               plotOutput("Hist_GS")),
                  sidebarPanel(width=6,
                               h3("Plot of the amount of silver that can be bought for 1 ounce of gold."),
                               plotOutput("GoldSilver_ratio"))
                ),
                verbatimTextOutput("summary_Gold_Silver_Ratio"),
                h4("During the 10-year period examined, the ratio of gold price to silver price ranged from 46.83 to 128.28."),
                h4("Dominant took the value of 76."),
                h4("The observations are unevenly distributed. The median is 73.31 and the mean is 72.67. Therefore, having relations of the type:"),
                h4("Dominant> Median> Average,"),
                h4("we can confirm that the gold to silver ratio in the 10-year period is characterized by left-hand asymmetry."),
                h4("The visible upward trend indicates the overall growth of the silver and gold markets combined with a slight advantage of the increase in the price of gold over the price of silver.")
              )
      ),
      
      tabItem("Index_WIG20",
              fluidPage(
                titlePanel(h1("Analysis of the WIG20 index")),
                sidebarLayout(
                  sidebarPanel(width=6,
                               h3("Histogram for WIG20 prices in the last 10 years"),
                               plotOutput("Hist_WIG20")
                  ),
                  sidebarPanel(width=6,
                               h3("Plot for WIG20 prices in the last 10 years"),
                               plotOutput("generic_WIG20")
                  )
                ),
                verbatimTextOutput("summary_WIG20"),
                h4("During the analyzed 10 years, the WIG20 index ranged from 1302 to 2632 [PLN]."),
                h4("The most numerous range is [2300,2400], which means that there is a dominant in this range."),
                h4("The observations are unevenly distributed. The median is 2269 and the average is 2201. Therefore, having relations of the type:"),
                h4("Dominant> Median> Average,"),
                h4("we can confirm that the distribution of WIG20 prices in the 10-year period is characterized by left-hand asymmetry."),
                h4("WIG20 drops or increases concern the Polish market rather than the international market, so it may turn out that the index price will react strongly to changes in the dollar exchange rate."),
                h4("This should have an impact, because the stronger the position of the zloty, the more attractive the Polish market is for foreign capital, which should result in price increases."),
              ),
              sidebarLayout(
                sidebarPanel(width=6,
                             h3("Correlation analisys of the dollar exchange rate and WIG20 prices"),
                             verbatimTextOutput("corWIG20USD"),
                             h4("A high negative correlation informs that the increase in the dollar exchange rate is accompanied by a decline in the average prices of the WIG20 index.")),
                sidebarPanel(width=6,
                             h4("Comparing the dollar exchange rate with the WIG20 index price, a negative correlation can be noticed on the chart."),
                             h4("Looking at the performance of the WIG20 index in the 10-year period, a moderate downward trend can be seen."),
                             h4("In the short term, you can also clearly see the breaking of the trend after 2020 and the attempt to rebound."))
              ),
              plotOutput("plot_WIG20USD")
      ),
      
      tabItem("Corelation",
              fluidPage(
                titlePanel(h1("Correlation of gold with the silver and the WIG20 index prices"),
                ),
                titlePanel(h4("Correlation tells us how the variables are related to each other. If the variables are strictly interdependent, we speak of a high correlation. Similarly, when the relationship between the variables is weak, we speak of a very low or no correlation.")
                ),
                sidebarLayout(
                  sidebarPanel(width=5,
                               h3("Correlation analysis of gold price with the  WIG20 index price"),
                               verbatimTextOutput("corGoldWIG20"),
                               h5("The negative correlation informs that the increase in the value of the gold price is accompanied by a decrease in the average value of the WIG20 index prices."),
                               h3("Correlation analysis of gold price with silver price"),
                               verbatimTextOutput("corGoldSilver"),
                               h5("The negative correlation informs that the increase in the value of the gold price is accompanied by a decrease in the average value of the WIG20 index prices.")
                  ),
                  sidebarPanel(width=7,
                               fluidRow(
                                 box(selectInput(inputId="choose",label="Choose a factor to correlate with the price of gold",
                                                 c("AVG_WIG20","Silver_PLN"),width="100%"),width = 100),
                                 box(plotOutput("correlation_plot"), width = "100%")
                               ))
                ),
                h4("The correlation coefficient allowed us to be sure of the division of assets according to their definitions. A strong negative correlation tells us that an increase in the price of gold causes a decline in the price of the WIG20 index. In fact, it seems more likely that gold and WIG20 characterize other investment groups. Also, with the global unrest, the price of gold grows stronger as people invest in physical metal over which they will have greater control. On the other hand, the quiet times following crises and downtime are rebooting the economy, resulting in an increase in capital flowing into the stock exchanges."), 
                h4("The correlation of the price of gold with the price of silver is logical as both investment assets represent the same group. Hence, when the price of one of them rises, the price of the other also follows the same rate (precious metals).")
                
              )
              
      ),
      tabItem("Linear_Regression",
              titlePanel(h3("This model uses the price of the WIG20 index as a prediction variable to predict the price of gold on a given day.")
              ),
              fluidPage(
                h4("Null hypothesis - There is no relationship between the prices of gold and the prices of WIG20 stocks."),
                h4("Alternative hypothesis - There is a relationship between the price of gold and the price of WIG20 stocks. "),
                verbatimTextOutput("summary_LM_Gold_WIG20"),
                h5("At the bottom of the result we can see that the residual standard error of this model is 833.8. This tells us that the regression model predicts the price of gold with an average error of around 833.8."),
                h5("The adjusted R squared value was 0.2763, which means that the price of the WIG20 affects the price of gold."),
                h5("The value of the F coefficient was 929. Taking into account the fact that the amount of data is quite large, we can conclude that the WIG20 price influences the price of gold."),
                h5("Additionally, a very small value of 'p value' indicates that there is little likelihood that we will notice a relationship between the explain variable and the explanatory variable by accident."),
                h5("Therefore, we reject the null hypothesis in favor of the alternative hypothesis, which is that there is a relationship between the prices of gold and the prices of the WIG20 index."),
                h5("The low value of the corrected R square indicates a poor fit of the model to the data, which makes it nor really useable in terms of forecasting the gold price.")
              ),
              titlePanel(h3("This model uses the silver price as a prediction variable to predict the gold price on a given day.")),
              fluidPage(
                h4("Null hypothesis - There is no relationship between the prices of gold and silver."),
                h4("Alternative hypothesis - There is a relationship between the price of gold and the price of silver."),
                verbatimTextOutput("summary_LM_Gold_Silver"),
                h5("At the bottom of the result we can see that the residual standard error of this model is 736.4. This tells us that the regression model predicts the price of gold with an average error of around 736.4."),
                h5("The adjusted R squared value was 0.4356, which means that the price of silver affects the price of gold."),
                h5("We therefore reject the null hypothesis in favor of the alternative hypothesis, which is that there is a relationship between the price of gold and the price of silver."),
                h5("Higher than in the case of the model based on the WIG20 index, the corrected R square value indicates a greater fit of the model to the data, although this model may still be insufficient for the purposes of forecasting the gold price.")
              ),
              titlePanel(h3("Worth paying attention to ... ... ")),
              fluidPage(h5("on the characteristics of variables. The prices of gold and silver depend primarily on the demand and supply, taking into account the condition of the United States as the country with the largest reserves. Additionally, the economic or political situation in the US determines the demand in a way."),
                        h5("Add to this the rank of a superpower as a country with the greatest impact on the rest of the world, it is therefore fair to say that the price of gold and silver depends primarily on the political and economic situation of the United States."),
                        h5("On the other hand, WIG20 is the 20 largest (in terms of capitalization ) companies on the Warsaw Stock Exchange. On the other hand, the dollar / zloty exchange rate depends on decisions made by central banks and on conducting appropriate market activities, therefore it seems that analyzing gold and silver prices in PLN may result in deterioration of data quality and, as a result, analysis. To verify this view, a comparison of the price of gold with the price of silver in USD is presented below."),
                        verbatimTextOutput("summary_LM_Gold_Silver_USD"),
                        h5("The value of the corrected R squared was 0.5611, which, compared to the value of 0.4356 in the 'gold / silver in PLN' model, shows that the model deteriorated with the transition from USD to PLN."),
                        h5("In addition, the average of the dollar [1] in the period in question was 3.61795, while the residual standard error in the model based on PLN prices is almost 4.7 times higher than in the model based on USD prices. This discrepancy may additionally indicate a deterioration of data quality."),
                        verbatimTextOutput("summary_USD_PLN")
              ),
              titlePanel(h3("Extension of the model using the silver price as a prediction variable with an additional variable in the form of the dollar exchange rate.")),
              fluidPage(h5("Increasing the number of variables may positively affect the level of model fit. Therefore, the dollar exchange rate was added to the model as another predictive variable."),
                        verbatimTextOutput("summary_LM_Gold_USD"),
                        plotOutput("plot_residuals"),
                        h5("In the last model, the distribution of residuals is not strongly symmetrical. It does, however, show signs of a symmetrical distribution. However, the model predicts some points that deviate from the actual observed values."),
                        h5("Additionally, a very small value of 'p value' indicates that there is little likelihood that we will notice a relationship between the target variable and the explanatory variables by chance."),
                        h5("The F statistic is an indicator of whether there is a relationship between the price of gold and the predictors. The further the F statistic is from zero, the better it is for the fit of the model. We can notice that among the tested models, the current one is characterized by the best fit."),
                        h5("The extension of the model by the dollar exchange rate allowed to obtain much more satisfactory results. The value of the residual standard error of 492.3 and the corrected R square of 0.7477 indicate the most exact fit of all the models considered."),
                        h5("The adjusted R squared tells us that ~ 75% of the target variable's variance can be accounted for by the explanatory variable, therefore it can be assumed that the model fits well with the actual data.")
              )
      ),
      
      tabItem("Summary",
              fluidPage(
                h3("Summary"),
                plotOutput("cumulative_plot"),
                h4("By paying attention to the price of silver, you can see exactly an increase at the time of the crisis. First, the price was high after the US housing crisis. After this period, there was a decline in prices for several years, after which the price has risen again since the onset of the covid pandemic."),
                h4("Gold followed a similar path, although the decline after the housing crisis was smaller and the rise in prices during the covid pandemic was more spectacular."),
                h4("In periods when precious metals fell, an increase in the prices of the WIG20 index can be observed. Consequently, if you want to make the right decisions regarding the choice of the investment object, you need to answer a few more questions."),
                h4("1) For what period do we plan to block your money? If for a short period, the WIG20 should be a better choice. Another big event should be the end of the covid19 pandemic, which will be followed by a good period for stock indices."),
                h4("2) Do we want to have access to our funds in real time or we can afford waiting a few days for sale. Brokerage accounts attached to bank accounts allow you to quickly buy and sell, while having physical metals makes it necessary to look for a buyer / mint."),
                h4("However, if you plan to invest for a longer period of time, it is worth paying attention to the periodicity of crises. We can notice a dependence here that the new crisis appears on average 8 years after the previous one. So a 10-year investment would be safer when choosing gold."),
                h4("Silver follows the same trends as gold, although due to the size of the market it is more risky. Both metals are now closer to their highs than their minimums, although looking at the trend that gold is driving, it seems to be a better long-term investment."),
                h4("It is worth noting that the 10-year comparison does not allow for good enough verification of the cyclical nature of crises. The WIG20 index has been in operation since 1994, so it seems that a longer period of time would allow a better examination of the cyclical nature of crises or the seasonality prevailing in the markets."),
                h4("Each investment strategy should involve diversification of resources. It means that when planning your investment portfolio realistically, you should find a place for both the stock exchange, i.e. WIG20 shares, and precious metals.")
              )
      ),
      
      tabItem("Sources",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(width=6,
                               h2("Sources of data"),
                               verticalLayout(
                                 a(href="https://stooq.pl/q/d/?s=wig20&c=0&d1=20111031&d2=20211031","Dane dotyczace WIG20"),
                                 a(href="https://www.money.pl/pieniadze/nbparch/srednie/?symbol=USD","Dane USD/PLN"),
                                 a(href="https://www.macrotrends.net/1470/historical-silver-prices-100-year-chart","Dane dotyczace kursu srebra"),
                                 a(href="https://www.nasdaq.com/market-activity/commodities/gc%3Acmx/historical","Dane dotyczace kursu zlota")
                               )),
                  sidebarPanel(width=6,
                               h2("Other sources:"),
                               verticalLayout(
                                 a(href="http://zsi.tech.us.edu.pl/~nowak/odzw/korelacje.pdf","Korelacja"),
                                 a(href="https://getbootstrap.com/docs/3.4/components/#glyphicons","Ikony do dashboardu"),
                                 a(href="https://cyrkiel.info/statystyka/korelacja-pearsona/","korelacja liniowa"),
                                 a(href="https://www.naukowiec.org/wiedza/statystyka/regresja-liniowa_765.html","Regresja liniowa"),
                               )
                  )
                )
                
              )
      )
    )
  ),skin="blue")


#---- SERVER LOGICS -----
server <- function(input,output){
  
  output$correlation_plot <- renderPlot({
    plot(as.numeric(total3$Gold_PLN),total3[[input$choose]],
         xlab="Average daily price of gold [PLN]",ylab="Price of the selected asset  [PLN]")
  },width=300)
  
  output$generic_gold <- renderPlot({
    ggplot(total3,
           aes(Date,Gold_PLN))+
      geom_line(aes())+
      geom_smooth(method="lm",color="red",size=1,show.legend=TRUE)
  })
  
  output$generic_silver <- renderPlot({
    ggplot(total3,
           aes(Date,Silver_PLN))+
      geom_line(aes())+
      geom_smooth(method="lm",color="red",size=1,show.legend=TRUE)
  })
  
  output$generic_WIG20 <- renderPlot({
    ggplot(total3,
           aes(Date,AVG_WIG20))+
      geom_line(aes())+
      geom_smooth(method="lm",color="red",size=1,show.legend=TRUE)
  })
  
  output$cumulative_plot <- renderPlot({
    ggplot(total4,
           aes(x=Date,y=value,color=variable))+
      geom_line(aes())+
      xlab("Date") +
      ylab("Value") +
      ggtitle("Selected investment assets over the last 10 years")+
      scale_color_discrete(name = "Assets")+
      facet_zoom(
        ylim=c(total3$USD_PLN,total3$Silver_PLN),
        zoom.size=1,
        split=TRUE,
        horizontal = FALSE
      )
  })  
  
  output$plot_WIG20USD <- renderPlot({
    ggplot(total6,
           aes(x=Date,y=value,color=variable))+
      geom_line(aes())+
      xlab("Date") +
      ylab("Value") +
      ggtitle("Comparison of the dollar exchange rate and the WIG20 index price")+
      scale_color_discrete(name = "Assets")+
      facet_zoom(
        ylim= total3$USD_PLN,
        zoom.size=1,
        split=TRUE,
        horizontal = FALSE )
 
  })
  
 
  

  
  output$total3 <- renderDataTable(total3)
  output$corGoldWIG20 <- renderPrint(cor(total3$Gold_PLN,total3$AVG_WIG20))
  output$corGoldSilver <- renderPrint(cor(total3$Gold_PLN,total3$Silver_PLN))
  output$corWIG20USD <- renderPrint(cor(total3$AVG_WIG20,total3$USD_PLN))
  output$summary_LM_Gold_Silver <-renderPrint(summary(lm(formula=total5$Gold_PLN~total5$Silver_PLN)))
  output$summary_LM_Gold_USD <-renderPrint(summary(lm(formula=total5$Gold_PLN~total5$Silver_PLN+total5$USD_PLN)))
  output$summary_LM_Gold_WIG20 <-renderPrint(summary(lm(formula=total5$Gold_PLN~total5$AVG_WIG20)))
  output$summary_LM_Gold_Silver_USD <-renderPrint(summary(lm(formula=total2$Gold_USD~total2$Silver_USD)))
  output$summary_USD_PLN <-renderPrint(mean(total3$USD_PLN))
  output$summary_Gold <- renderPrint(summary(total3$Gold_PLN))
  output$summary_Silver <- renderPrint(summary(total3$Silver_PLN))
  output$summary_WIG20 <- renderPrint(summary(total3$AVG_WIG20))
  output$summary_Gold_Silver_Ratio <- renderPrint(summary(total5$GSratio))
  
  output$plot_residuals <- renderPlot ({
    plot(gold.res,main="Plot of residuals in a linear regression model ",col.axis = "blue",col.lab = "dark blue",ylab="Reszty modelu regresji liniowej")
    abline(0,0)
  })
  
  output$Hist_gold <- renderPlot({
    hist(total3$Gold_PLN,
         ylab="Frequency",
         xlab="Gold price [PLN]",
         col="gold",
         border="purple",
         breaks=20,
         main="")
  })
  
  output$Hist_silver <- renderPlot({
    hist(total3$Silver_PLN,
         ylab="Frequency",
         xlab="Silver price [PLN]",
         col="grey",
         border="purple",
         main="")
  })
  
  output$Hist_WIG20 <- renderPlot({
    hist(total3$AVG_WIG20,
         ylab="Frequency",
         xlab="WIG20 price [PLN]",
         col="blue",
         border="purple",
         main="")
  })
  
  output$Hist_GS <- renderPlot({
    hist(total5$GSratio,
         ylab="Frequency",
         xlab="Quantity [uncja]",
         col="grey",
         border="purple",
         breaks=60,
         main="")
  })
  
  output$GoldSilver_ratio <- renderPlot ({
    ggplot(total5,
           aes(Date,GSratio))+
      xlab("Date [Year]") +
      ylab("Gold / silver ratio ") +
      ggtitle("Gold to silver ratio in the last 10 years ")+
      geom_line(aes())+
      geom_smooth(method="lm",color="red",size=1,show.legend=TRUE)
  })
  
  
}


#---- CALL APP ----
shinyApp(ui, server)