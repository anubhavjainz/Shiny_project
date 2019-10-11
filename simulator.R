install.packages("TTR")
library(TTR)
install.packages("forecast")
library(forecast)
setwd("C:\\Users\\791884\\Documents\\Radius\\Radius 2.0\\Modeling")
sales_data = select(read_excel("Humira_competitiors.xlsx", sheet = 2),PRODUCT_NAME, COMPETITOR_FLAG, DAY_DATE, TOTAL_RX_COUNT) 
sales_data$DATEID = format(sales_data$DAY_DATE,"%Y%m")
sales_data = sales_data[,-c(3)]
min_date = sqldf('Select PRODUCT_NAME,min(DATEID) from sales_data group by PRODUCT_NAME')
min_date$year = substr(min_date$`min(DATEID)`,0,4)
min_date$month = substr(min_date$`min(DATEID)`,5,6)
sales_data_Final = sqldf('select PRODUCT_NAME,DATEID,COMPETITOR_FLAG ,sum(TOTAL_RX_COUNT) as Rx_count from sales_data group by PRODUCT_NAME,DATEID,COMPETITOR_FLAG')
# own_brand = unique(sales_data_Final) %>% select(sales_data_Final, PRODUCT_NAME ) %>% filter(sales_data_Final, COMPETITOR_FLAG == 'N' ) 
# code to be written for removing Humira from competitor
competitors = filter(sales_data_Final, COMPETITOR_FLAG == 'Y' )
competitor_brand = unique(competitors$PRODUCT_NAME)
df_final <- df[0,]
for (comp in competitor_brand){
 cmp_data = filter(sales_data_Final, PRODUCT_NAME  == comp  )
 cmp_data = select(cmp_data,Rx_count )
 timeseries = ts(cmp_data, frequency = 12, start=c(2016,1))
 decomp_ts = decompose(timeseries)
 ts_sa = timeseries - decomp_ts$seasonal
 ts_forecast <- HoltWinters(ts_sa, gamma=FALSE)
 # plot(ts_forecast, main = comp)
 ts_forecast2 <- forecast(ts_forecast, h=6) 
  plot(ts_forecast2, main = comp  )
 ul = unlist(ts_forecast2[["mean"]])
 avg = tail(ul, n=1)
 df = data.frame(Brand = comp, Prediction = avg)
 df_final <- rbind(df_final,df)
}
piepercent<- round(100*df_final$Prediction/sum(df_final$Prediction), 1)
pie(df_final$Prediction, labels = piepercent, radius = 1 ,col = rainbow(length(df_final$Prediction)))
legend("topleft", legend = df_final$Brand, cex = 0.41, xjust = 0 ,fill = rainbow(length(df_final$Prediction)))
















 # print(timeseries)
 # plot.ts(timeseries)
 # abline(reg=lm(timeseries~time(timeseries)))
 # cycle(timeseries) 
 # plot(aggregate(timeseries,FUN=mean)) 
 # boxplot(timeseries~cycle(timeseries)) 
 # 
 # plot.ts(SMA(timeseries, n=5))
 # 
 # print(SMA(timeseries, n=5))
 # print(SMA(timeseries, n=3))
 # 
 # plot(decompose(timeseries)) 
 
 # plot(ts_sa) 
 # 
 # ts_forecast <- HoltWinters(ts_sa, gamma=FALSE)
 # ts_forecast$SSE
 # plot(ts_forecast) 
 # ts_forecast2 <- forecast.HoltWinters(ts_forecast, h=6) 
 # plot(ts_forecast2)
 # ts_forecast2
 # is.list(ts_forecast2)
 # 
 # 
 # plot.ts(ts_forecast2$residuals)            # make a time plot
 # plotForecastErrors(ts_forecast2$residuals) # make a histogram 
 # 