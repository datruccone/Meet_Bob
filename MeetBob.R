library(tidyverse)
library(BatchGetSymbols)
library("ggplot2")
library(grDevices)
library(hrbrthemes) #Themes for ggplot2
library(lubridate)
library(grid) # for textGrob
library(stats)
library(plyr)

rm(list = ls())

#Parameters:
Start_Date <- as.Date('2009-02-01') #Of the study
Holding_Period = 10  #Years since first buy till selling
Currency = 'EUR'
Monthly_Buy = 300
TER = 0 #% per Year
Gain_Scale = 1000
Index <- c('^GSPC') # Since 1928
#Index <- c('^STOXX50E') # Since 1987, but as STOXX50E and in EUR since 1999
#Index <- c('VGWL.DE') #Vanguard FTSE All-World UCITS ETF Distributing - Since 2012
#Index <- c('IQQY.DE') #iShares Core MSCI Europe UCITS ETF EUR (Dist) - Since 01/2009 (As SP500 but EU's)
#Index <- c('EXW1.DE') #iShares EURO STOXX 50 UCITS ETF (DE) (Dist) - Since 2001
#Index <- c('EUN2.DE') #iShares Core EURO STOXX 50 UCITS ETF EUR (Dist) - Since 2001
#Index <- c('EXS1.DE') #iShares Core DAX UCITS ETF (DE) - Since 2001
#Index <- c('ACWI') #iShares MSI All World (DE) - Since 04/2008


#Index Import, this piece of code is based on: https://cran.r-project.org/web/packages/BatchGetSymbols/vignettes/BatchGetSymbols-vignette.html
Index_Data <- BatchGetSymbols(tickers = Index, 
                         first.date = Start_Date %m-% months(1),   
                         last.date = Sys.Date(), 
                         #freq.data = 'monthly',
                         freq.data = 'daily',
                         thresh.bad.data = 0,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )$df.tickers # cache in tempdir()

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#elapsed_months(Sys.time(),Start_Date)

#Function to extract data between given dates
date_range <- function(x,y){
  return <- Index_Data[complete.cases(Index_Data[ , 1]),]
#  return <- return[return$ref.date >= x & return$ref.date <= y,c(7,1)] #c(8,7,5,1,9) for Open Price
  return <- return[return$ref.date >= x & return$ref.date <= y,c(7,2)] #c(8,7,5,2,9) for High Price
  return$Date <- format(return$ref.date, format="%Y-%m")
#  return <- ddply(return, .(Date), summarize, Price=first(price.open))
  return <- ddply(return, .(Date), summarize, Price=max(price.high))
}

Start <- as.Date(vector())
End <- as.Date(vector())
Invested <- vector()
Portfolio_atEnd <- vector()
Gain <- vector()
Gain_Perc <- vector()

Next_Date <- Start_Date

for (i in 1:(elapsed_months((Sys.time() %m-% months(Holding_Period*12)),Start_Date))){    
  Portfolio <- date_range(Next_Date,Next_Date %m+% months(Holding_Period*12))
  Portfolio$Price_TER <- (Portfolio$Price + (Portfolio$Price*(TER/1200)))
  Portfolio$CashFlow <- Monthly_Buy
  Portfolio$CashFlow_Cum <- cumsum(Portfolio$CashFlow)
  Portfolio$Shares_Bought <- 0
  Portfolio$Remainder  <- 0
  Portfolio$Cash <- Portfolio$CashFlow
  for (r in 1:nrow(Portfolio)){
    if (Portfolio$Cash[r] > Portfolio$Price_TER[r]){
      Portfolio$Shares_Bought[r] <- floor(Portfolio$Cash[r]/Portfolio$Price_TER[r])
      Portfolio$Remainder[r] <- floor(Portfolio$Cash[r]%%Portfolio$Price_TER[r])
      if (r<(nrow(Portfolio))){
        Portfolio$Cash[r+1] <- Portfolio$Remainder[r] + Portfolio$CashFlow[r+1]
      }
    } else {
      if (r<(nrow(Portfolio))){
        Portfolio$Cash[r+1] <- Portfolio$Cash[r] + Portfolio$CashFlow[r+1]
      }
    }   
  }
  Portfolio$Shares_Total <- cumsum(Portfolio$Shares_Bought)
  Portfolio$CashFlow_Cum <- cumsum(Portfolio$CashFlow)
#  Portfolio$Portfolio_Value <- round(Portfolio$Shares_Total * Portfolio$Price,2) #Without TER
  Portfolio$Portfolio_Value <- round(Portfolio$Shares_Total * Portfolio$Price,2)
  r = 1
  for (r in 1:nrow(Portfolio)){
    if (Portfolio$Shares_Total[r] > 0){
      Portfolio$Portfolio_Gain[r] <- round(Portfolio$Portfolio_Value[r] - Portfolio$CashFlow_Cum[r] ,2) #Not counting Remaining Cash which is going to be use to buy shares next month
    } else {
      Portfolio$Portfolio_Gain[r] <- 0
    }
  }
  Start[i] <- as.Date(Next_Date)
  End[i] <- Next_Date %m+% months(Holding_Period*12)
  Portfolio_atEnd[i] <- Portfolio$Portfolio_Value[Holding_Period*12]
  Gain[i] <- Portfolio$Portfolio_Gain[Holding_Period*12]
  Next_Date <- Next_Date %m+% months(1)
}

Invested <- Monthly_Buy*Holding_Period*12
Gain_Perc <- round(((Portfolio_atEnd/Invested)-1)*100,2)

Results <- data.frame(Start, End, Invested, Portfolio_atEnd, Gain,Gain_Perc)

Bob_Plot <- ggplot(Results,aes(End)) +
        theme_ft_rc(plot_title_size = 11,
                    axis_title_face = "bold", 
                    axis_title_just = "m",
                    axis_title_size = 9,
        )+
        geom_line(aes(y = Gain/Gain_Scale),size=1,colour = "#147b9a") + 
#        scale_y_continuous(name = paste0("Gain (x", Gain_Scale, ")"),breaks=seq(-100,10000,Breaks), sec.axis = sec_axis(~((((.*Gain_Scale)+Invested)/Invested)-1)*100,name = "Gain (%)",breaks=seq(-100,200,Breaks))) +
        scale_y_continuous(name = paste0("Gain (x", Gain_Scale, ")"), sec.axis = sec_axis(~((((.*Gain_Scale)+Invested)/Invested)-1)*100,name = "Gain (%)")) +
  #      scale_x_date(name = "End Year", date_labels = "%Y", date_breaks = "2 years")+
        scale_x_date(name = "End Year", date_labels = "%m/%y", date_breaks = "1 month")+
          ggtitle(paste0("Bob's Gain after Buying ",Currency," ", Monthly_Buy, " per Month of ",Index, " and Holding it for ",Holding_Period, " Years:"))+
#  geom_hline(yintercept = min(Gain)/Gain_Scale,colour = "Red",linetype="dashed")+
        geom_vline(xintercept = ((as.Date(Results[which.min(Results$Gain),2])) %m-% months(Holding_Period*12)),colour = "Red")+
        geom_vline(xintercept = as.Date(Results[which.min(Results$Gain),2]),colour = "Red")+
        annotate(geom="text",x=as.Date(Results[which.min(Results$Gain),2]),
           y=(((Results[which.max(Results$Gain),5] + Results[which.min(Results$Gain),5])/2)/Gain_Scale),
    #        y=0,
            label=paste0("Worst Period:\n",format(Results[which.min(Results$Gain),1],"%m/%y")," to ", format(Results[which.min(Results$Gain),2],"%m/%y"), "\n",round(Results[which.min(Results$Gain),5]/Gain_Scale)," (",round(Results[which.min(Results$Gain),6]),"%)"))+
        geom_hline(yintercept = mean(Results$Gain)/Gain_Scale,colour = "White",linetype="dashed")+
        geom_vline(xintercept = ((as.Date(Results[which.max(Results$Gain),2])) %m-% months(Holding_Period*12)),colour = "Green")+
        geom_vline(xintercept = as.Date(Results[which.max(Results$Gain),2]),colour = "Green")+
        annotate(geom="text",x=as.Date(Results[which.max(Results$Gain),2]),
        y=(((Results[which.max(Results$Gain),5] + Results[which.min(Results$Gain),5])/2)/Gain_Scale),
       #    y=0,
            label=paste0("Best Period:\n",format(Results[which.max(Results$Gain),1],"%m/%y")," to ", format(Results[which.max(Results$Gain),2],"%m/%y"), "\n",round(Results[which.max(Results$Gain),5]/Gain_Scale)," (",round(Results[which.max(Results$Gain),6]),"%)"))+
        annotation_custom(grobTree(textGrob("r/ETFs_Europe", x=0.1,  y=0.05,gp=gpar(col="red", fontsize=13, fontface="italic"))))
print(Bob_Plot)

ggsave(paste0("plots/","Bob_",Currency,"_",Monthly_Buy,"_",Index,"_",Holding_Period,".jpeg"),plot = Bob_Plot,width = 14, height = 8, dpi = 600, units = c("in"), limitsize = FALSE)
#View(Results[which.min(Results$Gain),],title = paste0("Worst ", Holding_Period, " Years Period for USD ", Monthly_Buy*Holding_Period*12, " Invested"))
cat (paste0("Worst ", Holding_Period, " Years Period for USD ", Monthly_Buy*Holding_Period*12, " Invested:"))
print.data.frame(Results[which.min(Results$Gain),],quote=FALSE,row.names = FALSE)
#View(Results[which.max(Results$Gain),],title = paste0("Best ", Holding_Period, " Years Period for USD ", Monthly_Buy*Holding_Period*12, " Invested"))
cat (paste0("Best ", Holding_Period, " Years Period for USD ", Monthly_Buy*Holding_Period*12, " Invested:"))
print.data.frame(Results[which.max(Results$Gain),],quote=FALSE,row.names = FALSE)
cat (paste0("Average: ",Currency, " ", round(mean(Results$Gain)), " (", round(mean(Results$Gain_Perc)),"%)"))
