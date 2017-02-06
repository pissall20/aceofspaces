setwd("C:/Users/Siddhesh Pisal/Documents/App-1")


#Import necessary packages
library(httr)
library(ggplot2)

#This is the start date if you want to start from a particular date
start_date <- as.Date("01-01-2016", "%d-%m-%Y")

#Assuming that you have the data already and want to update it, this fetches
#the end date from the data. You can also read the data from the working directory
end_date <-
  as.Date(newtable$Date[length(newtable$Date)], "%d-%b-%Y")

#This is your current date
my_date <- as.Date(Sys.Date(), "%d-%m-%Y")

#We will use this URL from downloading the data first time
myUrl <- paste0("http://portal.amfiindia.com/DownloadNAVHistoryReport_Po.aspx?mf=9&tp=1&frmdt=",as.character(start_date),"&todt=",as.character(my_date))

#We will use this URL to update the data. The changing can be done using an
#ifelse statement but we will edit the while loop to use the particular URL
#Use myUrl for first time and #MyUrl1 for updating
myUrl1 <- paste0("http://portal.amfiindia.com/DownloadNAVHistoryReport_Po.aspx?mf=9&tp=1&frmdt=", as.character(end_date),"&todt=",as.character(my_date))

#Write a function to download the data in one go.
#It reads data from a URL and formats it into a table, and sends it to environment
#It also creates a file in the working directory of the data table

datadown <- function(string) {
  response <- readLines(string)
  newlist <- response[-c((grep("Open", response)), (grep("HDFC Mutual Fund", response)))]
  newlist <- newlist[which(newlist != "")]
  newtable <- read.table(textConnection(newlist), sep = ";", header = T)
  newtable$Date <- as.Date(newtable$Date, "%d-%b-%Y")
  assign("newtable", newtable, envir = .GlobalEnv)
  write.csv(newtable, file = paste0("NAV_", end_date, ".csv"), row.names = F)
}

#This is the function that scrapes the data, edits it to put it into columns
#and it will make a table of the NAV data
#Use this function for the first time download, or else use the while loop below
datadown(myUrl)

#This is the loop for incremental download if you don't want
#to download the data from the start

while (end_date < my_date - 2) {
  datadown(myUrl1)
  end_date = end_date + 1
}

######

#We export the data to a .csv file in the working directory.

#write.csv(table1, file = paste0("NAV_",end_date,".csv"))
#table1$Date <- as.Date(table1$Date, "%d-%b-%Y")
str(newtable)
summary(newtable)

#Checking the number of different schemes
unique(newtable$Scheme.Code)

#Write a function to calculate the gain in the net asset values
#We will create a system to rank the Mutual funds based on the gain

gainfunc <- function(datatable) {
  #Here, we take all the schemes and their maximum Net asset values
  maxval <- aggregate(datatable$Net.Asset.Value,list(datatable$Scheme.Code),max)
  maxval$maxDate <- datatable[with(datatable, ave(Net.Asset.Value, FUN = max) == Net.Asset.Value), 6]
  names(maxval) <- c("Scheme.Code", "Max.Net.Asset.Value", "Max.Date")
  
  #Here, we take all the schemes and their minimum Net asset values
  minval <- aggregate(datatable$Net.Asset.Value,list(datatable$Scheme.Code),min)
  minval$minDate <- datatable[with(datatable, ave(Net.Asset.Value, FUN = min) == Net.Asset.Value), 6]
  names(minval) <- c("Scheme.Code", "Min.Net.Asset.Value", "Min.Date")
  
  #We bind the two tables above, so that we can find the asset value gain
  navtable <- merge(maxval, minval, by = "Scheme.Code")
  
  #We calculate the % gain so that it will be in a scaled proportion to compare
  navtable$Gain <- ((navtable$Max.Net.Asset.Value - navtable$Min.Net.Asset.Value) / navtable$Min.Net.Asset.Value) * 100
  assign("gaintable", navtable, envir = .GlobalEnv)
}
gainfunc(newtable)


#We create a function to find the top 10 competitors in the mutual funds list
#We will loop through the list of scheme codes and get the top 10 in the list
topfunds <- function(newtable, gaintable) {
  newnames <- c("top1","top2","top3","top4","top5","top6","top7","top8","top9","top10")
  for (i in 1:10) {
    topscheme <- subset(newtable, newtable$Scheme.Code == gaintable$Scheme.Code[i])
    assign(newnames[i], topscheme, envir = .GlobalEnv)
  }
}
#We use the function made above
topfunds(newtable, gaintable)

#We use a for loop to calculate the moving gain of the schemes

for (i in 1:length(top1$Net.Asset.Value)) {
  top1$MovGain[i] <-
    top1$Net.Asset.Value[i + 1] - top1$Net.Asset.Value[i]
  top2$MovGain[i] <-
    top2$Net.Asset.Value[i + 1] - top2$Net.Asset.Value[i]
  top3$MovGain[i] <-
    top3$Net.Asset.Value[i + 1] - top3$Net.Asset.Value[i]
  top4$MovGain[i] <-
    top4$Net.Asset.Value[i + 1] - top4$Net.Asset.Value[i]
  top5$MovGain[i] <-
    top5$Net.Asset.Value[i + 1] - top5$Net.Asset.Value[i]
  top6$MovGain[i] <-
    top6$Net.Asset.Value[i + 1] - top6$Net.Asset.Value[i]
  top7$MovGain[i] <-
    top7$Net.Asset.Value[i + 1] - top7$Net.Asset.Value[i]
  top8$MovGain[i] <-
    top8$Net.Asset.Value[i + 1] - top8$Net.Asset.Value[i]
  top9$MovGain[i] <-
    top9$Net.Asset.Value[i + 1] - top9$Net.Asset.Value[i]
  top10$MovGain[i] <-
    top10$Net.Asset.Value[i + 1] - top10$Net.Asset.Value[i]
}

###

#We bind the datasets of the top 10 schemes for passing it to Shiny app

topten <- rbind(top1, top2, top3, top4, top5, top6, top7, top8, top9, top10)
#topfive <- rbind(top1, top2, top3, top4, top5)
#topthree <- rbind(top1, top2, top3)

######

write.csv(topten, "toptenmutualfunds.csv", row.names = F)

#######
#Plot Trials

g <- ggplot(top1, aes(Date, Net.Asset.Value)) + geom_point(aes(y = Net.Asset.Value))
g + geom_line(color = "orange", size = 1) +  theme_bw()


g1 <- ggplot(top5, aes(Date, Net.Asset.Value))
g1 + geom_line(color = "orange", size = 1) +  theme_bw()


gainplot <- ggplot(topthree, aes(x = Date,y = MovGain,color = factor(Scheme.Name))) + geom_line(aes(group = factor(Scheme.Name)), size = 1)
gainplot + xlab("Time") + ylab("Gain in Net Asset Value")  + labs(color = "Scheme Name")
