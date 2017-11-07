library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library('dplyr')
library('tidyr')
library(ggplot2)
library(lubridate)
library(highcharter)
library(data.table)
library(plotly)

hr <- read.csv("C:/Users/HP/Desktop/BDA/BDA 503/HR/core_dataset.csv")
# check data
hr<-data.table(hr)

summary(hr)

hr <-na.omit(hr)

hr$Sex[hr$Sex =="male"] <-'Male'
hr$Hispanic.Latino[hr$Hispanic.Latino =="no"] <-'No'
hr$Hispanic.Latino[hr$Hispanic.Latino =="yes"] <-'Yes'
summary(hr$Sex)
summary(hr$Hispanic.Latino)


sexstatus<-hr[Reason.For.Term=="N/A - still employed", .(Total = length(unique(Employee.Number))), .(Sex)][order(-Total)]

sexstatus[,GP:=Total/sum(Total) ,]

plot_ly(sexstatus, labels = ~Sex, values = ~GP, type = 'pie') %>%
  layout(title = 'Performace of Employees by Gender',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


hchart(sexstatus,type="column",hcaes(x=Sex,y=Total))%>%
  hc_add_theme(hc_theme_google())

Martialstatus<-hr[Reason.For.Term=="N/A - still employed", .(Total = length(unique(Employee.Number))), .(MaritalDesc,Sex)][order(-Total)]
Martialstatus[,MP:=Total/sum(Total) ,.(Sex)]


hchart(Martialstatus,type="column",hcaes(x=MaritalDesc,y=MP,group=Sex))%>%
  hc_add_theme(hc_theme_google())  


Positionstatus<-hr[Reason.For.Term=="N/A - still employed", .(Total = length(unique(Employee.Number))), .(Position,Sex)][order(-Total)]


hchart(Positionstatus,type="column",hcaes(x=Position,y=Total,group=Sex)) %>%
  hc_add_theme(hc_theme_google())




plot1 <-ggplot(hr,aes(x=RaceDesc,y=Pay.Rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Pay Rate Vs Races-All ")

plot1

PayRatevsRaces<-hr[Reason.For.Term=="N/A - still employed", .(MedPayRate = median(Pay.Rate)), .(RaceDesc,Sex)][order(-MedPayRate)]

plot2 <-
  ggplot(PayRatevsRaces,aes(x=reorder(RaceDesc,-MedPayRate),y=MedPayRate,fill=Sex)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Sex) +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Pay Rate Vs Races by Gender")

plot2


SourceofEmployees <-hr[,.(Total = length(unique(Employee.Number))),.(Employee.Source,Sex)][order(-Total),]


ggplot(SourceofEmployees,aes(reorder(Employee.Source,-Total),y=Total,fill=Total)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~Sex) +
  ggtitle("Source of Employees by Sex")


PerfbyGender<-hr[Performance.Score !='N/A- too early to review',.(Total = length(unique(Employee.Number))),.(Department,Performance.Score,Sex)][order(-Total),]

ggplot(PerfbyGender,aes(x=reorder(Department,-Total),y=Total,fill=Performance.Score)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Sex) +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Performace of Employees by Gender")


PerfbyManager<-hr[,.(Total = length(unique(Employee.Number))),.(Manager.Name,Performance.Score,Sex)][order(-Total),]

ggplot(PerfbyManager,aes(x=Performance.Score,y=Total,fill=Performance.Score)) +
  geom_bar(stat="identity") +
  facet_wrap(~Manager.Name) +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Manager Wise Performance Rating")