options(java.parameters = "-Xmx64048m")

library(date)

mre <- read.csv(file = 'mre.csv')
td <- read.csv(file = 'td.csv')
m <- read.csv(file = 'm.csv')
uc <- read.csv(file = 'uc.csv')


source("DataQualityReport.R")

incomplete <- DataQualityReport(m)[,c(1,4)]
fights <- m[,which(incomplete$PercentComplete >90)]

fights <- fights %>% drop_na()

for (i in 1:ncol(fights)) {
  if (is.numeric(fights[,i])==TRUE) {fights[,i]<-as.numeric(fights[,i])}
  else if (is.character(fights[,i])==TRUE & colnames(fights[i])=='date') {fights[,i]<-as.Date(fights[,i],"%m/%d/%Y")} 
  else if (is.character(fights[,i])==TRUE) {fights[,i]<-as.factor(fights[,i])}}

fights$winner_name <- ifelse(fights$Winner == 'Red',as.character(fights$R_fighter),as.character(fights$B_fighter))

fights$winner_name <- as.factor(fights$winner_name)

fights$date <- as.Date(fights$date,"%m/%d/%Y")

