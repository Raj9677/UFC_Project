options(java.parameters = "-Xmx64048m")

mre <- read.csv(file = 'mre.csv')
td <- read.csv(file = 'td.csv')
m <- read.csv(file = 'm.csv')
uc <- read.csv(file = 'uc.csv')


source("DataQualityReport.R")

incomplete <- DataQualityReport(m)[,c(1,4)]
mstat <- m[,which(incomplete$PercentComplete >95)]

mstat <- mstat %>% drop_na()

for (i in 1:ncol(mstat)) {
  if (is.numeric(mstat[,i])==TRUE) {mstat[,i]<-as.numeric(mstat[,i])}
  else if (is.character(mstat[,i])==TRUE & colnames(mstat[i])=='date') {mstat[,i]<-as.Date(mstat[,i],"%m/%d/%Y")} 
  else if (is.character(mstat[,i])==TRUE) {mstat[,i]<-as.factor(mstat[,i])}}





mstat <- rbind((subset(mstat,select=c(R_fighter,R_wins,R_losses,R_draw,
                                                   R_win_by_KOTKO,R_win_by_TKO_Doctor_Stoppage,R_win_by_Decision_Majority,
                                                   R_win_by_Decision_Split,R_win_by_Decision_Unanimous,R_win_by_Submission,
                                                   R_current_win_streak,R_Stance,R_Height_cms,R_Reach_cms,R_Weight_lbs,date,R_age,weight_class,gender)) %>%
         `colnames<-` (c('name','wins','losses','draw',
                         'KOTKO','TKO_Doctor','Majority','Split', 'Unanimous', 'Submission',
                         'streak','Stance','Height_cms','Reach_cms','Weight_lbs','date','Age','weight_class','gender'))),
      
      (subset(mstat,select=c(B_fighter,B_wins,B_losses,B_draw,
                                                   B_win_by_KOTKO,B_win_by_TKO_Doctor_Stoppage,B_win_by_Decision_Majority,
                                                   B_win_by_Decision_Split,B_win_by_Decision_Unanimous,B_win_by_Submission,
                                                   B_current_win_streak,B_Stance,B_Height_cms,B_Reach_cms,B_Weight_lbs,date,B_age,weight_class,gender))) %>%
        `colnames<-` (c('name','wins','losses','draw',
                        'KOTKO','TKO_Doctor','Majority','Split', 'Unanimous', 'Submission',
                        'streak','Stance','Height_cms','Reach_cms','Weight_lbs','date','Age','weight_class','gender')))
  
  mstat <- unique(setorder(setDT(mstat), name, -date), by = "name")
  
  


