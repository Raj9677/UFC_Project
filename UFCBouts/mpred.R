library(stringr)
library(RMariaDB)
library(sqldf)

mpred <- rbind(subset(m,select=c(R_fighter,R_odds,R_tot_str_landed_bout,R_td_landed_bout,R_pass_bout,
                           R_kd_bout,R_sub_attempts_bout,R_sig_str_landed_bout,
                           R_sig_str_pct_bout,R_tot_str_attempted_bout,R_td_attempted_bout,R_td_pct_bout,date)) %>%
        `colnames<-` (c('name','odds','tot_stlanded_bout','td_landed_bout','pass_bout','kd_bout','sub_attempts_bout',
                       'sig_stlanded_bout','sig_stpct_bout','tot_stattempted_bout','td_attempted_bout','td_pct_bout','date')),
        subset(m,select=c(B_fighter,B_odds,B_tot_str_landed_bout,B_td_landed_bout,B_pass_bout,
                          B_kd_bout,B_sub_attempts_bout,B_sig_str_landed_bout,
                          B_sig_str_pct_bout,B_tot_str_attempted_bout,B_td_attempted_bout,B_td_pct_bout,date)) %>%
          `colnames<-` (c('name','odds','tot_stlanded_bout','td_landed_bout','pass_bout','kd_bout','sub_attempts_bout',
                          'sig_stlanded_bout','sig_stpct_bout','tot_stattempted_bout','td_attempted_bout','td_pct_bout','date'))
) 


for (i in 1:ncol(mpred)) 
{
  if (is.numeric(mpred[,i])==TRUE) {mpred[,i]<-as.numeric(mpred[,i])}
  else if (is.character(mpred[,i])==TRUE & colnames(mpred[i])=='date') {mpred[,i]<-as.Date(mpred[,i],"%m/%d/%Y")} 
  else if (is.character(mpred[,i])==TRUE) {mpred[,i]<-as.factor(mpred[,i])}
}


mpred$name <- as.character(mpred$name)


avg1 <- aggregate(cbind(tot_stlanded_bout,td_landed_bout,pass_bout,kd_bout,sub_attempts_bout,sig_stlanded_bout,sig_stpct_bout,tot_stattempted_bout,td_attempted_bout,td_pct_bout) ~ name, data=mpred, FUN=mean, na.rm=TRUE)
avg2 <- data.frame(t(colMeans(mpred[,3:12],na.rm=TRUE)))


mpred <- sqldf(
  'SELECT m.name, m.odds,
  case when m.tot_stlanded_bout is null then ifnull(a.tot_stlanded_bout,a2.tot_stlanded_bout)
  else m.tot_stlanded_bout end tot_stlanded_bout,
  case when m.td_landed_bout is null then ifnull(a.td_landed_bout,a2.td_landed_bout)
  else m.td_landed_bout end td_landed_bout,
  case when m.pass_bout is null then ifnull(a.pass_bout,a2.pass_bout)
  else m.pass_bout end pass_bout,
  case when m.kd_bout is null then ifnull(a.kd_bout,a2.kd_bout)
  else m.kd_bout end kd_bout,
  case when m.sub_attempts_bout is null then ifnull(a.sub_attempts_bout,a2.sub_attempts_bout)
  else m.sub_attempts_bout end sub_attempts_bout,
  case when m.sig_stlanded_bout is null then ifnull(a.sig_stlanded_bout,a2.sig_stlanded_bout)
  else m.sig_stlanded_bout end sig_stlanded_bout,
  case when m.sig_stpct_bout is null then ifnull(a.sig_stpct_bout,a2.sig_stpct_bout)
  else m.sig_stpct_bout end sig_stpct_bout,
  case when m.tot_stattempted_bout is null then ifnull(a.tot_stattempted_bout,a2.tot_stattempted_bout)
  else m.tot_stattempted_bout end tot_stattempted_bout,
  case when m.td_attempted_bout is null then ifnull(a.td_attempted_bout,a2.td_attempted_bout)
  else m.td_attempted_bout end td_attempted_bout,
  case when m.td_pct_bout is null then ifnull(a.td_pct_bout,a2.td_pct_bout)
  else m.td_pct_bout end td_pct_bout,
  m.date
  from mpred m, avg2 a2
  left join avg1 a on m.name=a.name'
)



for (i in 1:ncol(mpred)) 
{
  if (is.numeric(mpred[,i])==TRUE) {mpred[,i]<-as.numeric(mpred[,i])}
  else if (is.character(mpred[,i])==TRUE & colnames(mpred[i])=='date') {mpred[,i]<-as.Date(mpred[,i],"%m/%d/%Y")} 
  else if (is.character(mpred[,i])==TRUE) {mpred[,i]<-as.factor(mpred[,i])}
}

mpred <- unique(setorder(setDT(mpred), name, -date), by = "name")





