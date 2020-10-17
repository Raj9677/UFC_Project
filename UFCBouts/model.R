

# disconnect from db
# dbDisconnect(con)

display_uc = uc[,c("R_wins","R_Weight_lbs","R_Reach_cms","R_Height_cms","R_age","R_odds","R_fighter", "date","weight_class" ,"location", "B_fighter","B_odds","B_age","B_Height_cms","B_Reach_cms","B_Weight_lbs","B_wins")]

getwd()
source("DataQualityReport.R")

columns_not_related = c("index","R_fighter","B_fighter","date","location","country","constant_1","finish","finish_details","finish_round","finish_round_time","total_fight_time_secs",
                        "gender","no_of_rounds","title_bout","empty_arena")

new_m = m[, !(colnames(m) %in% columns_not_related)]

# First remove columns that have less than 60% data available
incomplete <- DataQualityReport(new_m)[,c(1,4)]
new_m <- new_m[,which(incomplete$PercentComplete >60)]

(columns_missing_data_2000 = colnames(new_m)[ colSums(is.na(new_m)) > 2000])

for (i in 1:ncol(new_m)) 
{
  if (is.numeric(new_m[,i])==TRUE) {new_m[,i]<-as.numeric(new_m[,i])}
  else if (is.character(new_m[,i])==TRUE & colnames(new_m[i])=='date') {new_m[,i]<-as.Date(new_m[,i],"%m/%d/%Y")} 
  else if (is.character(new_m[,i])==TRUE) {new_m[,i]<-as.factor(new_m[,i])}
}

dummies1 = dummyVars( ~ ., data = new_m)
ex1 = data.frame(predict(dummies1,newdata = new_m))
ex1


descrCor <-  cor(ex1$Winner.Blue, ex1,use = "pairwise.complete.obs")
descrCor

#mean(new_m$R_sig_str_pct_bout, na.rm = TRUE)
#mean(new_m$R_kd_bout, na.rm = TRUE)





summary(new_m$R_kd_bout)

descrCor1 <-  cor(ex1,use = "pairwise.complete.obs")
descrCor1



#Blue positive first , then blue negative - cutoff of 0.1
selected_features = c("R_sig_str_pct_bout","B_odds","R_kd_bout","R_pass_bout","R_tot_str_landed_bout",
                      "R_td_pct_bout","R_td_landed_bout","R_sig_str_landed_bout","R_sub_attempts_bout","R_tot_str_attempted_bout",
                      "R_sig_str_attempted_bout","B_kd_bout","R_odds","B_pass_bout","B_sig_str_pct_bout","B_tot_str_landed_bout","B_sig_str_landed_bout",
                      "B_td_landed_bout","B_td_pct_bout","B_tot_str_attempted_bout","B_sub_attempts_bout",
                      "age_dif","B_sig_str_attempted_bout","B_td_attempted_bout","win_streak_dif")


logit <- glm(Winner ~ R_sig_str_pct_bout+B_odds+R_kd_bout+R_pass_bout+R_tot_str_landed_bout+
               R_td_landed_bout+R_sig_str_landed_bout+R_sub_attempts_bout+
               B_kd_bout+B_pass_bout+B_sig_str_pct_bout+B_sig_str_landed_bout+
               B_td_pct_bout+B_tot_str_attempted_bout+B_sub_attempts_bout+
               B_td_attempted_bout, data=new_m, family= "binomial",na.action = na.exclude)

summary(logit)
round(logit$coefficients,6)
round(exp(logit$coefficients),4)
pLogit <- predict(logit, type="response") 
pLogit

yLogit <- ifelse(pLogit>=0.50,"Red","Blue")      # actual class labels
results = data.frame(m$Winner, yLogit, pLogit)
colnames(results) <- c("Winner","yLogit","pLogit")
results

(cm <- table(results$Winner, results$yLogit))

# overall model accuracy
sum(diag(cm))/sum(cm)
#0.8798701
# overall error rate
1-sum(diag(cm))/sum(cm)

table(results$Winner)
sum(cm)

# Load the pROC package
library(pROC)
results$Winner_num <- ifelse(results$Winner == "Blue", 0.5, 1)
# Create a ROC curve
ROC <- roc(results$Winner_num,results$pLogit)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)