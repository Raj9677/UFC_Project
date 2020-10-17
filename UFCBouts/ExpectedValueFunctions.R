Joined_Data = join(x=td, y=uc, by=c("R_fighter" = "R_fighter", "B_fighter" = "B_fighter"), type="inner") %>%
  select(R_fighter,B_fighter,R_odds,B_odds,R_prob)


get_bet_expected_value = function(odds, prob) {
  
  if (odds>0){
    return ((odds * prob) - (100 * (1-prob)) )
  }
  else{
    return ((100 / abs(odds))*100*prob - (100 * (1-prob)))
  } 
  
}

get_winner_plus_odds = function(DataSelected){
  
  Total_winnings = 0
  output = "Here Expected Value of earnings is calculated based on 100 untis of cash bet per fight.\n"
  for(i in 1:nrow(DataSelected)){
    Ev_Red = get_bet_expected_value(DataSelected[i,"R_odds"],DataSelected[i,"R_prob"])
    Ev_Blue = get_bet_expected_value(DataSelected[i,"B_odds"], (1 - DataSelected[i,"R_prob"]))
    
    output = paste(output,DataSelected[i,"R_fighter"], " (RED) vs.", DataSelected[i,"B_fighter"],"(RED) \n" )
    output = paste(output,DataSelected[i,"R_fighter"], "has a probability of", DataSelected[i,"R_prob"]* 100,
                   "percent of winning. His odds are",DataSelected[i,"R_odds"], "This gives a single bet EV of",Ev_Red, "\n" )
    output = paste(output,DataSelected[i,"B_fighter"], "has a probability of", (1-DataSelected[i,"R_prob"])* 100,
                   "percent of winning. His odds are",DataSelected[i,"B_odds"], "This gives a single bet EV of",Ev_Blue,"\n" )
    if (Ev_Red>Ev_Blue){
      output = paste(output,"Red is a better bet \n") 
      Total_winnings = Total_winnings + Ev_Red
    }else{
      output = paste(output,"Blue is a better bet \n")
      Total_winnings = Total_winnings + Ev_Blue
    }
    
  }
  output = paste(output,"Total Winnings=",Total_winnings,"\n")
  
  return (output)
  
}
