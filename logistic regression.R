library("MatchIt")
library("cobalt")    
library("ggplot2")   
library('dplyr')
library('lme4')
final_data <- read.csv('finalhaha.csv')
final_data$Gender <- as.factor(final_data$Gender)
final_data$Ethnicity <- as.factor(final_data$Ethnicity)
final_data$Econ_Index <- as.factor(final_data$Econ_Index)
final_data$tenure <- as.factor(final_data$tenure)  
final_data$Author.Position <- ordered(final_data$Author.Position, levels = c(0:4))
final_data$Journal_rank <- ordered(final_data$Journal_rank, levels = c(1:9))
final_data$Rank <- as.numeric(final_data$Rank)

psm_f<- matchit(Econ_Index ~ Journal_rank + Gender + Author.Position + Rank + Ethnicity + BA_to_PHD + PHD_to_Fac + Fac_to_decision,
                data = final_data, method='full', distance= 'logit',estimand='ATE')
summary(psm_f,un=FALSE)

matched_data <- match.data(psm_f)

#Gender+Ethnicity+Journal_rank+Author.Position+Rank
model<- glm(
  tenure ~ Econ_Index+Gender+Ethnicity+Journal_rank+Author.Position+Rank,
  data= matched_data,
  weights=weights,
  family= binomial
)



model<- glm(
  tenure ~ Econ_Index+Journal_rank + Gender + Author.Position + Rank + 
    Ethnicity + BA_to_PHD + PHD_to_Fac + Fac_to_decision,
  data    = matched_data,
  weights =weights,
  family  = binomial
)


summary(model)

logLik(model)
