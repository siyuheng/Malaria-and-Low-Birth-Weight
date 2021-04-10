#####Here we save "all_data_before_imputation"

individual_matched_without_missing_size<-individual_matched_without_missing_size[which(individual_matched_without_missing_size$KIDCURAGE<=1),]

repeat_times=500
number_fixed_effects_coeffs=14

est_ptr<-matrix(0, nrow = number_fixed_effects_coeffs, ncol = repeat_times)
est_std<-matrix(0, nrow = number_fixed_effects_coeffs, ncol = repeat_times)
prop_lowweight<-rep(0, repeat_times)
predict_bayes<-matrix(0, nrow = nrow(individual_matched_without_missing_size), ncol = repeat_times)

## Set random seed for replication purposes
set.seed(12345)
for (j in 1:repeat_times){
  prob_bayes<-rep(0, nrow(individual_matched_without_missing_size))
  SIM<-NULL
  SIM<-sim(bayeslogit, n.sims=nrow(individual_matched_without_missing_size))
  COE<-NULL
  COE<-SIM@coef
  for (i in 1:nrow(individual_matched_without_missing_size)){
    if (individual_matched_without_missing_size$low_birthweight[i]==99){
      logit_tmp<-logit
      logit_tmp$coefficients<-COE[i,]
      prob_bayes[i]=predict(logit_tmp, newdata = individual_matched_without_missing_size[i,], type = 'response')
      predict_bayes[i, j]<-rbinom(1,1,prob_bayes[i])
    }
    else {
      predict_bayes[i, j]=individual_matched_without_missing_size$low_birthweight[i]
    }
  }
  predict_out<-predict_bayes[,j]
  prop_lowweight[j]=sum(predict_out)/nrow(individual_matched_without_missing_size)
  individual_matched_without_missing_size_multiple_imputation<-cbind(individual_matched_without_missing_size, prob_bayes, predict_out)
  mix_effect <- lmer(predict_out ~ low_prevalence + time_indicator + group_indicator + AGE + I(AGE^2) + KIDBORD + I(KIDBORD^2)+WEALTHQ+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE + (1 | DHSID), data = individual_matched_without_missing_size_multiple_imputation)
  modelSummary <- summary(mix_effect)  # capture model summary as an object
  est_ptr[,j] <- modelSummary$coefficients[,1] # model coefficients
  est_std[,j] <- modelSummary$coefficients[,2]
  mix_effect<-NULL
  modelSummary<-NULL
  print(j)
}


mix_effect <- lmer(predict_out ~ low_prevalence + time_indicator + group_indicator + AGE + I(AGE^2) + KIDBORD + I(KIDBORD^2)+WEALTHQ+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE + (1 | DHSID), data = individual_matched_without_missing_size_multiple_imputation)
modelSummary <- summary(mix_effect)

#1) The average parameter estimate and estimated variance across imputations
ave_ptr<-rep(0, number_fixed_effects_coeffs)
for (i in 1:number_fixed_effects_coeffs){
  ave_ptr[i]=mean(est_ptr[i,])
}

#2) The between imputation variance
between_var<-rep(0, number_fixed_effects_coeffs)
for (i in 1:number_fixed_effects_coeffs){
  between_var[i]=var(est_ptr[i,])
}

#3) The average within imputation variance
within_var<-rep(0, number_fixed_effects_coeffs)
for (i in 1:number_fixed_effects_coeffs){
  within_var[i]=mean(est_std[i,]^2)
}

#4) The total variance for that parameter estimate (the sum of between and within imputation variance)
total_var=(1+1/repeat_times)*between_var+within_var

#5) The relative increase in variance due to missing data (which is the ratio of the between and within imputation variance)
ratio_var=((1+1/repeat_times)*between_var)/within_var

#6) The t-statistic / p-value based on the above
degree_freedom=(repeat_times-1)*(1+1/ratio_var)^2
t_stat<-rep(0, length(ave_ptr))
p_value<-rep(0, length(ave_ptr))
for (i in 1:length(ave_ptr)){
  t_stat[i]=ave_ptr[i]/sqrt(total_var[i])
  p_value[i]=1-pt(abs(t_stat[i]), df=degree_freedom[i])+pt(-abs(t_stat[i]), df=degree_freedom[i])
}

#7) Calculate the upper and lower ends of 95% confidence interval 
z_score = - qt(.025, degree_freedom)
UCB = ave_ptr + z_score * sqrt(total_var)
LCB = ave_ptr - z_score * sqrt(total_var)

## Get all the calculated into a table
matrix_table<-cbind(ave_ptr, between_var, within_var, total_var, ratio_var, degree_freedom, t_stat, p_value, LCB, UCB)

## Name the rows of our result

modelSummary$coeffi

rownames(matrix_table)<-rownames(modelSummary$coefficients)
colnames(matrix_table)<-c('average', 'between_var', 'within_var', 'total_var', 'ratio_var', 'degree_freedom', 't statistic', 'p value', 'lower CI', 'upper CI')

## Output the result
options(digits = 3)
## See the outcome
matrix_table
sum(individual_matched_without_missing_size$low_birthweight ==1 )/sum(individual_matched_without_missing_size$low_birthweight!=99)
prop_lowweight

## Output the data
write.csv(matrix_table, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data SA1/step3_Inference_with_mutiple_imputation.csv")

##We save data as "all_data_SA1"
