#install.packages("arm")
library(arm)

#####Here we save "all_data_before_imputation"
p_0=0.5
p_1_value<-c(0.025, 0.05, 0.075, 0.1, 0.025, 0.05, 0.075, 0.1, 0.025, 0.05, 0.075, 0.1, 0.025, 0.05, 0.075, 0.1)
p_2_value<-c(-0.1, -0.1, -0.1, -0.1, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1, 0.1)
repeat_times=500
number_fixed_effects_coeffs=15

for (s in 1:length(p_1_value)){
  p_1=p_1_value[s]  
  p_2=p_2_value[s]  
  est_ptr<-matrix(0, nrow = number_fixed_effects_coeffs, ncol = repeat_times)
  est_std<-matrix(0, nrow = number_fixed_effects_coeffs, ncol = repeat_times)
  ## Set random seed for replicative purposes
  set.seed(12345)
  for (j in 1:repeat_times){
    predict_out<-predict_bayes[,j]
    U<-rep(0, length(predict_out))
    index_11<-which(individual_matched_without_missing_size$low_prevalence==1 & predict_out==1)
    index_10<-which(individual_matched_without_missing_size$low_prevalence==1 & predict_out==0)
    index_01<-which(individual_matched_without_missing_size$low_prevalence==0 & predict_out==1)
    index_00<-which(individual_matched_without_missing_size$low_prevalence==0 & predict_out==0)
    U[index_11]<-rbinom(n = length(index_11), size = 1, prob = p_0+p_1+p_2)
    U[index_10]<-rbinom(n = length(index_10), size = 1, prob = p_0+p_1)
    U[index_01]<-rbinom(n = length(index_01), size = 1, prob = p_0+p_2)
    U[index_00]<-rbinom(n = length(index_00), size = 1, prob = p_0)
    individual_matched_without_missing_size_multiple_imputation<-cbind(individual_matched_without_missing_size, U, prob_bayes, predict_out)
    mix_effect <- lmer(predict_out ~ low_prevalence + time_indicator + group_indicator + AGE + I(AGE^2) + KIDBORD + I(KIDBORD^2)+WEALTHQ+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE+U+(1 | DHSID), data = individual_matched_without_missing_size_multiple_imputation)
    modelSummary <- summary(mix_effect)  # capture model summary as an object
    est_ptr[,j] <- modelSummary$coefficients[,1] # model coefficients
    est_std[,j] <- modelSummary$coefficients[,2]
    mix_effect<-NULL
    modelSummary<-NULL
  }
  
  mix_effect <- lmer(predict_out ~ low_prevalence + time_indicator + group_indicator + AGE + I(AGE^2) + KIDBORD + I(KIDBORD^2)+WEALTHQ+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE + U + (1 | DHSID), data = individual_matched_without_missing_size_multiple_imputation)
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
  rownames(matrix_table)<-rownames(modelSummary$coefficients)
  colnames(matrix_table)<-c('average', 'between_var', 'within_var', 'total_var', 'ratio_var', 'degree_freedom', 't statistic', 'p value', 'lower CI', 'upper CI')
  
  if (s==1){
    matrix_table_1<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_1, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table1.csv")
  }
  if (s==2){
    matrix_table_2<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_2, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table2.csv")
  }
  if (s==3){
    matrix_table_3<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_3, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table3.csv")
  }
  if (s==4){
    matrix_table_4<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_4, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table4.csv")
  }
  if (s==5){
    matrix_table_5<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_5, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table5.csv")
  }
  if (s==6){
    matrix_table_6<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_6, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table6.csv")
  }
  if (s==7){
    matrix_table_7<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_7, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table7.csv")
  }
  if (s==8){
    matrix_table_8<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_8, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table8.csv")
  }
  if (s==9){
    matrix_table_9<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_9, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table9.csv")
  }
  if (s==10){
    matrix_table_10<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_10, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table10.csv")
  }
  if (s==11){
    matrix_table_11<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_11, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table11.csv")
  }
  if (s==12){
    matrix_table_12<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_12, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table12.csv")
  }
  if (s==13){
    matrix_table_13<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_13, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table13.csv")
  }
  if (s==14){
    matrix_table_14<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_14, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table14.csv")
  }
  if (s==15){
    matrix_table_15<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_15, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table15.csv")
  }
  if (s==16){
    matrix_table_16<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_16, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table16.csv")
  }
  print(s)
}

matrix_table_1
matrix_table_2
matrix_table_3
matrix_table_4
matrix_table_5
matrix_table_6
matrix_table_7
matrix_table_8
matrix_table_9
matrix_table_10
matrix_table_11
matrix_table_12
matrix_table_13
matrix_table_14
matrix_table_15
matrix_table_16


################################################################

#####Here we save "all_data_before_imputation"
p_0=0.5
p_1_value<-c(-0.025, -0.05, -0.075, -0.1, -0.025, -0.05, -0.075, -0.1, -0.025, -0.05, -0.075, -0.1, -0.025, -0.05, -0.075, -0.1)
p_2_value<-c(-0.1, -0.1, -0.1, -0.1, -0.05, -0.05, -0.05, -0.05, 0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1, 0.1)
repeat_times=500
number_fixed_effects_coeffs=15

for (s in 1:length(p_1_value)){
  p_1=p_1_value[s]  
  p_2=p_2_value[s]  
  est_ptr<-matrix(0, nrow = number_fixed_effects_coeffs, ncol = repeat_times)
  est_std<-matrix(0, nrow = number_fixed_effects_coeffs, ncol = repeat_times)
  ## Set random seed for replicative purposes
  set.seed(12345)
  for (j in 1:repeat_times){
    predict_out<-predict_bayes[,j]
    U<-rep(0, length(predict_out))
    index_11<-which(individual_matched_without_missing_size$low_prevalence==1 & predict_out==1)
    index_10<-which(individual_matched_without_missing_size$low_prevalence==1 & predict_out==0)
    index_01<-which(individual_matched_without_missing_size$low_prevalence==0 & predict_out==1)
    index_00<-which(individual_matched_without_missing_size$low_prevalence==0 & predict_out==0)
    U[index_11]<-rbinom(n = length(index_11), size = 1, prob = p_0+p_1+p_2)
    U[index_10]<-rbinom(n = length(index_10), size = 1, prob = p_0+p_1)
    U[index_01]<-rbinom(n = length(index_01), size = 1, prob = p_0+p_2)
    U[index_00]<-rbinom(n = length(index_00), size = 1, prob = p_0)
    individual_matched_without_missing_size_multiple_imputation<-cbind(individual_matched_without_missing_size, U, prob_bayes, predict_out)
    mix_effect <- lmer(predict_out ~ low_prevalence + time_indicator + group_indicator + AGE + I(AGE^2) + KIDBORD + I(KIDBORD^2)+WEALTHQ+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE+U+(1 | DHSID), data = individual_matched_without_missing_size_multiple_imputation)
    modelSummary <- summary(mix_effect)  # capture model summary as an object
    est_ptr[,j] <- modelSummary$coefficients[,1] # model coefficients
    est_std[,j] <- modelSummary$coefficients[,2]
    mix_effect<-NULL
    modelSummary<-NULL
  }
  
  mix_effect <- lmer(predict_out ~ low_prevalence + time_indicator + group_indicator + AGE + I(AGE^2) + KIDBORD + I(KIDBORD^2)+WEALTHQ+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE + U + (1 | DHSID), data = individual_matched_without_missing_size_multiple_imputation)
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
  rownames(matrix_table)<-rownames(modelSummary$coefficients)
  colnames(matrix_table)<-c('average', 'between_var', 'within_var', 'total_var', 'ratio_var', 'degree_freedom', 't statistic', 'p value', 'lower CI', 'upper CI')
  
  if (s==1){
    matrix_table_17<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_17, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table17.csv")
  }
  if (s==2){
    matrix_table_18<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_18, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table18.csv")
  }
  if (s==3){
    matrix_table_19<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_19, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table19.csv")
  }
  if (s==4){
    matrix_table_20<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_20, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table20.csv")
  }
  if (s==5){
    matrix_table_21<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_21, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table21.csv")
  }
  if (s==6){
    matrix_table_22<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_22, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table22.csv")
  }
  if (s==7){
    matrix_table_23<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_23, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table23.csv")
  }
  if (s==8){
    matrix_table_24<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_24, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table24.csv")
  }
  if (s==9){
    matrix_table_25<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_25, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table25.csv")
  }
  if (s==10){
    matrix_table_26<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_26, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table26.csv")
  }
  if (s==11){
    matrix_table_27<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_27, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table27.csv")
  }
  if (s==12){
    matrix_table_28<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_28, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table28.csv")
  }
  if (s==13){
    matrix_table_29<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_29, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table29.csv")
  }
  if (s==14){
    matrix_table_30<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_30, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table30.csv")
  }
  if (s==15){
    matrix_table_31<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_31, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table31.csv")
  }
  if (s==16){
    matrix_table_32<-matrix_table
    matrix_table<-NULL
    write.csv(matrix_table_32, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table32.csv")
  }
  print(s)
}


matrix_table_17
matrix_table_18
matrix_table_19
matrix_table_20
matrix_table_21
matrix_table_22
matrix_table_23
matrix_table_24
matrix_table_25
matrix_table_26
matrix_table_27
matrix_table_28
matrix_table_29
matrix_table_30
matrix_table_31
matrix_table_32

########################################################
p_0=0.5
p_1=0
p_2=0
repeat_times=500
number_fixed_effects_coeffs=15
est_ptr<-matrix(0, nrow = number_fixed_effects_coeffs, ncol = repeat_times)
est_std<-matrix(0, nrow = number_fixed_effects_coeffs, ncol = repeat_times)
## Set random seed for replicative purposes
set.seed(12345)
for (j in 1:repeat_times){
  predict_out<-predict_bayes[,j]
  U<-rep(0, length(predict_out))
  index_11<-which(individual_matched_without_missing_size$low_prevalence==1 & predict_out==1)
  index_10<-which(individual_matched_without_missing_size$low_prevalence==1 & predict_out==0)
  index_01<-which(individual_matched_without_missing_size$low_prevalence==0 & predict_out==1)
  index_00<-which(individual_matched_without_missing_size$low_prevalence==0 & predict_out==0)
  U[index_11]<-rbinom(n = length(index_11), size = 1, prob = p_0+p_1+p_2)
  U[index_10]<-rbinom(n = length(index_10), size = 1, prob = p_0+p_1)
  U[index_01]<-rbinom(n = length(index_01), size = 1, prob = p_0+p_2)
  U[index_00]<-rbinom(n = length(index_00), size = 1, prob = p_0)
  individual_matched_without_missing_size_multiple_imputation<-cbind(individual_matched_without_missing_size, U, prob_bayes, predict_out)
  mix_effect <- lmer(predict_out ~ low_prevalence + time_indicator + group_indicator + AGE + I(AGE^2) + KIDBORD + I(KIDBORD^2)+WEALTHQ+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE+U+(1 | DHSID), data = individual_matched_without_missing_size_multiple_imputation)
  modelSummary <- summary(mix_effect)  # capture model summary as an object
  est_ptr[,j] <- modelSummary$coefficients[,1] # model coefficients
  est_std[,j] <- modelSummary$coefficients[,2]
  mix_effect<-NULL
  modelSummary<-NULL
}

mix_effect <- lmer(predict_out ~ low_prevalence + time_indicator + group_indicator + AGE + I(AGE^2) + KIDBORD + I(KIDBORD^2)+WEALTHQ+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE + U + (1 | DHSID), data = individual_matched_without_missing_size_multiple_imputation)
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
rownames(matrix_table)<-rownames(modelSummary$coefficients)
colnames(matrix_table)<-c('average', 'between_var', 'within_var', 'total_var', 'ratio_var', 'degree_freedom', 't statistic', 'p value', 'lower CI', 'upper CI')

matrix_table_0<-matrix_table
matrix_table<-NULL
write.csv(matrix_table_0, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data Sensitivity Analysis/table0.csv")

