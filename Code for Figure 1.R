
#load all_data_primary_analysis

############Mixed Effects Logistic Regression################
time_indicator<-rep(-1, nrow(individual_matched))
low_prevalence<-rep(-1, nrow(individual_matched))
group_indicator<-rep(-1, nrow(individual_matched))
prevalence<-rep(0, nrow(individual_matched))


individual_matched<-data.frame(individual_matched, time_indicator, prevalence, low_prevalence, group_indicator)

for (i in 1:nrow(individual_matched)){
  individual_matched$cluster_renumber[i]=paste(individual_matched$country_name[i], individual_matched$YEAR[i], individual_matched$CLUSTERNO[i])
}

Units_after_matching<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/Units_after_cardinality_matching.csv")

Units_after_matching$Country=as.character(Units_after_matching$Country)
individual_matched$country_name=as.character(individual_matched$country_name)

###########Check the column each time!!!!################
for (i in 1:nrow(individual_matched)){
  if (length(Units_after_matching[which(Units_after_matching$Country==individual_matched$country_name[i] & Units_after_matching$year_pre_dhs==individual_matched$YEAR[i] & Units_after_matching$cluster_1==individual_matched$CLUSTERNO[i]), 16])>0){
    individual_matched$prevalence[i]=Units_after_matching[which(Units_after_matching$Country==individual_matched$country_name[i] & Units_after_matching$year_pre_dhs==individual_matched$YEAR[i] & Units_after_matching$cluster_1==individual_matched$CLUSTERNO[i]), 16]
  }
  if (length(Units_after_matching[which(Units_after_matching$Country==individual_matched$country_name[i] & Units_after_matching$year_post_dhs==individual_matched$YEAR[i] & Units_after_matching$cluster_2==individual_matched$CLUSTERNO[i]), 17])>0){
    individual_matched$prevalence[i]=Units_after_matching[which(Units_after_matching$Country==individual_matched$country_name[i] & Units_after_matching$year_post_dhs==individual_matched$YEAR[i] & Units_after_matching$cluster_2==individual_matched$CLUSTERNO[i]), 17]
  }
  if (i%%1000==0){
    print(i)
  }
}

summary(individual_matched$prevalence)

#################Check Columns Every Time!!!###################

country_cluster_year_high_high<-Units_after_matching[which(Units_after_matching$high_high_or_high_low==0), c(3, 6, 7, 18, 19)]

country_cluster_year_high_low<-Units_after_matching[which(Units_after_matching$high_high_or_high_low==1), c(3, 6, 7, 18, 19)]

for (i in 1:nrow(individual_matched)){
  if (individual_matched$country_name[i]%in%country_cluster_year_high_high$Country & individual_matched$CLUSTERNO[i]%in%country_cluster_year_high_high$cluster_1 & individual_matched$YEAR[i]%in%country_cluster_year_high_high$year_pre_dhs){
    individual_matched$time_indicator[i]=0
    individual_matched$low_prevalence[i]=0
    individual_matched$group_indicator[i]=0
  }
  if (individual_matched$country_name[i]%in%country_cluster_year_high_high$Country & individual_matched$CLUSTERNO[i]%in%country_cluster_year_high_high$cluster_2 & individual_matched$YEAR[i]%in%country_cluster_year_high_high$year_post_dhs){
    individual_matched$time_indicator[i]=1
    individual_matched$low_prevalence[i]=0
    individual_matched$group_indicator[i]=0
  }
  if (individual_matched$country_name[i]%in%country_cluster_year_high_low$Country & individual_matched$CLUSTERNO[i]%in%country_cluster_year_high_low$cluster_1 & individual_matched$YEAR[i]%in%country_cluster_year_high_low$year_pre_dhs){
    individual_matched$time_indicator[i]=0
    individual_matched$low_prevalence[i]=0
    individual_matched$group_indicator[i]=1
  }
  if (individual_matched$country_name[i]%in%country_cluster_year_high_low$Country & individual_matched$CLUSTERNO[i]%in%country_cluster_year_high_low$cluster_2 & individual_matched$YEAR[i]%in%country_cluster_year_high_low$year_post_dhs){
    individual_matched$time_indicator[i]=1
    individual_matched$low_prevalence[i]=1
    individual_matched$group_indicator[i]=1
  }
  if (i%%500==0){
    print(i)
  }
}

sum(individual_matched$group_indicator==1)
sum(individual_matched$group_indicator==0)

table(individual_matched$BIRTHSZ)
table(individual_matched$low_birthweight)
table(individual_matched$KIDTWIN)

#Exclude multiple births!!!
individual_matched_without_multi_gestation<-individual_matched[which(individual_matched$KIDTWIN==10), ]
sum(individual_matched_without_multi_gestation$group_indicator==1)
sum(individual_matched_without_multi_gestation$group_indicator==1 & individual_matched_without_multi_gestation$low_birthweight==99)
sum(individual_matched$group_indicator==1)-sum(individual_matched_without_multi_gestation$group_indicator==1)

sum(individual_matched_without_multi_gestation$group_indicator==0)
sum(individual_matched$group_indicator==0)-sum(individual_matched_without_multi_gestation$group_indicator==0)
sum(individual_matched_without_multi_gestation$group_indicator==0 & individual_matched_without_multi_gestation$low_birthweight==99)
