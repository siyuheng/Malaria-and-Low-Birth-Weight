###load "all_data_primary_analysis

predict_ave<-rep(0, nrow(predict_bayes))
for (i in 1:length(predict_ave)){
  predict_ave[i]=mean(predict_bayes[i,])
}

individual_for_plot<-cbind(individual_matched_without_missing_size, predict_ave)

ID<-seq(1:nrow(country_cluster_year_high_high))
Country<-country_cluster_year_high_high$Country
Cluster<-country_cluster_year_high_high$cluster_1
Year<-country_cluster_year_high_high$year_pre_dhs
Low_Birthweight_Rate<-rep(0, nrow(country_cluster_year_high_high))
Period<-rep("Early Year", nrow(country_cluster_year_high_high))
high_high_early<-data.frame(ID, Country, Cluster, Year, Low_Birthweight_Rate, Period)

ID<-seq(1:nrow(country_cluster_year_high_high))
Country<-country_cluster_year_high_high$Country
Cluster<-country_cluster_year_high_high$cluster_2
Year<-country_cluster_year_high_high$year_post_dhs
Low_Birthweight_Rate<-rep(0, nrow(country_cluster_year_high_high))
Period<-rep("Late Year", nrow(country_cluster_year_high_high))
high_high_late<-data.frame(ID, Country, Cluster, Year, Low_Birthweight_Rate, Period)

ID<-seq(1:nrow(country_cluster_year_high_low))
Country<-country_cluster_year_high_low$Country
Cluster<-country_cluster_year_high_low$cluster_1
Year<-country_cluster_year_high_low$year_pre_dhs
Low_Birthweight_Rate<-rep(0, nrow(country_cluster_year_high_low))
Period<-rep("Early Year", nrow(country_cluster_year_high_low))
high_low_early<-data.frame(ID, Country, Cluster, Year, Low_Birthweight_Rate, Period)

ID<-seq(1:nrow(country_cluster_year_high_low))
Country<-country_cluster_year_high_low$Country
Cluster<-country_cluster_year_high_low$cluster_2
Year<-country_cluster_year_high_low$year_post_dhs
Low_Birthweight_Rate<-rep(0, nrow(country_cluster_year_high_low))
Period<-rep("Late Year", nrow(country_cluster_year_high_low))
high_low_late<-data.frame(ID, Country, Cluster, Year, Low_Birthweight_Rate, Period)


for (i in 1:nrow(high_high_early)){
  high_high_early$Low_Birthweight_Rate[i]=mean(individual_for_plot$predict_ave[which(individual_for_plot$country_name==high_high_early$Country[i] & individual_for_plot$CLUSTERNO==high_high_early$Cluster[i] & individual_for_plot$YEAR==high_high_early$Year[i])])
}

for (i in 1:nrow(high_high_late)){
  high_high_late$Low_Birthweight_Rate[i]=mean(individual_for_plot$predict_ave[which(individual_for_plot$country_name==high_high_late$Country[i] & individual_for_plot$CLUSTERNO==high_high_late$Cluster[i] & individual_for_plot$YEAR==high_high_late$Year[i])])
}

for (i in 1:nrow(high_low_early)){
  high_low_early$Low_Birthweight_Rate[i]=mean(individual_for_plot$predict_ave[which(individual_for_plot$country_name==high_low_early$Country[i] & individual_for_plot$CLUSTERNO==high_low_early$Cluster[i] & individual_for_plot$YEAR==high_low_early$Year[i])])
}

for (i in 1:nrow(high_low_late)){
  high_low_late$Low_Birthweight_Rate[i]=mean(individual_for_plot$predict_ave[which(individual_for_plot$country_name==high_low_late$Country[i] & individual_for_plot$CLUSTERNO==high_low_late$Cluster[i] & individual_for_plot$YEAR==high_low_late$Year[i])])
}

high_high<-rbind(high_high_early, high_high_late)
high_low<-rbind(high_low_early, high_low_late)



library(ggplot2)

ggplot(high_high, aes(Period, Low_Birthweight_Rate)) +
  geom_boxplot(width=0.3, size=1.5, fatten=1.5, colour="grey70") +
  geom_point(colour="red", size=2, alpha=0.5) +
  geom_line(aes(group=ID), colour="red", linetype="11") +
  theme_classic() + ylab("Low Birthweight Rate") + xlab("High-high Pairs of Clusters") + ylim(0, 0.52)

 ggplot(high_low, aes(Period, Low_Birthweight_Rate)) +
  geom_boxplot(width=0.3, size=1.5, fatten=1.5, colour="grey70") +
  geom_point(colour="blue", size=2, alpha=0.5) +
  geom_line(aes(group=ID), colour="blue", linetype="11") +
  theme_classic() + ylab("Low Birthweight Rate") + xlab("High-low Pairs of Clusters") + ylim(0, 0.52)



