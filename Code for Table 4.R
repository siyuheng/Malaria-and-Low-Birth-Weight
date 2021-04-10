library(geosphere)
Matched_pp<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/Units_after_cardinality_matching.csv")
HH_E_Long<-rep(0, nrow(Matched_pp)/2)
HH_E_Lat<-rep(0, nrow(Matched_pp)/2)
HH_L_Long<-rep(0, nrow(Matched_pp)/2)
HH_L_Lat<-rep(0, nrow(Matched_pp)/2)
HL_E_Long<-rep(0, nrow(Matched_pp)/2)
HL_E_Lat<-rep(0, nrow(Matched_pp)/2)
HL_L_Long<-rep(0, nrow(Matched_pp)/2)
HL_L_Lat<-rep(0, nrow(Matched_pp)/2)
HH_distance<-rep(0, nrow(Matched_pp)/2)
HL_distance<-rep(0, nrow(Matched_pp)/2)
for (i in 1:nrow(Matched_pp)/2){
  HL_E_Long[i]=Matched_pp$longitude_1[2*i-1]
  HL_E_Lat[i]=Matched_pp$latitude_1[2*i-1]
  HL_L_Long[i]=Matched_pp$longitude_2[2*i-1]
  HL_L_Lat[i]=Matched_pp$latitude_2[2*i-1]
  HH_E_Long[i]=Matched_pp$longitude_1[2*i]
  HH_E_Lat[i]=Matched_pp$latitude_1[2*i]
  HH_L_Long[i]=Matched_pp$longitude_2[2*i]
  HH_L_Lat[i]=Matched_pp$latitude_2[2*i]
}
HL_d1<-matrix(0, nrow = nrow(Matched_pp)/2, ncol = nrow(Matched_pp)/2)
HH_d1<-matrix(0, nrow = nrow(Matched_pp)/2, ncol = nrow(Matched_pp)/2)
HL_d1=distm(cbind(HL_E_Long, HL_E_Lat), cbind(HL_L_Long, HL_L_Lat), fun = distHaversine)
HH_d1=distm(cbind(HH_E_Long, HH_E_Lat), cbind(HH_L_Long, HH_L_Lat), fun = distHaversine)
for (i in 1:nrow(Matched_pp)/2){
  HL_distance[i]=HL_d1[i, i]
  HH_distance[i]=HH_d1[i, i]
}



options(digits = 10)


mean(HL_distance)
mean(HH_distance)

mean(HL_E_Long)
mean(HL_E_Lat)
mean(HL_L_Long)
mean(HL_L_Lat)
mean(HH_E_Long)
mean(HH_E_Lat)
mean(HH_L_Long)
mean(HH_L_Lat)

cor(HL_E_Long, HL_L_Long)
cor(HH_E_Long, HH_L_Long)
cor(HL_E_Lat, HL_L_Lat)
cor(HH_E_Lat, HH_L_Lat)


data_unmatched<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/step0_unmatched_data.csv")
HH_E_PE<-rep(0, nrow(Matched_pp)/2)
HH_E_PL<-rep(0, nrow(Matched_pp)/2)
HH_L_PE<-rep(0, nrow(Matched_pp)/2)
HH_L_PL<-rep(0, nrow(Matched_pp)/2)
HL_E_PE<-rep(0, nrow(Matched_pp)/2)
HL_E_PL<-rep(0, nrow(Matched_pp)/2)
HL_L_PE<-rep(0, nrow(Matched_pp)/2)
HL_L_PL<-rep(0, nrow(Matched_pp)/2)

Matched_pp$Country<-as.character(Matched_pp$Country)
data_unmatched$Country<-as.character(data_unmatched$Country)

#######Check column every time!!!############
year_col_2000=2000-which(colnames(data_unmatched)=="Prevalence_2000")
for (i in 1:(nrow(Matched_pp)/2)){
  if (Matched_pp$year1[2*i-1]<2000){
    year1=2000
  }
  else {
    year1=Matched_pp$year1[2*i-1]
  }
  if (Matched_pp$year2[2*i-1]>2015){
    year2=2015
  }
  else {
    year2=Matched_pp$year2[2*i-1]
  }
  if (Matched_pp$year1[2*i]<2000){
    year3=2000
  }
  else {
    year3=Matched_pp$year1[2*i]
  }
  if (Matched_pp$year2[2*i]>2015){
    year4=2015
  }
  else {
    year4=Matched_pp$year2[2*i]
  }
  HL_E_PE[i]=Matched_pp$prevalence_pre[2*i-1]
  HL_E_PL[i]=data_unmatched[which(data_unmatched$Longitude==Matched_pp$longitude_1[2*i-1] & data_unmatched$Latitude==Matched_pp$latitude_1[2*i-1]) & data_unmatched$Cluster==Matched_pp$cluster_1[2*i-1] & data_unmatched$Year_Obseved==Matched_pp$year1[2*i-1] & data_unmatched$Country==Matched_pp$Country[2*i-1], year2-year_col_2000]
  HL_L_PE[i]=data_unmatched[which(data_unmatched$Longitude==Matched_pp$longitude_2[2*i-1] & data_unmatched$Latitude==Matched_pp$latitude_2[2*i-1]) & data_unmatched$Cluster==Matched_pp$cluster_2[2*i-1] & data_unmatched$Year_Obseved==Matched_pp$year2[2*i-1] & data_unmatched$Country==Matched_pp$Country[2*i-1], year1-year_col_2000]
  HL_L_PL[i]=Matched_pp$prevalence_post[2*i-1]
  HH_E_PE[i]=Matched_pp$prevalence_pre[2*i]
  HH_E_PL[i]=data_unmatched[which(data_unmatched$Longitude==Matched_pp$longitude_1[2*i] & data_unmatched$Latitude==Matched_pp$latitude_1[2*i]) & data_unmatched$Cluster==Matched_pp$cluster_1[2*i] & data_unmatched$Year_Obseved==Matched_pp$year1[2*i] & data_unmatched$Country==Matched_pp$Country[2*i], year4-year_col_2000]
  HH_L_PE[i]=data_unmatched[which(data_unmatched$Longitude==Matched_pp$longitude_2[2*i] & data_unmatched$Latitude==Matched_pp$latitude_2[2*i]) & data_unmatched$Cluster==Matched_pp$cluster_2[2*i] & data_unmatched$Year_Obseved==Matched_pp$year2[2*i] & data_unmatched$Country==Matched_pp$Country[2*i], year3-year_col_2000]
  HH_L_PL[i]=Matched_pp$prevalence_post[2*i]
}
mean(HL_E_PE)
mean(HL_E_PL)
mean(HL_L_PE)
mean(HL_L_PL)
mean(HH_E_PE)
mean(HH_E_PL)
mean(HH_L_PE)
mean(HH_L_PL)

