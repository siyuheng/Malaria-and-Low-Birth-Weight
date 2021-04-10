#######################################################
## Step 0: Clean data and generate original structure:
##
## - In this step, we extract prevalence information 
## from images, and attach the prevalence to clusters
## via geographical matching. 
#######################################################

#You will also need the following R packages:

#install R packages
install.packages("raster")
install.packages("sp")
install.packages("rgdal")
install.packages("maptools")
install.packages("lattice")
install.packages("ggplot2")
install.packages("spatstat")
install.packages("geostatsp")
install.packages("geosphere")
install.packages("optmatch")

# Load R libraries
library(raster)
library(sp)
library(rgdal)
library(maptools)
library(lattice)
library(ggplot2)
library(spatstat)
library(geostatsp)
library(geosphere)

################################################

# Import all DHS cluster GPS coordinates
# The GPS coordinates are stored in .shp files, with a different folder
# for each DHS country/year. 
# e.g. Kenya's 2003 cluster data is in a folder called "KEGE7AFL Folder"
#      the .shp file in this folder is called "KEGE7AFL.shp"
# We have 109 of these folders. This code will loop through all of 
# these folders and import the .shp file within each one, and combine
# them into one big data frame of GPS coordinates. 
all.folders <- dir("/Users/heng6/Desktop/Malaria/GPS", pattern="Folder")
shp.files <- file.path(paste0(
  "/Users/heng6/Desktop/Malaria/GPS",
  "/",
  all.folders, 
  "/",
  unlist(lapply(strsplit(all.folders, split = '\ '), '[[', 1)),
  ".shp"
))
all.spdf <- lapply(shp.files, readOGR)
gps.spdf <- do.call(rbind, all.spdf)


head(gps.spdf)   ##Information of DHS ID, Country, DHS YEAR (GPS year, could be different from IPUMS-DHS), cluster number, Urban_rural, latnum, longnum

## All information we need from gps.spdf
DHSID<-gps.spdf$DHSID
Cluster<-gps.spdf$DHSCLUST
Country<-gps.spdf$DHSCC
Latitude<-gps.spdf$LATNUM
Longitude<-gps.spdf$LONGNUM
Urban_or_Rural<-gps.spdf$URBAN_RURA
Year_Obseved<-gps.spdf$DHSYEAR

Table<-data.frame(Cluster, Country, Year_Obseved, Urban_or_Rural, Longitude, Latitude)

Table

data_2000<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2000.PR.rmean.tif")

data_2001<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2001.PR.rmean.tif")

data_2002<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2002.PR.rmean.tif")

data_2003<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2003.PR.rmean.tif")

data_2004<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2004.PR.rmean.tif")

data_2005<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2005.PR.rmean.tif")

data_2006<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2006.PR.rmean.tif")

data_2007<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2007.PR.rmean.tif")

data_2008<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2008.PR.rmean.tif")

data_2009<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2009.PR.rmean.tif")

data_2010<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2010.PR.rmean.tif")

data_2011<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2011.PR.rmean.tif")

data_2012<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2012.PR.rmean.tif")

data_2013<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2013.PR.rmean.tif")

data_2014<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2014.PR.rmean.tif")

data_2015<-raster("/Users/heng6/Desktop/Malaria/pixel-level_realization_summaries/MODEL43.2015.PR.rmean.tif")

Prevalence_2000<-rep(0, length(gps.spdf))

Prevalence_2001<-rep(0, length(gps.spdf))

Prevalence_2002<-rep(0, length(gps.spdf))

Prevalence_2003<-rep(0, length(gps.spdf))

Prevalence_2004<-rep(0, length(gps.spdf))

Prevalence_2005<-rep(0, length(gps.spdf))

Prevalence_2006<-rep(0, length(gps.spdf))

Prevalence_2007<-rep(0, length(gps.spdf))

Prevalence_2008<-rep(0, length(gps.spdf))

Prevalence_2009<-rep(0, length(gps.spdf))

Prevalence_2010<-rep(0, length(gps.spdf))

Prevalence_2011<-rep(0, length(gps.spdf))

Prevalence_2012<-rep(0, length(gps.spdf))

Prevalence_2013<-rep(0, length(gps.spdf))

Prevalence_2014<-rep(0, length(gps.spdf))

Prevalence_2015<-rep(0, length(gps.spdf))

Data_unmatched<-data.frame(DHSID, Cluster, Country, Year_Obseved, Urban_or_Rural, Longitude, Latitude, Prevalence_2000, Prevalence_2001, Prevalence_2002, Prevalence_2003, Prevalence_2004, Prevalence_2005, Prevalence_2006, Prevalence_2007, Prevalence_2008, Prevalence_2009, Prevalence_2010, Prevalence_2011, Prevalence_2012, Prevalence_2013, Prevalence_2014, Prevalence_2015)

Data_unmatched$Prevalence_2000<-extract(data_2000, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2001<-extract(data_2001, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2002<-extract(data_2002, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2003<-extract(data_2003, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2004<-extract(data_2004, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2005<-extract(data_2005, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2006<-extract(data_2006, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2007<-extract(data_2007, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2008<-extract(data_2008, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2009<-extract(data_2009, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2010<-extract(data_2010, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2011<-extract(data_2011, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2012<-extract(data_2012, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2013<-extract(data_2013, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2014<-extract(data_2014, y=cbind(Table$Longitude, Table$Latitude), method='simple')

Data_unmatched$Prevalence_2015<-extract(data_2015, y=cbind(Table$Longitude, Table$Latitude), method='simple')

write.csv(Data_unmatched,file="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/step0_unmatched_data.csv")


#######################################################
## Step 1: - Match the Unmatched early and late years 
##           clusters using their GPS location 
##
## - We also adjust for the fact that GPS data may 
## have different year with IPUMS-DHS data, but their
## DHSID is the same, so we store dhs_year in output
#######################################################

library(optmatch)
library(MASS)

## The clusters with prevalence (output in step 0)
unmatched_data<-read.csv('/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/step0_unmatched_data.csv')


####We record these clsuters that constantly has prevalence zero
index_zero=which(unmatched_data$Prevalence_2000==0
                                      & unmatched_data$Prevalence_2001==0
                                      & unmatched_data$Prevalence_2002==0
                                      & unmatched_data$Prevalence_2003==0
                                      & unmatched_data$Prevalence_2004==0
                                      & unmatched_data$Prevalence_2005==0
                                      & unmatched_data$Prevalence_2006==0
                                      & unmatched_data$Prevalence_2007==0
                                      & unmatched_data$Prevalence_2008==0
                                      & unmatched_data$Prevalence_2009==0
                                      & unmatched_data$Prevalence_2010==0
                                      & unmatched_data$Prevalence_2011==0
                                      & unmatched_data$Prevalence_2012==0
                                      & unmatched_data$Prevalence_2013==0
                                      & unmatched_data$Prevalence_2014==0
                                      & unmatched_data$Prevalence_2015==0)

#####
##### Match early and late years' clusters:
#####

### Function for rank based Mahalanobis distance. 
# It will prevent outliers from inflating the variance for a variable too much,
# thereby decreasing its importance, and be more robust.
# Also, the variances are not permitted to decrease as ties 
# become more common, so that, for example, it is not more important
# to match on a rare binary variable than on a common binary variable.
# Notation: z is a vector, length(z)=n, with 1 for treated, 0 for control
# X is a matrix with n rows containing variables in the distance
smahal=
  function(z,X){
    X<-as.matrix(X)
    n<-dim(X)[1]
    rownames(X)<-1:n
    k<-dim(X)[2]
    m<-sum(z)
    for (j in 1:k) X[,j]<-rank(X[,j])
    cv<-cov(X)
    vuntied<-var(1:n)
    rat<-sqrt(vuntied/diag(cv))
    cv<-diag(rat)%*%cv%*%diag(rat)
    out<-matrix(NA,m,n-m)
    Xc<-X[z==0,]
    Xt<-X[z==1,]
    rownames(out)<-rownames(X)[z==1]
    colnames(out)<-rownames(X)[z==0]
    library(MASS)
    icov<-ginv(cv)
    for (i in 1:m) out[i,]<-mahalanobis(Xc,Xt[i,],icov,inverted=T)
    out
  }

# Function for adding a propensity score caliper to a distance matrix dmat
# calipersd is the caliper in terms of standard deviation of the logit propensity scoe
addcaliper=function(dmat,z,logitp,calipersd=.2,penalty=1000){
  sd.logitp=sd(logitp)
  adif=abs(outer(logitp[z==1],logitp[z==0],"-"))
  adif=(adif-(calipersd*sd.logitp))*(adif>(calipersd*sd.logitp))
  dmat=dmat+adif*penalty
  dmat
}

### Function that matching "early" and "late" clusters within the same country to a pair
determine_the_unit<-function(COUNTRY, YEAR_ONE, YEAR_TWO){
  
  country<-COUNTRY
  year_1=YEAR_ONE
  year_2=YEAR_TWO
  data_sub_1<-unmatched_data[which(unmatched_data$Country==country & unmatched_data$Year_Obseved==year_1),] #Note that input year is GPS year (Year_Obseved)
  data_sub_2<-unmatched_data[which(unmatched_data$Country==country & unmatched_data$Year_Obseved==year_2),]
  
  
  if (min(nrow(data_sub_1),nrow(data_sub_2))==0){
    return(NULL)
  } else {
    
    treated_control_index<-rep(0, nrow(data_sub_1)+nrow(data_sub_2))
    nrow_min=min(nrow(data_sub_1),nrow(data_sub_2))
    for (i in 1:nrow_min){
      treated_control_index[i]=1
    }
    
    if (nrow(data_sub_1)<nrow(data_sub_2)){
      data_sub<-rbind(data_sub_1, data_sub_2)
    } else {
      data_sub<-rbind(data_sub_2, data_sub_1)
    }
    
    data_sub=cbind(treated_control_index,data_sub)
    
    propscore.model=glm(treated_control_index ~ Longitude + Latitude, family=binomial,x=TRUE,y=TRUE, data=data_sub)
    treated=propscore.model$y
    
    # Matrix of covariates, excluding intercept
    Xmat=propscore.model$x[,-1]
    # Rank based Mahalanobis distance
    distmat=smahal(treated,Xmat)
    # Add caliper
    logit.propscore=predict(propscore.model)
    distmat2=addcaliper(distmat,treated,logit.propscore)
    
    ### Create a subject index and name the rows and columns of distance matrix by ### this subject index
    subject.index=seq(1,length(treated),1)
    rownames(distmat2)=subject.index[treated==1]
    colnames(distmat2)=subject.index[treated==0]
    
    # Pair Matching
    matchvec=pairmatch(distmat2)
    # Note: Can ignore warning message from matching
    
    # Create vectors of the subject indices of the treatment units ordered by
    # their matched set and corresponding control unit
    treated.subject.index=rep(0,sum(treated==1))
    matched.control.subject.index=rep(0,length(treated.subject.index))
    matchedset.index=substr(matchvec,start=3,stop=10)
    matchedset.index.numeric=as.numeric(matchedset.index)
    subjects.match.order=as.numeric(names(matchvec)) # The subject indices in 
    # the order of matchvec
    for(i in 1:length(treated.subject.index)){
      matched.set.temp=which(matchedset.index.numeric==i)
      matched.set.temp.indices=subjects.match.order[matched.set.temp]
      if(treated[matched.set.temp.indices[1]]==1){
        treated.subject.index[i]=matched.set.temp.indices[1]
        matched.control.subject.index[i]=matched.set.temp.indices[2]
      }
      if(treated[matched.set.temp.indices[2]]==1){
        treated.subject.index[i]=matched.set.temp.indices[2]
        matched.control.subject.index[i]=matched.set.temp.indices[1]
      }
    }
    
    data_matched<-rbind(data_sub[treated.subject.index[1],], data_sub[matched.control.subject.index[1],])
    
    for (i in 2:nrow_min){
      data_tmp<-rbind(data_sub[treated.subject.index[i],], data_sub[matched.control.subject.index[i],])
      data_matched<-rbind(data_matched, data_tmp)
    }
    
    num_col_2000<-2000-which(colnames(data_matched)=="Prevalence_2000")
    print(num_col_2000)
    
    index_1<-rep(0, nrow_min)
    index_2<-rep(0, nrow_min)
    cluster_1<-rep(0, nrow_min)
    cluster_2<-rep(0, nrow_min)
    year1<-rep(0, nrow_min) #This corresponds to GPS year
    year2<-rep(0, nrow_min)
    longitude_1<-rep(0, nrow_min)
    longitude_2<-rep(0, nrow_min)
    latitude_1<-rep(0, nrow_min)
    latitude_2<-rep(0, nrow_min)
    urban_or_rural_1<-rep(0, nrow_min)
    urban_or_rural_2<-rep(0, nrow_min)
    prevalence_pre<-rep(0, nrow_min)
    prevalence_post<-rep(0, nrow_min)
    
    if (data_matched$Year_Obseved[1]<data_matched$Year_Obseved[2]){ #That is, if the early year is treated (has less clusters)
      for (i in 1:nrow_min){
        index_1[i]=data_matched$X[2*i-1]
        cluster_1[i]=data_matched$Cluster[2*i-1]
        year1[i]=data_matched$Year_Obseved[2*i-1]
        longitude_1[i]=data_matched$Longitude[2*i-1]
        latitude_1[i]=data_matched$Latitude[2*i-1]
        if (data_matched$Urban_or_Rural[2*i-1]=='U'){
          urban_or_rural_1[i]=1
        }
        index_2[i]=data_matched$X[2*i]
        cluster_2[i]=data_matched$Cluster[2*i]
        year2[i]=data_matched$Year_Obseved[2*i]
        longitude_2[i]=data_matched$Longitude[2*i]
        latitude_2[i]=data_matched$Latitude[2*i]
        if (data_matched$Urban_or_Rural[2*i]=='U'){
          urban_or_rural_2[i]=1
        }
        if (data_matched$Country[i]=='CI'){
          prevalence_pre[i]=data_matched[2*i-1, year_1+2-num_col_2000] ##############Check every time###############
          prevalence_post[i]=data_matched[2*i, year_2-num_col_2000]
        }
        else if (data_matched$Country[i]=='TZ'){
          prevalence_pre[i]=data_matched[2*i-1, year_1+1-num_col_2000]
          prevalence_post[i]=data_matched[2*i, year_2-num_col_2000]
        }
        else {
          prevalence_pre[i]=data_matched[2*i-1, year_1-num_col_2000]
          prevalence_post[i]=data_matched[2*i, year_2-num_col_2000]
        }
      }
    } else {
      for (i in 1:nrow_min){
        index_1[i]=data_matched$X[2*i]
        cluster_1[i]=data_matched$Cluster[2*i]
        year1[i]=data_matched$Year_Obseved[2*i]
        longitude_1[i]=data_matched$Longitude[2*i]
        latitude_1[i]=data_matched$Latitude[2*i]
        if (data_matched$Urban_or_Rural[2*i]=='U'){
          urban_or_rural_1[i]=1
        }
        index_2[i]=data_matched$X[2*i-1]
        cluster_2[i]=data_matched$Cluster[2*i-1]
        year2[i]=data_matched$Year_Obseved[2*i-1]
        longitude_2[i]=data_matched$Longitude[2*i-1]
        latitude_2[i]=data_matched$Latitude[2*i-1]
        if (data_matched$Urban_or_Rural[2*i-1]=='U'){
          urban_or_rural_2[i]=1
        }
        if (data_matched$Country[i]=='CI'){
          prevalence_pre[i]=data_matched[2*i, year_1+2-num_col_2000]
          prevalence_post[i]=data_matched[2*i-1, year_2-num_col_2000]
        }
        else if (data_matched$Country[i]=='TZ'){
          prevalence_pre[i]=data_matched[2*i, year_1+1-num_col_2000]
          prevalence_post[i]=data_matched[2*i-1, year_2-num_col_2000]
        }
        else {
          prevalence_pre[i]=data_matched[2*i, year_1-num_col_2000]
          prevalence_post[i]=data_matched[2*i-1, year_2-num_col_2000]
        }
      }
    }
    
    Country<-rep(COUNTRY, nrow_min)
    
    unit_by_location<-data.frame(Country, index_1, index_2, cluster_1, cluster_2, year1, year2, longitude_1, latitude_1, longitude_2, latitude_2, urban_or_rural_1, urban_or_rural_2, prevalence_pre, prevalence_post)
    
    return(unit_by_location)
    
  }
  
}

## The selected country with the corresponding early and late GPS years (see paper)
##These years correspond to Year_Obseved (GPS year)
country_name<-c('BJ', 'BF', 'CM', 'CD', 'CI', 'ET', 'GH', 'GN', 'KE', 'MW', 'ML', 'NM', 'NG', 'RW', 'SN', 'TZ', 'UG', 'ZM', 'ZW')
year_pre<-c(2001, 2003, 2004, 2007, 1998, 2000, 2003, 2005, 2003, 2000, 2001, 2000, 2003, 2005, 2005, 1999, 2000, 2007, 2005)
year_post<-c(2012, 2010, 2011, 2013, 2012, 2010, 2014, 2012, 2014, 2010, 2012, 2013, 2013, 2014, 2010, 2015, 2011, 2013, 2015)


## Get the matched units
Units_by_locations<-NULL
for (i in 1:length(year_pre)){
  unit_temp<-determine_the_unit(country_name[i], year_pre[i], year_post[i])
  Units_by_locations<-rbind(Units_by_locations, unit_temp)
}

table(Units_by_locations$Country)

## Determine among the matched pairs, what are the high-high pairs and high-low pairs
Units_for_matching_high_high<-Units_by_locations[which(Units_by_locations$prevalence_pre> 0.4 & Units_by_locations$prevalence_post > 0.4 & abs(Units_by_locations$prevalence_pre- Units_by_locations$prevalence_post)<0.1 ), ]
Units_for_matching_high_low<-Units_by_locations[which(Units_by_locations$prevalence_pre> 0.4 & Units_by_locations$prevalence_post < 0.2), ]

####We discard any clusters with constantly zero prevalence
Units_for_matching_high_low<-Units_for_matching_high_low[-which(Units_for_matching_high_low$index_2 %in% index_zero),]

## give the group indicator of whether the pair is high-high (0), or high-low (1)

high_high_or_high_low<-rep(0, nrow(Units_for_matching_high_high))
Units_for_matching_high_high<-cbind(high_high_or_high_low, Units_for_matching_high_high)
high_high_or_high_low<-rep(1, nrow(Units_for_matching_high_low))
Units_for_matching_high_low<-cbind(high_high_or_high_low, Units_for_matching_high_low)

## combine all the h-h and h-l
Units_for_matching<-rbind(Units_for_matching_high_low, Units_for_matching_high_high)

## Generate the IMPUS-DHS years
year_pre_dhs<-Units_for_matching$year1
year_post_dhs<-Units_for_matching$year2
Units_for_matching<-data.frame(Units_for_matching, year_pre_dhs, year_post_dhs)

## Take account for the difference in IPUMS-DHS with GPS years (see paper) Check on IPUMS-DHS and the overleaf
##This corresponds to IPUMS-DHS year
for (i in 1:nrow(Units_for_matching)){
  if (Units_for_matching$Country[i]=='BJ'){
    Units_for_matching$year_post_dhs[i]=2011
  }
  if (Units_for_matching$Country[i]=='CI'){
    Units_for_matching$year_post_dhs[i]=2011  
  }
  if (Units_for_matching$Country[i]=='ET'){
    Units_for_matching$year_post_dhs[i]=2011
  }
  if (Units_for_matching$Country[i]=='UG'){
    Units_for_matching$year_pre_dhs[i]=2001
  }
}

write.csv(Units_for_matching, '/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/step1_Units_for_card_match.csv')

#######################################################
## Step 2: - Categorize the household covariates &
##         - Match the covariates to clusters via DHSID
##         - Identify the high-high and high-low pairs
##         - Match each proper high-high and high-low
##         
## - In this step, we get the Household level data from 
## IPUMS-DHS, and matching the data with the output 
## in Step 1, the "Units_for_matching", using DHSID - 
## the unique code for identifying the same cluster.
##
## - Before matching, we also categorize the covariates
## in the data so as to better structured it for our 
## purpose.
## 
## - Prevalence > 0.4 or < 0.2 are identify to be High
## prevalence or low prevalence
##
## - Match h-h to h-l via optimal cardinality matching
## with respect to the cluster level averaged covariate
## value, so as to control confounders
#######################################################

#We then calculate the averages of each covariate
toilet_pre<-rep(0, nrow(Units_for_matching))
floor_pre<-rep(0, nrow(Units_for_matching))
electricity_pre<-rep(0, nrow(Units_for_matching))
mother_edu_pre<-rep(0, nrow(Units_for_matching))
family_plan_prop_pre<-rep(0, nrow(Units_for_matching))
toilet_post<-rep(0, nrow(Units_for_matching))
floor_post<-rep(0, nrow(Units_for_matching))
electricity_post<-rep(0, nrow(Units_for_matching))
mother_edu_post<-rep(0, nrow(Units_for_matching))
family_plan_prop_post<-rep(0, nrow(Units_for_matching))

Units_for_matching<-data.frame(Units_for_matching, toilet_pre, toilet_post, floor_pre, floor_post, electricity_pre, electricity_post, mother_edu_pre, mother_edu_post, family_plan_prop_pre, family_plan_prop_post)


########We categorize each covariate##########################

covariate<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Input Data/idhs_00045.csv.gz") #Household members level data from IPUMS-DHS


table(covariate$FLOOR)
table(covariate$TOILETTYPE)
table(covariate$ELECTRCHH)

sum(is.na(covariate$FLOOR))  # zero
sum(is.na(covariate$TOILETTYPE))  # zero
sum(is.na(covariate$ELECTRCHH))  #zero

## FLOOR: Household floor material
## Our levels:
## 1 - Natural or earth base
## 2 - Rudimentary
## 3 - Finished
## 99 - Missing or other 
covariate$FLOOR[covariate$FLOOR<200]=1
covariate$FLOOR[covariate$FLOOR>=200 & covariate$FLOOR<300]=2
covariate$FLOOR[covariate$FLOOR>=300 & covariate$FLOOR<400]=3

covariate$TOILETTYPE[covariate$TOILETTYPE>0 & covariate$TOILETTYPE<6000]=1

table(covariate$FLOOR)
table(covariate$TOILETTYPE)
table(covariate$ELECTRCHH)

sum(covariate$FLOOR>3)/sum(!is.na(covariate$FLOOR))  # 0.002
sum(covariate$TOILETTYPE>1)/sum(!is.na(covariate$TOILETTYPE))  # 0.0008
sum(covariate$ELECTRCHH>1)/sum(!is.na(covariate$ELECTRCHH))   # 0.001

covariate_edu<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Input Data/idhs_00046.csv.gz") #Births level data from IPUMS-DHS


table(covariate_edu$EDUCLVL)
sum(is.na(covariate_edu$EDUCLVL)) # zero

covariate_edu$EDUCLVL[covariate_edu$EDUCLVL==3]=2
table(covariate_edu$EDUCLVL)
sum(covariate_edu$EDUCLVL>2)/sum(!is.na(covariate_edu$EDUCLVL))   # 0.00003

covariate_fp<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Input Data/idhs_00048.csv.gz") #Births level data from IPUMS-DHS

sum(is.na(covariate_fp$KIDDESIRE)) #zero
sum(is.na(covariate_fp$FPTYPNOW)) # zero

table(covariate_fp$KIDDESIRE) 
table(covariate_fp$FPTYPNOW) # No missing or unspecified data 

covariate_fp$FPTYPNOW[covariate_fp$FPTYPNOW<20]=0
covariate_fp$FPTYPNOW[covariate_fp$FPTYPNOW==20]=1
table(covariate_fp$FPTYPNOW) # No missing or unspecified data 


write.csv(covariate,file="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/covariate_household.csv")
write.csv(covariate_edu, file = "/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/covariate_births_edu.csv")
write.csv(covariate_fp, file = "/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/covariate_births_fp.csv")


############run categorize and then run this###########

covariate_household<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/covariate_household.csv")
covariate_births_edu<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/covariate_births_edu.csv")
covariate_births_fp<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/covariate_births_fp.csv")
covariate_children<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Input Data/idhs_00049.csv.gz") #Children level data from IPUMS-DHS

country_name<-rep('nation', nrow(covariate_household))
country_name<-as.character(substr(covariate_household$DHSID, 1, 2))
covariate_household<-data.frame(country_name, covariate_household)

country_name<-rep('nation', nrow(covariate_births_edu))
country_name<-as.character(substr(covariate_births_edu$DHSID, 1, 2))
covariate_births_edu<-data.frame(country_name, covariate_births_edu)

country_name<-rep('nation', nrow(covariate_births_fp))
country_name<-as.character(substr(covariate_births_fp$DHSID, 1, 2))
covariate_births_fp<-data.frame(country_name, covariate_births_fp)

country_name<-rep('nation', nrow(covariate_children))
country_name<-as.character(substr(covariate_children$DHSID, 1, 2))
covariate_children<-data.frame(country_name, covariate_children)


table(covariate_household$TOILETTYPE)
table(covariate_household$FLOOR)
table(covariate_household$ELECTRCHH)
table(covariate_births_edu$EDUCLVL)
table(covariate_births_fp$FPTYPNOW)


#Note: Check the columns every time! Check after the above step! Note that we did not match on roof since there are many missing records.


for (i in 1:nrow(Units_for_matching)){
  country_temp=as.character(Units_for_matching$Country[i])
  Units_for_matching$toilet_pre[i]=mean(covariate_household[which(covariate_household$country_name==country_temp & covariate_household$YEAR==Units_for_matching$year_pre_dhs[i] & covariate_household$CLUSTERNO==Units_for_matching$cluster_1[i] & covariate_household$TOILETTYPE!=9998 & !is.na(covariate_household$TOILETTYPE)), 19])
  Units_for_matching$toilet_post[i]=mean(covariate_household[which(covariate_household$country_name==country_temp & covariate_household$YEAR==Units_for_matching$year_post_dhs[i] & covariate_household$CLUSTERNO==Units_for_matching$cluster_2[i] & covariate_household$TOILETTYPE!=9998 & !is.na(covariate_household$TOILETTYPE)), 19])
  Units_for_matching$floor_pre[i]=mean(covariate_household[which(covariate_household$country_name==country_temp & covariate_household$YEAR==Units_for_matching$year_pre_dhs[i] & covariate_household$CLUSTERNO==Units_for_matching$cluster_1[i] & covariate_household$FLOOR!=400 & covariate_household$FLOOR!=998 & !is.na(covariate_household$FLOOR)), 16])
  Units_for_matching$floor_post[i]=mean(covariate_household[which(covariate_household$country_name==country_temp & covariate_household$YEAR==Units_for_matching$year_post_dhs[i] & covariate_household$CLUSTERNO==Units_for_matching$cluster_2[i] & covariate_household$FLOOR!=400 & covariate_household$FLOOR!=998 & !is.na(covariate_household$FLOOR)), 16])
  Units_for_matching$electricity_pre[i]=mean(covariate_household[which(covariate_household$country_name==country_temp & covariate_household$YEAR==Units_for_matching$year_pre_dhs[i] & covariate_household$CLUSTERNO==Units_for_matching$cluster_1[i] & covariate_household$ELECTRCHH!=6 & covariate_household$ELECTRCHH!=8 & !is.na(covariate_household$ELECTRCHH)), 18] )
  Units_for_matching$electricity_post[i]=mean(covariate_household[which(covariate_household$country_name==country_temp & covariate_household$YEAR==Units_for_matching$year_post_dhs[i] & covariate_household$CLUSTERNO==Units_for_matching$cluster_2[i] & covariate_household$ELECTRCHH!=6 & covariate_household$ELECTRCHH!=8 & !is.na(covariate_household$ELECTRCHH)), 18] )
  Units_for_matching$mother_edu_pre[i]=mean(covariate_births_edu[which(covariate_births_edu$country_name==country_temp & covariate_births_edu$YEAR==Units_for_matching$year_pre_dhs[i] & covariate_births_edu$CLUSTERNO==Units_for_matching$cluster_1[i] & covariate_births_edu$EDUCLVL!=8 & !is.na(covariate_births_edu$EDUCLVL)), 33] )
  Units_for_matching$mother_edu_post[i]=mean(covariate_births_edu[which(covariate_births_edu$country_name==country_temp & covariate_births_edu$YEAR==Units_for_matching$year_post_dhs[i] & covariate_births_edu$CLUSTERNO==Units_for_matching$cluster_2[i] & covariate_births_edu$EDUCLVL!=8 & !is.na(covariate_births_edu$EDUCLVL)), 33] )
  Units_for_matching$family_plan_prop_pre[i]=mean(covariate_births_fp[which(covariate_births_fp$country_name==country_temp & covariate_births_fp$YEAR==Units_for_matching$year_pre_dhs[i] & covariate_births_fp$CLUSTERNO==Units_for_matching$cluster_1[i]), 36])
  Units_for_matching$family_plan_prop_post[i]=mean(covariate_births_fp[which(covariate_births_fp$country_name==country_temp & covariate_births_fp$YEAR==Units_for_matching$year_post_dhs[i] & covariate_births_fp$CLUSTERNO==Units_for_matching$cluster_2[i]), 36])
  if (i%%20==0){
    print(i)
  }
}


Units_for_matching_copy<-Units_for_matching

sum(is.na(Units_for_matching)) # zero 

write.csv(Units_for_matching, file = "/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/Units_before_cardinality_matching.csv") 


#####
##### Select high-low and high-high pairs and match via optimal cardinality matching
#####

library(designmatch)

t_ind=Units_for_matching$high_high_or_high_low

mom_covs=cbind(Units_for_matching$urban_or_rural_1, Units_for_matching$urban_or_rural_2, 
               Units_for_matching$toilet_pre, Units_for_matching$toilet_post,
               Units_for_matching$floor_pre, Units_for_matching$floor_post,
               Units_for_matching$electricity_pre, Units_for_matching$electricity_post,
               Units_for_matching$mother_edu_pre, Units_for_matching$mother_edu_post,
               Units_for_matching$family_plan_prop_pre, Units_for_matching$family_plan_prop_post)

mom_tols=absstddif(mom_covs, t_ind, 0.1)
mom = list(covs = mom_covs, tols = mom_tols, targets = NULL)

t_max = 60*30
name = "glpk"
approximate = 1
solver = list(name = name, t_max = t_max, approximate = approximate, round_cplex = 0, trace_cplex = 0)

## Get the subset of h-h and h-l pairs that are proper to be matched
out_1 = cardmatch(t_ind, mom = mom, solver = solver)
subset_index = out_1

t_id_1 = subset_index$t_id ## index in "treated" (high-low) group
c_id_1 = subset_index$c_id ## index in "control" (high-high) group

## Check they are same length
length(t_id_1) == length(c_id_1) ## TRUE

## The h-h and h-l pairs in the selected subset 
Units_for_matching_selected<-rbind(Units_for_matching[subset_index$t_id,], Units_for_matching[subset_index$c_id, ])


### Fit a propensity score using logistic regression with each Household_covariates entering 
## linearly into the logistic link function
## Put x=TRUE in order to have model object include design matrix
propscore.model=glm(high_high_or_high_low ~ urban_or_rural_1+urban_or_rural_2+toilet_pre+toilet_post+floor_pre+floor_post+electricity_pre+electricity_post+mother_edu_pre+mother_edu_post+family_plan_prop_pre+family_plan_prop_post,family=binomial,x=TRUE,y=TRUE,data=Units_for_matching_selected)
treated=propscore.model$y
propscore.model_before=glm(high_high_or_high_low ~ urban_or_rural_1+urban_or_rural_2+toilet_pre+toilet_post+floor_pre+floor_post+electricity_pre+electricity_post+mother_edu_pre+mother_edu_post+family_plan_prop_pre+family_plan_prop_post,family=binomial,x=TRUE,y=TRUE,data=Units_for_matching)
treated_before=propscore.model_before$y

smahal=
  function(z,X){
    X<-as.matrix(X)
    n<-dim(X)[1]
    rownames(X)<-1:n
    k<-dim(X)[2]
    m<-sum(z)
    for (j in 1:k) X[,j]<-rank(X[,j])
    cv<-cov(X)
    vuntied<-var(1:n)
    rat<-sqrt(vuntied/diag(cv))
    cv<-diag(rat)%*%cv%*%diag(rat)
    subset_index<-matrix(NA,m,n-m)
    Xc<-X[z==0,]
    Xt<-X[z==1,]
    rownames(subset_index)<-rownames(X)[z==1]
    colnames(subset_index)<-rownames(X)[z==0]
    library(MASS)
    icov<-ginv(cv)
    for (i in 1:m) subset_index[i,]<-mahalanobis(Xc,Xt[i,],icov,inverted=T)
    subset_index
  }

## Function for adding a propensity score caliper to a distance matrix dmat
## calipersd is the caliper in terms of standard deviation of the logit propensity scoe
addcaliper=function(dmat,z,logitp,calipersd=.2,penalty=1000){
  sd.logitp=sd(logitp)
  adif=abs(outer(logitp[z==1],logitp[z==0],"-"))
  adif=(adif-(calipersd*sd.logitp))*(adif>(calipersd*sd.logitp))
  dmat=dmat+adif*penalty
  dmat
}

## Matrix of Household_covariatess, excluding intercept
Xmat=propscore.model$x[,-1]
Xmat_before=propscore.model_before$x[,-1]
## Rank based Mahalanobis distance
distmat=smahal(treated,Xmat)
## Add caliper
logit.propscore=predict(propscore.model)
distmat2=addcaliper(distmat,treated,logit.propscore)

### Create a subject index and name the rows and columns of distance matrix by ### this subject index
subject.index=seq(1,length(treated),1)
rownames(distmat2)=subject.index[treated==1]
colnames(distmat2)=subject.index[treated==0]

## Pair Matching
matchvec=pairmatch(distmat2)
## Note: Can ignore warning message from matching

## Create vectors of the subject indices of the treatment units ordered by
## their matched set and corresponding control unit
treated.subject.index=rep(0,sum(treated==1))
matched.control.subject.index=rep(0,length(treated.subject.index))
matchedset.index=substr(matchvec,start=3,stop=10)
matchedset.index.numeric=as.numeric(matchedset.index)
subjects.match.order=as.numeric(names(matchvec)) # The subject indices in 
## the order of matchvec
for(i in 1:length(treated.subject.index)){
  matched.set.temp=which(matchedset.index.numeric==i)
  matched.set.temp.indices=subjects.match.order[matched.set.temp]
  if(treated[matched.set.temp.indices[1]]==1){
    treated.subject.index[i]=matched.set.temp.indices[1]
    matched.control.subject.index[i]=matched.set.temp.indices[2]
  }
  if(treated[matched.set.temp.indices[2]]==1){
    treated.subject.index[i]=matched.set.temp.indices[2]
    matched.control.subject.index[i]=matched.set.temp.indices[1]
  }
}

### Check balance
## Calculate standardized differences.
## Household_covariatess used in propensity score model
Xmat=propscore.model$x;
Xmat_before=propscore.model_before$x

## Standardized differences before matching
treatedmat.before=Xmat_before[treated_before==1,];
controlmat.before=Xmat_before[treated_before==0,];
treatedmean.before=apply(treatedmat.before,2,mean);
controlmean.before=apply(controlmat.before,2,mean);
treatvar=apply(treatedmat.before,2,var);
controlvar=apply(controlmat.before,2,var);
stand.diff.before=(treatedmean.before-controlmean.before)/sqrt((treatvar+controlvar)/2);

## Standardized differences after matching
treatedmat.after=Xmat[treated==1,];
treatedmean.after=apply(treatedmat.after,2,mean);
controlmat.after=Xmat[matched.control.subject.index,];
controlmean.after=apply(controlmat.after,2,mean);
stand.diff.after=(treatedmean.after-controlmean.after)/sqrt((treatvar+controlvar)/2);

## Table for it. #****************************Love plot?
cbind(treatedmean.before, controlmean.before, treatedmean.after, controlmean.after, stand.diff.before, stand.diff.after)

## Get the matched (high-high, high-low) pairs
Units_after_cardinality_matching<-NULL
for (i in 1:length(treated.subject.index)){
  Units_after_cardinality_matching<-rbind(Units_after_cardinality_matching, Units_for_matching_selected[treated.subject.index[i],], Units_for_matching_selected[matched.control.subject.index[i],])
}

## Keep track of the output of step 2
write.csv(Units_after_cardinality_matching, file = "/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/Units_after_cardinality_matching.csv")

## Save the output of the Step 2, where the Units_for_matching are now updated with covarites
## And the Units_after_cardinality_matching are matched (h-h, h-l) pairs
save(subset_index, Units_for_matching, Units_after_cardinality_matching, file = "/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/Units_after_cardinality_matching_with_index.RData")


#######################################################
## Step 3: - Categorize the children level covariates &
##         - Match children to their clusters via DHSID
##         - Imputate the missing values in the response 
##         - Do causal inference by mixed effect linear regression
##         
## - In this step, we get the Individual level data from 
## IPUMS-DHS, and match the data with the output 
## in Step 2, "Units_after_cardinality_matching"
## using DHSID 
##
## - Before matching, we also categorize the covariates
## in the children level data so as to better structured
## it for our purposes.
## 
## - 
#######################################################


## Categorize children's level covariates from IPUMS-DHS for imputation
Children_covariates<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Input Data/idhs_00051.csv.gz") #Children level data

## Load from step2: subset_index, Units_for_matching and Units_after_cardinality_matching
#load(file = "C:/Users/siyuheng/Desktop/Malaria and Birthweight/Output Data PA/Units_after_cardinality_matching.RData")

## Extract country name from DHSID as before
country_name<-rep('nation', nrow(Children_covariates))
country_name=as.character(substr(Children_covariates$DHSID, 1, 2))
Children_covariates<-data.frame(country_name, Children_covariates)

## rename for convenience and uniformity
Unit_before_cardinality_matching <- Units_for_matching

## Set the 4 blocks: the High in H-L, the Low in H-L, the 1st H in H-H, the 2nd H in H-H
individual_treated_early <- NULL #block 1
individual_treated_late <- NULL  #block 2
individual_control_early <- NULL #block 3
individual_control_late <- NULL  #block 4


### We add the primal covariates columns from IPUMS-DHS to the 4 blocks

## Block 1: the High in H-L
for (i in 1:length(subset_index$t_id)){
  country_temp=as.character(Unit_before_cardinality_matching$Country[subset_index$t_id[i]])
  temp<-Children_covariates[which(Children_covariates$country_name==country_temp & Children_covariates$YEAR==Unit_before_cardinality_matching$year_pre_dhs[subset_index$t_id[i]] & Children_covariates$CLUSTERNO==Unit_before_cardinality_matching$cluster_1[subset_index$t_id[i]]), ]
  individual_treated_early<-rbind(individual_treated_early, temp)
}

## Block 2: the Low in H-L
for (i in 1:length(subset_index$t_id)){
  country_temp=as.character(Unit_before_cardinality_matching$Country[subset_index$t_id[i]])
  temp<-Children_covariates[which(Children_covariates$country_name==country_temp & Children_covariates$YEAR==Unit_before_cardinality_matching$year_post_dhs[subset_index$t_id[i]] & Children_covariates$CLUSTERNO==Unit_before_cardinality_matching$cluster_2[subset_index$t_id[i]]), ]
  individual_treated_late<-rbind(individual_treated_late, temp)
}

## Block 3: the 1st H in H-H
for (i in 1:length(subset_index$c_id)){
  country_temp=as.character(Unit_before_cardinality_matching$Country[subset_index$c_id[i]])
  temp<-Children_covariates[which(Children_covariates$country_name==country_temp & Children_covariates$YEAR==Unit_before_cardinality_matching$year_pre_dhs[subset_index$c_id[i]] & Children_covariates$CLUSTERNO==Unit_before_cardinality_matching$cluster_1[subset_index$c_id[i]]), ]
  individual_control_early<-rbind(individual_control_early, temp)
}

## Block 4: the 2nd H in H-H
for (i in 1:length(subset_index$c_id)){
  country_temp=as.character(Unit_before_cardinality_matching$Country[subset_index$c_id[i]])
  temp<-Children_covariates[which(Children_covariates$country_name==country_temp & Children_covariates$YEAR==Unit_before_cardinality_matching$year_post_dhs[subset_index$c_id[i]] & Children_covariates$CLUSTERNO==Unit_before_cardinality_matching$cluster_2[subset_index$c_id[i]]), ]
  individual_control_late<-rbind(individual_control_late, temp)
}

## The 4 blocks constructed save into individual_matched (17202 individuals)
individual_matched<-rbind(individual_treated_early, individual_treated_late, individual_control_early, individual_control_late)


## Check there's no NA in the data
sum(is.na(individual_matched$KIDBORD))        # 0 
sum(is.na(individual_matched$URBAN))      # 0 
sum(is.na(individual_matched$BIRTHSZ))       # 0 
sum(is.na(individual_matched$AGE))# 0
sum(is.na(individual_matched$WEALTHQ))      # 0
sum(is.na(individual_matched$MARSTAT))       # 0 
sum(is.na(individual_matched$KIDSEX))   # 0
sum(is.na(individual_matched$ANCARE)) # 0
sum(is.na(individual_matched$EDUCLVL)) # 0

## An illustration of the primal IPUMS-DHS factor levels Need or No Need for Categorize
table(individual_matched$KIDBORD)      # Need
table(individual_matched$URBAN)      # Need
table(individual_matched$BIRTHSZ)       # Need
table(individual_matched$AGE)      # No
table(individual_matched$WEALTHQ)      # No
table(individual_matched$MARSTAT)       # Need
table(individual_matched$KIDSEX)  # Need
table(individual_matched$ANCARE) # Need
table(individual_matched$EDUCLVL) # Need

## We copy the individual_matched for further purpose
individual_matched_copy <- individual_matched
#individual_matched <- individual_matched_copy


### Now, we categorize the children's covariates for our convenience purposes

individual_matched$KIDBORD[individual_matched$KIDBORD>=2 & individual_matched$KIDBORD<=4]=2
individual_matched$KIDBORD[individual_matched$KIDBORD>=5]=3

## URBAN: Urban or rural 
## Our levels:
## 1 - Urban
## 0 - Rural
individual_matched$URBAN[individual_matched$URBAN == 2] = 0 

## BIRTHSZ: Children's birth size
## Our levels:
## 1 - Very small
## 2 - Smaller than average
## 3 - Average
## 4 - Larger than average
## 5 - Very large
## 97/98/99 - missing (will discarded since only <5%)
individual_matched$BIRTHSZ[individual_matched$BIRTHSZ==32]=1
individual_matched$BIRTHSZ[individual_matched$BIRTHSZ==31]=2
individual_matched$BIRTHSZ[individual_matched$BIRTHSZ==20]=3
individual_matched$BIRTHSZ[individual_matched$BIRTHSZ==12]=4
individual_matched$BIRTHSZ[individual_matched$BIRTHSZ==11]=5

low_size<-rep(0, nrow(individual_matched))
large_size<-rep(0, nrow(individual_matched))

low_size<-as.numeric(individual_matched$BIRTHSZ<=2)
large_size<-as.numeric(individual_matched$BIRTHSZ>=4 & individual_matched$BIRTHSZ<=5)

## WEALTHQ: the household wealth the child lived
## Our levels:
## 1 - Poorest
## 2 - Poorer
## 3 - Middle
## 4 - Richer
## 5 - Richest
## No operations needed, since automatically correct!

## MARSTAT: Woman's current marital or union status
## Our levels:
## 0 - Never married or formerly in union
## 1 - Married or living together
individual_matched$MARSTAT[individual_matched$MARSTAT==21 | individual_matched$MARSTAT==22]=1
individual_matched$MARSTAT[individual_matched$MARSTAT==10 | individual_matched$MARSTAT>=31]=0

## KIDSEX: 
## Our levels:
## 0 - Female
## 1 - Male
individual_matched$KIDSEX[individual_matched$KIDSEX == 2] = 0

## ANCARE
individual_matched$ANCARE[individual_matched$ANCARE!=1]=0

## EDUCLVL: Women respondents' Education level
## Our levels:
## 0 - No education
## 1 - Primary
## 2 - Secondary or higher
individual_matched$EDUCLVL[individual_matched$EDUCLVL==3]=2

table(individual_matched$KIDBORD)      # no missing data
table(individual_matched$URBAN)      # no
table(individual_matched$BIRTHSZ)       # with
table(individual_matched$AGE)     # no
table(individual_matched$WEALTHQ)      # no
table(individual_matched$MARSTAT)       # no
table(individual_matched$KIDSEX)  # no
table(individual_matched$ANCARE) # no
table(individual_matched$EDUCLVL) # no


individual_matched<-data.frame(individual_matched, low_size, large_size)

#write.csv(individual_matched, "/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/indivisual_matched_before_imputation.csv")

#individual_matched<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/indivisual_matched_before_imputation.csv")

low_birthweight<-rep(0, nrow(individual_matched))

for (i in 1:nrow(individual_matched)){
  if (individual_matched$BIRTHWT[i]<2500){
    low_birthweight[i]=1
  }
  else if (individual_matched$BIRTHWT[i]>9000){
    low_birthweight[i]=99
  }
  else {
    low_birthweight[i]=0
  }
}

individual_matched<-cbind(individual_matched, low_birthweight)


table(individual_matched$BIRTHSZ)
table(individual_matched$low_birthweight)
table(individual_matched$KIDTWIN)

#Exclude multiple births
individual_matched_without_multi_gestation<-individual_matched[which(individual_matched$KIDTWIN==10), ]
nrow(individual_matched)
nrow(individual_matched_without_multi_gestation)

sum(individual_matched_without_multi_gestation$low_birthweight>1)/nrow(individual_matched_without_multi_gestation)

#We exclude records with missing birth size
individual_matched_without_missing_size<-individual_matched[which(individual_matched$BIRTHSZ<=5 & individual_matched$KIDTWIN==10), ]
nrow(individual_matched_without_missing_size)

sum(individual_matched_without_missing_size$low_birthweight==99)
sum(individual_matched_without_missing_size$low_birthweight==99)/nrow(individual_matched_without_missing_size)
sum(individual_matched_without_missing_size$low_birthweight!=99)
sum(individual_matched_without_missing_size$low_birthweight==1)
sum(individual_matched_without_missing_size$low_birthweight==1)/sum(individual_matched_without_missing_size$low_birthweight!=99)


############Mixed Effects Linear Regression################
time_indicator<-rep(-1, nrow(individual_matched_without_missing_size))
low_prevalence<-rep(-1, nrow(individual_matched_without_missing_size))
group_indicator<-rep(-1, nrow(individual_matched_without_missing_size))
prevalence<-rep(0, nrow(individual_matched_without_missing_size))


individual_matched_without_missing_size<-data.frame(individual_matched_without_missing_size, time_indicator, prevalence, low_prevalence, group_indicator)

for (i in 1:nrow(individual_matched_without_missing_size)){
  individual_matched_without_missing_size$cluster_renumber[i]=paste(individual_matched_without_missing_size$country_name[i], individual_matched_without_missing_size$YEAR[i], individual_matched_without_missing_size$CLUSTERNO[i])
}

Units_after_matching<-read.csv("/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/Units_after_cardinality_matching.csv")

Units_after_matching$Country=as.character(Units_after_matching$Country)
individual_matched_without_missing_size$country_name=as.character(individual_matched_without_missing_size$country_name)

###########Check the column each time!################
for (i in 1:nrow(individual_matched_without_missing_size)){
  if (length(Units_after_matching[which(Units_after_matching$Country==individual_matched_without_missing_size$country_name[i] & Units_after_matching$year_pre_dhs==individual_matched_without_missing_size$YEAR[i] & Units_after_matching$cluster_1==individual_matched_without_missing_size$CLUSTERNO[i]), 16])>0){
    individual_matched_without_missing_size$prevalence[i]=Units_after_matching[which(Units_after_matching$Country==individual_matched_without_missing_size$country_name[i] & Units_after_matching$year_pre_dhs==individual_matched_without_missing_size$YEAR[i] & Units_after_matching$cluster_1==individual_matched_without_missing_size$CLUSTERNO[i]), 16]
  }
  if (length(Units_after_matching[which(Units_after_matching$Country==individual_matched_without_missing_size$country_name[i] & Units_after_matching$year_post_dhs==individual_matched_without_missing_size$YEAR[i] & Units_after_matching$cluster_2==individual_matched_without_missing_size$CLUSTERNO[i]), 17])>0){
    individual_matched_without_missing_size$prevalence[i]=Units_after_matching[which(Units_after_matching$Country==individual_matched_without_missing_size$country_name[i] & Units_after_matching$year_post_dhs==individual_matched_without_missing_size$YEAR[i] & Units_after_matching$cluster_2==individual_matched_without_missing_size$CLUSTERNO[i]), 17]
  }
  if (i%%1000==0){
    print(i)
  }
}

summary(individual_matched_without_missing_size$prevalence)

#################Check Columns Every Time!###################

country_cluster_year_high_high<-Units_after_matching[which(Units_after_matching$high_high_or_high_low==0), c(3, 6, 7, 18, 19)]

country_cluster_year_high_low<-Units_after_matching[which(Units_after_matching$high_high_or_high_low==1), c(3, 6, 7, 18, 19)]

for (i in 1:nrow(individual_matched_without_missing_size)){
  if (individual_matched_without_missing_size$country_name[i]%in%country_cluster_year_high_high$Country & individual_matched_without_missing_size$CLUSTERNO[i]%in%country_cluster_year_high_high$cluster_1 & individual_matched_without_missing_size$YEAR[i]%in%country_cluster_year_high_high$year_pre_dhs){
    individual_matched_without_missing_size$time_indicator[i]=0
    individual_matched_without_missing_size$low_prevalence[i]=0
    individual_matched_without_missing_size$group_indicator[i]=0
  }
  if (individual_matched_without_missing_size$country_name[i]%in%country_cluster_year_high_high$Country & individual_matched_without_missing_size$CLUSTERNO[i]%in%country_cluster_year_high_high$cluster_2 & individual_matched_without_missing_size$YEAR[i]%in%country_cluster_year_high_high$year_post_dhs){
    individual_matched_without_missing_size$time_indicator[i]=1
    individual_matched_without_missing_size$low_prevalence[i]=0
    individual_matched_without_missing_size$group_indicator[i]=0
  }
  if (individual_matched_without_missing_size$country_name[i]%in%country_cluster_year_high_low$Country & individual_matched_without_missing_size$CLUSTERNO[i]%in%country_cluster_year_high_low$cluster_1 & individual_matched_without_missing_size$YEAR[i]%in%country_cluster_year_high_low$year_pre_dhs){
    individual_matched_without_missing_size$time_indicator[i]=0
    individual_matched_without_missing_size$low_prevalence[i]=0
    individual_matched_without_missing_size$group_indicator[i]=1
  }
  if (individual_matched_without_missing_size$country_name[i]%in%country_cluster_year_high_low$Country & individual_matched_without_missing_size$CLUSTERNO[i]%in%country_cluster_year_high_low$cluster_2 & individual_matched_without_missing_size$YEAR[i]%in%country_cluster_year_high_low$year_post_dhs){
    individual_matched_without_missing_size$time_indicator[i]=1
    individual_matched_without_missing_size$low_prevalence[i]=1
    individual_matched_without_missing_size$group_indicator[i]=1
  }
  if (i%%500==0){
    print(i)
  }
}


table(individual_matched_without_missing_size$time_indicator)
table(individual_matched_without_missing_size$low_prevalence)
table(individual_matched_without_missing_size$group_indicator)

#install.packages('arm')
library(arm)
logit<-glm(low_birthweight~AGE+I(AGE^2)+WEALTHQ+KIDBORD+I(KIDBORD^2)+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE+low_size+large_size, family = binomial(link = "logit"), data=individual_matched_without_missing_size[which(individual_matched_without_missing_size$low_birthweight!=99),])
bayeslogit<-bayesglm(low_birthweight~AGE+I(AGE^2)+WEALTHQ+KIDBORD+I(KIDBORD^2)+URBAN+EDUCLVL+KIDSEX+MARSTAT+ANCARE+low_size+large_size, family = binomial(link = "logit"), data=individual_matched_without_missing_size[which(individual_matched_without_missing_size$low_birthweight!=99),]) 
summary(bayeslogit)
summary(logit)

#####Here we save "all_data_before_imputation"

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
matrix_table[, c(2,3,5)]
sum(individual_matched_without_missing_size$low_birthweight ==1 )/sum(individual_matched_without_missing_size$low_birthweight!=99)
prop_lowweight

## Output the data
write.csv(matrix_table, file ="/Users/heng6/Desktop/Code and Data for Malaria and Low Birth Weight/Output Data PA/step3_Inference_with_mutiple_imputation.csv")

####Save all the data "all_data_primary_analysis

modelSummary$coefficients

low_birthweight_1=individual_matched_without_missing_size$low_birthweight[individual_matched_without_missing_size$low_prevalence==0 & individual_matched_without_missing_size$time_indicator==0 & individual_matched_without_missing_size$group_indicator==1]
low_birthweight_2=individual_matched_without_missing_size$low_birthweight[individual_matched_without_missing_size$low_prevalence==1 & individual_matched_without_missing_size$time_indicator==1 & individual_matched_without_missing_size$group_indicator==1]
low_birthweight_3=individual_matched_without_missing_size$low_birthweight[individual_matched_without_missing_size$low_prevalence==0 & individual_matched_without_missing_size$time_indicator==0 & individual_matched_without_missing_size$group_indicator==0]
low_birthweight_4=individual_matched_without_missing_size$low_birthweight[individual_matched_without_missing_size$low_prevalence==0 & individual_matched_without_missing_size$time_indicator==1 & individual_matched_without_missing_size$group_indicator==0]

mean(low_birthweight_1[low_birthweight_1!=99]) #0.0933
mean(low_birthweight_2[low_birthweight_2!=99]) #0.0752
mean(low_birthweight_3[low_birthweight_3!=99]) #0.0918
mean(low_birthweight_4[low_birthweight_4!=99]) #0.0906

DID=(mean(low_birthweight_2[low_birthweight_2!=99])-mean(low_birthweight_1[low_birthweight_1!=99]))-(mean(low_birthweight_4[low_birthweight_4!=99])-mean(low_birthweight_3[low_birthweight_3!=99]))

mean(low_birthweight_2[low_birthweight_2!=99])-mean(low_birthweight_1[low_birthweight_1!=99])

predict_ave<-rep(0, nrow(predict_bayes))
for (i in 1:length(predict_ave)){
  predict_ave[i]=mean(predict_bayes[i,])
}

individual_for_estimated_rate<-cbind(individual_matched_without_missing_size, predict_ave)

mean(individual_for_estimated_rate$predict_ave[individual_for_estimated_rate$low_prevalence==0 & individual_for_estimated_rate$time_indicator==0 & individual_for_estimated_rate$group_indicator==1]) # 0.1048
mean(individual_for_estimated_rate$predict_ave[individual_for_estimated_rate$low_prevalence==1 & individual_for_estimated_rate$time_indicator==1 & individual_for_estimated_rate$group_indicator==1]) # 0.0844

