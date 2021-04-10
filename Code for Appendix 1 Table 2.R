#Load the output data from the primary analysis

country_name<-c('BJ', 'BF', 'CM', 'CD', 'CI', 'ET', 'GH', 'GN', 'KE', 'MW', 'ML', 'NM', 'NG', 'RW', 'SN', 'TZ', 'UG', 'ZM', 'ZW')
year_pre<-c(2001, 2003, 2004, 2007, 1998, 2000, 2003, 2005, 2003, 2000, 2001, 2000, 2003, 2005, 2005, 1999, 2000, 2007, 2005)
year_post<-c(2012, 2010, 2011, 2013, 2012, 2010, 2014, 2012, 2014, 2010, 2012, 2013, 2013, 2014, 2010, 2015, 2011, 2013, 2015)


num_early_before_matching<-rep(0, length(country_name))
num_late_before_matching<-rep(0, length(country_name))
num_high_high_step_1<-rep(0, length(country_name))
num_high_low_step_1<-rep(0, length(country_name))
num_high_high_step_2<-rep(0, length(country_name))
num_high_low_step_2<-rep(0, length(country_name))

for (i in 1:length(country_name)){
  num_early_before_matching[i]=sum(Units_by_locations$Country==country_name[i] & Units_by_locations$year1==year_pre[i])
  num_late_before_matching[i]=sum(Units_by_locations$Country==country_name[i] & Units_by_locations$year2==year_post[i])
  num_high_high_step_1[i]=sum(Units_for_matching_high_high$Country==country_name[i])
  num_high_low_step_1[i]=sum(Units_for_matching_high_low$Country==country_name[i])
  num_high_high_step_2[i]=sum(Units_after_cardinality_matching$high_high_or_high_low ==0 & Units_after_cardinality_matching$Country==country_name[i])
  num_high_low_step_2[i]=sum(Units_after_cardinality_matching$high_high_or_high_low ==1 & Units_after_cardinality_matching$Country==country_name[i])
}


cbind(country_name, num_early_before_matching, num_late_before_matching, num_high_high_step_1, num_high_low_step_1, num_high_high_step_2, num_high_low_step_2)

sum(num_early_before_matching)

sum(num_high_high_step_1)
sum(num_high_low_step_1)
