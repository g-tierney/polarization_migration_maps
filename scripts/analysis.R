# Script to load and format election data 
#    1) Load data
#    2) Shape into panel of county-year level data

require(openxlsx)
require(dplyr)
require(classInt)
require(maptools)     # loads sp library too
require(RColorBrewer) # creates nice color schemes
require(classInt)     # finds class intervals for continuous variables
require(rgeos)

source("scripts/functions.R")

#################
### Load Data ###
#################

#load vote data
election_data <- read.xlsx("election_data/county_presidential_2004_2012.xlsx",
                           sheet = "us-presidential-election-county", colNames = T)
#election_data$fips[election_data$state == "AK"] <- gsub("AKL","2")
election_data[,c("fips")] <- sapply(election_data[,c("fips")],as.numeric) #AK as weird fips codes that become NA
election_data <- filter(election_data,state != "AK")

#check correlation bt county dem vote % accross years (x=2008,y=2004)
election_wide <- merge(election_data[election_data$year == 2008,],election_data[election_data$year == 2004,],by = "fips",all.x = T)
cor(x = election_wide$pct_dem.x, y = election_wide$pct_dem.y)
summary(lm(pct_dem.x ~ pct_dem.y, data = election_wide))

#seems to be a difference here (x=2008,y=2010)
election_wide <- merge(election_data[election_data$year == 2008,],election_data[election_data$year == 2012,],by = "fips",all.x = T)
cor(x = election_wide$pct_dem.x, y = election_wide$pct_dem.y)
summary(lm(pct_dem.x ~ pct_dem.y, data = election_wide))

#load full migration data
full_migration <- stack_migration("migration_data/county-to-county-2008-2012-ins-outs-nets-gross.xlsx")

###########################
### Transition Matrices ###
###########################

quintile_mat <- make_trans_mat(full_migration,election_data,
           5,"quantile","flow_from_A_to_B_est",election_year = 2008)
quintile_mat

equal_length_mat <- make_trans_mat(full_migration,election_data,
                                    8,"length","flow_from_A_to_B_est",election_year = 2008)

equal_length_mat
write.csv(equal_length_mat,file = "output/trans_matrix_equal_length.csv")

#########################
### Weighted Averages ###
#########################

merged_mig_votes <- merge_mig_elec(full_migration,election_data,2008,c("fips","pct_dem"))
merged_mig_votes <- filter(merged_mig_votes,pct_dem_A >= 0 & pct_dem_B >= 0 & 
                                            !is.na(pct_dem_A) & !is.na(pct_dem_B))
county_state <- select(merged_mig_votes,state_name_A,county_name_A,state_county_code_A,pct_dem_A)
county_state <- unique(county_state)

#average destination democratic vote percentage accross all counties
sum(merged_mig_votes$pct_dem_A*merged_mig_votes$flow_from_B_to_A_est/sum(merged_mig_votes$flow_from_B_to_A_est,na.rm=T),na.rm=T)
sum(merged_mig_votes$pct_dem_B*merged_mig_votes$flow_from_A_to_B_est/sum(merged_mig_votes$flow_from_A_to_B_est,na.rm=T),na.rm=T)

#average difference accross all counties
merged_mig_votes$dif_pct_dem_B_less_A <- merged_mig_votes$pct_dem_B - merged_mig_votes$pct_dem_A
sum(merged_mig_votes$dif_pct_dem_B_less_A*merged_mig_votes$flow_from_A_to_B_est/sum(merged_mig_votes$flow_from_A_to_B_est,na.rm=T),na.rm=T)

#Number moving to a more democratic county
sum(merged_mig_votes$flow_from_A_to_B_est[merged_mig_votes$dif_pct_dem_B_less_A > 0])
#number moving to a less democratic county
sum(merged_mig_votes$flow_from_A_to_B_est[merged_mig_votes$dif_pct_dem_B_less_A < 0])

#find average destination pct_dem (A to B)
county_state$average_to_pct_dem <- sapply(county_state$state_county_code_A,FUN = average_to_value,
                                          mean_var = "pct_dem_B",data = merged_mig_votes,weight_var = "flow_from_A_to_B_est")

#find average difference between destination and origination pct_dem (A to B)
county_state$dif_dest_home <- sapply(county_state$state_county_code_A,FUN = average_to_value,
                                    mean_var = "dif_pct_dem_B_less_A",data = merged_mig_votes,weight_var = "flow_from_A_to_B_est")

####################
### Plot Results ###
####################

#scatter plot of average destination and best-fit line

#regression
lm <- lm(average_to_pct_dem ~ pct_dem_A, data = county_state)
summary(lm)
slope <- round(lm$coefficients[2],2)

pdf("output/scatter_plot.pdf")
plot(county_state$pct_dem_A,county_state$average_to_pct_dem,
     main = "Figure 4\nDestination and Origin Democratic Vote Percentages \nCounty Level",
     xlab = "Origin Democratic Vote Share",ylab = "Average Destination Democratic Vote Share")
abline(lm, col = "red",lwd = 2)
abline(a=0,b=1, col= "blue",lwd = 2)
legend("bottomright",legend = c(paste0("Regression (slope of ",slope,")"),"Unity (slope of 1)"),col = c("red","blue"),
       lwd = 2)
dev.off()

#map results
county.shp <-  readShapePoly("county_shapefile/cb_2013_us_county_500k.shp")
county.shp$fips <- as.numeric(paste0(county.shp$STATEFP,county.shp$COUNTYFP))
county.shp <- merge(county.shp,county_state,
                        by.y=c("state_county_code_A"),
                        by.x = c("fips"), all.x = T)

#Plot county maps

pdf("output/maps.pdf")
plot_county_results("pct_dem_A","Figure 1\nDemocratic Vote Share of County", fixed_median = 50,
                    sub_title = "Data are from 2008 presidential election.")
plot_county_results("average_to_pct_dem","Figure 2\nAverage Destination Democratic Vote Share for Out-Migration",fixed_median = 50,
                    sub_title = "Data are from 2008 presidential election and 2008-2012 migration.")
plot_county_results("dif_dest_home","Figure 3\nDifference Between Destination and Home Democratic Vote Share", fixed_median = 0,
                    sub_title = "Data are from 2008 presidential election and 2008-2012 migration.")
dev.off()

