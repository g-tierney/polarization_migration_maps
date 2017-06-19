#' Functions for transtion matrices
#' 

###########################
### Migration Functions ###
###########################

#' Merge all sheets from a census migration file
#' 
#' @param file file path to migration data file
#' 
#' @return stacked dataset

stack_migration <- function(file){
  sheet_names <- openxlsx::getSheetNames("migration_data/county-to-county-2008-2012-ins-outs-nets-gross.xlsx")
  
  results_list <- lapply(sheet_names,FUN = import_migration_sheet,file = file)
  
  results.stacked <- rbind(results_list[[1]],results_list[[2]])
  for(i in 3:length(sheet_names)){
    results.stacked <- rbind(results.stacked,results_list[[i]])
  }
  return(results.stacked)
}

import_migration_sheet <- function(sheet_name, file){
  data <- read.xlsx(file, sheet = sheet_name, colNames = T)
  
  names(data) <- c("state_code_A","fips_code_A","state_code_B","fips_code_B",
                   "state_name_A","county_name_A","state_name_B","county_name_B",
                   "flow_from_B_to_A_est","flow_from_B_to_A_moe",
                   "flow_from_A_to_B_est","flow_from_A_to_B_moe",
                   "net_from_B_to_A_est","net_from_B_to_A_moe",
                   "gross_between_A_and_B_est","gross_between_A_and_B_moe")
  #delete extra col name rows
  data <- data[3:nrow(data),] 
  #delete footer notes
  data <- filter(data, !is.na(state_name_A))
  
  #set numeric variables to numeric
  numeric_vars <- 9:16
  data[,numeric_vars] <- sapply(data[,numeric_vars],as.numeric)
  
  #combine state and county codes
  data <- data %>%
    mutate(state_county_code_A = as.numeric(paste0(state_code_A,fips_code_A)),
           state_county_code_B = as.numeric(paste0(state_code_B,fips_code_B)))
  
  data
}

###########################
### Vote Data Functions ###
###########################

#'  Make a variable storing bucketed ranks of the given percentage
#'  
#'  @param var Variable to bucket
#'  @param num_quantiles Number of quantiles to create
#'  @param type "quantile" for equally sized buckets, "length" for buckets of equal length
#'  
#'  @return Vector of ranks

bucket <- function(var,num_quantiles,type = "quantile"){
  
  #make buckets
  if(type == "quantile"){
    buckets <- quantile(var,probs = seq(0,1,length.out = num_quantiles+1))
  } else if(type == "length"){
    buckets <- seq(0,1,length.out = num_quantiles + 1)*100
  } else{
    print("Invalid type.  Must be 'quantile' or 'length'")
  }
  
  #tag which bucket each value is in
  var_buckets <- sapply(var,function(value){min(max(which(buckets <= value)),num_quantiles)})
  return(var_buckets)
}

#'  Merge election and migration data for a given year and set of variables
#' 
#'  @param migration_data dataset of migration flows
#'  @param election_data dataset of county-level election results
#'  @param election_year the year of results to use from election_data (default 2008)
#'  @param election_vars the variables from election_data to merge onto migration_data (must include fips)
#'  
#'  @return merged dataset of entire migration dataset with election_vars from election dataset

merge_mig_elec <- function(migration_data,election_data,
                           election_year = 2008,
                           election_vars = c("fips","pct_dem")){
  
  #ensure fips is always in election_vars (needed for merge)
  if(!("fips" %in% election_vars)){
    election_vars <- c("fips",election_vars)
  }
  
  election_subset <- election_data %>% filter(year == election_year) %>%
    select(one_of(election_vars)) %>% select(fips,everything())
  
  names(election_subset)[-1] <- paste0(names(election_subset)[-1],"_A")
  merged_mig_votes <- merge(migration_data,election_subset,
                            by.x = c("state_county_code_A"), by.y= c("fips"),all.x = T,all.y = F)
  #merged_mig_votes <- rename(merged_mig_votes,pct_dem_A = pct_dem)
  
  names(election_subset)[-1] <- gsub(x = names(election_subset)[-1],pattern = "_A",replacement = "")
  names(election_subset)[-1] <- paste0(names(election_subset)[-1],"_B")
  merged_mig_votes <- merge(merged_mig_votes,election_subset,
                            by.x = c("state_county_code_B"), by.y= c("fips"),all.x = T,all.y = F)
  #merged_mig_votes <- rename(merged_mig_votes,pct_dem_B = pct_dem)
  
  return(merged_mig_votes)
}

##################################
### Transtion Matrix Functions ###
##################################

#'  Sum transtion_variable by all to group levels for one level of the from group
#'  
#'  @param data dataset containing all variables used in other arguments
#'  @param by_group_number the level of the from group to subset to
#'  @param transition_variable the variable to be summed (usually flow from A to B)
#'  @param group_variable_A the name of the from variable
#'  @param group_variable_B the name of the to variable
#'  
#'  @return Sums by from group

transition_counts <- function(data,by_group_number,transition_variable,
                              group_variable_A,group_variable_B){
  data %>%
    filter_(paste0(group_variable_A," == ",by_group_number)) %>%
    group_by_(group_variable_B) %>%
    summarize_(paste0("sum(",transition_variable,",na.rm=T)"))
}

#'  Make transition matrix for group A to group B weighted by another variable
#'  
#'  @param data dataset holding all variables mentioned in other parameters
#'  @param group_variable_A name of the from group
#'  @param group_variable_B name of the to group
#'  @param transition_variable name of the weight (migration) variable
#'  @param group_level vector of the levels of the from and to group levels
#'  
#'  @return Transtion matrix of group A to group B

trans_matrix <- function(data,group_variable_A,group_variable_B,transition_variable,group_levels){
  transtion_levels <- expand.grid(group_levels,group_levels)
  
  list_results <- apply(X = as.matrix(group_levels),FUN = transition_counts, MARGIN = 1,data = data,
                        transition_variable = transition_variable,
                        group_variable_A = group_variable_A,group_variable_B = group_variable_B)
  result <- t(sapply(X=list_results,FUN=t))[,2*(1:length(group_levels))]
  rownames(result) <- group_levels
  colnames(result) <- group_levels
  result
}

#'  Make transition matrix from a migration and election dataset subject to user parameters
#'  
#'  @param migration_data dataset of county-level migration flows
#'  @param election_data dataset of county-level election results
#'  @param num_groups number of groups to bucket counties into
#'  @param type_groups type of buckets to use (quantile or euqal length)
#'  @param transition_variable migration variable to weight transitions by (usually flow_from_A_to_B_est)
#'  @param election_year year of election results to use (default is 2008)
#'  
#'  @return transtion matrix by democratic percentage of the vote

make_trans_mat <- function(migration_data,election_data,
                           num_groups,type_groups,
                           transition_variable,
                           election_year = 2008){
  
  #make groups with 2008 results
  election_subset <- election_data %>% filter(year == election_year, pct_dem >= 0) %>%
    mutate(vote_group = bucket(var = pct_dem,num_quantiles = num_groups,type = type_groups)) %>%
    select(year, fips, pct_dem, vote_group)
  
  merged_mig_votes <- merge_mig_elec(migration_data,election_subset,election_year,
                                     election_vars = c("fips","pct_dem","vote_group"))
  
  
  trans_mat <- trans_matrix(merged_mig_votes,"vote_group_A","vote_group_B",transition_variable,1:num_groups)
  
  trans_mat <- data.frame(trans_mat)
  trans_mat[,"total_migration_from"] <- rowSums(trans_mat)
  trans_mat[num_groups+1,] <- colSums(trans_mat)
  rownames(trans_mat)[num_groups+1] <- "total_migration_to"
  trans_mat[1:num_groups,1:num_groups] <- trans_mat[1:num_groups,1:num_groups]/trans_mat$total_migration[1:num_groups]
  
  #name rows with buckets
  if(type_groups == "quantile"){
    bucket_names <- quantile(election_data$pct_dem[election_data$pct_dem >= 0],probs = seq(0,1,length.out = num_groups+1))
  } else if(type_groups == "length"){
    bucket_names <- seq(0,1,length.out = num_groups + 1)*100
    bucket_names <- round(bucket_names,2)
  }
  names <- c(paste0("0% to ",bucket_names[2],"%"))
  for(i in 2:(num_groups-1)){
    names <- c(names,paste0(bucket_names[i],"% to ",bucket_names[i+1],"%"))
  }
  names <- c(names,paste0(bucket_names[(num_groups)],"% to 100%"))
  
  rownames(trans_mat)[1:num_groups] <- names
  colnames(trans_mat)[1:num_groups] <- names
  trans_mat
}


#########################
### Average Functions ###
#########################

#can get expected democratic voute % for destination county, separated by moving county (or some histograms)
average_to_value <- function(data,from_county,mean_var,weight_var = "flow_from_A_to_B_est"){
  subset <- filter(data, state_county_code_A == from_county )
  subset <- subset[!is.na(subset[,mean_var]),]
  var <- subset[,mean_var]
  wts <- subset[,weight_var]/sum(subset[,weight_var])
  avg <- weighted.mean(var,wts)
  return(avg)
}

##########################
### Plotting Functions ###
##########################

#'  Plot destination dem vote share
#'  
#'  @param plot_var The county-level variable to plot
#'  @param title The title of the plot
#'  @param fixed_median The fixed mid-point of the color shading (for vote percentages use 50, for change in vote percentages use 0)
#'  @subtitle Note citing the data.  
#'  
#'  @return A map of US counties shaded by the plot variable.  

plot_county_results <- function(plot_var,title,fixed_median,
                                sub_title = "Data are from 2008 presidential election and 2008-2012 migration."){
  plotvar <- county.shp@data[,plot_var]
  nclr <- 10
  plotclr <- brewer.pal(nclr,"RdBu")
  
  quantile_rep <- quantile(plotvar[plotvar < fixed_median],na.rm=T,probs = seq(0,1,length.out = nclr/2))
  quantile_dem <- quantile(plotvar[plotvar >= fixed_median],na.rm=T,probs = seq(0,1,length.out = nclr/2))
  class <-  classIntervals(plotvar, nclr, style="fixed",fixedBreaks = c(quantile_rep[1:(nclr/2 - 1)],fixed_median,quantile_dem[2:(nclr/2)]))
  
  colcode <- findColours(class, plotclr)
  
  plot(county.shp[(county.shp$fips %in% county_state$state_county_code_A & 
                     county.shp$STATEFP != "15"),])
  plot(county.shp[(county.shp$fips %in% county_state$state_county_code_A & 
                     county.shp$STATEFP != "15"),], 
       col=colcode[(county.shp$fips %in% county_state$state_county_code_A & 
                      county.shp$STATEFP != "15")], add=T)
  
  title(main=title, 
        sub=sub_title)
  legend("bottomleft", legend=names(attr(colcode, "table")), 
         fill=attr(colcode, "palette"), cex=0.6, bty="n")
}

