# Encounter Rate Calculations
# 3/31/2015 - May need to break this up into more functions if many ER datasets
# are desired.
f.er.beach.calc <- function(dataset, input_df) {
    # This is a function used to create COASST datasets that are ready for 
    # further analysis or visualization.  This function calculates Encounter Rate
    # (ER) for potentially any number of factors by beach.
    # Example usage: er.data <- f.er.calc("Beach",output_df) will return ER by Beach per year/month
    # Args: 
      # dataset: The dataset you would like to calculate with ER. Acceptable args are:
      # "Group", "Beach", "Species". Additional args can easily be added to this
      # function.
      # input_df: This has to be the output_df dataframe produced by the encounter_rate
      # script. 
      # Returns: A dataset with critical location and date information along with
      # the ER for the dataset. ER can be futher aggregated to region or other
      # factors as needed.
      # If individual species ER are of interest the following code is an easy way to 
      # to subset the data for a particular species.
      # Example dataset for one species:
        #ind_species <-final_result_species[which(final_result_species$Species.Name == "Common Murre"),]
      # The final dataset will also be saved as .csv into your work space.
    # Aggregating into regional ER's shoudl be done using the f.er.region.calc
  library(dplyr)
  # Check to make sure arguments are present
  if (missing(dataset))
    stop("Need to specify an output dataset for which you want ER calculated.")
  if (missing(input_df))
    stop("Need to load the output_df dataframe from the encounter_rate.R program.")
  if (dataset == "Group") {
    ##-------------------------------------------------------------------------
    # Setup the categories or ways to filter. Uncomment the desired option
    # By Group.Name
    input_df$merge <- paste(input_df$beach_year_month,input_df$Group.Name)
    # Send to f.merge. See instructions
    merge_output_df <- f.merge(input_df)
    final_result_group<-ddply(merge_output_df, .(Region,Beach.Name,year_month,Group.Name,Species.Name,merge), summarise,
                                er = sum(er_include)/(mean(Beach.Length)*length(unique(Survey.Code))))
    # Write out the datafile
    write.csv(final_result_group,"final_result_group.csv")
    return(final_result_group) # Return dataframe with COASST and ER data
    
  
  } else if (dataset == "Species") {
    ##-------------------------------------------------------------------------
    # Setup the categories or ways to filter. Uncomment the desired option
    # OR by species
    input_df$merge <- paste(output_df$beach_year_month,output_df$Species.Name)
    # Use for Species
    ################################################################
    # Send to f.merge. See instructions
    merge_output_df <- f.merge(input_df)
    ###################
    final_result_species<-ddply(merge_output_df, .(Region,Beach.Name,year_month,Species.Name,merge), summarise,
                                er = sum(er_include)/(mean(Beach.Length)*unique(num_survey)))
    
    write.csv(final_result_species,"final_result_species.csv")
    return(final_result_species) # Return dataframe with COASST and ER data
  
  } else if (dataset == "Beach") {
    ##-------------------------------------------------------------------------
    # Setup the categories or ways to filter. Uncomment the desired option
    # OR by beach
    input_df$merge <- input_df$beach_year_month
    #################################################################
    # Send to f.merge. See instructions
    merge_output_df <- f.merge(input_df)
    ##################
    # Produce a simple output for graphing and data visualization
    final_result_beach<-ddply(merge_output_df, .(Region,Beach.Name,year_month,merge), summarise,
                              er = sum(er_include)/(mean(Beach.Length)*unique(num_survey)))
    #sum(beach_date$er_include)/(unique(beach_date$Beach.Length)*length(unique_surveys))
    write.csv(final_result_beach,"final_result_beach.csv")
    return(final_result_beach) # Return dataframe with COASST and ER data
    
  } else {
    print(paste("Incorrect dataset name. See the function instructions."))
  }
}

f.merge <- function(output_df) {
  # This function creates a merged dataset that is ready for the final ER
  # calculation. It sets the framework to calculate the ER by 
  # Args: output_df with the "merge" column appropriately created for the 
  # desired dataset
  # Returns: the merged dataset
  # Get number of rows per category 
  cat_rows <- ddply(output_df,c("merge"),c("nrow"))
  # Merge the counts and outupt df into a new dataframe. This is the final comprehensive
  # dataset.
  merge_output_df <- merge(output_df,cat_rows,by="merge")
  return(merge_output_df)
}