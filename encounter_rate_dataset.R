# Encounter_rate.r

# Clear everything
rm(list = ls())
# Set your working directory
#setwd("~/work/UWCOASST/EncounterRate")

# General approach
# Load Data and manipulate for analysis
# Include data QC checking throughout. 
# Run through 3 Use Cases 
## 1: Only 1 survey per beach/month and no refinds
## 2: More than 1 survey per beach/month and no refinds
## 3: More than 1 survey per beach/month and refinds
# Combine all cases back into one large dataset that can later be used
# for analysis. Including beach encounter rate as a column for working with
# Beach's and aggregating to Regions.

# Load R Packages
library(plyr)
library(ggplot2)
library(dplyr)
library(data.table)
# Load snapshot of database on March 30,2015. Replace with SQL connection to 
# the database
bird_data <- read.csv("../Data_in/coasst_06-01-15_09-09-15.csv")
#bird_data <- read.csv("../Data_in/COASST_RAW_AllYrs.csv")
# Load data for comparison with SAS output
#bird_data <-read.csv("1314COASST_forEric.csv")
#bird_data <-read.csv("coasst_11-01-14_03-31-15.csv")
# Final testing datasets
#bird_data <-read.csv("Humboldt_06to15_050715.csv")


# Filter the data by the key columns from bird_data:
#"Survey Date"
#"Region"
#"Beach.Name"
#"Month"
#"Year"
#"Survey.Code"
#"Beach.Length"
#"Substrate"
#"Is.Survey"
#"Bird.Refound" 
#"Tie.Number"
#"Species.Name"
#"Subgroup.Name"
#"Group.Name" 
#"Is.Bird"
#"Verified" 
#"Bird.Code
bird_data2 <- bird_data[,c(1,22,23,3,4,5,24,54,58,60,75,78,79,80,89,95,6)]

# Filter the dataset by Is.Survey = Yes 
survey_data <- bird_data2[which(bird_data$Is.Survey =='Yes'),]
# JD- Discussion - =! is.bird= 'No' (so will include yes and blanks). Is.bird=blank means no bird
#found
# Establish census/month initially and then keep this number for rest of analysis regardless of data slicing
# 

# Create a column that can easily be binned by beach and month
beach_year_month <- paste(survey_data$Beach.Name,survey_data$Year,survey_data$Month)
# Create a column to bin up year/month for graphing
year_month <- paste(survey_data$Year,survey_data$Month)

# Add these columns to the main dataframe
survey_data$beach_year_month <-as.factor(beach_year_month)
survey_data$year_month <- as.character(year_month)
survey_data$er_include <- NA

# Get a unique vector of beach and month
unique_beach_year_month <- unique(beach_year_month)
# Generate an output dataframe that mirrors the input data frame
output_df <- survey_data[0,]

# Start the main loop to apply the rules and create an output dataset for all observations
# by beach and month.  This output sdataframe can be used to calculate encounter rate
# for any data combinatiosn/aggregation. Encounter rate per beach/month is also calculated
# here to compare results with encounter rate calculation function. 
for (i in 1:length(unique_beach_year_month)) {
  #iterate through each beach and month dataset
  beach_date <- survey_data[which(unique_beach_year_month[i] == survey_data$beach_year_month),] 
  beach_date$er_beach <- NA # Create and assign NAs to beach_date
  beach_date$case <- NA # Track the various use cases as described below
  beach_date$num_survey <- NA # Create a place to track surveys/level of effort per beach/month
  #Filter beach/month datasets by whether they have 1 or more than one survey
  if (length(unique(beach_date$Survey.Code)) == 1) {
    # CASE #1: Only one survey. Flag all refinds and Is.bird != Yes (i.e., surveys with no birds) 
    
    # Code refinds with 0
    beach_date$er_include[which(beach_date$Bird.Refound == "Yes")] <- 0
    beach_date$er_beach[which(beach_date$Bird.Refound == "Yes")] <- 0
    beach_date$case[which(beach_date$Bird.Refound == "Yes")] <-1.1
    
    # Do not calculate enconter rate where no birds were found. Encounter rate should = zero
    # Code non bird records with er_include = 0
    beach_date$er_include[which(beach_date$Is.Bird != "Yes")] <-0
    beach_date$er_beach[which(beach_date$Is.Bird != "Yes")] <- 0 
    beach_date$case[which(beach_date$Is.Bird != "Yes")] = 1.2
    
    # Find all records that have birds to be included in ER calculation 
    #0  do not include 1  include for er_include
    beach_date$er_include[which(beach_date$Is.Bird == "Yes" & beach_date$Bird.Refound == "No")] <-1
    beach_date$case[which(beach_date$Is.Bird == "Yes" & beach_date$Bird.Refound == "No")] <- 1.3
    
    unique_surveys <- unique(beach_date$Survey.Code)
    #adding in number of surveys for output data
    beach_date$num_survey <- length(unique_surveys) 
    #calculates er_beach for checking purposes
    beach_date$er_beach <- sum(beach_date$er_include)/(unique(beach_date$Beach.Length)*length(unique_surveys))
    
    # Append to output dataframe
    output_df <- rbind(output_df,beach_date)
    
  } else if (length(unique(beach_date$Survey.Code)) > 1) {
    # More than one survey per beach per month
    # Get unique vector of all beach surveys in this beach/month
    unique_surveys<- unique(beach_date$Survey.Code)
    # Track the number of surveys per beach/month
    beach_date$num_survey <- length(unique_surveys)
    
    # CASE# 2: MORE THAN ONE SURVEY AND NO REFINDS
    # If survey has no refinds proceed to calculating ER. Verify that a survey
    # dataset has no refinds by: 1) no repeated tie numbers in a month and 2) all Bird.Refound = No and 3) all records have Is.Bird = yes
    if ((length(unique(beach_date$Tie.Number)) == length(beach_date$Tie.Number)) && unique(beach_date$Bird.Refound) == "No" && unique(beach_date$Is.Bird == "Yes")) {
      # Calculate the encounter rate
      beach_date$er_include <- 1
      beach_date$case <- 2
      #use all birds and divide by length of beach times number of surveys within that month
      beach_date$er_beach <- sum(beach_date$er_include)/(unique(beach_date$Beach.Length)*length(unique_surveys))
      
      # Append to output dataframe
      output_df <- rbind(output_df,beach_date)
    } else {
      # Case #3:MORE THAN ONE SURVEY AND ANY REFINDS or No Birds Founds (refinds = Yes or refinds = null)
      # Bird.Code = NA; means no birds.
      
      beach_date$er_include[which(beach_date$Is.Bird != "Yes")] <-0
      beach_date$case[which(beach_date$Is.Bird != "Yes")] <-3.1
      
      # Check for inconsistencies in data. Error in tie number
      #if (!("Yes" %in% unique(beach_date$Bird.Refound))) {
      #  print(paste("Error. Tie number duplicated in a single survey: unique_beach_year_month index =",i))
      #}
      # This is duplicated above#########################
      ## Get unique vector of all beach surveys in this beach/month
      #unique_surveys<- unique(beach_date$Survey.Code)
      ## Track the number of surveys per beach/month
      #beach_date$num_survey <- length(unique_surveys)
      #########################
      # Create and index and for all refinds
      #refind_index <- which(beach_date$Bird.Refound=="Yes")
      
      # MORE THAN ONE SURVEY PER BEACH/MONTH AND ANY REFINDS 
      # Remove all refinds when they were initially found in other beach/months
      # Loop through all refinds.
      beach_date_no <-filter(beach_date,Bird.Refound !="Yes")
      # Assingn er_include records where bird refinds = No to a 1 while taking into
      # account the previously assigned er_include 0 records
      # beach_date_no$er_include[-which(beach_date_no$er_include == 0)] <- 1
      beach_date_no$er_include[which(is.na(beach_date_no$er_include))] <- 1
      beach_date_no$er_include[which(beach_date_no$er_include !=0)] <- 1
      beach_date_no$case[which(is.na(beach_date_no$case))] <- 3.2
      # Pull out all the refinds
      beach_date_yes <-filter(beach_date,Bird.Refound=="Yes")
      
      #Handle Bird Refound = Yes scenarios here. Null Bird.Refound's are skipped
      if (nrow(beach_date_yes) == 0 ) {
        # Handling situations where there are no refinds. This implies there are 
        # surveys with no birds found.
        beach_date <- beach_date_no
      } else if (nrow(beach_date_yes) > 0) {
        remove_tie_nums <-as.numeric()
        for (j in 1:nrow(beach_date_yes)) {
          # Remove refinds = Yes where Tie number exists AND Refind = No by comparing################CHECK THIS ASSUMPTION AGAIN WITH REFINDS
          # refinds = yes with those of refind=NO. If there is NOT a corresponding
          # record tie number with refind = NO ...remove it.
          if (length(which(beach_date_yes$Tie.Number[j] == beach_date_no$Tie.Number)) != 1) {
            remove_tie_nums <- rbind(remove_tie_nums,j)
          }
        }
        # Combine the two datasets back together.
        if (length(remove_tie_nums) == 0) {
          # No refinds should be removed  
          beach_date_yes$er_include <- 1
          beach_date_yes$case <- 3.3
          beach_date <- rbind(beach_date_no,beach_date_yes)
          
        } else {
          # Assign a 0 to refinds that should be removed
          beach_date_yes_removed <-beach_date_yes[remove_tie_nums,]
          beach_date_yes_removed$er_include <- 0 
          beach_date_yes_removed$case <- 3.4
          beach_date <- rbind(beach_date_no,beach_date_yes_removed)
          # Assign a 1 to refinds that were found in that month
          if (nrow(beach_date_yes[-remove_tie_nums,]) > 0) {
            # Remove them from beach_date_yes
            beach_date_yes_not_removed <- beach_date_yes[-remove_tie_nums,]
            beach_date_yes_not_removed$er_include <- 1
            beach_date_yes_not_removed$case <- 3.5
            beach_date <- rbind(beach_date,beach_date_yes_not_removed)
          }         
        }  
      }   
      # Calculate the encounter rate
      beach_date$er_beach <- sum(beach_date$er_include)/(unique(beach_date$Beach.Length)*length(unique_surveys))      
      output_df <- rbind(output_df,beach_date)
    }
  }   
}

# Save out dataset for future loading (if needed)
sysdate = Sys.Date()
filename= paste("output_df",sysdate)
save(output_df, file=filename,compress="gzip")
write.csv(../Data_out/output_df,"coast_output_new.csv")

# Code to quickly find out problems with this script. Find the beach/year/month
# where there script broke.
which(output_df$beach_year_month == "Mad River Park N 2007 Mar")
[1] 1225
output_df[which(output_df$beach_year_month == "Battery Point N 2008 Dec")]
which(unique_beach_year_month == "Clam Beach North 2011 Jan")
which(unique_beach_year_month == "Battery Point N 2014 Mar")




