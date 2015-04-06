# Encounter_rate.r
#
# Clear everything
rm(list = ls())
# Set your working directory
setwd("~/work/UWCOASST/EncounterRate")

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
bird_data <- read.csv("COASST1999-2014.csv")
# Load data for comparison with SAS output
#bird_data <-read.csv("1314COASST_forEric.csv")

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
bird_data2 <- bird_data[,c(1,22,23,3,4,5,24,54,58,60,75,78,79,80,89,95)]

# Filter the dataset by Is.Survey = Yes and Is.Bird =Yes
survey_data <- bird_data2[which(bird_data$Is.Survey =='Yes' & bird_data$Is.Bird == 'Yes'),]
# Create a column that can easily be binned by beach and month
beach_year_month <- paste(survey_data$Beach.Name,survey_data$Year,survey_data$Month)
# Create a column to bin up year/month for graphing
year_month <- paste(survey_data$Year,survey_data$Month)

# Add these columns to the main dataframe
survey_data$beach_year_month <-as.factor(beach_year_month)
survey_data$year_month <- as.character(year_month)

# Get a unique vector of beach and month
unique_beach_year_month <- unique(beach_year_month)
# Generate an output dataframe that mirrors the input data frame
output_df <- survey_data[0,]

# Start the main loop to apply the rules and create an output dataset for all observations
# by beach and month.  This output dataframe can be used to calculate encounter rate
# for any data combination/aggregation. Encounter rate per beach/month is also calculated
# here to compare results with encounter rate calculation function. 
for (i in 1:length(unique_beach_year_month)) {
  #iterate through each beach and month dataset
  beach_date <- survey_data[which(unique_beach_year_month[i] == survey_data$beach_year_month),] 
  beach_date$er_beach <- NA # Create and assign NAs to beach_date
  beach_date$case <- NA # 
  #Filter beach/month datasets by whether they have 1 or more than one survey
  if (length(unique(beach_date$Survey.Code)) == 1) {
    # CASE #1: Only one survey and remove all refinds. 
    if ("Yes" %in% unique(beach_date$Bird.Refound)) {
      # Remove refinds when there is only 1 survey in that beach/month
      beach_date <- beach_date[-which(beach_date$Bird.Refound == "Yes"),]
    }
    # Calc BeachER for comparison with ddply ER rate codee 
    unique_surveys<- unique(beach_date$Survey.Code)
    beach_date$er_beach <- nrow(beach_date)/(unique(beach_date$Beach.Length)*length(unique_surveys))
    if (nrow(beach_date) >0) {
      beach_date$case = 1
    }
    # Append to output dataframe
    output_df <- rbind(output_df,beach_date)
  } else if (length(unique(beach_date$Survey.Code)) > 1) {
    # More than one survey per beach per month
    # Get unique vector of all beach surveys in this beach/month
    unique_surveys<- unique(beach_date$Survey.Code)
    # CASE# 2: MORE THAN ONE SURVEY AND NO REFINDS
    # If survey has no refinds proceed to calculating ER. Verify that a survey
    # dataset has no refinds by: 1) no repeated tie numbers in a month and 2) all Bird.Refound = No
    if ((length(unique(beach_date$Tie.Number)) == length(beach_date$Tie.Number)) && unique(beach_date$Bird.Refound) == "No") {
      # Proceed here if no refinds
       # Calculate the encounter rate
       beach_date$er_beach <- nrow(beach_date)/(unique(beach_date$Beach.Length)*length(unique_surveys))
       if (nrow(beach_date) > 0) {
         beach_date$case = 2
       }
       # Append to output dataframe
       output_df <- rbind(output_df,beach_date)
    } else {
      # MORE THAN ONE SURVEY AND REFINDS
      # Check for inconsistencies in data. Error in tie number
      if (!("Yes" %in% unique(beach_date$Bird.Refound))) {
        print(paste("Error. Tie number duplicated in a single survey: unique_beach_year_month index =",i))
      }
      # Create and index and for all refinds
      refind_index <- which(beach_date$Bird.Refound=="Yes")
  
      # Case #3: MORE THAN ONE SURVEY PER BEACH/MONTH AND REFINDS 
      # Remove all refinds when they were initially found in other beach/months
      # Loop through all refinds.
      beach_date_no <-filter(beach_date,Bird.Refound=="No")
      beach_date_yes <-filter(beach_date,Bird.Refound=="Yes")
      remove_tie_nums <-as.numeric()
      for (j in 1:nrow(beach_date_yes)) {
        # Remove refinds = Yes where Tie number exists AND Refind = No by comparing
          # refinds = yes with those of refind=NO. If there is NOT a corresponding
          # record tie number with refind = NO ...remove it.
        if (length(which(beach_date_yes$Tie.Number[j] == beach_date_no$Tie.Number)) != 1) {
          remove_tie_nums <- rbind(remove_tie_nums,j)
        }
      } 
      # Take out refinds that were not initially found in that beach/month
      beach_date_yes_removed <- beach_date_yes[-remove_tie_nums,]
      # Combine refinds = yes and no back together to replace beach_date
      beach_date <- rbind(beach_date_no,beach_date_yes_removed)
      # Calculate the encounter rate
      beach_date$er_beach <- nrow(beach_date)/(unique(beach_date$Beach.Length)*length(unique_surveys))
      if (nrow(beach_date) > 0) {
        beach_date$case = 3
      }
      output_df <- rbind(output_df,beach_date)
    }
  }   
}
# Save out dataset for future loading (if needed)
write.csv(output_df,"coast_output.csv")
