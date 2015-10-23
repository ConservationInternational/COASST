#COASST Data Quality
# 1. Confirm that all beaches have the same beach length - DONE
#SPECIES HIERARCHY CHECK - JANE SENT A FILE AND THIS SHOUDL BE EDITED
# 2. Build report or export file for Tie number Error so these can be easily fixed. This consists of the same tie number in a beach/year/month/survey
# 3.a Species, Age, plumage and sex for a tie number should not change for a bird at a beach.
#  Ex. Tie # 313 Commun Murre found in Beach/yeah/month. For the next 12 months the tie number should be the same species, age, plumage and sex for tie number 313. 
# 3.b Look at results and look to ensure there is a new observation and then all the rest are refinds. When there are 2 or more new observations this could be an error or the start of new bird observation.
# 4. Species hierarchy test - DONE written in R
# 5. There should never be two verified surveys (is.survey = yes) at the same beach/date (year/month/day). If this happens then the data should be merged together (Jane).
# 6. Survey duration should not be over 6 hours (not hard rule but be checked). Duration is calculated from survey start and end time.
# 7. Travel time should not be greater than 6 hours. Travel time is in the survey_volunteers table.
# 8. For all birds (is.bird=no) all tie numbers/beach/year/month should be is.bird=no until the tie is reused.
# 9. If collected = yes, tie number/beach/year/month should never be found again.
# 10. Remind = yes & intact=yes should be rare. Look at all these instances. 
# 11. Beach’s that have split into two (Need a list from Jane)…we want to ensure that a beach per month (the splits and original name) are not happening (ensure there is not data for original and the split)
# 12. Tie numbers should never be NA or 0 with a bird code.  DONE 123 (did manually in excel)

# Load in the full dataset output
#load("output_df 2015-07-07")  # From bird_data <- read.csv("COASST_RAW_AllYrs.csv")
data_check <- select(output_df,Region,Beach.Name,beach_year_month,year_month,Survey.Code,er_beach,case)
data_check_unique1 <- unique(output_df$beach_year_month)
data_check_unique <- unique(data_check$beach_year_month)
data_check_unique[which(duplicated(data_check_unique)=='TRUE')]

# Compare output_df er_beach values with those being produced by the function. beach_data_final is the function output
for (i in 1:length(data_check_unique)) {
  if (unique(data_check$er_beach[which(data_check_unique[i] == data_check$beach_year_month)]) !=  beach_data_final$er[which(data_check_unique[i]==beach_data_final$merge)]) {
    print(paste("Not Ok",i,data_check_unique[i]))    
  }
}

# Species Hiearchy check
species_hierarchy <- data.frame(output_df$Species.Name,output_df$Subgroup.Name,output_df$Group.Name)
species_hierarchy_unique <- unique(species_hierarchy)
write.csv(species_hierarchy_unique,"species_hierarchy.csv")


# Data Validation - Compaure ER results with validation dataset

#############################################################################
# DATA QUALITY CHECKS
#Confirm that all beaches have the same beach length
#beach_length_check <-data.frame(cbind(as.character(bird_data$Beach.Name),bird_data$Beach.Length))
bl <- data.frame(bird_data$Beach.Name,bird_data$Beach.Length)
setnames(bl,"bird_data.Beach.Name","BeachName")
setnames(bl,"bird_data.Beach.Length","BeachLength")
unique_bl <- unique(bl)
unique_bl_sort <- unique_bl[order(unique_bl$BeachName),]
dups_bl<- which(duplicated(unique_bl$BeachName) == "TRUE")
print(paste("There are", length(dups_bl),"beaches with multiple beach lengths"))
########
#Confirm that a beach/month and survey has unique tie numbers.
beach_tie_check <- data.frame(paste(bird_data$Beach.Name,bird_data$Year,bird_data$Month,bird_data$Survey.Code,bird_data$Tie.Number))
setnames(beach_tie_check,"paste.bird_data.Beach.Name..bird_data.Year..bird_data.Month..", "btc")
dups_btc <- beach_tie_check[duplicated(beach_tie_check),]
print(paste("There are", length(dups_btc),"instances where there are multiple records with the same tie number repeated in a beach/year/month/survey"))
########
# Age, plumage and sex for a tie number (within 12 month period by beach) should not change.
beach_attr_check <- select(bird_data,Beach.Name,Year,Month,Survey.Code,Tie.Number,Age,Plumage,Sex)
beach_attr_check <- data.frame(bird_data$Beach.Name,bird_data$Year,bird_data$Month,bird_data$Survey.Code,bird_data$Tie.Number,bird_data$Age,bird_data$Plumage,bird_data$Sex)

################
# Look for situations where tie.number is NA and taxonomic information is available)
bird_data$Species.Name[which(bird_data$Species.Name == "")] <-NA
bird_data$Group.Name[which(bird_data$Group.Name == "")] <-NA
bird_data$Subgroup.Name[which(bird_data$Subgroup.Name == "")] <-NA


d<- bird_data[which(is.na(bird_data$Tie.Number) & (!is.na(bird_data$Species.Name))),]
d<- bird_data[which(is.na(bird_data$Tie.Number) & (!is.na(bird_data$Group.Name))),]
d<- bird_data[which(is.na(bird_data$Tie.Number) & (!is.na(bird_data$Subgroup.Name))),]


# || !is.na(bird_data$Group.Name) || !is.na(bird_data$Subgroup.Name))),]
# Assign NAs for empty factors
d$Species.Name[which(d$Species.Name == "")] <-NA
d$Species.Name[which(d$Species.Name == "")] <-NA

a<- distinct(beach_attr_check,Beach.Name,Year,Month,Survey.Code)

unique_btc <- unique(beach_tie_check)
dups_btc <-which(duplicated(unique_btc) == "FALSE")
