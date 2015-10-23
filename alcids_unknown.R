#Alcids
query_data <-output_df[which(output_df$Group.Name == "Small Alcids" & output_df$Species.Name == "Unknown"),]
er_data_species <- f.er.beach.calc("Group",query_data)

# Re-establish a beach_year_month for the resulting dataset
er_data_species$beach_year_month <- paste(er_data_species$Region,er_data_species$Beach.Name,er_data_species$year_month,sep=",")
full_data <-output_df
full_data$beach_year_month <-NA
full_data$beach_year_month <- paste(full_data$Region,full_data$Beach.Name,full_data$year_month,sep=",")
# Subset the data by species

# Put the results of an individual species (or many species) together with 0 ER values
# all the rest of the beach/year/month where surveys were completed but none of the 
# selected species were found.
full_data_unique_bym <- unique(full_data$beach_year_month)
er_data_species_bym <- unique(er_data_species$beach_year_month)

# Find all beach_year_months that should have a zero ER value
bym_er_zeros <- setdiff(full_data_unique_bym,er_data_species_bym)
# build another data frame of zero er beach_year_month
m <- matrix(unlist(strsplit(bym_er_zeros,",")),byrow=TRUE,ncol=3)
df_join <-as.data.frame(m)
colnames(df_join) <- c("Region","Beach.Name","year_month")
df_join$year_month <- as.character(df_join$year_month)
df_join$Group.Name <- as.factor(NA)
df_join$Species.Name <- as.factor(NA)
df_join$merge <- as.character(NA)
df_join$er <- 0
df_join$beach_year_month <- as.character(NA)
final_df <- rbind(er_data_species,df_join)
# Export final_df
write.csv(final_df,"alcids_unknown_full.csv")