#ER_region_calculate
#er_data_species is output from f.er.calc
er_data_species <- f.er.beach.calc("Species",output_df)
# Re-establish a beach_year_month for the resulting dataset
er_data_species$beach_year_month <- paste(er_data_species$Region,er_data_species$Beach.Name,er_data_species$year_month,sep=",")
# Subset the data by species
ind_species <- er_data_species[which(er_data_species$Species.Name == "Cassin's Auklet"),]
ind_species <- er_data_species[which(er_data_species$Species.Name == "Common Murre"),]

# Put the results of an individual species (or many species) together with 0 ER values
# all the rest of the beach/year/month where surveys were completed but none of the 
# selected species were found.
ind_species_unique_bym <- unique(ind_species$beach_year_month)
er_data_species_bym <- unique(er_data_species$beach_year_month)

# Find all beach_year_months that should have a zero ER value
bym_er_zeros <- setdiff(er_data_species_bym,ind_species_unique_bym)
# build another data frame of zero er beach_year_month
m <- matrix(unlist(strsplit(bym_er_zeros,",")),byrow=TRUE,ncol=3)
df_join <-as.data.frame(m)
colnames(df_join) <- c("Region","Beach.Name","year_month")
df_join$year_month <- as.character(df_join$year_month)
df_join$Species.Name <- as.factor(NA)
df_join$merge <- as.character(NA)
df_join$er <- 0
df_join$beach_year_month <- as.character(NA)
final_df <- rbind(ind_species,df_join)
# Export final_df
# MAKE THE NAME OF THE FILE DYNAMIC BY THE SPECIES OF CONCERN
write.csv(final_df,"Cassins Auklet_allyears.csv")
write.csv(final_df,"Common_murre.csv")

ind_species <- er_data_species[which(er_data_species$Species.Name == "California Gull" || is.(er_data_species$Species.Name),]
which(ind_species_unique_bym == er_data_species_bym)

region_er <- ddply(ind_species, .(Region,year_month,Species.Name), summarise, er_region=mean(er), sd_error=sd(er),n=length(Beach.Name))