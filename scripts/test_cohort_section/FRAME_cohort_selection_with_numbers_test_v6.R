########################################
# FRAME cohort selection
# test version 1
########################################

# women aged 18+ years at time of conception
# needed person table
# hypertensive disorder
# need medication table for labetalol or nifedipine
# need conduction occurrence table for pregnancy


# import packages needed

if (!require("dplyr")) {
  install.packages("dplyr")
}
library("dplyr")


if (!require("tidyverse")) {
  install.packages("tidyverse")
}
library("tidyverse")


if (!require("readxl")) {
  install.packages("readxl")
}
library("readxl")


if (!require("lubridate")) {
  install.packages("lubridate")
}
library("lubridate")


if (!require("bit64")) {
  install.packages("bit64")
}
library("bit64")


if (!require("data.table")) {
  install.packages("data.table")
}
library("data.table")



# set folders

omop_folder <- "B:/BRC_Elixir/Durbaba- MIREDA/test OMOP/OMOP CMD/2026-07-14/"

vocab_folder <- "B:/BRC_Elixir/Durbaba- MIREDA/OMOP vocabulary/bundle OMOP vocabulary_download_v5_2026-05-21/"

mapping_folder <- "B:/BRC_Elixir/Durbaba- MIREDA/test OMOP/mapping/OMOPed/"

ethnic_lookup_file <- "Ethnicities_long_format_OMOP.xlsx"

out_folder <- "B:/BRC_Elixir/Durbaba- MIREDA/test OMOP/FRAME OMOP/version 1/"  

ethnic_lookup_file <- "Ethnicities_long_format_OMOP.xlsx"


#Set file names#

script_version = 6


#main data file
out_file <- paste0("FRAME mothers data for cohort section v",as.character(script_version)," ",as.character(Sys.Date()),".csv")

# additional data files

out_summary_file <-  paste0( "FRAME mothers r script v",as.character(script_version)," ",as.character(Sys.Date())," summary for cohort section.csv")

out_numbers_file <-  paste0( "FRAME mothers r script v",as.character(script_version)," ",as.character(Sys.Date())," numbers for excel file.csv")

out_codebook_file <-  paste0( "FRAME mothers r script v",as.character(script_version)," ",as.character(Sys.Date())," codebook for cohort section.csv")

out_hyp_file <-  paste0( "FRAME mothers hypertensive disorder vocab use v",as.character(script_version)," ",as.character(Sys.Date()),".csv")

out_drug_file <-  paste0( "FRAME mothers labetalol or nifedipine vocab use v",as.character(script_version)," ",as.character(Sys.Date()),".csv")

out_hypertensive_disorder_file <- paste0( "hypertensive disorder descendants"," ",as.character(Sys.Date()),".csv")

out_dm_file <- paste0( "dm descendants"," ",as.character(Sys.Date()),".csv")
  
  
# set write_additional to TRUE of write the additional files, Set to FALSE if not required
write_additional = TRUE

#set variables
# The ethinc data have the  regions 'Wales' and 'England'
reporting_region <-'England'

# imd score 2019 threshold between 2 and 3
imd_2_3_threshold <- 21.555

# set concept_id vars needed
#pregnancy - condition_occurrence
pregnancy_concept_id <- 4299535


# drug concept_ids
# need labetalol or nifedipine
labetalol_concept_id <- 1386957
nifedipine_concept_id <- 1318853

drug_list <- c(labetalol_concept_id,nifedipine_concept_id)

# Need hypertensive_disorder
hypertensive_disorder_concept_id <- 316866

# Diabetes mellitus (DM)
dm_concept_id <- 201820


#35815052	Number of babies delivered from single pregnancy

num_babies_concept_id <- 35815052

#imd_score
imd_score_concept_id <- 35812882

# exclusion observations
#nifedipine_allergy_concept_id  <- 4166010 #Allergy to nifedipine
#labetalol_allergy_concept_id  <- 4169378	#Allergy to labetalol

#exclude_obs_list<-c(nifedipine_allergy_concept_id,labetalol_allergy_concept_id)


# exclusion conditions

#maternal_tuberculosis_concept_id <- 434416	#Maternal tuberculosis during pregnancy, childbirth and the puerperium (exclude due to issue with nifedipine)

#exclude_co_list<-c(maternal_tuberculosis_concept_id )



# relationships
person_domain_concept_id <- 1147314  #(person table)

co_domain_concept_id <- 1147333 #(condition _occurrence table) mainly as domain_concept_id_1
m_domain_concept_id <- 1147330 #(measurement table)
po_domain_concept_id <- 1147301 # (procedure_occurrence table)
drug_domain_concept_id <- 1147339 # (drug_exposure table)
o_domain_concept_id <- 1147304 # (observation table)
dev_domain_concept_id <- 1147305		#( device_exposure table)

crt_relationship_id <-  46233685 #(condition relevant to )
rco_relationship_id <-  46233684 #(Relevant condition of)

# list for files needed
omop_needed_files <- c("condition_occurrence.tsv","drug_exposure.tsv","fact_relationship.tsv",
                       "person.tsv","observation.tsv")


# list the vocab files needed
vocab_needed_files <- c("CONCEPT.csv","CONCEPT_ANCESTOR.csv")


# get the vocab data needed and then free up memory

# create empty data frame for the vocab dataframe list
vocab_df_list <- list()


# read the data in the folder
for( file_name in vocab_needed_files){
  # only need the csv files
  if (grepl("\\.csv$", file_name)) {
    # print table name
    print(gsub(".csv" ,"" ,file_name))
    # read data into data frame list
    vocab_df_list[[gsub(".csv" ,"" ,file_name)]] <- fread(paste0(vocab_folder,file_name),sep="\t") %>% as.data.frame()
  }
}





# hypertensive_disorder_descendants
hypertensive_descendants_list <- c( hypertensive_disorder_concept_id,  vocab_df_list[["CONCEPT_ANCESTOR"]] %>% filter(ancestor_concept_id == hypertensive_disorder_concept_id ) %>%
                                      select(descendant_concept_id) %>% unlist(use.names = FALSE))

# hypertensive_disorder_df
hypertensive_descendants_df <-  vocab_df_list[["CONCEPT"]] %>% filter( concept_id %in% hypertensive_descendants_list )




# Diabetes mellitus_descendants

dm_descendants_list <- c( dm_concept_id,  vocab_df_list[["CONCEPT_ANCESTOR"]] %>% filter(ancestor_concept_id == dm_concept_id ) %>%
                                      select(descendant_concept_id) %>% unlist(use.names = FALSE))

# dm_disorder_df
dm_descendants_df <-  vocab_df_list[["CONCEPT"]] %>% filter( concept_id %in% dm_descendants_list )



# labetalol descendants
labetalol_descendants_list <- c( labetalol_concept_id , vocab_df_list[["CONCEPT_ANCESTOR"]] %>% filter(ancestor_concept_id  == labetalol_concept_id ) %>%
                                   select(descendant_concept_id) %>% unlist(use.names = FALSE))

# nifedipine_descendants
nifedipine_descendants_list <- c( nifedipine_concept_id , vocab_df_list[["CONCEPT_ANCESTOR"]] %>% filter(ancestor_concept_id  == nifedipine_concept_id ) %>%
                                   select(descendant_concept_id) %>% unlist(use.names = FALSE))





# drug_list_descendants
drug_list_descendants_list <- c( drug_list , vocab_df_list[["CONCEPT_ANCESTOR"]] %>% filter(ancestor_concept_id %in%  drug_list ) %>%
                                   select(descendant_concept_id) %>% unlist(use.names = FALSE))

# drug_list_descendants_df
drug_list_descendants_df <-  vocab_df_list[["CONCEPT"]] %>% filter( concept_id %in% drug_list_descendants_list )


# delete the main vocab data from memeroy 
rm(vocab_df_list)


# clear memory
gc( verbose = T)




# read the omop data

# create empty data frame for the OMOP dataframe list
omop_df_list <- list()


# read the data in the folder
for( file_name in omop_needed_files ){
  # only need the tsv files
  if (grepl("\\.tsv$", file_name)) {
    # print table name
    print(gsub(".tsv" ,"" ,file_name))
    # read data into data frame list
    omop_df_list[[gsub(".tsv" ,"" ,file_name)]] <- fread(paste0(omop_folder,file_name),sep="\t") %>% as.data.frame()
  }
}


# ethnic mapping data
ethnic_mapping_df <- read_excel(paste0(mapping_folder,ethnic_lookup_file))





# get the pregnancy data
preg_df <- omop_df_list[["condition_occurrence"]] %>% filter(condition_concept_id == pregnancy_concept_id )

# eyeball pregnancy data structure
str(preg_df)

# rename columns for merge hypertensive disorder
names(preg_df)[!grepl("person_id", names(preg_df))]  <-  paste0("preg_", names(preg_df)[!grepl("person_id", names(preg_df))])

# eyeball pregnancy data structure
str(preg_df)


# number of unique person_id
length(unique(preg_df$person_id))

# number of unique pregnancies 
length(unique(preg_df$preg_condition_occurrence_id))


# get the pregnant mother data
preg_mum_df <- omop_df_list[["person"]] %>% filter(person_id %in%  preg_df[,'person_id'] )

# eyeball the pregnant mother data
str(preg_mum_df)

# calculate week 11 from start date of pregnancy
#preg_df$preg_week_11_date <- ymd(preg_df$condition_start_date) + weeks(11)


#labetalol_concept_id
#nifedipine_concept_id


# has drugs recorded labetalol
has_labetalol_df <-  omop_df_list[["drug_exposure"]] %>% filter(drug_concept_id %in% labetalol_descendants_list )

# has drugs recorded labetalol or nifedipine 
has_nifedipine_df <-  omop_df_list[["drug_exposure"]] %>% filter(drug_concept_id %in% nifedipine_descendants_list )



# has drugs recorded labetalol or nifedipine 
has_drugs_df <-  omop_df_list[["drug_exposure"]] %>% filter(drug_concept_id %in% drug_list_descendants_list )

#eyeball
str(has_drugs_df)

# has hypertensive disorder recorded
has_hyp_df <- omop_df_list[["condition_occurrence"]] %>% filter(condition_concept_id %in% hypertensive_descendants_list )

# has DM recorded
has_hyp_df <- omop_df_list[["condition_occurrence"]] %>% filter(condition_concept_id %in% hypertensive_descendants_list )


# eyeball
str(has_hyp_df)


# rename columns for merge hypertensive disorder
names(has_hyp_df)[!grepl("person_id", names(has_hyp_df))]  <-  paste0("hyp_", names(has_hyp_df)[!grepl("person_id", names(has_hyp_df))])


# eyeball
str(has_hyp_df)


# has DM recorded
has_dm_df <- omop_df_list[["condition_occurrence"]] %>% filter(condition_concept_id %in% dm_descendants_list )


# rename columns for merge hypertensive disorder
names(has_dm_df)[!grepl("person_id", names(has_hyp_df))]  <-  paste0("dm_", names(has_dm_df)[!grepl("person_id", names(has_dm_df))])



#num_babies_concept_id
has_numb_df <- omop_df_list[["observation"]] %>% filter(observation_concept_id == num_babies_concept_id)

# rename columns for num_babies
names(has_numb_df)[!grepl("person_id", names(has_hyp_df))]  <-  paste0("numb_", names(has_numb_df)[!grepl("person_id", names(has_numb_df))])


#imd_score
has_imds_df <- omop_df_list[["observation"]] %>% filter(observation_concept_id == imd_score_concept_id)

# rename columns for imd_score
names(has_imds_df)[!grepl("person_id", names(has_hyp_df))]  <-  paste0("imds_", names(has_imds_df)[!grepl("person_id", names(has_imds_df))])

# get the drugs required fact relationship
str(omop_df_list[["fact_relationship"]])

# get the fact relationship to link pregnancy as condition relevant to hypertensive_disorder condition
rf_has_hyp_df <- omop_df_list[["fact_relationship"]] %>% 
  filter(domain_concept_id_1 == co_domain_concept_id ) %>%  
  filter(domain_concept_id_2 == co_domain_concept_id ) %>%
  filter(relationship_concept_id == crt_relationship_id ) %>%
  filter( fact_id_2 %in% c( has_hyp_df %>% select(hyp_condition_occurrence_id)  %>% unlist(use.names = FALSE )  ) )

# get the fact relationship to link pregnancy as condition relevant to dm condition
rf_has_dm_df <- omop_df_list[["fact_relationship"]] %>% 
  filter(domain_concept_id_1 == co_domain_concept_id ) %>%  
  filter(domain_concept_id_2 == co_domain_concept_id ) %>%
  filter(relationship_concept_id == crt_relationship_id ) %>%
  filter( fact_id_2 %in% c( has_dm_df %>% select(dm_condition_occurrence_id)  %>% unlist(use.names = FALSE )  ) )

# get the fact relationship to link pregnancy as condition relevant to number babies observation
rf_has_numb_df <- omop_df_list[["fact_relationship"]] %>% 
  filter(domain_concept_id_1 == co_domain_concept_id ) %>%  
  filter(domain_concept_id_2 == o_domain_concept_id ) %>%
  filter(relationship_concept_id == crt_relationship_id ) %>%
  filter( fact_id_2 %in% c( has_numb_df %>% select(numb_observation_id)  %>% unlist(use.names = FALSE )  ) )


# get the fact relationship to link pregnancy as condition relevant to imd score observation
rf_has_imds_df <- omop_df_list[["fact_relationship"]] %>% 
  filter(domain_concept_id_1 == co_domain_concept_id ) %>%  
  filter(domain_concept_id_2 == o_domain_concept_id) %>%
  filter(relationship_concept_id == crt_relationship_id ) %>%
  filter( fact_id_2 %in% c( has_imds_df %>% select(imds_observation_id)  %>% unlist(use.names = FALSE )  ) )



# get the fact relationship to link pregnancy as condition relevant to labetalol
rf_labetalol_df <- omop_df_list[["fact_relationship"]] %>% 
  filter(domain_concept_id_1 == co_domain_concept_id ) %>%  
  filter(domain_concept_id_2 == drug_domain_concept_id ) %>%
  filter(relationship_concept_id == crt_relationship_id ) %>%
  filter( fact_id_2 %in% c(has_labetalol_df %>% select(drug_exposure_id)  %>% unlist(use.names = FALSE)) ) 


# get the fact relationship to link pregnancy as condition relevant to nifedipine 
rf_nifedipine_df <- omop_df_list[["fact_relationship"]] %>% 
  filter(domain_concept_id_1 == co_domain_concept_id ) %>%  
  filter(domain_concept_id_2 == drug_domain_concept_id ) %>%
  filter(relationship_concept_id == crt_relationship_id ) %>%
  filter( fact_id_2 %in% c(has_nifedipine_df %>% select(drug_exposure_id)  %>% unlist(use.names = FALSE)) ) 



# get the fact relationship to link pregnancy as condition relevant to labetalol and nifedipine 
rf_has_drugs_df <- omop_df_list[["fact_relationship"]] %>% 
  filter(domain_concept_id_1 == co_domain_concept_id ) %>%  
  filter(domain_concept_id_2 == drug_domain_concept_id ) %>%
  filter(relationship_concept_id == crt_relationship_id ) %>%
  filter( fact_id_2 %in% c(has_drugs_df %>% select(drug_exposure_id)  %>% unlist(use.names = FALSE)) ) 




# megre preg_mum_df to preg_df to calculate age and excluded any not aged 18+ at conception
preg_link_df <- preg_df %>% inner_join(preg_mum_df ,by=join_by( person_id))

#eyeball preg_link_df
str(preg_link_df)

# remove pergancies under 18
preg_18_link_df <- preg_link_df %>%
  mutate(
    birthday_18 = ymd(birth_datetime) + years(18)
  ) %>%
  filter( preg_condition_start_date >= birthday_18)

# eyeball
str(preg_18_link_df)


# excluded due to age
nrow(preg_link_df)- nrow(preg_18_link_df)


#add condition exclution


# add a column for  hypertension in pregnancy 

# merge the pregnancy condition with fact relationship link to merge with hyp
preg_link_has_hyp_df <- preg_18_link_df %>% 
  inner_join(rf_has_hyp_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_hyp_df %>% select(hyp_condition_occurrence_id  ,hyp_condition_concept_id,hyp_condition_start_date ),by=join_by(fact_id_2 ==hyp_condition_occurrence_id )) %>%
  mutate(hyp_in_preg = if_else(  hyp_condition_start_date >=  preg_condition_start_date &   hyp_condition_start_date <=  preg_condition_end_date,TRUE,FALSE )) %>%
  mutate(hyp_before_preg = if_else(  hyp_condition_start_date <=  preg_condition_start_date ,TRUE,FALSE ))

# eyeball
str(preg_link_has_hyp_df)

# merge the pregnancy condition with fact relationship link to merge with dm
preg_link_has_dm_df <- preg_18_link_df %>% 
  inner_join(rf_has_dm_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_dm_df %>% select(dm_condition_occurrence_id  ,dm_condition_concept_id,dm_condition_start_date ),by=join_by(fact_id_2 ==dm_condition_occurrence_id )) %>%
  mutate(dm_in_preg = if_else(  dm_condition_start_date >=  preg_condition_start_date &   dm_condition_start_date <=  preg_condition_end_date,TRUE,FALSE )) %>%
  mutate(dm_before_preg = if_else(  dm_condition_start_date <=  preg_condition_start_date ,TRUE,FALSE ))

# eyeball
str(preg_link_has_dm_df)


# merge the pregnancy condition with fact relationship link to merge with number babaies
preg_link_has_numb_df <- preg_18_link_df %>% 
  inner_join(rf_has_numb_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_numb_df %>% select(numb_observation_id  ,numb_observation_concept_id,numb_observation_date,numb_value_as_number ),by=join_by(fact_id_2 ==numb_observation_id ))

# eyeball
str(preg_link_has_numb_df)

# merge the pregnancy condition with fact relationship link to merge with imd score
preg_link_has_imds_df <- preg_18_link_df %>% 
  inner_join(rf_has_imds_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_imds_df %>% select(imds_observation_id  ,imds_observation_concept_id,imds_observation_date,imds_value_as_number ),by=join_by(fact_id_2 ==imds_observation_id ))

# eyeball
str(preg_link_has_imds_df)



# merge the pregnancy condition with fact relationship link to merge with nifedipine  and then merge with person table for DOB
preg_link_nifedipine_df <- preg_18_link_df %>% 
  inner_join(rf_nifedipine_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_nifedipine_df %>% select(drug_exposure_id ,drug_concept_id,drug_exposure_start_date ),by=join_by(fact_id_2 ==drug_exposure_id )) %>%
  mutate(nifedipine_in_preg = if_else(  (drug_exposure_start_date >=  preg_condition_start_date )  &   (drug_exposure_start_date <=  preg_condition_end_date),TRUE,FALSE )) %>%
  mutate(nifedipine_in_preg_9 = if_else(  (drug_exposure_start_date >=  preg_condition_start_date )  &   (drug_exposure_start_date <=  preg_condition_end_date + days(9)),TRUE,FALSE )) %>%
  mutate(nifedipine_before_preg = if_else(  drug_exposure_start_date <=  preg_condition_start_date ,TRUE,FALSE ))

#+ days(7)
# eyeball
str(preg_link_nifedipine_df)

# get date first nifedipine in pregnancy
preg_first_n_df <- preg_link_nifedipine_df %>% group_by(preg_condition_occurrence_id,person_id) %>% 
  summarise(min_date = min(drug_exposure_start_date)) %>% as.data.frame()

# eyeball
str(preg_first_n_df)


head(preg_first_n_df)


# merge the pregnancy condition with fact relationship link to merge with labetalol  and then merge with person table for DOB
preg_link_labetalol_df <- preg_18_link_df %>% 
  inner_join(rf_labetalol_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_labetalol_df %>% select(drug_exposure_id ,drug_concept_id,drug_exposure_start_date ),by=join_by(fact_id_2 ==drug_exposure_id )) %>%
  mutate(labetalol_in_preg = if_else(  (drug_exposure_start_date >=  preg_condition_start_date )  &   (drug_exposure_start_date <=  preg_condition_end_date),TRUE,FALSE )) %>%
  mutate(labetalol_in_preg_9 = if_else(  (drug_exposure_start_date >=  preg_condition_start_date   )  &   (drug_exposure_start_date <=  preg_condition_end_date + days(9)),TRUE,FALSE )) %>%
  mutate(labetalol_before_preg = if_else(  drug_exposure_start_date <=  preg_condition_start_date ,TRUE,FALSE ))


  
#+ days(7)


# eyeball
str(preg_link_labetalol_df)

# get date first labetalol in pregnancy
preg_first_l_df <- preg_link_labetalol_df %>% group_by(preg_condition_occurrence_id,person_id) %>% 
  summarise(min_date = min(drug_exposure_start_date)) %>% as.data.frame()

# eyeball
str(preg_first_l_df)


head(preg_first_l_df)

preg_link_labetalol_df %>% filter(drug_exposure_start_date == as.Date('2021-09-11'))

preg_first_l_df %>% filter(person_id == 22670)


# merge the pregnancy condition with fact relationship link to merge with labetalol and nifedipine  and then merge with person table for DOB
preg_link_has_drugs_df <- preg_18_link_df %>% 
  inner_join(rf_has_drugs_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_drugs_df %>% select(drug_exposure_id ,drug_concept_id,drug_exposure_start_date ),by=join_by(fact_id_2 ==drug_exposure_id )) %>%
  mutate(drugs_in_preg = if_else(  (drug_exposure_start_date >=  preg_condition_start_date )  &   (drug_exposure_start_date <=  preg_condition_end_date),TRUE,FALSE )) %>%
  mutate(drugs_in_preg_9 = if_else(  (drug_exposure_start_date >=  preg_condition_start_date )  &   (drug_exposure_start_date <=  preg_condition_end_date + days(9)),TRUE,FALSE )) %>%
  mutate(drugs_before_preg = if_else(  drug_exposure_start_date <=  preg_condition_start_date ,TRUE,FALSE ))




# eyeball
str(preg_link_has_drugs_df)


# get date first labetalol or nifedipine in pregnancy
preg_first_l_n_df <- preg_link_has_drugs_df %>% group_by(preg_condition_occurrence_id,person_id) %>% 
  summarise(min_date = min(drug_exposure_start_date)) %>% as.data.frame()

# eyeball
str(preg_first_l_n_df)




# start building the reporting
preg_18_reporting_df <- preg_18_link_df %>% 
  select(preg_condition_occurrence_id,person_id,preg_condition_start_datetime, preg_condition_end_datetime,birth_datetime,race_concept_id) %>%
  mutate(hyp_recorded = if_else(preg_condition_occurrence_id %in%  preg_link_has_hyp_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  mutate(hyp_in_preg = if_else(preg_condition_occurrence_id %in%  preg_link_has_hyp_df$preg_condition_occurrence_id[preg_link_has_hyp_df$hyp_in_preg == T], TRUE, FALSE ) ) %>%
  mutate(hyp_before_preg = if_else(preg_condition_occurrence_id %in%  preg_link_has_hyp_df$preg_condition_occurrence_id[preg_link_has_hyp_df$hyp_before_preg == T], TRUE, FALSE ) ) %>%
  mutate(l_recorded = if_else(preg_condition_occurrence_id %in%  preg_link_labetalol_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  mutate(l_in_preg = if_else(preg_condition_occurrence_id %in%  preg_link_labetalol_df$preg_condition_occurrence_id[preg_link_labetalol_df$labetalol_in_preg == T], TRUE, FALSE ) ) %>%
  mutate(l_in_preg_9 = if_else(preg_condition_occurrence_id %in%  preg_link_labetalol_df$preg_condition_occurrence_id[preg_link_labetalol_df$labetalol_in_preg_9 == T], TRUE, FALSE ) ) %>%
  mutate(l_before_preg = if_else(preg_condition_occurrence_id %in%  preg_link_labetalol_df$preg_condition_occurrence_id[preg_link_labetalol_df$labetalol_before_preg == T], TRUE, FALSE ) ) %>%
  left_join(preg_link_labetalol_df %>% group_by(preg_condition_occurrence_id) %>% summarise(l_initiated_date=min(drug_exposure_start_date)),by=join_by(preg_condition_occurrence_id==preg_condition_occurrence_id)) %>% 
  mutate(n_recorded = if_else(preg_condition_occurrence_id %in%  preg_link_nifedipine_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  mutate(n_in_preg = if_else(preg_condition_occurrence_id %in%  preg_link_nifedipine_df$preg_condition_occurrence_id[preg_link_nifedipine_df$nifedipine_in_preg == T], TRUE, FALSE ) ) %>%
  mutate(n_in_preg_9 = if_else(preg_condition_occurrence_id %in%  preg_link_nifedipine_df$preg_condition_occurrence_id[preg_link_nifedipine_df$nifedipine_in_preg_9 == T], TRUE, FALSE ) ) %>%
  mutate(n_before_preg = if_else(preg_condition_occurrence_id %in%  preg_link_nifedipine_df$preg_condition_occurrence_id[preg_link_nifedipine_df$nifedipine_before_preg == T], TRUE, FALSE ) ) %>%
  left_join(preg_link_nifedipine_df %>% group_by(preg_condition_occurrence_id) %>% summarise(n_initiated_date=min(drug_exposure_start_date)),by=join_by(preg_condition_occurrence_id==preg_condition_occurrence_id)) %>% 
  mutate(l_n_drugs_recorded = if_else(preg_condition_occurrence_id %in%  preg_link_has_drugs_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  mutate(l_n_drugs_in_preg = if_else(preg_condition_occurrence_id %in%  preg_link_has_drugs_df$preg_condition_occurrence_id[preg_link_has_drugs_df$drugs_in_preg == T], TRUE, FALSE ) ) %>%
  mutate(l_n_drugs_in_preg_9 = if_else(preg_condition_occurrence_id %in%  preg_link_has_drugs_df$preg_condition_occurrence_id[preg_link_has_drugs_df$drugs_in_preg_9 == T], TRUE, FALSE ) ) %>%
  mutate(l_n_drugs_before_preg = if_else(preg_condition_occurrence_id %in%  preg_link_has_drugs_df$preg_condition_occurrence_id[preg_link_has_drugs_df$drugs_before_preg == T], TRUE, FALSE ) ) %>%
  left_join(preg_link_has_drugs_df %>% group_by(preg_condition_occurrence_id) %>% summarise(l_n_drugs_initiated_date=min(drug_exposure_start_date)),by=join_by(preg_condition_occurrence_id==preg_condition_occurrence_id)) %>% 
  mutate(frame_cohort = if_else(( (hyp_recorded==TRUE) &  (l_n_drugs_in_preg==TRUE) ),TRUE,FALSE   ))


#list names
names(preg_18_reporting_df)

# clear memory
gc( verbose = T)

# eyeball some tables
table(preg_18_reporting_df$hyp_recorded)
table(preg_18_reporting_df$hyp_in_preg )
table(preg_18_reporting_df$hyp_before_preg)
table(preg_18_reporting_df$l_n_drugs_recorded)
table(preg_18_reporting_df$l_n_drugs_in_preg )
table(preg_18_reporting_df$l_n_drugs_before_preg)
table(preg_18_reporting_df$frame_cohort)
table(preg_18_reporting_df$n_in_preg )
table(preg_18_reporting_df$l_in_preg )


# check names
names(preg_18_reporting_df)



# generate supporting data
#hyp codes used
omoped_hyp_concepts_df <- preg_link_has_hyp_df %>% group_by(hyp_condition_concept_id) %>% tally() %>% 
  rename(concept_id = hyp_condition_concept_id) %>%
  rename(count_omoped_concept_id = n) %>%
  full_join(hypertensive_descendants_df,,by=join_by(concept_id==concept_id )) %>%
  mutate(count_omoped_concept_id = ifelse(is.na(count_omoped_concept_id) ,0,count_omoped_concept_id ))%>%
  arrange(-count_omoped_concept_id) %>%
  mutate(count_omoped_concept_id = if_else( ((count_omoped_concept_id <10) & ( count_omoped_concept_id >0 ) ), "<10", as.character(count_omoped_concept_id) )  ) 
  



# get the list of omoped 
omoped_drug_concepts_df <- preg_link_has_drugs_df %>% group_by(drug_concept_id) %>% tally() %>% 
  rename(concept_id = drug_concept_id) %>%
  rename(count_omoped_concept_id = n) %>%
  full_join(drug_list_descendants_df,,by=join_by(concept_id==concept_id )) %>%
  mutate(count_omoped_concept_id = ifelse(is.na(count_omoped_concept_id) ,0,count_omoped_concept_id ))%>%
  arrange(-count_omoped_concept_id) %>%
  mutate(count_omoped_concept_id = if_else( ((count_omoped_concept_id <10) & ( count_omoped_concept_id >0 ) ), "<10", as.character(count_omoped_concept_id) )  ) 


# overall numbers
res_df <- data.frame(item='number of pregnancies',item_value = nrow(preg_df))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+',item_value = nrow(preg_18_link_df)))
res_df <- res_df %>% bind_rows(data.frame(item='number of hypertensive disorder concept_ids in vocab',item_value = nrow(hypertensive_descendants_df)))
res_df <- res_df %>% bind_rows(data.frame(item='number of hypertensive disorder concept_ids in OMPED data',item_value =length(unique(preg_link_has_hyp_df[["hyp_condition_concept_id"]]))))
res_df <- res_df %>% bind_rows(data.frame(item='number of labetalol or nifedipine concept_ids in vocab',item_value = nrow(drug_list_descendants_df)))
res_df <- res_df %>% bind_rows(data.frame(item='number of labetalol or nifedipiner concept_ids in OMPED data',item_value =length(unique(preg_link_has_drugs_df[["drug_concept_id"]]))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with hypertensive disorder code ever recorded ',item_value =nrow(preg_18_reporting_df %>% filter(hyp_recorded ==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with hypertensive disorder code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(hyp_in_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with hypertensive disorder code recorded before pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(hyp_before_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol code ever recorded ',item_value =nrow(preg_18_reporting_df %>% filter(l_recorded ==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(l_in_preg==TRUE)),notes ='When a drug given is recoded as days after birth or days from EDD, therefore  date calculated with either truncated date of birth (1st, 8th, 15th, 22nd of the month ),  EDD derived for admission data if available  or truncated EED date from  intrapartum and postnatal Information (1st of the month).  The calculated drug given date is compared with truncated date of birth to indicated if given within pregnancy but there will be time errors due to truncated dates'))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol code recorded during pregnancy plus 9 days',item_value =nrow(preg_18_reporting_df %>% filter(l_in_preg_9==TRUE)),notes ='When a drug given is recoded as days after birth or days from EDD, therefore  date calculated with either truncated date of birth (1st, 8th, 15th, 22nd of the month ),  EDD derived for admission data if available  or truncated EED date from  intrapartum and postnatal Information (1st of the month).  The calculated drug given date is compared with truncated date of birth plus 9 days to indicated if given within pregnancy but there will be time errors due to truncated dates'))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol code recorded before pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(l_before_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with nifedipine code ever recorded ',item_value =nrow(preg_18_reporting_df %>% filter(n_recorded ==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with nifedipine code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(n_in_preg==TRUE)),notes ='When a drug given is recoded as days after birth or days from EDD, therefore  date calculated with either truncated date of birth (1st, 8th, 15th, 22nd of the month ),  EDD derived for admission data if available  or truncated EED date from  intrapartum and postnatal Information (1st of the month).  The calculated drug given date is compared with truncated date of birth to indicated if given within pregnancy but there will be time errors due to truncated dates'))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with nifedipine code recorded during pregnancy plus 9 days',item_value =nrow(preg_18_reporting_df %>% filter(n_in_preg_9==TRUE)),notes ='When a drug given is recoded as days after birth or days from EDD, therefore  date calculated with either truncated date of birth (1st, 8th, 15th, 22nd of the month ),  EDD derived for admission data if available  or truncated EED date from  intrapartum and postnatal Information (1st of the month).  The calculated drug given date is compared with truncated date of birth plus 9 days to indicated if given within pregnancy but there will be time errors due to truncated dates'))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with nifedipine code recorded before pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(n_before_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol or nifedipine code ever recorded ',item_value =nrow(preg_18_reporting_df %>% filter(l_n_drugs_recorded ==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol or nifedipine code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(l_n_drugs_in_preg==TRUE)),notes ='When a drug given is recoded as days after birth or days from EDD, therefore  date calculated with either truncated date of birth (1st, 8th, 15th, 22nd of the month ),  EDD derived for admission data if available  or truncated EED date from  intrapartum and postnatal Information (1st of the month).  The calculated drug given date is compared with truncated date of birth to indicated if given within pregnancy but there will be time errors due to truncated dates'))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol or nifedipine code recorded during pregnancy plus 9 days',item_value =nrow(preg_18_reporting_df %>% filter(l_n_drugs_in_preg_9==TRUE)),notes ='When a drug given is recoded as days after birth or days from EDD, therefore  date calculated with either truncated date of birth (1st, 8th, 15th, 22nd of the month ),  EDD derived for admission data if available  or truncated EED date from  intrapartum and postnatal Information (1st of the month).  The calculated drug given date is compared with truncated date of birth plus 9 days to indicated if given within pregnancy but there will be time errors due to truncated dates'))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol or nifedipine code recorded before pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(l_n_drugs_before_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with with hypertensive disorder code ever recorded and  labetalol or nifedipine code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(frame_cohort==TRUE))))

res_df<-  res_df %>% mutate(notes = ifelse(is.na(notes), "", notes))

# ,notes ='When a drug driven when  is recoded as days after birth or days from EDD, therefore  date calculated with either truncated date of birth (1st, 8th, 15th, 22nd of the month ),  EDD derived for admission data if available  or truncated EED date from  intrapartum and postnatal Information (1st of the month).  The calculated drug given date is compared with truncated date of birth to indicated if given within pregnancy but there will be time errors due to truncated dates'

# create  column_classes_list 
column_classes_list <- sapply(preg_18_reporting_df, function(x) paste(class(x), collapse = ", "))

# Create a new data frame with results
info_vars_df <- data.frame( column = names(column_classes_list), class = unname(column_classes_list))






# build dataframe for  reporting number


# reporting  for labetalol
l_in_preg_reporting_df <-  preg_18_reporting_df %>% filter(l_in_preg==TRUE) %>%  
  select(person_id,preg_condition_occurrence_id,preg_condition_start_datetime,l_in_preg) %>% as.data.frame()

#eyeball numbers
nrow(l_in_preg_reporting_df )


# reporting  for nifedipine
n_in_preg_reporting_df <-  preg_18_reporting_df %>% filter(n_in_preg==TRUE) %>% 
  select(person_id,preg_condition_occurrence_id,preg_condition_start_datetime,n_in_preg) %>% as.data.frame()

#eyeball numbers
nrow(n_in_preg_reporting_df )


# reporting  for labetalol or nifedipine
l_n_in_preg_reporting_df <- preg_18_reporting_df %>% filter(l_n_drugs_in_preg==TRUE)%>% 
  select(person_id,preg_condition_occurrence_id,preg_condition_start_datetime,l_n_drugs_in_preg) %>% as.data.frame()

#eyeball numbers
nrow(l_n_in_preg_reporting_df )

# get the ethnic groups needed
str(ethnic_mapping_df)

# get the mireda observation set 1 data for the ethnic groups
wanted_eth_concept_id_df <- ethnic_mapping_df %>% filter(coding_group=='mireda observation set 1') %>%
  filter(region==reporting_region) %>%  select(ons_ethnic_5_group,concept_id) %>% distinct() %>% as.data.frame()

# get the eth 5 observations
eth_5_df <- omop_df_list[["observation"]] %>% 
  filter(observation_concept_id %in% wanted_eth_concept_id_df[,'concept_id']) %>%
  inner_join(wanted_eth_concept_id_df,by=join_by(observation_concept_id==concept_id)) %>%
  select(person_id,ons_ethnic_5_group) %>% as.data.frame()

# join the ethic data


# for labetalol merge ethic_5
# reporting  for labetalol 
l_in_preg_reporting_df <-  preg_18_reporting_df %>% filter(l_recorded==TRUE)%>% 
  select(person_id,preg_condition_occurrence_id,preg_condition_start_datetime,l_in_preg)%>% 
  left_join(eth_5_df,by=join_by(person_id)) %>%
  mutate(ons_ethnic_5_group = ifelse(is.na(ons_ethnic_5_group), "MISSING", ons_ethnic_5_group)) %>% 
  mutate(has_dm = if_else(preg_condition_occurrence_id %in%  preg_link_has_dm_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  left_join(unique(preg_link_has_numb_df[,c("person_id","preg_condition_occurrence_id","numb_value_as_number")]),by=join_by(person_id,preg_condition_occurrence_id))  %>%
  left_join(preg_link_has_imds_df[,c("person_id","preg_condition_occurrence_id","imds_value_as_number")] %>% distinct(),by=join_by(person_id,preg_condition_occurrence_id)) %>%
  left_join(preg_first_l_df,by=join_by(person_id,preg_condition_occurrence_id)) %>%
  mutate(singleton = ifelse(numb_value_as_number == 1, TRUE, FALSE)) %>%
  mutate(imd_groups = ifelse(imds_value_as_number < imd_2_3_threshold, "IMD Q3-5", "IMD Q1-2")) %>%
  mutate(imd_groups = ifelse(is.na(imd_groups), "IMD MISSING", imd_groups)) %>%
  mutate(gest_age_drug = as.numeric(difftime(min_date  ,preg_condition_start_datetime, units = c("weeks")) )) %>%
  as.data.frame()


nrow(l_in_preg_reporting_df)
names(l_in_preg_reporting_df)
  
table(l_in_preg_reporting_df['ons_ethnic_5_group'])
table(l_in_preg_reporting_df['has_dm'])
table(l_in_preg_reporting_df['singleton'])
table(l_in_preg_reporting_df['imd_groups'],useNA = "ifany")
mean(l_in_preg_reporting_df$gest_age_drug,na.rm = TRUE)
summary(l_in_preg_reporting_df$gest_age_drug,na.rm = TRUE)

boxplot(l_in_preg_reporting_df$gest_age_drug,
        main = "Gestational age at Labetalol drug exposure for pregnancy",
        ylab = "Gestational age",
        col = "lightblue")



l_res_df <- data.frame(item='Drug',item_value = 'Labetalol')
#l_res_df <- l_res_df %>% bind_rows(data.frame(item='drug',item_value = 'Labetalol'))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='Number of pregnancies',item_value = toString( nrow(l_in_preg_reporting_df))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='WHITE pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'WHITE')))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='BLACK OR BLACK BRITISH pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'BLACK OR BLACK BRITISH')))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='ASIAN OR ASIAN BRITISH pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'ASIAN OR ASIAN BRITISH')))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='MIXED / MULTIPLE ETHNIC GROUP pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'MIXED / MULTIPLE ETHNIC GROUP')))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='OTHER ETHNIC GROUPS pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'OTHER ETHNIC GROUPS')))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='MISSING ETHNICITY pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'MISSING')))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='Singleton pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(singleton == TRUE)))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='Multiple pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(singleton == FALSE)))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='Diabetes present in pregnancy',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(has_dm == FALSE)))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='Average gestational age at initiation of medicationt in pregnancy',item_value = toString(round( mean(l_in_preg_reporting_df$gest_age_drug,na.rm = TRUE), digits = 2) )))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='IMD quintiles 1 to 2 (most deprived neighbourhoods) pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(imd_groups == "IMD Q1-2")))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='IMD quintiles 3 to 5 pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(imd_groups == "IMD Q3-5")))))
l_res_df <- l_res_df %>% bind_rows(data.frame(item='IMD quintile missing pregnancies',item_value = toString( nrow(l_in_preg_reporting_df %>% filter(imd_groups== "IMD MISSING")))))





# for nifedipine merge ethic_5
# reporting  for  nifedipine
n_in_preg_reporting_df <-  preg_18_reporting_df %>% filter(n_recorded==TRUE) %>% 
  select(person_id,preg_condition_occurrence_id,preg_condition_start_datetime,n_in_preg) %>%
  left_join(eth_5_df,by=join_by(person_id)) %>%
  mutate(ons_ethnic_5_group = ifelse(is.na(ons_ethnic_5_group), "MISSING", ons_ethnic_5_group)) %>%
  mutate(has_dm = if_else(preg_condition_occurrence_id %in%  preg_link_has_dm_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  left_join(unique(preg_link_has_numb_df[,c("person_id","preg_condition_occurrence_id","numb_value_as_number")]),by=join_by(person_id,preg_condition_occurrence_id)) %>%
  left_join(preg_link_has_imds_df[,c("person_id","preg_condition_occurrence_id","imds_value_as_number")] %>% distinct(),by=join_by(person_id,preg_condition_occurrence_id)) %>%
  left_join(preg_first_n_df,by=join_by(person_id,preg_condition_occurrence_id)) %>%
  mutate(singleton = ifelse(numb_value_as_number == 1, TRUE, FALSE)) %>%
  mutate(imd_groups = ifelse(imds_value_as_number < imd_2_3_threshold, "IMD Q3-5", "IMD Q1-2")) %>%
  mutate(imd_groups = ifelse(is.na(imd_groups), "IMD MISSING", imd_groups)) %>%
  mutate(gest_age_drug = as.numeric(difftime(min_date  ,preg_condition_start_datetime, units = c("weeks")) )) %>%
  as.data.frame()

table(n_in_preg_reporting_df['ons_ethnic_5_group'])
table(n_in_preg_reporting_df['has_dm'])
table(n_in_preg_reporting_df['singleton'])
table(n_in_preg_reporting_df['imd_groups'],useNA = "ifany")
mean(n_in_preg_reporting_df$gest_age_drug,na.rm = TRUE)
summary(n_in_preg_reporting_df$gest_age_drug,na.rm = TRUE)


boxplot(n_in_preg_reporting_df$gest_age_drug,
        main = "Gestational age at Nifedipine drug exposure for pregnancy",
        ylab = "Gestational age",
        col = "lightgreen")

n_res_df <- data.frame(item='Drug',item_value = 'Nifedipine')
n_res_df <- n_res_df %>% bind_rows(data.frame(item='Number of pregnancies',item_value = toString( nrow(n_in_preg_reporting_df))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='WHITE pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'WHITE')))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='BLACK OR BLACK BRITISH pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'BLACK OR BLACK BRITISH')))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='ASIAN OR ASIAN BRITISH pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'ASIAN OR ASIAN BRITISH')))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='MIXED / MULTIPLE ETHNIC GROUP pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'MIXED / MULTIPLE ETHNIC GROUP')))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='OTHER ETHNIC GROUPS pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'OTHER ETHNIC GROUPS')))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='MISSING ETHNICITY pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(ons_ethnic_5_group == 'MISSING')))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='Singleton pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(singleton == TRUE)))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='Multiple pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(singleton == FALSE)))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='Diabetes present in pregnancy',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(has_dm == FALSE)))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='Average gestational age at initiation of medicationt in pregnancy',item_value = toString( round(mean(n_in_preg_reporting_df$gest_age_drug,na.rm = TRUE), digits = 2) )))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='IMD quintiles 1 to 2 (most deprived neighbourhoods) pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(imd_groups == "IMD Q1-2")))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='IMD quintiles 3 to 5 pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(imd_groups == "IMD Q3-5")))))
n_res_df <- n_res_df %>% bind_rows(data.frame(item='IMD quintile missing pregnancies',item_value = toString( nrow(n_in_preg_reporting_df %>% filter(imd_groups== "IMD MISSING")))))




# for labetalol or nifedipine merge ethic_5
# reporting  for labetalol or nifedipine
l_n_in_preg_reporting_df <- preg_18_reporting_df %>% filter(l_n_drugs_recorded==TRUE)%>% 
  select(person_id,preg_condition_occurrence_id,preg_condition_start_datetime,l_n_drugs_in_preg) %>%
  left_join(eth_5_df,by=join_by(person_id)) %>%
  mutate(ons_ethnic_5_group = ifelse(is.na(ons_ethnic_5_group), "MISSING", ons_ethnic_5_group)) %>%
  mutate(has_dm = if_else(preg_condition_occurrence_id %in%  preg_link_has_dm_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  left_join(unique(preg_link_has_numb_df[,c("person_id","preg_condition_occurrence_id","numb_value_as_number")]),by=join_by(person_id,preg_condition_occurrence_id)) %>%
  left_join(preg_link_has_imds_df[,c("person_id","preg_condition_occurrence_id","imds_value_as_number")] %>% distinct(),by=join_by(person_id,preg_condition_occurrence_id)) %>%
  left_join(preg_first_l_n_df,by=join_by(person_id,preg_condition_occurrence_id)) %>%
  mutate(singleton = ifelse(numb_value_as_number == 1, TRUE, FALSE)) %>%
  mutate(imd_groups = ifelse(imds_value_as_number < imd_2_3_threshold, "IMD Q3-5", "IMD Q1-2")) %>%
  mutate(imd_groups = ifelse(is.na(imd_groups), "IMD MISSING", imd_groups)) %>%
  mutate(gest_age_drug = as.numeric(difftime(min_date  ,preg_condition_start_datetime, units = c("weeks")) )) %>%
  
  as.data.frame()



names(l_n_in_preg_reporting_df)

l_n_in_preg_reporting_df$gest_age_drug[l_n_in_preg_reporting_df$person_id == 22670]


table(l_n_in_preg_reporting_df['ons_ethnic_5_group'])
table(l_n_in_preg_reporting_df['has_dm'])
table(l_n_in_preg_reporting_df['singleton'])
table(l_n_in_preg_reporting_df['imd_groups'], useNA = "ifany")
mean(l_n_in_preg_reporting_df$gest_age_drug,na.rm = TRUE)


summary(l_n_in_preg_reporting_df$gest_age_drug,na.rm = TRUE)

boxplot(l_n_in_preg_reporting_df$gest_age_drug,
        main = "Gestational age at Labetalol or Nifedipine drug exposure for pregnancy",
        ylab = "Gestational age",
        col = "lightyellow")



## rename columns 
names(l_res_df) <- c('item','Labetalol')
names(n_res_df) <- c('item','Nifedipine')

# merge results
out_l_n_res <- l_res_df   %>% inner_join(n_res_df,by=join_by(item))

names(hypertensive_descendants_df)
str(hypertensive_descendants_df)
str(has_dm_df)

#get the dm info
out_dm_descendants_df <- dm_descendants_df %>%
  mutate(is_in_omop_data = ifelse(concept_id %in% has_dm_df$dm_condition_concept_id, 1, 0))
  
table(out_dm_descendants_df$is_in_omop_data)


out_hypertensive_descendants_df <- hypertensive_descendants_df %>%
  mutate(is_in_omop_data = ifelse(concept_id %in% has_hyp_df$hyp_condition_concept_id, 1, 0))

table(out_hypertensive_descendants_df$is_in_omop_data)

# write the FRAME cohorot data
write.table(preg_18_reporting_df, paste0( out_folder, out_file), row.names=FALSE, sep=",", na="")



# check if additional files needed
if ( write_additional == TRUE) {
  
  # write the varibale information
  write.table(info_vars_df, paste0( out_folder, out_codebook_file), row.names=FALSE, sep=",", na="")
  
  
  # write summary data
  write.table(res_df, paste0( out_folder, out_summary_file), row.names=FALSE, sep=",", na="")
  
  # write the FRAME numbers data need for the FRAME excel file
  write.table(out_l_n_res, paste0( out_folder, out_numbers_file), row.names=FALSE, sep=",", na="")
  
  
  # write hypertensive disorder voca data
  write.table(omoped_hyp_concepts_df, paste0( out_folder, out_hyp_file), row.names=FALSE, sep=",", na="")
  
  # write hypertensive disorder voca data
  write.table(omoped_drug_concepts_df, paste0( out_folder, out_drug_file ), row.names=FALSE, sep=",", na="")
  
  # write hypertensive disorder voca data
  write.table(out_hypertensive_descendants_df, paste0( out_folder, out_hypertensive_disorder_file ), row.names=FALSE, sep=",", na="")
  
  # write DM vocab data
  write.table(out_dm_descendants_df, paste0( out_folder, out_dm_file ), row.names=FALSE, sep=",", na="")
  
}

