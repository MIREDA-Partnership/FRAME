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

omop_folder = "B:/BRC_Elixir/Durbaba- MIREDA/test OMOP/OMOP CMD/2026-06-01/"

vocab_folder = "B:/BRC_Elixir/Durbaba- MIREDA/OMOP vocabulary/bundle OMOP vocabulary_download_v5_2026-05-21/"

mapping_folder = "B:/BRC_Elixir/Durbaba- MIREDA/test OMOP/mapping/OMOPed/"

ethnic_lookup_file = "Ethnicities_long_format_OMOP.xlsx"

out_folder = "B:/BRC_Elixir/Durbaba- MIREDA/test OMOP/FRAME OMOP/version 1/"  


#Set file names#

script_version = 3


#main data file
out_file = paste0("FRAME mothers data for cohort section v",as.character(script_version),".csv")

# additional data files

out_summary_file =  paste0( "FRAME mothers r script v",as.character(script_version)," summary for cohort section.csv")

out_codebook_file =  paste0( "FRAME mothers r script v",as.character(script_version)," codebook for cohort section.csv")

out_hyp_file =  paste0( "FRAME mothers hypertensive disorder vocab use v",as.character(script_version),".csv")

out_drug_file =  paste0( "FRAME mothers labetalol or nifedipine vocab use v",as.character(script_version),".csv")

# set write_additional to TRUE of write the additional files, Set to FALSE if not required
write_additional = TRUE

# read the data


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
omop_needed_files <- c("condition_occurrence.tsv","drug_exposure.tsv","fact_relationship.tsv","person.tsv")


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


# read the vocab data





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

# eyeball
str(has_hyp_df)

# rename columns for merge hypertensive disorder
names(has_hyp_df)[!grepl("person_id", names(has_hyp_df))]  <-  paste0("hyp_", names(has_hyp_df)[!grepl("person_id", names(has_hyp_df))])


# eyeball
str(has_hyp_df)

# get the drugs required fact relationship
str(omop_df_list[["fact_relationship"]])

# get the fact relationship to link pregnancy as condition relevant to hypertensive_disorder condition
rf_has_hyp_df <- omop_df_list[["fact_relationship"]] %>% 
  filter(domain_concept_id_1 == co_domain_concept_id ) %>%  
  filter(domain_concept_id_2 == co_domain_concept_id ) %>%
  filter(relationship_concept_id == crt_relationship_id ) %>%
  filter( fact_id_2 %in% c( has_hyp_df %>% select(hyp_condition_occurrence_id)  %>% unlist(use.names = FALSE )  ) )

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

# merge the pregnancy condition with fact relationship link to merge with nifedipine  and then merge with person table for DOB
preg_link_nifedipine_df <- preg_18_link_df %>% 
  inner_join(rf_nifedipine_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_nifedipine_df %>% select(drug_exposure_id ,drug_concept_id,drug_exposure_start_date ),by=join_by(fact_id_2 ==drug_exposure_id )) %>%
  mutate(nifedipine_in_preg = if_else(  (drug_exposure_start_date >=  preg_condition_start_date )  &   (drug_exposure_start_date <=  preg_condition_end_date),TRUE,FALSE )) %>%
  mutate(nifedipine_before_preg = if_else(  drug_exposure_start_date <=  preg_condition_start_date ,TRUE,FALSE ))


# eyeball
str(preg_link_nifedipine_df)


# merge the pregnancy condition with fact relationship link to merge with labetalol  and then merge with person table for DOB
preg_link_labetalol_df <- preg_18_link_df %>% 
  inner_join(rf_labetalol_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_labetalol_df %>% select(drug_exposure_id ,drug_concept_id,drug_exposure_start_date ),by=join_by(fact_id_2 ==drug_exposure_id )) %>%
  mutate(labetalol_in_preg = if_else(  (drug_exposure_start_date >=  preg_condition_start_date )  &   (drug_exposure_start_date <=  preg_condition_end_date),TRUE,FALSE )) %>%
  mutate(labetalol_before_preg = if_else(  drug_exposure_start_date <=  preg_condition_start_date ,TRUE,FALSE ))


# eyeball
str(preg_link_labetalol_df)


# merge the pregnancy condition with fact relationship link to merge with labetalol and nifedipine  and then merge with person table for DOB
preg_link_has_drugs_df <- preg_18_link_df %>% 
  inner_join(rf_has_drugs_df,by=join_by(preg_condition_occurrence_id==fact_id_1)) %>%
  inner_join(has_drugs_df %>% select(drug_exposure_id ,drug_concept_id,drug_exposure_start_date ),by=join_by(fact_id_2 ==drug_exposure_id )) %>%
  mutate(drugs_in_preg = if_else(  (drug_exposure_start_date >=  preg_condition_start_date )  &   (drug_exposure_start_date <=  preg_condition_end_date),TRUE,FALSE )) %>%
  mutate(drugs_before_preg = if_else(  drug_exposure_start_date <=  preg_condition_start_date ,TRUE,FALSE ))


# eyeball
str(preg_link_has_drugs_df)


# start building the reporting
preg_18_reporting_df <- preg_18_link_df %>% 
  select(preg_condition_occurrence_id,person_id,preg_condition_start_datetime, preg_condition_end_datetime,birth_datetime,race_concept_id) %>%
  mutate(hyp_recorded = if_else(preg_condition_occurrence_id %in%  preg_link_has_hyp_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  mutate(hyp_in_preg = if_else(preg_condition_occurrence_id %in%  preg_link_has_hyp_df$preg_condition_occurrence_id[preg_link_has_hyp_df$hyp_in_preg == T], TRUE, FALSE ) ) %>%
  mutate(hyp_before_preg = if_else(preg_condition_occurrence_id %in%  preg_link_has_hyp_df$preg_condition_occurrence_id[preg_link_has_hyp_df$hyp_before_preg == T], TRUE, FALSE ) ) %>%
  mutate(l_recorded = if_else(preg_condition_occurrence_id %in%  preg_link_labetalol_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  mutate(l_in_preg = if_else(preg_condition_occurrence_id %in%  preg_link_labetalol_df$preg_condition_occurrence_id[preg_link_labetalol_df$labetalol_in_preg == T], TRUE, FALSE ) ) %>%
  mutate(l_before_preg = if_else(preg_condition_occurrence_id %in%  preg_link_labetalol_df$preg_condition_occurrence_id[preg_link_labetalol_df$labetalol_before_preg == T], TRUE, FALSE ) ) %>%
  left_join(preg_link_labetalol_df %>% group_by(preg_condition_occurrence_id) %>% summarise(l_initiated_date=min(drug_exposure_start_date)),by=join_by(preg_condition_occurrence_id==preg_condition_occurrence_id)) %>% 
  mutate(n_recorded = if_else(preg_condition_occurrence_id %in%  preg_link_nifedipine_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  mutate(n_in_preg = if_else(preg_condition_occurrence_id %in%  preg_link_nifedipine_df$preg_condition_occurrence_id[preg_link_nifedipine_df$nifedipine_in_preg == T], TRUE, FALSE ) ) %>%
  mutate(n_before_preg = if_else(preg_condition_occurrence_id %in%  preg_link_nifedipine_df$preg_condition_occurrence_id[preg_link_nifedipine_df$nifedipine_before_preg == T], TRUE, FALSE ) ) %>%
  left_join(preg_link_nifedipine_df %>% group_by(preg_condition_occurrence_id) %>% summarise(n_initiated_date=min(drug_exposure_start_date)),by=join_by(preg_condition_occurrence_id==preg_condition_occurrence_id)) %>% 
  mutate(l_n_drugs_recorded = if_else(preg_condition_occurrence_id %in%  preg_link_has_drugs_df$preg_condition_occurrence_id, TRUE, FALSE ) ) %>%
  mutate(l_n_drugs_in_preg = if_else(preg_condition_occurrence_id %in%  preg_link_has_drugs_df$preg_condition_occurrence_id[preg_link_has_drugs_df$drugs_in_preg == T], TRUE, FALSE ) ) %>%
  mutate(l_n_drugs_before_preg = if_else(preg_condition_occurrence_id %in%  preg_link_has_drugs_df$preg_condition_occurrence_id[preg_link_has_drugs_df$drugs_before_preg == T], TRUE, FALSE ) ) %>%
  left_join(preg_link_has_drugs_df %>% group_by(preg_condition_occurrence_id) %>% summarise(l_n_drugs_initiated_date=min(drug_exposure_start_date)),by=join_by(preg_condition_occurrence_id==preg_condition_occurrence_id)) %>% 
  mutate(frame_cohort = if_else(( (hyp_recorded==TRUE) &  (l_n_drugs_in_preg==TRUE) ),TRUE,FALSE   ))


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
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(l_in_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol code recorded before pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(l_before_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with nifedipine code ever recorded ',item_value =nrow(preg_18_reporting_df %>% filter(n_recorded ==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with nifedipine code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(n_in_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with nifedipine code recorded before pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(n_before_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol or nifedipine code ever recorded ',item_value =nrow(preg_18_reporting_df %>% filter(l_n_drugs_recorded ==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol or nifedipine code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(l_n_drugs_in_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with labetalol or nifedipine code recorded before pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(l_n_drugs_before_preg==TRUE))))
res_df <- res_df %>% bind_rows(data.frame(item='number of pregnancies aged 18+ with with hypertensive disorder code ever recorded and  labetalol or nifedipine code recorded during pregnancy',item_value =nrow(preg_18_reporting_df %>% filter(frame_cohort==TRUE))))


# create  column_classes_list 
column_classes_list <- sapply(preg_18_reporting_df, function(x) paste(class(x), collapse = ", "))

# Create a new data frame with results
info_vars_df <- data.frame( column = names(column_classes_list), class = unname(column_classes_list))

# write the FRAME chorot date
write.table(preg_18_reporting_df, paste0( out_folder, out_file), row.names=FALSE, sep=",", na="")

# check if additional files needed
if ( write_additional == TRUE) {

# write the varibale information
write.table(info_vars_df, paste0( out_folder, out_codebook_file), row.names=FALSE, sep=",", na="")


# write summary data
write.table(res_df, paste0( out_folder, out_summary_file), row.names=FALSE, sep=",", na="")

# write hypertensive disorder voca data
write.table(omoped_hyp_concepts_df, paste0( out_folder, out_hyp_file), row.names=FALSE, sep=",", na="")

# write hypertensive disorder voca data
write.table(omoped_drug_concepts_df, paste0( out_folder, out_drug_file ), row.names=FALSE, sep=",", na="")


}


# end test
