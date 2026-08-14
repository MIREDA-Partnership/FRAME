# FRAME Cohort Identification from OMOP CDM core

<!--![FRAME Logo](FRAME_logo.jpg)-->

The following scripts have the purpose of using the MIREDA OMOP CDM from duckdb to create a new cohort for the FRAME study.
If the following R packages are not already installed on your system, you should install them now:
- **roxygen2**
- **tidyverse**
- **duckdb**
- **DBI**
- **dbplyr**
- **dtplyr**
- **data.table**
- **writexl**
- **bit64**
- **writexl**

The scripts run based on using an Rproj file for FRAME or, at least, having set the working directory.

---

## Files:
- 0_myomoptools.R
- 1_FRAME_setup.R
- 2_FRAME_drug_identification.R
- 3_FRAME_duckdb_creation.R
- 4_FRAME_cohort_selection.R

The files are numbered in order but if you have already made all the changes to files as recommended below than, as long as you have 
already installed the myomoptools package from file 0, you can run script 4 alone. It will run the previous scrips 1-3 using the source()
command. 

---

## 0_myomoptools.R

This script creates a local package, **myomoptools** of useful OMOP CDM functions that are used in some of the following script.
- Ensure **RTools** is installed on your device before running this script,
- Change the **pkg_path** to the location you wish to save the package,
then run the whole script.

It will write the necessary files for the package in the location specified and then use roxygen2 to create the package. It can 
then be called in R using the usual commands e.g. **library(myomoptools)**.

You only need to run the script once.

## 1_FRAME_setup.R

This will check if the myomoptools package is already in its designated file location. If it isn't, it will run the myomoptools.R
script to install it.
  - Edit package file destination if needed
  - Edit myomoptools.R destination before running if different to that shown.

It loads the required package libraries for this script:
- **DBI**
- **duckdb**
- **myomoptools** (from previous file).

Next it will set up a connection to your OMOP duckdb.
- Edit **db_path** to match the location of your OMOP duckdb path.

Finally, the script will set up an R object in your global environment called 'cdm'. This will be a list of lazy tables contained
within your OMOP duckdb so that they can be called using the format: **cdm$condition_occurrence** for example.

## 2_FRAME_drug_identification.R

If you want to run this file alone, you will have to ensure the file 1 is already edited and uncomment the source() line in this script.
If you have already just run file 1, this one will run as-is. 

It uses the Athena OMOP vocabularies in your duckdb to identify all possible standard code descendants of the RxNorm ingredients 
'nifedipine' and 'labetalol'.The script then filters out results leaving only the concept_id for the ingredients AND products which 
contain only a single active ingredient and are oral drug dosage forms.

## 3_FRAME_duckdb_creation.R

If you want to run this file alone, you will have to ensure the files 1 and 2 are already edited and **uncomment lines 11 & 14**, the 
source() lines, in this script.
If you have already just run files 1 and 2, this one will run without uncommenting the source() lines. 

**Line 198** writes an exclusion file. Change this if you want to it written elsewhere.

The script will take your MIREDA cohort and and successively filter out different components of the cohort leaving the list of mothers eligible to 
be in the FRAME cohort. It adds helper fields and sets up a base table of variables for the cohort. Exclusions are recorded and written to an xlsx 
output file. All duckdb tables for the mothers identified and the babies born to those pregnancies will be written to a new FRAME duckdb. This will 
include the Athena vocabulary tables. 

You will need to amend the details of your ** target_db_path** and **src_con** on **lines 291 & 292** to reflect your own MIREDA duckdb location and 
the intended location of the new FRAME duckdb.

## 4_FRAME_cohort_selection.R

This will add additional fields that are important for the matching and propensity scoring file that will come later.

Again this contains source files for the above which can be uncommented to run them all if you have not run them previously. But if you 
have been updating one file at a time, then you can leave commented and run this script.

The script assumes you still have the target_db_path in your global environment from running file 3. If this has been removed, you will need to add it 
manually before running the script.

No other changes should be necessary unless you wish to change the name of the list of lazy tables for this duckdb. This is currently set to 'frame'. 
This can me amended in **line 462**.
