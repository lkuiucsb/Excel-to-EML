rm(list = ls())

#------------------------------------
#read the batch file and load all the libraries
library(EMLassemblyline)
library(XML)
library(readxl)
library(dplyr)

#####user edit zone

#change the following number based on each of the datasets
dataset_id=99

folder_path<- "/Users/lkui/Desktop/Excel_to_EML_Assembly/"
#end user edit zone###########
######################################
#loading all the functions
source(paste0(folder_path,'EML_generation/EML_funs/get_meta_xlsx.R'))
source(paste0(folder_path,'EML_generation/EML_funs/generate_EML_Assemblyline.R'))

#read the metadata content out of xlsx
metadata <- get_meta_xlsx(folder_path=folder_path,dataset_id=dataset_id)

#fill the EML content into the template
eml_in_template <- generate_EML_Assemblyline(project_path= paste0(folder_path,"project.",dataset_id,"/"),
                                             excel_input=metadata,
                                             dataset_id_input=dataset_id)

# Export EML --------------------------------------------------------------------
EML::write_eml(eml_in_template, paste0(folder_path,"project.",dataset_id,"/", metadata$dataset$packageid, ".xml"))
