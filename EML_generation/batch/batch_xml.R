# batch for all the EML generation

library(EML)
library(rmarkdown)
library(dplyr)
library(data.table)
library(tools)
library(methods)
library(readxl)

#loading all the functions
source("EML_funs/datatable.r")
source("EML_funs/dataset.r")

# input excel
wb <-"../Metadata/Metadata.xlsx"

dataset <- read_xlsx (wb, sheet = "DataSet",na="")
meta <- read_xlsx (wb, sheet = "DataSetAttributes",na="")
fact<- read_xlsx (wb, sheet = "EMLAttributeCodeDefinition",na="")

unit_raw <- read_xlsx (wb, sheet = "EMLUnitDictionary",na="")

unit <- meta %>%
  select(datasetid,unit) %>%
  distinct() %>%
  filter(!is.na(unit)) %>%
  rename(id=unit) %>%
  left_join(unit_raw,by="id")

creator <- read_xlsx  (wb, sheet = "DataSetPersonnel", na="")
keyword <- read_xlsx  (wb, sheet = "DataSetKeywords", na="")

filetype <- read_xlsx  (wb, sheet = "FileTypeList", na="")

entities <-read_xlsx  (wb, sheet = "DataSetEntities", na="")%>%
  left_join(filetype, by="filetype")

method <- read_xlsx  (wb, sheet = "DataSetMethods", na="")

geo <- read_xlsx  (wb, sheet = "DataSetSites", na="")

tempo <- read_xlsx (wb, sheet = "DataSetTemporal", na="") 


