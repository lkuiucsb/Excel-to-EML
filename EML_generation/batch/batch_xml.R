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

dataset <- as.data.frame(read_xlsx (wb, sheet = "DataSet",na=""))
meta <- as.data.frame(read_xlsx (wb, sheet = "DataSetAttributes",na=""))
fact<- as.data.frame(read_xlsx (wb, sheet = "EMLAttributeCodeDefinition",na=""))

unit_raw <- as.data.frame(read_xlsx (wb, sheet = "EMLUnitDictionary",na=""))

unit <- as.data.frame(meta) %>%
  select(datasetid,unit) %>%
  distinct() %>%
  filter(!is.na(unit)) %>%
  rename(id=unit) %>%
  left_join(unit_raw,by="id")

creator <- as.data.frame(read_xlsx  (wb, sheet = "DataSetPersonnel", na=""))
keyword <- as.data.frame(read_xlsx  (wb, sheet = "DataSetKeywords", na=""))

filetype <- as.data.frame(read_xlsx  (wb, sheet = "FileTypeList", na=""))

entities <-as.data.frame(read_xlsx  (wb, sheet = "DataSetEntities", na=""))%>%
  left_join(filetype, by="filetype")

method <- as.data.frame(read_xlsx  (wb, sheet = "DataSetMethods", na=""))

geo <- as.data.frame(read_xlsx  (wb, sheet = "DataSetSites", na=""))

tempo <- as.data.frame(read_xlsx (wb, sheet = "DataSetTemporal", na="") )


