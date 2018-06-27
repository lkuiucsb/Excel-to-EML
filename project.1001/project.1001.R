 rm(list = ls())

#------------------------------------
#read the batch file and load all the libraries
 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 
setwd("../EML_generation/")
source("batch/batch_xml.R")

#_----------------------------------
#generate EML
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

datasetid=1001

datatable1<-datatablefun(datasetid       = datasetid,
                         entity          = 1)
#----------------------------------------------
#generate EML
eml<-datasetfun(datasetid       = datasetid,
                dataTable       = datatable1)

eml_validate(eml)


write_eml(eml, "XML_1001.xml")
