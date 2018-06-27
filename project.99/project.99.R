 rm(list = ls())

#------------------------------------
#read the batch file and load all the libraries
 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 
setwd("../EML_generation/")
source("batch/batch_xml.R")

#_----------------------------------
#generate EML
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

datasetid=99

datatable1<-datatablefun(datasetid       = datasetid,
                         entity          = 1)
datatable2<-datatablefun(datasetid       = datasetid,
                         entity          = 2)
datatable3<-datatablefun(datasetid       = datasetid,
                         entity          = 3)
otherentity1<-datatablefun(datasetid      = datasetid,
                          entity          = 4)
#----------------------------------------------
#generate EML
eml<-datasetfun(datasetid       = datasetid,
                dataTable       = c(datatable1,datatable2,datatable3),
                otherEntity     = otherentity1)

eml_validate(eml)


write_eml(eml, "XML_99.xml")

