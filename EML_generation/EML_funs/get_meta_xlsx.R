#' @title Connect to excel and query metadata.
#' 
#' 
get_meta_xlsx <- function(folder_path=folder_path,dataset_id=dataset_id) {

wb= meta_path = paste0(folder_path,"Metadata/Metadata.xlsx")

boilerplate <-  as.data.frame(read_xlsx (wb, sheet = "Boilerplate", na="") )
  
dataset <- as.data.frame(read_xlsx (wb, sheet = "DataSet",na="")) %>%
  mutate(packageid=ifelse(!is.na(alternatedid),paste0(alternatedid,".",version),NA),
         temporal_begindate=as.character(temporal_begindate),
         temporal_enddate=as.character(temporal_enddate)) %>%
  filter(datasetid==dataset_id) %>%
  left_join(boilerplate,by="bp_setting")

attributes <- as.data.frame(read_xlsx (wb, sheet = "DataSetAttributes",na="")) %>%
  filter(datasetid==dataset_id)

factors<- as.data.frame(read_xlsx (wb, sheet = "DataSetAttributeCodeDefinition",na=""))%>%
  filter(datasetid==dataset_id)

unit_raw <- as.data.frame(read_xlsx(wb, sheet = "ListUnitDictionary",na=""))

unit <- as.data.frame(attributes) %>%
  select(datasetid,unit) %>%
  distinct() %>%
  filter(!is.na(unit)) %>%
  rename(id=unit) %>%
  left_join(unit_raw,by="id")

creator_raw <- as.data.frame(read_xlsx  (wb, sheet = "DataSetPersonnel", na=""))%>%
  filter(datasetid==dataset_id)

people <- as.data.frame(read_xlsx  (wb, sheet = "ListPeople", na=""))

creator <-dataset %>%
  filter(datasetid==dataset_id) %>%
  select(project_PI,project_funding_title,project_funding_agency,project_funding_code) %>%
  rename(peopleid=project_PI,projectTitle=project_funding_title,fundingAgency=project_funding_agency,fundingNumber=project_funding_code) %>%
  mutate(authorshiprole="PI") %>%
  bind_rows(creator_raw) %>%
  filter(!is.na(peopleid)) %>%
  left_join(people,by="peopleid") 
  
keyword <- as.data.frame(read_xlsx  (wb, sheet = "DataSetKeywords", na=""))%>%
  filter(datasetid==dataset_id)

filetype <- as.data.frame(read_xlsx  (wb, sheet = "ListFileType", na=""))

entities <-as.data.frame(read_xlsx  (wb, sheet = "DataSetEntities", na=""))%>%
  left_join(filetype, by="filetype") %>%
  mutate(dataTableUrl=ifelse(!is.na(urlpath),paste0(urlpath,filename),NA))%>%
  filter(datasetid==dataset_id)

geo <- as.data.frame(read_xlsx  (wb, sheet = "DataSetSites", na="")) %>%
  filter(datasetid==dataset_id)
 
 # short names order has to match order of expected views
 dfs <- list(
    attributes=attributes,
    factors=factors,
    unit=unit,
    creator=creator,
    keyword=keyword,
    entities=entities,
    dataset=dataset,
    geo=geo
  )
  
  
  return(dfs)
}
