#' @title generate EML using Assemblyline and content from Excel workbook.
#' 
#' 
generate_EML_Assemblyline <- function(project_path, excel_input,dataset_id_input) {

#run the get_meta_xlsx function
entity<-excel_input$entities

#test to see if there is data table or other entity
if (length(entity[entity$entitytype=="dataTable","filename"])!=0) {datatable_present=1} else {datatable_present=0}
if (length(entity[entity$entitytype=="otherEntity","filename"])!=0) {otherentity_present=1} else {otherentity_present=0}

#create a template
eal_inputs <- EMLassemblyline::template_arguments(
  empty = T, 
  data.path = project_path,
  data.table = if (datatable_present==1) {entity[entity$entitytype=="dataTable","filename"]} else {NULL},
  other.entity = if (otherentity_present==1){entity[entity$entitytype=="otherEntity","filename"]} else {NULL}
)

#dataset level
eal_inputs$dataset.title <- excel_input$dataset$title
eal_inputs$data.path = eal_inputs$eml.path <- project_path
eal_inputs$maintenance.description <- 'Completed'
eal_inputs$package.id <- excel_input$dataset$packageid
# some packages don't have the scope 
if (!is.na(excel_input$dataset$scope)) {eal_inputs$user.domain<-excel_input$dataset$scope}
eal_inputs$user.id <- excel_input$dataset$userid



#data table
if (datatable_present==1){
  eal_inputs$data.table <- entity[entity$entitytype=="dataTable","filename"]
  eal_inputs$data.table.name <- entity[entity$entitytype=="dataTable","entityname"]
  eal_inputs$data.table.description <- entity[entity$entitytype=="dataTable","entitydescription"]
  eal_inputs$data.table.quote.character  <- entity[entity$entitytype=="dataTable","quotecharacter"]
  if (!is.na(entity[entity$entitytype=="dataTable","dataTableUrl"][1])) {eal_inputs$data.table.url <- entity[entity$entitytype=="dataTable","dataTableUrl"]}

  entities_order <- excel_input$entities$entity_position
  
  for (i in entities_order) {
    f <- paste0(
      "attributes_",
      tools::file_path_sans_ext(excel_input$entities$filename[excel_input$entities$entity_position==i]),
      ".txt")
    
    attribute <- excel_input$attributes[excel_input$attributes$entity_position==i,]
    
    eal_inputs$x$template[[f]]$content <- data.frame(
      attributeName = as.character(attribute$attributeName),
      attributeDefinition = as.character(attribute$attributeDefinition),
      class = as.character(attribute$class),
      unit = as.character(attribute$unit),
      dateTimeFormatString = as.character(attribute$formatString),
      missingValueCode = as.character(attribute$missingValueCode),
      missingValueCodeExplanation = as.character(attribute$missingValueCodeExplanation),
      stringsAsFactors = F)
  }
  
  # categorical data
  if (nrow(excel_input$factors)>0) {
  for (j in entities_order) {
    f <- paste0(
      "catvars_",
      tools::file_path_sans_ext(excel_input$entities$filename[excel_input$entities$entity_position==j]),
      ".txt")
    
    category <- excel_input$factors[excel_input$factors$entity_position==j,]
    
    eal_inputs$x$template[[f]]$content <- data.frame(
      attributeName = as.character(category$attributeName),
      code = as.character(category$code),
      definition = as.character(category$definition),
      stringsAsFactors = F)
    }
  }
  
  #customize unit
  if (nrow(excel_input$unit)>0) {
  eal_inputs$x$template$custom_units.txt$content <- data.frame(
    id = as.character(excel_input$unit$id),
    unitType = as.character(excel_input$unit$unitType),
    parentSI = as.character(excel_input$unit$parentSI),
    multipliertoSI= as.character(excel_input$unit$multiplierToSI),
    description = as.character(excel_input$unit$description),
    stringsAsFactors = F)
  }
  
}

# other entity
if (otherentity_present==1){
  eal_inputs$other.entity <- entity[entity$entitytype=="otherEntity","filename"]
  eal_inputs$other.entity.name <- entity[entity$entitytype=="otherEntity","entityname"]
  eal_inputs$other.entity.description <- entity[entity$entitytype=="otherEntity","entitydescription"]
  if (!is.na(entity[entity$entitytype=="otherEntity","dataTableUrl"][1])) {eal_inputs$other.entity.url <-entity[entity$entitytype=="otherEntity","dataTableUrl"]}
}

# geography
eal_inputs$x$template$geographic_coverage.txt$content <- data.frame(
  geographicDescription = as.character(excel_input$geo$geographicdescription),
  northBoundingCoordinate = as.character(excel_input$geo$northboundingcoordinate),
  eastBoundingCoordinate = as.character(excel_input$geo$eastboundingcoordinate),
  southBoundingCoordinate = as.character(excel_input$geo$southboundingcoordinate),
  westBoundingCoordinate = as.character(excel_input$geo$westboundingcoordinate),
  stringsAsFactors = F)

#temporal
eal_inputs$temporal.coverage <- as.character(excel_input$dataset[,c("temporal_begindate","temporal_enddate")])

# Create a directory with your abstract, methods
# template_arguments()
texttypes <- EMLassemblyline::template_arguments(path = project_path)
eal_inputs$x$template$abstract.docx$content <- texttypes$x$template$abstract.docx$content
eal_inputs$x$template$methods.docx$content <- texttypes$x$template$methods.docx$content

# For ccby license only
eal_inputs$x$template$intellectual_rights.txt$content<- EML::set_TextType(
  file = system.file("/templates/intellectual_rights_ccby4.0.txt", package = "EMLassemblyline"))

## if we want cc0 license 
# eal_inputs$x$template$intellectual_rights.txt$content<- EML::set_TextType(
#   file = system.file("/templates/intellectual_rights_cc0.txt", package = "EMLassemblyline"))

# if user wants to read in the word documents, uncomment the code below.
#eal_inputs$x$template$abstract.txt$content <- EML::set_TextType(file = paste0(project_path,excel_input$dataset$abstract))
#eal_inputs$x$template$methods.txt$content <- EML::set_methods(methods_file = paste0(project_path,excel_input$dataset$methodDocument))
#eal_inputs$x$template$intellectual_rights.txt$content <-EML::set_TextType(file = paste0(folder_path,"EML_generation/Shared_document/IntellectualRights.docx"))
  
#keyword
eal_inputs$x$template$keywords.txt$content <- data.frame(
  keyword = as.character(excel_input$keyword$keyword),
  keywordThesaurus = as.character(excel_input$keyword$keyword_thesaurus),
  stringsAsFactors = F)

#personnel
eal_inputs$x$template$personnel.txt$content <- data.frame(
  givenName = as.character(excel_input$creator$givenname),
  middleInitial= as.character(excel_input$creator$givenname2),
  surName = as.character(excel_input$creator$surname),
  organizationName = as.character(excel_input$creator$organization),
  electronicMailAddress= as.character(excel_input$creator$email),
  userId= as.character(excel_input$creator$orcid),
  role=as.character(excel_input$creator$authorshiprole),
  projectTitle = as.character(excel_input$creator$projectTitle),
  fundingAgency = as.character(""),
  fundingNumber = as.character(excel_input$creator$fundingNumber),
  stringsAsFactors = F)


# #this section is added here because EMLAssemblyline has not been able to accommondate this at this point. 
# # once the package get improved, we will modify this section
eal_inputs$return.obj=T
eal_inputs$write.file <-F

if (otherentity_present==1) {

  erl_input_modify <-do.call(make_eml, eal_inputs[names(eal_inputs) %in% names(formals(make_eml))])

en_num <- nrow(entity[entity$entitytype=="otherEntity",])

  for (et in 1:en_num) {

    en_name <- erl_input_modify$dataset$otherEntity[[et]]$physical$objectName

    erl_input_modify$dataset$otherEntity[[et]]$physical$dataFormat$externallyDefinedFormat$formatName <-entity[entity$entitytype=="otherEntity"&entity$filename==en_name,"formatname"]
    erl_input_modify$dataset$otherEntity[[et]]$entityType <-entity[entity$entitytype=="otherEntity"&entity$filename==en_name,"typename"]
  }
} else {
erl_input_modify <-do.call(make_eml, eal_inputs[names(eal_inputs) %in% names(formals(make_eml))])
}

return(erl_input_modify)

}
