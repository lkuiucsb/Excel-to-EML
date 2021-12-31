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

if (!is.na(excel_input$dataset$maintenance)) {eal_inputs$maintenance.description <- excel_input$dataset$maintenance}

# some packages don't have the scope or domain
if (!is.na(excel_input$dataset$packageid)) {eal_inputs$package.id <- excel_input$dataset$packageid}
if (!is.na(excel_input$dataset$userid)) {eal_inputs$user.id <- excel_input$dataset$userid}
if (!is.na(excel_input$dataset$scope)) {eal_inputs$user.domain<-excel_input$dataset$scope}

#data table
if (datatable_present==1){
  eal_inputs$data.table <- entity[entity$entitytype=="dataTable","filename"]
  eal_inputs$data.table.name <- entity[entity$entitytype=="dataTable","entityname"]
  eal_inputs$data.table.description <- entity[entity$entitytype=="dataTable","entitydescription"]
  eal_inputs$data.table.quote.character  <- entity[entity$entitytype=="dataTable","quotecharacter"]
  eal_inputs$data.table.url <- entity[entity$entitytype=="dataTable","dataTableUrl"]
  
  entities_order <- excel_input$entities$entity_position[excel_input$entities$entitytype=="dataTable"]
  
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
  eal_inputs$other.entity.url <-entity[entity$entitytype=="otherEntity","dataTableUrl"]
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

#Borrow the read_txt code from Assemblyline to overcome issue with txt file##############
read_txt <- function(f) {
  if (tools::file_ext(f) == "txt") {
    # .txt is not well supported by EML::set_TextType(). Reading the .txt
    # file as character strings and parsing to paragraphs works better.
    para <- as.list(
      unlist(
        stringr::str_split(
          readr::read_file(f), 
          pattern = "(\r\r)|(\n\n)|(\r\n\r\n)")))
  } else if (tools::file_ext(f) == "md") {
    # TODO: Update this section when the EML R library has better support 
    # for markdown
    if (stringr::str_detect(
      basename(f), 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "text") & (attr_tmp$template_name == "methods")],
        collapse = "|"))) {
      txt <- EML::set_methods(f)
      txt <- list(
        methodStep = list(
          description = txt$methodStep$description))
      return(txt)
    } else {
      para <- as.list(
        unlist(
          stringr::str_split(
            readr::read_file(f),
            pattern = "(\r\r)|(\n\n)|(\r\n\r\n)")))
    }
  } else if (tools::file_ext(f) == "docx") {
    # .docx is not well supported by EML::set_TextType() but a 
    # refactoring of some of this funcions underlying code improves 
    # performance.
    docbook <- to_docbook(f)
    use_i <- stringr::str_detect(
      xml2::xml_name(xml2::xml_children(docbook)), 
      "sect")
    if (any(use_i)) {
      # To simplify parsing, if <section> is present then section titles 
      # and children <para> will be flattened into <para>
      xpath <- paste0(
        "/article/", 
        unique(xml2::xml_name(xml2::xml_children(docbook))[use_i]))
      xml <- xml2::xml_new_root("article")
      xml2::xml_add_child(xml, "title")
      lapply(
        xml2::xml_find_all(docbook, xpath),
        function(m) {
          lapply(
            xml2::xml_children(m),
            function(n) {
              xml2::xml_set_name(n, "para")
              xml2::xml_add_child(xml, n)
            })
        })
      para <- emld::as_emld(xml)$para
    } else {
      para <- emld::as_emld(to_docbook(f))$para
    }
  }
  # Adjust outputs to match unique structure of abstract, additional_info, 
  # intellectual_rights, and methods nodes. Use default EML::set_TextType() 
  # output when NULL otherwise an asynchronous process will occur between 
  # between "templates" and "tfound" objects.
  if (stringr::str_detect(
    basename(f), 
    paste(
      attr_tmp$regexpr[
        (attr_tmp$type == "text") & (attr_tmp$template_name == "methods")],
      collapse = "|"))) {
    if (is.null(para)) {
      txt <- EML::set_methods(f)
    } else {
      txt <- list(
        methodStep = list(
          description = list(
            para = para)))
    }
  } else {
    if (is.null(para)) {
      txt <- EML::set_TextType(f)
    } else {
      txt <- list(
        section = list(),
        para = para)
    }
  }
  txt
}


read_template_attributes <- function() {
  data.table::fread(
    system.file(
      '/templates/template_characteristics.txt',
      package = 'EMLassemblyline'),
    fill = TRUE,
    blank.lines.skip = TRUE)
}

attr_tmp <- read_template_attributes()
###############################################3

# For ccby license only
if (excel_input$dataset$intellectual_right =="CCBY") {
eal_inputs$x$template$intellectual_rights.txt$content<- read_txt(
  system.file("/templates/intellectual_rights_ccby4.0.txt", package = "EMLassemblyline"))}

## if we want cc0 license 
if (excel_input$dataset$intellectual_right =="CC0") {
 eal_inputs$x$template$intellectual_rights.txt$content<- read_txt(
   system.file("/templates/intellectual_rights_cc0.txt", package = "EMLassemblyline"))}

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
  fundingAgency = as.character(excel_input$creator$fundingAgency),
  fundingNumber = as.character(excel_input$creator$fundingNumber),
  stringsAsFactors = F)

return(eal_inputs)

}
