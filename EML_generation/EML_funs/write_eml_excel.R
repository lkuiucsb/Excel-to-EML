#' @title write_eml function, different from the EML:: write_eml
#' This function is not neccessary and could be combine with the R script in each project folder.
#' We keep it here in case sometimes the Assemblyline can't make chance 
#' and this is the place to edit the EML before generating it. 
#' 
#' 
write_eml_excel <- function(eml_input) {
  eml_input$write.file <-T
  do.call(make_eml, eml_input[names(eml_input) %in% names(formals(make_eml))])

}
