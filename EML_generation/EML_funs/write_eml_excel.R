#' @title write_eml function, different from the EML:: write_eml
#' 
#' 
write_eml_excel <- function(eml_input,write_path) {
  
  message(paste0("Writing EML documents..."))
  emld::eml_version("eml-2.2.0")
  EML::write_eml(eml_input,write_path)
  txt <- readLines(write_path, encoding = "UTF-8")
  invisible(
    lapply(
      seq_along(txt),
      function(x) {
        txt[x] <<- stringr::str_replace_all(txt[x], "&amp;gt;", "&gt;")
        txt[x] <<- stringr::str_replace_all(txt[x], "&amp;lt;", "&lt;")
        txt[x] <<- stringr::str_replace_all(txt[x], "&amp;amp;", "&amp;")
      }))
  writeLines(txt, write_path, useBytes = T)
}
