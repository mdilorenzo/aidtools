#' A Function for Converting Documents to the AidData Flagship template (or any template, really)
#'
#' This function is basically just pandoc but in R. Accordingly, it requires that pandoc is installed.
#' @param input_file Name of document to convert.
#' @param output_file Desired name of output file.
#' @param reference_file Name of .dotx file to use as a reference for output style.
#' @keywords 
#' @export
#' @examples
#' pcomms()


pcomms <- function(input_file,
                   output_file,
                   reference_file = ""){
  
  ref_command <- ifelse(nchar(reference_file) > 0,
                        "--reference-docx=",
                        "")
  
  system_command <- paste0("pandoc ",
                           ref_command,
                           reference_file,
                           " ",
                           input_file,
                           " -o ",
                           output_file)
  
  system(system_command)
  
}

