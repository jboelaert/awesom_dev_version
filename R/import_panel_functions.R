
#' Import .csv/txt data
#'
#'Based on data.table::fread
#' We should create seperate functions that operate outside of shiny to not use the datapath thing
#' @param input_dataFile shiny-related dataFile object 
#' @param input_header character indicating if header is in file. one of c("auto", "TRUE", "FALSE")
#' @param input_sep  chraracter indicating which seperator. one of  c("auto", "Comma", "Semicolon", "Tab", "Space")
#' @param input_quote character indicating quotations. one of c("None", "Double Quote \", "Single Quote \", )
#' @param input_dec character.... on of c("Period", "Comma")
#' @param input_encoding file encoding. on of c("unknown", "UTF-8", "Latin-1")
#' @param input_dataFile_datapath datapath
#'
#' @return dataframe object of imported data
#'
#' @examples
ok.data.function.csv.txt <- function(input_dataFile, input_header, input_sep, input_quote, input_dec, input_encoding,
                                     input_dataFile_datapath){
   if (is.null((input_dataFile))){ 
    return(NULL)
  }
  
  the.header <- switch(input_header, "Auto"= "auto", 
                       "Header"= TRUE, "No Header"= FALSE)
  the.sep <- switch(input_sep, "Auto"= "auto", 
                    "Comma ','"=",", "Semicolon ';'"=";", 
                    "Tab"="\t", "Space"=" ")
  the.quote <- switch(input_quote, "None"= "",
                      "Double Quote \""= '"',
                      "Single Quote '"= "'")
  the.dec <- switch(input_dec, 'Period "."'=".", 'Comma ","'=",")
  the.encoding <- switch(input_encoding, "unknown" = "unknown", 
                         "UTF-8" = "UTF-8", "Latin-1" = "Latin-1")
  
  data_read_reproducible <- paste0("ok.data <- data.table::fread('", 
                                   input_dataFile$name, "', ",
                                    "header = '", the.header, 
                                    "', sep ='", the.sep, 
                                    "', quote = '", the.quote, 
                                    "', dec = '", the.dec, 
                                    "', stringsAsFactors = TRUE, encoding = '", 
                                   the.encoding, "')\n")

  data <- try(data.frame(data.table::fread(input_dataFile_datapath, 
                                           header=the.header, sep=the.sep, 
                                           quote=the.quote, dec=the.dec, 
                                           stringsAsFactors=T,
                                           encoding = the.encoding)))
  if(class(data) == "try-error"){ return(NULL)}
  return(list(data, data_read_reproducible))
}


#' Import .xlsx data
#' based on read_excel {readxl}
#' @param input_dataFile shiny-related dataFile object 
#' @param input_column_names TRUE to use the first row as column names, FALSE to get default names
#' @param input_trim_spaces Should leading and trailing whitespace be trimmed?
#' @param input_range_specified_bol boolean to indicate if range should be specified
#' @param input_range_specs A cell range to read from. See documentation ?read_xlsx
#' @param input_worksheet_specified_bol boolean to indicate if worksheet should be specified
#' @param input_worksheet_specs Sheet to read. See documentation ?read_xlsx
#' @param input_dataFile_datapath datapath
#' @param input_rows_to_skip Minimum number of rows to skip before reading anything. Default is 0
#'
#' @return
#' @export
#'
#' @examples
ok.data.function.excel_xlsx <- function(input_dataFile, input_column_names, input_trim_spaces, input_range_specified_bol,
                                        input_range_specs, input_worksheet_specified_bol,  input_worksheet_specs,
                                        input_dataFile_datapath, input_rows_to_skip){
  if (is.null((input_dataFile))){ 
    return(NULL)
  }
  library(readxl)
  the.header <- switch(input_column_names, "TRUE"= TRUE, 
                       "FALSE" = FALSE)
  the.trim_spaces <- switch(input_trim_spaces, "TRUE"= TRUE, 
                            "FALSE" = FALSE)
  the.range <- NULL
  if(input_range_specified_bol == TRUE & input_range_specs != "") the.range <- input_range_specs
  the.sheet <- NULL
  if(input_worksheet_specified_bol == TRUE & input_worksheet_specs != "") the.sheet <- input_worksheet_specs
  
  
  data_read_reproducible <- paste0("ok.data <- data.frame(read_xlsx('", 
                                    input$dataFile$name, "', ",
                                    "col_names = '", the.header, 
                                    "', range ='", the.range, 
                                    "', sheet = '", the.sheet, 
                                    "', trim_ws = '", the.trim_spaces, 
                                  
                                    "skip =", input_rows_to_skip,
                                    
                                    ")\n")
  
  
  
  
  
  
  data <- try(data.frame(read_xlsx(input_dataFile_datapath,
                                   col_names = the.header,
                                   range = the.range,
                                   sheet= the.sheet,
                                   trim_ws = the.trim_spaces,
                                   skip = input_rows_to_skip)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, data_read_reproducible))
  
  
}


#' Import .xls data
#'based on read_xls
#' @param input_dataFile  shiny-related dataFile object 
#' @param input_column_names_xls TRUE to use the first row as column names, FALSE to get default names
#' @param input_trim_spaces_xls Should leading and trailing whitespace be trimmed?
#' @param input_range_specified_bol_xls boolean to indicate if range should be specified
#' @param input_range_specs_xls A cell range to read from. See documentation ?read_xls
#' @param input_worksheet_specified_bol_xls boolean to indicate if worksheet should be specified
#' @param input_worksheet_specs_xls Sheet to read. See documentation ?read_xls
#' @param input_dataFile_datapath datapath
#' @param input_rows_to_skip_xls Minimum number of rows to skip before reading anything. Default is 0
#'
#' @return
#' @export
#'
#' @examples
ok.data.function.excel_xls <- function(input_dataFile, input_column_names_xls, input_trim_spaces_xls, input_range_specified_bol_xls,
                                       input_range_specs_xls, input_worksheet_specified_bol_xls, input_worksheet_specs_xls,
                                       input_dataFile_datapath, input_rows_to_skip_xls){
  if (is.null((input_dataFile))){ 
    return(NULL)
  }
  library(readxl)
  the.header <- switch(input_column_names_xls, "TRUE"= TRUE, 
                       "FALSE" = FALSE)
  the.trim_spaces <- switch(input_trim_spaces_xls, "TRUE"= TRUE, 
                            "FALSE" = FALSE)
  the.range <- NULL
  if(input_range_specified_bol_xls == TRUE & input_range_specs_xls != "") the.range <- input_range_specs_xls
  the.sheet <- NULL
  if(input_worksheet_specified_bol_xls == TRUE & input_worksheet_specs_xls != "") the.sheet <- input_worksheet_specs_xls
  
  
  data_read_reproducible <- paste0("ok.data <- data.frame(read_xls('", 
                                   input$dataFile$name, "', ",
                                   "col_names = '", the.header, 
                                   "', range ='", the.range, 
                                   "', sheet = '", the.sheet, 
                                   "', trim_ws = '", the.trim_spaces, 
                                   
                                   "skip =", input_rows_to_skip_xls,
                                   
                                   ")\n")
  
  
  
  
  data <- try(data.frame(read_xls(input_dataFile_datapath,
                                  col_names = the.header,
                                  range = the.range,
                                  sheet= the.sheet,
                                  trim_ws = the.trim_spaces,
                                  skip = input_rows_to_skip_xls)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, data_read_reproducible))
}






#' Import SPSS data
#'based on read_spss
#' @param input_dataFile shiny-related dataFile object 
#' @param input_dataFile_datapath datapath
#'
#' @return
#'
#' @examples
ok.data.function.spss <- function(input_dataFile, input_dataFile_datapath){
  if (is.null((input_dataFile))){ 
    return(NULL)
  }
  library(haven)
  the.spss.encoding <- NULL
  
  
  data_read_reproducible <- paste0("ok.data <- data.frame(read_spss('", 
                                   input$dataFile$name, "'",
                                   
                                   ")\n")
  
  
  data <- try(data.frame(read_spss(file = input_dataFile_datapath))) #<- encoding as an argument cannot be matched for some reason
  if(class(data) == "try-error") return(NULL)
  return(list(data, data_read_reproducible))
  
}




#' Import Stata data
#' bvased on read.dta
#' @param input_dataFile   shiny-related dataFile object 
#' @param input_dataFile_datapath datapath
#'
#' @return
#'
#' @examples
ok.data.function.stata <- function(input_dataFile, input_dataFile_datapath){
  if (is.null((input_dataFile))){ 
    return(NULL)
  }
  
  data_read_reproducible <- paste0("ok.data <- data.frame(read.dta('", 
                                   input$dataFile$name, "'",
                                   ")\n")
  
  
  data <- try(data.frame(read.dta(file = input_dataFile_datapath)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, data_read_reproducible))
  
  
  
}


#' Import SAS data
#'based on read_sas
#' @param input_dataFile 
#' @param input_dataFile_datapath 
#' @return
#'
#' @examples
ok.data.function.sas.data <- function(input_dataFile, input_dataFile_datapath ){
                                     
  if (is.null((input_dataFile))){ 
    return(NULL)
  }
    
  
    data_read_reproducible <- paste0("ok.data <- data.frame(read_sas('", 
                                   input$dataFile$name, "'",
                                   ")\n")
  
    
    data <- try(data.frame(read_sas(data_file = input_dataFile_datapath)))
    if(class(data) == "try-error") return(NULL)
    return(list(data, data_read_reproducible))
  
}


