# Daten_speichern - Saves the inputted and transformed data into .csv, .txt or .RData
# Input: x - an object of class pieceSelection containing the optimization results
Daten_speichern <- function(x){
  # open a dialog to choose file destination
  types <- c("csv", "xlsx", "txt", "RData")
  path <- tclvalue(tkgetSaveFile(
    filetypes = paste0(paste0("{ {", types, " ", getText("exp_files"), "} {.", 
                              types, "} }", collapse = " "), " { {", 
                       getText("exp_allFiles"), "} * }"), 
    defaultextension = ".csv"))
  chosenType <- gsub(".*[.]([^.]*)$", "\\1", path)
  
  if(nchar(path) > 0){
    # Force all x$data (a list of dataframes) into one matrix
    dat <- matrix(NA, ncol = 2 * length(x$data), nrow = max(sapply(x$data, nrow)))
    colnames(dat) <- sapply(x$data, colnames)
    for(i in seq(length(x$data))){
      dat[seq(nrow(x$data[[i]])), c(2 * i - 1, 2 * i)] <- x$data[[i]]
    }
    
    
    switch(chosenType,
           "csv" = write.csv(dat, file = path, fileEncoding = "UTF8", row.names = FALSE),
           "xlsx" = {getPackage("writexl")
             write_xlsx(as.data.frame(dat), path = path)},
           "txt" = write.table(dat, file = path, fileEncoding = "UTF8", row.names = FALSE),
           "RData" = save(dat, file = path))
  }
}