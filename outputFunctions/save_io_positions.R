# Output function that saves the found optimal positions of the pieces
# Input: x - an object of class pieceSelection containing the optimization results
Positionen_speichern <- function(x){
  # open a dialog to choose file destination
  types <- c("csv", "xlsx", "txt", "RData")
  path <- tclvalue(tkgetSaveFile(
    filetypes = paste0(paste0("{ {", types, " ", getText("exp_files"), "} {.", 
                              types, "} }", collapse = " "), " { {", 
                       getText("exp_allFiles"), "} * }"), 
    defaultextension = ".csv"))
  chosenType <- gsub(".*[.]([^.]*)$", "\\1", path)
  
  if(nchar(path) > 0)
    switch(chosenType,
           "csv" = write.csv(x$`piecePositions`, path, row.names = TRUE),
           "xlsx" = {getPackage("writexl")
             write_xlsx(x$`piecePositions`, path)},
           "txt" = write.table(x$`piecePositions`, path, row.names = TRUE),
           "RData" = save(x$`piecePositions`, file = path))
}