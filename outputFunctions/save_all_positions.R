# Output function that saves the found optimal positions of the pieces

Positionen_speichern <- function(x){
  # open a dialog to choose file destination
  types <- c("csv", "xlsx", "txt", "RData")
  path <- tclvalue(tkgetSaveFile(
    filetypes = paste0(paste0("{ {", types, " ", getText("exp_files"), "} {.", 
                              types, "} }", collapse = " "), " { {", 
                       getText("exp_allFiles"), "} * }"), 
    defaultextension = ".csv"))
  chosenType <- gsub(".*[.]([^.]*)$", "\\1", path)
  
  if(length(x$piecePositions) != 0){
  	niO <- cbind(c("start", round(x$piecePositions, 4)[,2]), c(round(x$piecePositions, 4)[,1], "end"))
   	iO <- rbind(round(x$piecePositions, 4), c("NA", "NA"))
	positions <- data.frame(cbind(iO, niO))
	}else{
		niO <- c("start", "end")
		iO <- c(0,0)
		positions <- data.frame(t(c(iO, niO)))
	}
   
   colnames(positions) <- c("Start_Gutbereich", "Ende_Gutbereich", "Start_Ausschuss", "Ende_Ausschuss")
  
  if(nchar(path) > 0)
    switch(chosenType,
           "csv" = write.csv(positions, path, row.names = TRUE),
           "xlsx" = {getPackage("writexl")
			   require("writexl")
             write_xlsx(positions, path)},
           "txt" = write.table(positions, path, row.names = TRUE),
           "RData" = save(positions, file = path))
}