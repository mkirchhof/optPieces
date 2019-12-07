# WS_speichern - saves the pieceSelection R-object for manual analysis
# Input: x - an object of class pieceSelection containing the optimization results
WS_speichern <- function(x){
	# open a dialog to choose file destination
	path <- tclvalue(tkgetSaveFile(
					filetypes = paste0(paste0("{ {", "RData", " ", getText("exp_files"), "} {.", 
									"RData", "} }", collapse = " "), " { {", 
							getText("exp_allFiles"), "} * }"), 
					defaultextension = ".RData"))
	
	save(x, file = path)
}