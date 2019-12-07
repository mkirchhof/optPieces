# Output Function that saves the currently displayed plot
# Input: x - an object of class pieceSelection containing the optimization results
Plot_speichern <- function(x){
  # open a dialog to choose file destination
  types <- c("png", "jpeg", "svg", "pdf")
  path <- tclvalue(tkgetSaveFile(
    filetypes = paste0(paste0("{ {", types, " ", getText("exp_files"), "} {.", 
                              types, "} }", collapse = " "), " { {", 
                       getText("exp_allFiles"), "} * }"), 
    defaultextension = ".png"))
  chosenType <- gsub(".*[.]([^.]*)$", "\\1", path)
  
  
  if(nchar(path) > 0)
    createResPlot(x, path, chosenType)
}