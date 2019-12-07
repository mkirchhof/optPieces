# getPackage - uses library to load a package and downloads it before if necessary
# Input: pac - character containting the name of the package
# Output: nothing
getPackage <- function(pac){
  if(!paste0("package:", pac) %in% search() && 
     !library(pac, logical.return = TRUE, character.only = TRUE)){
    install.packages(pac)
    library(pac, character.only = TRUE)
  }
  require(pac, character.only = TRUE)
}


# readText - reads the language.csv and puts it into global env.
# Output: character vector of available languages
readText <- function(){
  lang <<- read.csv("./settings/language.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
  return(colnames(lang)[-1])
}


# getText - takes a token and looks up the translation of it in the desired 
#           language. readText() has to be applied before.
# Input: 
#   token - character, which part of the UI is desired (for all possible values,
#           see language.csv and feel free to add more)
#   requestLang - character, which language should the output be (see columns
#                 of language.csv and feel free to add more)
getText <- function(token, requestLang = language){
  tokenMatch <- lang$token == token
  if(sum(tokenMatch) == 1){
    return(lang[tokenMatch, requestLang])
  } else {
    return(token)
  }
}


# validateNumber - Checks whether a number entered in the UI can correctly be 
#                  transformed into a numeric value. Creates a popup if not.
# Input: num - character
# Output: TRUE / FALSE, when num can / cannot be transformed into a numeric
validateNumber <- function(num){
  if(any(nchar(num) == 0)){
    # an empty string cannot be transformed into a numeric
    tkmessageBox(message = getText("val_Empty"), icon = "warning", type = "ok")
    return(FALSE)
  } else {
    wrong <- is.na(as.numeric(num))
    if(any(wrong)){
      # use singular/plural
      if(sum(wrong) > 1){
        tkmessageBox(
          message = paste0(getText("val_plural1"), 
                           paste(num[wrong], collapse = getText("val_plural2")), 
                           getText("val_plural3")),
          icon = "warning", type = "ok")
      } else {
        tkmessageBox(
          message = paste0(getText("val_singular1"), num[wrong], 
                           getText("val_singular2")),
          icon = "warning", type = "ok")
      }
      return(FALSE)
    }
    return(TRUE)
  }
}


# getFunctions - searches for R Functions in a given directory 
# Input: path - character with folder to search in
# Output: list where each list element is a function
getFunctions <- function(path = "./inputFunctions/"){
  fileNames <- list.files(pattern="[.]R$", path = path)
  functions <- sapply(paste0(path, fileNames), dget)
  names(functions) <- sapply(gsub("[.]R$", "", fileNames), getText)
  return(functions)
}


# removePath - removes the path at the end of a variable
# Input:
#   text - character in the format "name (path)"
# Output: character with format "name"
removePath <- function(text){
  return(gsub("^([^(]*) .*", "\\1", text))
}


# getPath - gives back the path at the end of a variable
# Input:
#   text - character in the format "name (path)"
# Output: character with format "path"
getPath <- function(text){
  return(paste0(" ", gsub("^[^(]* ", "", text)))
}


# createResPlot - plots result of analysis and caches it into a file
# Input:
#      x - object of class "pieceSelection"
#   path - character, filepath. If missing, a temporary file is created
#   type - character, filetype ("png", "pdf", "jpeg" or "svg")
#  width - numeric, width of plot in pixels
# height - numeric, height of plot in pixels
# Output: character, path to the picture of the plot
createResPlot <- function(x, path, type = "png", clean = TRUE){
  if(missing(path)){
    # use a temp file, but before delete the possible previous temp files,
    # so that the harddrive is not overflooded
    oldF <- list.files(tempdir())
    oldF <- oldF[grep("piecePlot", oldF)]
    if(clean && length(oldF) > 0)
      file.remove(paste0(tempdir(), "\\", oldF))
    path <- tempfile("piecePlot", fileext = paste(".", type, sep = "", collapse = ""))
  }
  
  # if type does not match type used in path, use type
  if(!length(grep(paste0(".", type, "$"), path)))
    path <- paste0(path, ".", type)
  
  # make the plots look the same in each device
  switch(type,
         "png" = png(path, width = x$width / 150, height = x$height / 150,
                     unit = "in", res = 150),
         "pdf" = pdf(path, width = x$width / 150, height = x$height / 150),
         "jpeg" = jpeg(path, width = x$width / 150, height = x$height / 150,
                       unit = "in", res = 150),
         "svg" = svg(path, width = x$width / 150, height = x$height / 150))
  
  print(plot(x))
  dev.off()
  
  return(path)
}


# output - creates a UI showing plot, summary and save options
# Input:
#   res - object of class pieceSelection
# Output: nothing
output <- function(res){
  
  # plotGUI - UI that appears when user clicks on the plot in order to edit it
  # Input:
  #   tempRes - object of class pieceSelection
  # Output: object of class pieceSelection with modified values for plotting
  plotGUI <- function(tempRes){
    
    # refreshPlot: redraws the plot with the inputted settings
    refreshPlot <- function(){
      if(validateNumber(tclvalue(fsize$env$var$width)) &&
         validateNumber(tclvalue(fsize$env$var$height)) &&
         validateNumber(tclvalue(fsize$env$var$text)) &&
         validateNumber(tclvalue(fsize$env$var$point))){
        
        # get new titles and subtitle
        tempRes$title <<- tclvalue(ftitle$env$var$top)
        tempRes$subtitle <<- tclvalue(ftitle$env$var$bot)
        
        # get new parameter names
        for(i in seq(n)){
          colnames(tempRes$data[[i]]) <<- c(tclvalue(flab$env$vary[[i]]),
                                            tclvalue(flab$env$varx[[i]]))
        }
        
        # get dimensions and settings of the plot
        tempRes$width <<- as.numeric(tclvalue(fsize$env$var$width))
        tempRes$height <<- as.numeric(tclvalue(fsize$env$var$height))
        tempRes$cexlab <<- as.numeric(tclvalue(fsize$env$var$text))
        tempRes$cexpoint <<- as.numeric(tclvalue(fsize$env$var$point))
        tempRes$zoom <<- tclvalue(fsize$env$var$zoom) == "1"
        
        # draw the new plot
        drawPlot(tempRes)
      }
    }
    
    # refreshAndQuit - refreshes the plot and deletes the editing UI
    refreshAndQuit <- function(){
      refreshPlot()
      tkdestroy(pgui)
    }
    
    n <- length(tempRes$data)
    pgui <- tktoplevel()
    tktitle(pgui) <- getText("edit_title")
    
    # frame for changing overall title
    ftitle <- ttklabelframe(pgui, relief = "flat", borderwidth = 5, 
                            padding = 5, text = getText("edit_texts"))
    
    ftitle$env$lab$top <- ttklabel(ftitle, text = getText("edit_main"))
    ftitle$env$var$top <- tclVar(as.character(tempRes$title))
    ftitle$env$ent$top <- ttkentry(ftitle, width = 30, textvariable = ftitle$env$var$top)
    ftitle$env$lab$bot <- ttklabel(ftitle, text = getText("edit_sub"))
    ftitle$env$var$bot <- tclVar(as.character(tempRes$subtitle))
    ftitle$env$ent$bot <- ttkentry(ftitle, width = 30, textvariable = ftitle$env$var$bot)
    
    tkgrid(ftitle$env$lab$top, ftitle$env$ent$top, 
           sticky = "w", pady = c(4, 0), padx = c(2, 2))
    tkgrid(ftitle$env$lab$bot, ftitle$env$ent$bot, 
           sticky = "w", pady = c(4, 0), padx = c(2, 2))
    
    tkpack(ftitle, anchor="w", padx=10, pady=10)
    
    # frame for changing xlab and ylab
    flab <- ttklabelframe(pgui, relief = "flat", borderwidth = 5, 
                          padding = 5, text = getText("edit_axis"))
    
    for(i in seq(n)){
      flab$env$laby[[i]] <- ttklabel(flab, text = paste0(i, getText("edit_yaxis")))
      flab$env$vary[[i]] <- tclVar(colnames(tempRes$data[[i]])[1])
      flab$env$enty[[i]] <- ttkentry(flab, width = 20, textvariable = flab$env$vary[[i]])
      flab$env$labx[[i]] <- ttklabel(flab, text = getText("edit_xaxis"))
      flab$env$varx[[i]] <- tclVar(colnames(tempRes$data[[i]])[2])
      flab$env$entx[[i]] <- ttkentry(flab, width = 20, textvariable = flab$env$varx[[i]])
      
      tkgrid(flab$env$laby[[i]], flab$env$enty[[i]], 
             flab$env$labx[[i]], flab$env$entx[[i]], pady = c(4, 0), padx = c(2, 2))
      tkgrid.configure(flab$env$labx[[i]], padx=c(10, 0), sticky="w")
    }
    
    tkpack(flab, anchor="w", fill="x", padx=10, pady=10)
    
    # frame for changing size and cex
    fsize <- ttklabelframe(pgui, relief = "flat", borderwidth = 5, 
                           padding = 5, text = getText("edit_size"))
    
    fsize$env$lab$height <- ttklabel(fsize, text = getText("edit_height"))
    fsize$env$var$height <- tclVar(as.character(tempRes$height))
    fsize$env$ent$height <- ttkentry(fsize, width = 10, textvariable = fsize$env$var$height)
    fsize$env$lab$width <- ttklabel(fsize, text = getText("edit_width"))
    fsize$env$var$width <- tclVar(as.character(tempRes$width))
    fsize$env$ent$width <- ttkentry(fsize, width = 10, textvariable = fsize$env$var$width)
    fsize$env$lab$text <- ttklabel(fsize, text = getText("edit_textsize"))
    fsize$env$var$text <- tclVar(as.character(tempRes$cexlab))
    fsize$env$ent$text <- ttkentry(fsize, width = 10, textvariable = fsize$env$var$text)
    fsize$env$lab$point <- ttklabel(fsize, text = getText("edit_symbolsize"))
    fsize$env$var$point <- tclVar(as.character(tempRes$cexpoint))
    fsize$env$ent$point <- ttkentry(fsize, width = 10, textvariable = fsize$env$var$point)
    fsize$env$var$zoom <- tclVar(ifelse(tempRes$zoom, "1", "0"))
    fsize$env$box$zoom <- ttkcheckbutton(fsize, variable = fsize$env$var$zoom,
                                         text = getText("edit_zoom"))
    
    tkgrid(fsize$env$lab$height, fsize$env$ent$height, 
           fsize$env$lab$text, fsize$env$ent$text, pady = c(4, 0), 
           padx = c(2, 2), sticky = "w")
    tkgrid(fsize$env$lab$width, fsize$env$ent$width,
           fsize$env$lab$point, fsize$env$ent$point, pady = c(4, 0), 
           padx = c(2, 2), sticky = "w")
    tkgrid(fsize$env$box$zoom, pady = c(4, 0), padx = c(2, 2),
           columnspan = 4, sticky = "w")
    tkgrid.configure(fsize$env$ent$height, fsize$env$ent$width,
                     padx = c(2, 10))
    
    tkpack(fsize, anchor="w", fill="x", padx=10, pady=10)
    
    # frame for control buttons
    fbut <- ttkframe(pgui, relief = "flat")
    
    fbut$env$but$apply <- ttkbutton(fbut, width = 10, text = getText("edit_apply"),
                                    command = refreshPlot)
    fbut$env$but$ok <- ttkbutton(fbut, width = 10, text = getText("edit_ok"),
                                 command = refreshAndQuit)
    
    tkgrid(fbut$env$but$apply, fbut$env$but$ok)
    tkgrid.configure(fbut$env$but$apply, fbut$env$but$ok,
                     pady = c(0, 5), padx = c(2, 0))
    tkgrid.configure(fbut$env$but$ok, padx = c(2, 9))
    
    tkpack(fbut, side="bottom", anchor = "e")
    
    # Hitting Enter anywhere triggers refreshplot
    tkbind(ftitle$env$ent$top, "<Return>", refreshPlot)
    tkbind(ftitle$env$ent$bot, "<Return>", refreshPlot)
    for(i in seq(n)){
      tkbind(flab$env$entx[[i]], "<Return>", refreshPlot)
      tkbind(flab$env$enty[[i]], "<Return>", refreshPlot)
    }
    tkbind(fsize$env$ent$height, "<Return>", refreshPlot)
    tkbind(fsize$env$ent$width, "<Return>", refreshPlot)
    tkbind(fsize$env$ent$text, "<Return>", refreshPlot)
    tkbind(fsize$env$ent$point, "<Return>", refreshPlot)
    tkbind(fbut$env$but$apply, "<Return>", refreshPlot)
    tkbind(fbut$env$but$ok, "<Return>", refreshAndQuit)
    
    tkwait.window(pgui)
    return(tempRes)
  }
  
  # Set standard values for plotting:
  res$height <- length(res$data) * 250 + 100
  res$width <- 600
  res$cexlab <- 1.0
  res$cexpoint <- 1.0
  res$title <- getText("plot_main")
  res$subtitle <- paste(getText("plot_sub"), Sys.time())
  for(i in seq(length(res$data))){
    colnames(res$data[[i]]) <- gsub(" [(]", "\\\n(", colnames(res$data[[i]]))
  }
  res$zoom <- TRUE
  
  # main window:
  ausschussPlot <- tktoplevel()
  tktitle(ausschussPlot) <- getText("res_title")
  
  # show plot: (as canvas, so that it is scrollable)
  fPlot <- tkframe(ausschussPlot, borderwidth = 3, relief = "flat")
  canv <- tkcanvas(fPlot, relief = "sunken")
  
  tkconfigure(canv, width = 600, height = 400)
  sbarV <- tkscrollbar(fPlot, orient = "vertical")
  sbarH <- tkscrollbar(fPlot, orient = "horizontal")
  tkconfigure(sbarV, command = function(...) tkyview(canv, ...))
  tkconfigure(sbarH, command = function(...) tkxview(canv, ...))
  tkconfigure(canv, yscrollcommand = function(...) tkset(sbarV, ...))
  tkconfigure(canv, xscrollcommand = function(...) tkset(sbarH, ...))
  tkpack(sbarV, side = "right", fill = "y")
  tkpack(sbarH, side = "bottom", fill = "x")
  tkpack(canv, expand = "yes", fill = "both")
  D <- ""
  tkbind(canv, "<MouseWheel>",
         function(D) {
           tkyview(canv, "scroll", - as.integer(as.numeric(D) / 50), "units")
         })
  plotGUIwrapper <- function(){
    res <<- plotGUI(res)
    drawPlot(res)
  }
  tkbind(canv, "<Button-1>", plotGUIwrapper)
  drawPlot <- function(x){
    tkconfigure(canv, scrollregion = c(0, 0, x$width, x$height))
    tkdelete(canv, "all")
    tkcreate(canv, "image", x$width/2, x$height/2, image = 
               tkimage.create("photo", tclVar(), file = createResPlot(x)))
  }
  drawPlot(res)
  
  tkpack(fPlot, expand = TRUE, fill = "both")
  
  # show summary:
  fRes <- ttklabelframe(ausschussPlot, relief = "flat", borderwidth = 5,
                        padding = 5, text = getText("res_summary"))
  
  resSummary <- res$summary
  tkgrid(tklabel(fRes, text = getText("res_length"), fg = "green4"),
         tklabel(fRes, text = getText("res_number"), fg = "green4"),
         tklabel(fRes, text = getText("res_good"), fg = "green4"),
         tklabel(fRes, text = getText("res_material")),
         tklabel(fRes, text = getText("res_badabs"), fg = "firebrick3"),
         tklabel(fRes, text = getText("res_badrel"), fg = "firebrick3"))
  tkgrid(tklabel(fRes, text = round(resSummary[1], 3), fg = "green4"),
         tklabel(fRes, text = round(resSummary[2], 3), fg = "green4"),
         tklabel(fRes, text = round(resSummary[3], 3), fg = "green4"),
         tklabel(fRes, text = round(resSummary[4], 3)),
         tklabel(fRes, text = round(resSummary[5], 3), fg = "firebrick3"),
         tklabel(fRes, text = paste0(round(resSummary[6], 3), "%"), fg = "firebrick3"))
  
  tkpack(fRes, side = "left", padx = 10, pady = 10)
  
  # Frame that includes the combobox for output functions
  fSave <- tkframe(ausschussPlot, borderwidth = 3, relief = "flat")
  
  fSave$env$outputfct <- getFunctions("./outputFunctions/")
  fSave$env$var$out <- tclVar(getText("res_save"))
  fSave$env$box$out <- ttkcombobox(ausschussPlot, 
                                   values = names(fSave$env$outputfct), 
                                   textvariable = fSave$env$var$out,
                                   state = "readonly")
  tkbind(fSave$env$box$out, "<<ComboboxSelected>>", 
         function(){
           tryCatch(fSave$env$outputfct[[tclvalue(fSave$env$var$out)]](res),
                    error = function(e){
                      dev.off()
                      cat("Error in ", tclvalue(fSave$env$var$out), 
                          ": ", e$message, "\n")
                      invisible(e)
                    })
           tclvalue(fSave$env$var$out) <- getText("res_save")
         })
  
  tkpack(fSave$env$box$out, anchor = "se", side = "right",
         padx = 10, pady = 11)
  
  tkpack(fSave, anchor = "w", padx = 10, pady = 10)
  
}


# getSetting - gives back saved program settings (loadSetting() has to be
#              called before)
# Input:
#   type - character of the desired setting
# Output: the desired setting
getSetting <- function(type = "length"){
  return(setting[1, type])
}


# loadSetting - loads or creates the settings file
loadSetting <- function(){
  if(file.exists("./settings/pieceToolSettings.txt")){
    return(read.table("./settings/pieceToolSettings.txt", 
                      stringsAsFactors = FALSE))
  } else {
    setting <- data.frame(length = "4.5", 
                          plotType = "png", outputType = "csv",
                          stringsAsFactors = FALSE, language = "english")
    return(setting)
  }
}


# saveSetting - saves a changed setting to the settings file
saveSetting <- function(type = "length", value){
  setting[1, type] <<- value
  write.table(setting, "./settings/pieceToolSettings.txt")
}


# askForSort - UI function that asks user if he wants to sort his data
# Input:
#   text - character, name of parameter to ask for
# Output: TRUE / FALSE
askForSort <- function(text){
  answer <- FALSE
  
  askSort <- tktoplevel()
  tktitle(askSort) <- getText("sort_title")
  
  textf <- tkframe(askSort)
  butf <- tkframe(askSort)
  
  askSort$env$lab$text <- ttklabel(textf, text = paste(getText("sort_lab1"), 
                                                       text, 
                                                       getText("sort_lab2"),
                                                       sep = "\n"))
  askSort$env$lab$quest <- ttklabel(textf, text = getText("sort_lab3"))
  
  # yes or no button
  askSort$env$but$no <- ttkbutton(butf, text = getText("sort_no"), 
                                  command = function(){
                                    answer <<- FALSE
                                    tkdestroy(askSort)
                                  })
  askSort$env$but$yes <- ttkbutton(butf, text = getText("sort_yes"), 
                                   command = function(){
                                     answer <<- TRUE
                                     tkdestroy(askSort)
                                   })
  
  tkpack(askSort$env$lab$text, anchor = "w")
  tkpack(askSort$env$lab$quest, pady = c(2, 0), anchor = "w")
  tkgrid(askSort$env$but$no, askSort$env$but$yes, sticky = "e", pady = c(5, 2),
         padx = c(0, 2))
  
  tkpack(textf)
  tkpack(butf, anchor = "e")
  
  tkwait.window(askSort)
  
  return(answer)
}

# input - UI that leads the user from giving the data location to specifying
#         limits and prepares everything to apply an optFunction on it. Runs
#         output() when done but does not return anything.
#         This is done because the input() UI is the main window and shall not
#         close when it's done, but stay ready for more user interactions.
input <- function(){
  # load language strings and settings for UI:
  readText()
  setting <<- loadSetting()
  language <<- getSetting("language")
  
  turnOff <- function(){
    tkdestroy(getParams)
    tclvalue(running) <- "notRun"
  }
  
  getParams <- tktoplevel()
  tktitle(getParams) <- getText("in_title")
  
  # Stop if UI is closed:
  running <- tclVar("run")
  tkwm.protocol(getParams, "WM_DELETE_WINDOW", turnOff)
  
  # frames
  fmain <- ttkframe(getParams)
  tkpack(fmain)
  fFileBoth <- ttklabelframe(fmain, relief = "flat", borderwidth = 5, 
                             padding = 5, text = getText("in_input"))
  fLanguage <- tkframe(fmain, relief = "flat", borderwidth = 3)
  tkpack(fLanguage, anchor = "e")
  tkpack(fFileBoth, anchor = "w", padx = 10, pady = 10)
  fFile <- tkframe(fFileBoth, borderwidth = 3, relief = "flat")
  fAddF <- tkframe(fFileBoth, borderwidth = 3, relief = "flat")
  fValueBoth <- ttklabelframe(fmain, relief = "flat", borderwidth = 5,
                              padding = 5, text = getText("in_limits"))
  tkpack(fValueBoth, anchor = "w", padx = 10, pady = 10)
  fValue <- tkframe(fValueBoth, borderwidth = 3, relief = "flat")
  fAddV <- tkframe(fValueBoth, borderwidth = 3, relief = "flat")
  fOpt <- ttklabelframe(fmain, borderwidth = 5, relief = "flat",
                        padding = 5, text = getText("in_opt"))
  fBottom <- tkframe(fmain, relief = "flat", borderwidth = 3)
  
  # Make main frame scrollable
  # TODO: Implement a visual scrollbar. Failed so far because tk is having
  #       problems scrolling a tkframe and thus the scrollbar will be out of sync
  pos <- 0
  D <- ""
  tkbind(getParams, "<MouseWheel>",
         function(D) {
           # tkyview(canv, "scroll", as.integer(D), "units")
           maxScroll <- as.numeric(gsub("^[[:digit:]]*x([[:digit:]]*)+.*$", "\\1", tkwm.geometry(getParams))) - as.numeric(tkwinfo("height", fmain))
           pos <<- max(min(pos + as.integer(D), 0), maxScroll)
           tkplace(fmain, y = pos)
         })
  
  # language frame
  fLanguage$env$var$lang <- tclVar(getSetting("language"))
  fLanguage$env$box$lang <- ttkcombobox(fLanguage, 
                                        values = colnames(lang)[-1], 
                                        textvariable = fLanguage$env$var$lang,
                                        state = "readonly")
  tkbind(fLanguage$env$box$lang, "<<ComboboxSelected>>", 
         function() {
           saveSetting("language", tclvalue(fLanguage$env$var$lang))
           tkmessageBox(message = getText("in_language", tclvalue(fLanguage$env$var$lang)), 
                        icon = "info", type = "ok")
         })
  tkpack(fLanguage$env$box$lang)
  
  # file frame
  fFile$env$var$file <- list()
  fFile$env$ent$file <- list()
  fFile$env$but$file <- list()
  fFile$env$fct$file <- list()
  fFile$env$inputfct <- getFunctions("./inputFunctions/")
  fFile$env$box$file <- list()
  fFile$env$but$add <- list()
  extraInfo <- list()
  
  # open Browser for i-th input file
  chooseFile <- function(i){
    tclvalue(fFile$env$var$file[[i]]) <<- tclvalue(tkgetOpenFile())
    insertFileFct(i)
  }
  
  # insertFileFct - guesses the input function via the file ending
  insertFileFct <- function(i){
    # search for fitting input function:
    functionRow <- fileFct$ending == gsub(".*\\.([[:alpha:]]+)$", "\\1", 
                                          tclvalue(fFile$env$var$file[[i]]))
    
    # if found, insert it
    if(sum(functionRow) == 1){
      tclvalue(fFile$env$fct$file[[i]]) <- fileFct[functionRow, "fct"]
    }
  }
  
  # loadFileFct - loads users preferred input functions from a file or creates
  #               a new file if none was found
  loadFileFct <- function(){
    if(file.exists("./settings/pieceToolFileFct.txt")){
      return(read.table("./settings/pieceToolFileFct.txt", 
                        stringsAsFactors = FALSE, skipNul = TRUE))
    } else {
      fileFct <- as.data.frame(matrix(ncol = 2, nrow = 0))
      colnames(fileFct) <- c("ending", "fct")
      return(fileFct)
    }
  }
  
  # saveFileFct - saves users preferred input functions to a file
  saveFileFct <- function(){
    for(fileIndex in seq(along = fFile$env$var$file)){
      fileEnding <- gsub(".*\\.([[:alpha:]]+)$", "\\1", 
                         tclvalue(fFile$env$var$file[[fileIndex]]))
      functionRow <- fileFct$ending == fileEnding
      # if file ending was never used before, create new row
      if(!any(functionRow)) functionRow <- dim(fileFct)[1] + 1
      fileFct[functionRow, "ending"] <<- fileEnding
      fileFct[functionRow, "fct"] <<- tclvalue(fFile$env$fct$file[[fileIndex]])
    }
    write.table(fileFct, "./settings/pieceToolFileFct.txt")
  }
  
  fileFct <- loadFileFct()
  
  # removeFile - deletes a row in fFile if the user clicks on a "-" next to it
  removeFile <- function(i){
    tkgrid.forget(fFile$env$but$add[[i]]$ID)
    tkgrid.forget(fFile$env$ent$file[[i]]$ID)
    tkgrid.forget(fFile$env$but$file[[i]]$ID)
    tkgrid.forget(fFile$env$box$file[[i]]$ID)
    
    # reset var so that it is ignored in the rest of the program
    # (unable to delete because that would screw the indices)
    tclvalue(fFile$env$var$file[[i]]) <- getText("in_choosefile")
  }
  
  # addFile - adds a row in fFile to allow multi-file input if the user presses
  #           on the "+"
  addFile <- function(i = 1){
    fFile$env$but$add[[i]] <<- ttkbutton(fFile, text = "-", width = 2,
                                         command = function() removeFile(i))
    fFile$env$var$file[[i]] <<- tclVar(getText("in_choosefile"))
    fFile$env$ent$file[[i]] <<- ttkentry(fFile, width = 30, 
                                         textvariable = fFile$env$var$file[[i]])
    fFile$env$but$file[[i]] <<- ttkbutton(fFile, text = getText("in_browse"),
                                          command = function() chooseFile(i))
    fFile$env$fct$file[[i]] <<- tclVar(getText("in_format"))
    fFile$env$box$file[[i]] <<- ttkcombobox(fFile, 
                                            values = names(fFile$env$inputfct), 
                                            textvariable = fFile$env$fct$file[[i]],
                                            state = "readonly")
    
    # When a filename is edited, try to guess the input function
    tkbind(fFile$env$ent$file[[i]], "<Key>", function() insertFileFct(i))
    
    tkgrid(fFile$env$but$add[[i]], fFile$env$ent$file[[i]], 
           fFile$env$but$file[[i]], fFile$env$box$file[[i]],
           pady = c(0, 4), sticky = "w", padx = c(2, 2))
    tkgrid.configure(fFile$env$but$file[[i]], padx = c(0, 2))
    tkgrid.configure(fFile$env$ent$file[[i]], padx = c(2, 0))
  }
  
  # show first file input:
  tkgrid("x", ttklabel(fFile, text = getText("in_file")), "x",
         ttklabel(fFile, text = getText("in_filetype")), sticky = "w")
  addFile(1)
  tkpack(fFile)
  
  # addFile frame
  fAddF$env$but$add <- ttkbutton(fAddF, text = "+", width = 2,
                                 command = function() addFile(length(fFile$env$var$file) + 1))
  
  # readInput - reads all files via their corresponding input function
  readInput <- function(){
    inputData <<- list()
    
    # checks whether a string is the standard string ("please insert a filename here" or so)
    isStandardValue <- sapply(fFile$env$var$file, function(f) 
      tclvalue(f) == getText("in_choosefile"))
    inputIndex <- 1
    
    # only read those files that are not the standard value
    for(fileIndex in seq(along = fFile$env$var$file)[!isStandardValue]){
      temp <- try(fFile$env$inputfct[[tclvalue(fFile$env$fct$file[[fileIndex]])]](
        tclvalue(fFile$env$var$file[[fileIndex]])), silent = TRUE)
      
      # if it could not be read, notify the user
      if(class(temp)[1] == "try-error")
        tkmessageBox(title = getText("inerr_title"), message = paste(
          getText("inerr_1"), tclvalue(fFile$env$fct$file[[fileIndex]]), getText("inerr_2"),
          tclvalue(fFile$env$var$file[[fileIndex]]), getText("inerr_3")),
          icon = "warning", type = "ok")
      
      # Attach the filenames to the parameter names
      colnames(temp) <- paste0(colnames(temp), " (",
                               basename(tclvalue(fFile$env$var$file[[fileIndex]])), ")")
      
      for(col in seq(along = colnames(temp))){
        inputData[[inputIndex]] <<- temp[, col, drop = FALSE]
        inputIndex <- inputIndex + 1
      }
      
      # attach extra info of the input functions if given
      if(!is.null(temp$info))
        extraInfo <<- c(extraInfo, temp$info)
    }
    
    params <<- unlist(sapply(inputData, function(i) colnames(i)))
    
    # If some files have already been read before and therefore the rest of the
    # UI already is shown, destroy and recreate it
    if(length(as.character(tkpack.slaves(fmain))) > 2){
      tkdestroy(fValue)
      tkdestroy(fAddV)
      tkdestroy(fValueBoth)
      tkdestroy(fOpt)
      tkdestroy(fBottom)
      fValueBoth <<- ttklabelframe(fmain, relief = "flat", borderwidth = 5,
                                   padding = 5, text = getText("in_limits"))
      fValue <<- tkframe(fValueBoth, borderwidth = 3, relief = "flat")
      fAddV <<- tkframe(fValueBoth, borderwidth = 3, relief = "flat")
      fOpt <<- ttklabelframe(fmain, borderwidth = 5, relief = "flat",
                             padding = 5, text = getText("in_opt"))
      fBottom <<- tkframe(fmain, relief = "flat", borderwidth = 3)
    }
    
    # (re-)draw frames
    tkpack(fValue, anchor = "w")
    tkpack(fAddV, anchor = "w")
    tkpack(fValueBoth, anchor = "w", padx = 10, pady = 10)
    tkpack(fOpt, anchor = "w", padx = 10, pady = 10)
    tkpack(fBottom, anchor = "e")
    
    # save which file endings were matched to which input functions
    saveFileFct()
    
    # create the specification limit frame
    openSecondUIArea()
  }
  fAddF$env$but$read <- ttkbutton(fAddF, text = getText("in_readall"), 
                                  command = readInput)
  
  tkgrid(fAddF$env$but$add, "x",
         fAddF$env$but$read)
  tkgrid.configure(fAddF$env$but$add, sticky = "w", padx = c(2, 2))
  tkgrid.configure(fAddF$env$but$read, sticky = "e", padx = c(as.numeric(getText("in_readallxDist")), 0))
  
  inputData <- NULL
  
  tkpack(fAddF, anchor = "e", fill = "x")
  
  # openSecondUIArea - creates the part of the UI where specification limits are inputted
  openSecondUIArea <- function(){
    
    # prepareAnalysis - reads all UI inputs, creates an object that can be
    #                   passed to the optFunctions and applies them
    prepareAnalysis <- function(){
      # Only process those parameters that are not removed or empty
      correct <- seq(along = fValue$env$var$param)[
        !sapply(fValue$env$removed, "[") &
          sapply(fValue$env$var$param, function(x) tclvalue(x)) != getText("in_param") &
          sapply(fValue$env$var$pos, function(x) tclvalue(x)) != getText("in_position")]
      
      # Check if all entries are valid:
      if(validateNumber(c(sapply(fValue$env$var$input[correct], 
                                 function(x) sapply(x, tclvalue)),
                          tclvalue(fOpt$env$var$length)))){
        
        # Save user entered settings
        saveDefault(default)
        saveSetting("length", tclvalue(fOpt$env$var$length))
        
        names(inputData) <- sapply(inputData, colnames)
        
        # bring input files in the correct format for the optimizers
        datalist <- list()
        paramlist <- list()
        
        for(i in correct){
          # connect parameter values with their corresponding positions
          datalist[[i]] <- cbind(inputData[[tclvalue(fValue$env$var$param[[i]])]],
                                 inputData[[tclvalue(fValue$env$var$pos[[i]])]])
          colnames(datalist[[i]]) <- c(tclvalue(fValue$env$var$param[[i]]),
                                       tclvalue(fValue$env$var$pos[[i]]))
          
          # read the desired value, lower and upper spec limit and allowed outliers
          paramlist[[i]] <- c(as.numeric(tclvalue(fValue$env$var$input[[i]][[2]])),
                              as.numeric(tclvalue(fValue$env$var$input[[i]][[1]])),
                              as.numeric(tclvalue(fValue$env$var$input[[i]][[3]])),
                              as.numeric(tclvalue(fValue$env$var$input[[i]][[4]])))
          
          # remove non-numerics and NAs from data
          datalist[[i]] <- try(apply(datalist[[i]], 2, as.numeric))
          curNA <- sum(is.na(datalist[[i]]))
          datalist[[i]] <- datalist[[i]][!(is.na(datalist[[i]][,1]) | 
                                             is.na(datalist[[i]][,2])),]
          
          # if any data now has 0 entries after the cleanup, notify the user
          if(dim(datalist[[i]])[1] == 0){
            tkmessageBox(
              message = paste0(getText("prep_wrongplural1"), 
                               paste(colnames(datalist[[i]]), collapse = getText("prep_wrongplural2")), 
                               getText("prep_wrongplural3")),
              icon = "warning", type = "ok")
          } else if(curNA > 0){
            tkmessageBox(
              message = paste0(getText("prep_naplural1"), 
                               paste(colnames(datalist[[i]]), collapse = getText("prep_naplural2")), 
                               getText("prep_naplural3")),
              icon = "warning", type = "ok")
          }
          
          # Warn if positions of the data are not ascending:
          # maybe sort them: (this is very slow for some reason. program might 
          # produce wrong results however if values are not sorted)
          if(any(datalist[[i]][-1,2] < datalist[[i]][-dim(datalist[[i]])[1],2])){
            sortUserInput <- askForSort(colnames(datalist[[i]])[2])
            if(sortUserInput){
              datalist[[i]] <- datalist[[i]][order(datalist[[i]][,2]),]
            }
          }
        }
        
        # clean from NULLs
        datalist <- datalist[!sapply(datalist, is.null)]
        paramlist <- paramlist[!sapply(paramlist, is.null)]
        
        # apply optimization algorithm to the data
        searchRes <- cutSplitSearch(datalist, paramlist, 
                                    as.numeric(tclvalue(fOpt$env$var$length)))
        searchRes$extraInfo <- extraInfo
        
        # hand the result to the output UI:
        output(searchRes)
      }
    }
    
    # limits frame
    # create the header of the table
    fValue$env$lab$tab <- list("x",
                               ttklabel(fValue, text = getText("in_params")), 
                               ttklabel(fValue, text = getText("in_min")), 
                               ttklabel(fValue, text = getText("in_mean")), 
                               ttklabel(fValue, text = getText("in_max")),
                               ttklabel(fValue, text = getText("in_out")))
    
    tkgrid(fValue$env$lab$tab[[1]], fValue$env$lab$tab[[2]], 
           fValue$env$lab$tab[[3]], fValue$env$lab$tab[[4]],
           fValue$env$lab$tab[[5]], fValue$env$lab$tab[[6]],
           sticky = "w", pady = c(0, 0), padx = c(2, 2))
    
    # insertDefault - search if parameter i (except its specific filename) has 
    #                 already had limits and positional data in the past
    insertDefault <- function(i){
      paramRow <- default$param == removePath(tclvalue(fValue$env$var$param[[i]]))
      if(sum(paramRow) == 1){
        for(varIndex in seq(along = fValue$env$var$input[[i]])){
          tclvalue(fValue$env$var$input[[i]][[varIndex]]) <- 
            default[paramRow, varIndex + 1]
        }
        helpPos <- paste0(default[paramRow, "pos"],
                          getPath(tclvalue(fValue$env$var$param[[i]])))
        if(helpPos %in% params){
          tclvalue(fValue$env$var$pos[[i]]) <- helpPos
        }
      } else {
        for(varIndex in seq(along = fValue$env$var$input[[i]])){
          tclvalue(fValue$env$var$input[[i]][[varIndex]]) <- ""
        }
      }
    }
    
    # loadDefault - load past limits from a file
    loadDefault <- function(){
      if(file.exists("./settings/pieceToolDefaults.txt")){
        return(read.table("./settings/pieceToolDefaults.txt", 
                          stringsAsFactors = FALSE))
      } else {
        default <- as.data.frame(matrix(ncol = 6, nrow = 0))
        colnames(default) <- c("param", "min", "opt", "max", "outliers", "pos")
        return(default)
      }
    }
    
    # saveDefault - save current limits to a file
    saveDefault <- function(default){
      # Only process those that are not removed or empty
      correct <- seq(along = fValue$env$var$param)[
        !sapply(fValue$env$removed, "[") &
          sapply(fValue$env$var$param, function(x) tclvalue(x)) != getText("in_param") &
          sapply(fValue$env$var$pos, function(x) tclvalue(x)) != getText("in_position")]
      
      for(paramIndex in correct){
        # check if parameter has been used before
        paramRow <- default$param == removePath(tclvalue(fValue$env$var$param[[paramIndex]]))
        
        # append new row if parameter has not been used before
        if(!any(paramRow)) 
          paramRow <- dim(default)[1] + 1
        
        # override or add the values
        default[paramRow, "param"] <- removePath(tclvalue(fValue$env$var$param[[paramIndex]]))
        for(varIndex in seq(along = fValue$env$var$input[[paramIndex]])){
          default[paramRow, varIndex + 1] <- 
            tclvalue(fValue$env$var$input[[paramIndex]][[varIndex]])
        }
        default[paramRow, "pos"] <- removePath(tclvalue(fValue$env$var$pos[[paramIndex]]))
      }
      
      write.table(default, "./settings/pieceToolDefaults.txt")
    }
    
    default <- loadDefault()
    
    fValue$env$var$input <- list()
    fValue$env$ent$input <- list()
    fValue$env$var$param <- list()
    fValue$env$box$param <- list()
    fValue$env$var$pos <- list()
    fValue$env$box$pos <- list()
    fValue$env$lab$pos <- list()
    fValue$env$but$add <- list()
    fValue$env$removed <- list()
    
    # removeValue - action when user clicks on "remove limit" ("-") next to the
    #               i-th limit row
    removeValue <- function(i){
      tkgrid.forget(fValue$env$but$add[[i]]$ID)
      tkgrid.forget(fValue$env$box$param[[i]]$ID)
      tkgrid.forget(fValue$env$ent$input[[i]][[1]]$ID)
      tkgrid.forget(fValue$env$ent$input[[i]][[2]]$ID)
      tkgrid.forget(fValue$env$ent$input[[i]][[3]]$ID)
      tkgrid.forget(fValue$env$ent$input[[i]][[4]]$ID)
      tkgrid.forget(fValue$env$lab$pos[[i]]$ID)
      tkgrid.forget(fValue$env$box$pos[[i]]$ID)
      # reset var so that it is ignored in the rest of the program
      # (unable to delete because that would screw the indices)
      fValue$env$removed[[i]] <<- TRUE
    }
    
    # addValue - add the i-th limit row to the UI
    addValue <- function(i){
      fValue$env$removed[[i]] <<- FALSE
      fValue$env$but$add[[i]] <<- ttkbutton(fValue, text = "-", width = 2,
                                            command = function() removeValue(i))
      fValue$env$var$param[[i]] <<- tclVar(getText("in_param"))
      fValue$env$box$param[[i]] <<- ttkcombobox(fValue, values = params, 
                                                textvariable = fValue$env$var$param[[i]],
                                                state = "readonly")
      fValue$env$var$input[[i]] <<- lapply(rep("", 4), function(p) tclVar(p))
      fValue$env$ent$input[[i]] <<- lapply(fValue$env$var$input[[i]], function(v)
        ttkentry(fValue, width = 9, textvariable = v))
      fValue$env$lab$pos[[i]] <<- ttklabel(fValue, text = getText("in_postext"))
      fValue$env$var$pos[[i]] <<- tclVar(getText("in_position"))
      fValue$env$box$pos[[i]] <<- ttkcombobox(fValue, values = params, 
                                              textvariable = fValue$env$var$pos[[i]],
                                              state = "readonly")
      
      tkgrid(fValue$env$but$add[[i]], fValue$env$box$param[[i]],
             fValue$env$ent$input[[i]][[1]], fValue$env$ent$input[[i]][[2]],
             fValue$env$ent$input[[i]][[3]], fValue$env$ent$input[[i]][[4]],
             sticky = "w", padx = c(2, 2), pady = c(0, 2))
      tkgrid("x", fValue$env$lab$pos[[i]], fValue$env$box$pos[[i]],
             pady = c(0, 4), padx = c(2, 2), sticky = "e")
      tkgrid.configure(fValue$env$box$pos[[i]], column = 2, columnspan = 4, 
                       sticky = "w")
      tkgrid.configure(fValue$env$but$add[[i]], rowspan = 2, ipady = 8,
                       pady = c(0, 0), sticky = "n")
      
      # Try to guess fitting limits as soon as user chooses a parameter
      tkbind(fValue$env$box$param[[i]], "<<ComboboxSelected>>", 
             function() {
               insertDefault(i)
             })
      insertDefault(i)
    }
    
    # addValueButton frame
    addValueButtonFunction <- function(){
      addValue(length(fValue$env$var$pos) + 1)
    }
    fAddV$env$but$add <- ttkbutton(fAddV, text = "+", width = 2,
                                   command = addValueButtonFunction)
    
    
    tkgrid(fAddV$env$but$add, padx = c(2, 2))
    tkgrid.configure(fAddV$env$but$add, sticky = "w")
    
    addValueButtonFunction()
    
    # Write the note that the order of the parameters matters
    fAddV$env$lab$prio <- ttklabel(fAddV, text = getText("in_prioOrder"))
    tkgrid(fAddV$env$lab$prio, columnspan = 2, padx = c(2, 2))
    
    # optimization frame
    fOpt$env$var$length <- tclVar(getSetting("length"))
    fOpt$env$ent$length <- ttkentry(fOpt, width = 9, 
                                    textvariable = fOpt$env$var$length)
    fOpt$env$lab$length <- ttklabel(fOpt, text = getText("in_length"))
    
    tkgrid(fOpt$env$lab$length, fOpt$env$ent$length, padx = c(2, 2),
           sticky = "w")
    
    # bottom frame
    fBottom$env$but$done <- ttkbutton(fBottom, text = getText("in_analyze"), 
                                      command = prepareAnalysis)
    tkpack(fBottom$env$but$done, anchor = "e", padx = 7, pady = 5)
  }
  
  # stop the function if the user closes the UI
  tkwait.variable(running)
}

