print.pieceSelection <- function(x){
  y <- list()
  y[["summary"]] <- x$summary
  y[["pieceOutliers"]] <- x$pieceOutliers	
  y[["piecePositions"]] <- x$piecePositions
  print(y)
}


# plot.pieceSelection - graphically shows where the pieces are cut
# Input:
#  x - an object of class "pieceSelection"
#  extraEmpty - numeric, increases mfrow to include future plots
#  The following inputs can also be given as elements of x. Arguments given this
#  way will be prioritized.
#  main - character, title of the plot
#  sub - character, subtitle of the plot
#  cexlab - numeric, expansion factor of the texts
#  cexpoint - numeric, expansion factor of all lines and points
#  zoom - logical, should the plot be truncated to the LSL and USL?
# Output:
#  nothing, but a plot is plotted.
plot.pieceSelection <- function(x, extraEmpty = 0L, main = "", sub = "", 
                                cexlab = 1, cexpoint = 1, zoom = FALSE){
  if(is.null(x$title)) x$title <- main
  if(is.null(x$subtitle)) x$subtitle <- sub
  if(is.null(x$cexlab)) x$cexlab <- cexlab
  if(is.null(x$cexpoint)) x$cexpoint <- cexpoint
  if(is.null(x$zoom)) x$zoom <- zoom
  
  # Compute how long the subtitle is (how many \n):
  subLines <- lengths(regmatches(x$subtitle, gregexpr("\n", x$subtitle))) + 1
  
  oma <- c(x$cexlab * subLines + 0.1, 0, x$cexlab + 1.1, 0)
  if(x$title == "")
    oma[3] <- 0
  if(x$subtitle == "")
    oma[1] <- 0
  
  par(mfrow = c(length(x$data) + extraEmpty, 1), 
      mai = (c(4 + x$cexlab, 4 + x$cexlab, 1, 1) + 0.1) / 10,
      cex.lab = x$cexlab, cex.axis = x$cexlab, cex = 0.5, 
      cex.main = x$cexlab, cex.sub = x$cexlab, lwd = 0.7 * x$cexpoint,
      omi = oma / 10)
  
  # plotting region on the x-axis for each parameter
  maxPos <- sapply(x$data, function(dat) max(dat[,2])) 
  minPos <- sapply(x$data, function(dat) min(dat[,2]))
  
  # For each parameter k, draw one plot
  for(k in seq(length(x$data))){
    # if requested, zoom the y-axis into the area of the specification limts
    if(x$zoom){
      ylims <- c(x$params[[k]][2] - 0.5 * (x$params[[k]][3] - x$params[[k]][2]),
                 x$params[[k]][3] + 0.5 * (x$params[[k]][3] - x$params[[k]][2]))
    } else {
      ylims <- c(min(min(x$data[[k]][,1]), x$params[[k]][2]),
                 max(max(x$data[[k]][,1]), x$params[[k]][3]))
    }
    
    # create a blank plot with red background. background only stretches over
    # the area where all parameters have points
    plot(NA, xlab = colnames(x$data[[k]])[2],
         ylab = colnames(x$data[[k]])[1], type = "n",
         ylim = ylims, 
         xlim = c(min(minPos), max(maxPos)))
    rect(xleft = max(minPos), xright=min(maxPos), ybottom = par('usr')[3], 
         ytop = par('usr')[4], col = "indianred1")
    
    # set off selected pieces with green background color
    if(x$summary["nPieces"] > 0){
      for(i in 1:length(x$'piecePositions'[,1])){
        rect(xleft = x$'piecePositions'[i,1], 
             xright = x$'piecePositions'[i,2], 
             ybottom = par('usr')[3], ytop = par('usr')[4], col = "lightgreen")
      }
    }
    
    # Add the data points. If the plot is zoomed in and a point lies outside 
    # the ylims, mark it as a triangle at the top/bottom part of the plot
    abbrY <- x$data[[k]][,1]
    abbrPch <- rep(19, length(abbrY))
    abbrPch[abbrY < ylims[1]] <- 6
    abbrPch[abbrY > ylims[2]] <- 2
    abbrY[abbrY < ylims[1]] <- ylims[1]
    abbrY[abbrY > ylims[2]] <- ylims[2]
    points(x$data[[k]][,2], abbrY, pch = abbrPch, cex = x$cexpoint * 0.7)
    
    # draw small horizontal lines that indicate which area a point represents
    xline <- c(x$data[[k]][1, 2], 
               rep((x$data[[k]][-1,2] + x$data[[k]][-dim(x$data[[k]])[1],2]) / 2, each = 3),
               x$data[[k]][dim(x$data[[k]])[1], 2])
    yline <- rep(x$data[[k]][, 1], each = 3)
    yline <- yline[-length(yline)]
    xline[seq(3, length(xline), by = 3)] <- NA
    yline[seq(3, length(yline), by = 3)] <- NA
    lines(xline, yline, lwd = x$cexpoint)
    
    # Add darkgreen borders to the left and right end of green areas (= pieces)
    if(x$summary["nPieces"] > 0){
      for(i in 1:length(x$'piecePositions'[,1])){
        abline(v = x$'piecePositions'[i,1], col = "darkgreen", 
               lwd = 2 * x$cexpoint * 0.7)
        abline(v = x$'piecePositions'[i,2], col = "darkgreen", 
               lwd = 2 * x$cexpoint * 0.7)
      }
    }
    
    box()
    
    # add specification limits:
    abline(h = x$params[[k]][3], lwd = 2 * x$cexpoint * 0.7)
    abline(h = x$params[[k]][2], lwd = 2 * x$cexpoint * 0.7)
  }
  
  # add main title and subtitle
  if(x$title != "")
    title(main = x$title, outer = TRUE, cex = x$cexlab)
  if(x$subtitle != "")
    title(sub = x$subtitle, outer = TRUE, cex = x$cexlab, line = oma[1] - 1)
}
