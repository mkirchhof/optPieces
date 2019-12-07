# These functions (cutNaive, cutOneOpt, cutSeqOpt) are used to find the positions
# of optimal pieces in the material. Optimality is given by the relative area of
# a piece that exceeds the given specification limits. A piece is rejected if 
# this area exceeds a certain threshold (usually 5% of the piece). 
# The algorithms try to find the maximum number of possible pieces as well as the
# best possible positions. The optimality is defined by the sum of outliers in 
# all pieces. 
# cutNaive is a fast approach that determines the maximum number of piceces and 
# gives back a first possible selection. This selection is, however, not optimal.
# cutOneOpt is a knapsack strategy that optimizes the first parameter (that is
# the one that is at position 1 in the data list), but does not further optimize
# the others.
# cutSeqOpt is a slower approach, but it optimizes all given parameters in the
# order of their occurence in the list. That means, if there are several possible
# selections of pieces that have the same optimality value regarding the first 
# parameter, the second parameter is evaluated on those selections. This yields
# a sequential optimization.


# cutNaive - fast approach that determines the maximum number of piceces and 
#            gives back a first possible selection. This selection is, however, 
#            not optimal.
# Input:
#  data - list of dataframes (or matrices). Each dataframe corresponds to one 
#         quality parameter and has 2 columns. The first one is the measurement 
#         and the second one the position of this measurement.
#  params - list of numerics. Each vector corresponds to one quality parameter 
#           and includes its target value, lower specification limt, upper
#           specification limit and the relative amount of allowed outliers per
#           pieces.
#  l - numeric of length 1. The desired length of pieces.
#  keepDetails - logical. Wheter to return measurements and positions for each piece.
#
# Output:
# object of class pieceSelection including:
#  summary - numeric with a summary of the found pieces, including length, number,
#            sum of length, length of given material, waste of material, relative
#            amount of waste
#  piecePos - matrix with two columns. Each row corresponds to one found piece.
#             First column gives its start position, second column its end position.
#  pieceOutliers - matrix, relative amount of outliers per piece (row) and 
#                  parameter (column)
#  data - the input argument data
#  params - the input argument params
#  pieceData - only returned if keepDetails == TRUE. list of lists of dataframes.
#              Each is a part of the input argument data, but restrained on the
#              area of the pieces.
cutNaive <- function(data, params, l, keepDetails=TRUE){
  # outsideWeight - searches the outlier points in parameter k for a piece 
  #                 starting at a certain start position
  outsideWeight <- function(start, k){
    end <- start + l
    
    # If cell lies outside the recorded data, it cannot be cut
    if(start < pointStarts[[k]][1] | pointEnds[[k]][length(data[[k]][, 2])] < end) 
      return(list(pos = points,
                  outData = NA,
                  relOutside = 1))
    
    # Find points that are completly overlayed by the cell:
    points <- which(start <= pointStarts[[k]] & pointEnds[[k]] <= end)
    
    # Find points at the beginning or end of the cell that are partly overlaid:
    points <- c(max(which(pointStarts[[k]] <= start)), points,
                min(which(pointEnds[[k]] >= end)))
    points <- unique(points)
    
    # calculate how much area each point represents in the piece (called weights)
    tempWeight <- numeric()
    tempWeight[points] <- weights[[k]][points]
    # Check if the weight of the first point has to be lowered:
    if(start > pointStarts[[k]][points[1]])
      tempWeight[points[1]] <- tempWeight[points[1]] - 
      (start - pointStarts[[k]][points[1]])
    # Check if the weight of the last point has to be lowered
    if(end < pointEnds[[k]][points[length(points)]]){
      tempWeight[points[length(points)]] <- tempWeight[points[length(points)]] - 
        (pointEnds[[k]][points[length(points)]] - end)
    }
    
    # find outlier points and the relative amount of outliers per piece
    outPoints <- ! (params[[k]][2] <= data[[k]][, 1][points] & data[[k]][, 1][points] <= params[[k]][3])
    outPoints <- points[outPoints]
    relOutside <- sum(tempWeight[outPoints]) / sum(tempWeight[points])
    
    return(list(pos = points,
                outData = outPoints,
                relOutside = relOutside))
  }
  
  # Each datapoint is representative for a certain area of the band. The borders
  # of these areas lay in the middle of two adjacent points. Very first and very
  # last point is handled specially.
  # e.g. if there are points recorded at positions 1, 4, 6 and 8, then 
  # point 1's area is [1, 2.5), point 2's is [2.5, 5), 3's is [5, 7) and 8's [7, 8]
  # This is stored in pointStarts and pointEnds, which would be:
  # pointStarts = 1,   2.5, 5, 7
  # pointEnds =   2.5,   5, 7, 8
  weights <- list()
  pointEnds <- list()
  pointStarts <- list()
  maxPos <- numeric(length(data))
  minPos <- numeric(length(data))
  for(k in 1:length(data)){
    pointEnds[[k]] <- c((data[[k]][-1, 2] + data[[k]][-nrow(data[[k]]), 2]) / 2, 
                        data[[k]][, 2][nrow(data[[k]])])
    pointStarts[[k]] <- c(data[[k]][1, 2], pointEnds[[k]][-nrow(data[[k]])])
    weights[[k]] <- pointEnds[[k]] - pointStarts[[k]]
    
    maxPos[k] <- max(data[[k]][, 2])
    minPos[k] <- min(data[[k]][, 2])
  }
  
  pieceInd <- 1
  start <- max(minPos)
  piecePositions <- matrix(numeric(0), ncol = 2, nrow = 0)
  colnames(piecePositions) <- c("start", "end")
  pieceDetails <- list()
  pieceOutliers <- matrix(ncol = length(data), nrow = 0)
  
  # Minimum step size in order to overcome convergence problems
  minStep <- mean(weights[[1]], na.rm = TRUE) / 1000
  # As long as we have not reached the end of data yet, check if a piece ending
  # at the current position is valid (then take it) or not (then go ahead)
  while(start + l <= min(maxPos)){
    hasFewOutliers <- logical()
    tempRes <- list()
    tempPos <- list()
    tempOut <- numeric(length(data))
    for(k in 1:length(data)){
      tempRes[[k]] <- outsideWeight(start, k)
      tempPos[[k]] <- tempRes[[k]]$pos
      tempOut[k] <- tempRes[[k]]$relOutside
      hasFewOutliers[k] <- tempOut[k] <= params[[k]][4]
    }
    
    # if all parameters do not exceed their outlier limits, take the piece, else
    # reject it and go on
    if(all(hasFewOutliers)){
      pieceDetails[[pieceInd]] <- list()
      for(k in 1:length(data)){
        pieceDetails[[pieceInd]][[k]] <- cbind("indData" = as.integer(tempPos[[k]]),
                                               "pos" = data[[k]][tempPos[[k]], 2], 
                                               "value" = data[[k]][tempPos[[k]], 1])
      }
      piecePositions <- rbind(piecePositions, c("start" = start, 
                                                "end" = start + l))
      pieceOutliers <- rbind(pieceOutliers, tempOut)
      
      start <- start + l
      pieceInd <- pieceInd + 1
    } else {
      # Find which parameter exceeds its outlier limit the most and take that
      # overhead as length to move on
      maxOutside <- 0
      for(k in 1:length(data)){
        maxOutside <- max(maxOutside, tempRes[[k]]$relOutside - params[[k]][4])
      }
      
      start <- start + max(maxOutside * l, minStep)
    }
  }
  nPieces <- pieceInd - 1
  
  # Construct Output
  results <- list()
  results[["summary"]] <- c("length" = l,
                            "nPieces" = nPieces, 
                            "goodMaterial" = l * nPieces, 
                            "allMaterial" = max(maxPos - minPos), 
                            "badMaterial" = max(maxPos - minPos) - l * nPieces, 
                            "badMaterialPercent" = (1 - (l * nPieces / max(maxPos - minPos))) * 100)
  results[["piecePositions"]] <- piecePositions
  if(keepDetails) results[["pieceData"]] <- pieceDetails
  rownames(pieceOutliers) <- NULL
  results[["pieceOutliers"]] <-	pieceOutliers
  results[["data"]] <- data
  results[["params"]] <- params
  class(results) <- "pieceSelection"
  
  return(results)
}


# makeCostMatrix - an internal preparation function for oneOpt and seqOpt.
#                  Generates a matrix giving the relative amount of outliers in
#                  each parameter given that a piece is cut out at the interval
#                  [x - l, x], where x is any of the first parameter's positions.
#                  Also outputs some other stuff necessary in later parts of the 
#                  optimizers (it would be a shame to recalculate them).
# Input:
#  data - list of dataframes (or matrices). Each dataframe corresponds to one 
#         quality parameter and has 2 columns. The first one is the measurement 
#         and the second one the position of this measurement.
#  params - list of numerics. Each vector corresponds to one quality parameter 
#           and includes its target value, lower specification limt, upper
#           specification limit and the relative amount of allowed outliers per
#           pieces.
#  l - numeric of length 1. The desired length of pieces.
#
# Output:
# list of 4 elements:
#  cost - matrix with each row corresponding to each row corresponding to a 
#         parameter and each col corresponding to one end position. Gives the
#         number of relative area of a piece ending there (or Inf, if any of
#         the parameters exceeds their limit)
#  ends - numeric with as many entries as possible ending positions showing 
#         which ending position each index is connected to
#  correspondingEnds - matrix where each col j corresponds to an ending positions
#                      and each row i corresponds to a parameter. Used for matching
#                      the different parameters' positions. Each each entry ij
#                      is saying "the piece ending at position i in parameter 1
#                      ends at this index in parameter j"
#  nInvolvedPositions - matrix where each col corresponds to an ending position and
#                    each row corresponds to a parameter. Gives the number of 
#                    measurements starting from the correspondingEnd of a piece
#                    that belong to that piece in every parameter
makeCostMatrix <- function(data, params, l){
  # find the endings of the area each datapoint represents:
  ends <- list()
  for(j in seq(along = data)){
    ends[[j]] <- c((data[[j]][-1,2] + data[[j]][-dim(data[[j]])[1],2]) / 2, 
                   data[[j]][dim(data[[j]])[1], 2])
  }
  
  # if a piece ends at position i, how many of the datapoints prior to i belong
  # to that piece? (this does not have to be the same for all parameters)
  nInvolvedPositions <- matrix(nrow = dim(data[[1]])[1], ncol = length(data))
  correspondingEnds <- matrix(nrow = dim(data[[1]])[1], ncol = length(data))
  for(j in seq(along = nInvolvedPositions[1,])){
    for(i in seq(along = nInvolvedPositions[,1])){
      # as the parameters might have different positions, calculate for each
      # parameter which point is the last one of a piece
      correspondingEnds[i,j] <- ifelse(j == 1, i, min(which(ends[[j]] >= ends[[1]][i])))
      nInvolvedPositions[i,j] <- correspondingEnds[i,j] - 
        suppressWarnings(max(which(ends[[j]] <= ends[[1]][i] - l)))
    }
  }
  
  # Determine weights of points according to their covered area:
  weights <- list()
  for(k in seq(along = data)){
    weights[[k]] <- c(0, data[[k]][-1, 2] - data[[k]][-dim(data[[k]])[1], 2]) / 2 + 
      c(data[[k]][-1, 2] - data[[k]][-dim(data[[k]])[1], 2], 0) / 2
  }
  
  # per row (parameter): Costs of a piece that ends at position (col)
  cost <- matrix(Inf, ncol = dim(data[[1]])[1], nrow = length(data))
  for(i in seq(dim(data[[1]])[1])){
    if(any(nInvolvedPositions[i,] == Inf)){
      # if no piece can end here (too early to cut a piece), return Inf costs
      cost[, i] <- Inf
    } else {
      timesOutside <- numeric(length(data))
      for(j in seq(along = timesOutside)){
        # which points belong to piece i in parameter j
        relevantPoints <- seq(from = correspondingEnds[i,j] - nInvolvedPositions[i,j] + 1,
                              to = correspondingEnds[i,j], by = 1)
        
        # calculate how much area each point represents in the piece (called weights)
        tempWeight <- numeric()
        tempWeight[relevantPoints] <- weights[[j]][relevantPoints]
        # if(relevantPoints[1] != 1){
        #   # Where the Area of the first point starts:
        #   startFirst <- ends[[j]][relevantPoints[1] - 1]
        #   # if(ends[[1]][i] - l < startFirst){
        #   #   # Add previous point with corrected weight
        #   #   relevantPoints <- c(relevantPoints[1] - 1, relevantPoints)
        #   #   tempWeight[relevantPoints[1]] <- startFirst - start
        #   # }
        #   if(ends[[1]][i] - l > startFirst){
        #     # Take away weight from first point
        #     tempWeight[relevantPoints[1]] <- tempWeight[relevantPoints[1]] - 
        #       (ends[[1]][i] - l - startFirst)
        #   }
        # }
        
        # Weight of the first one always has to be shortened:
        tempWeight[relevantPoints[1]] <- ends[[j]][relevantPoints[1]] - 
          (ends[[1]][i] - l)
        
        # Weight of last one has to be shortened if it is not the first param:
        if(j > 1){
          tempWeight[relevantPoints[length(relevantPoints)]] <- 
            tempWeight[relevantPoints[length(relevantPoints)]] - 
            (ends[[j]][correspondingEnds[i, j]] - ends[[1]][i])
        }
        
        # find outlier points and the relative amount of outliers per piece
        outPoints <- !(params[[j]][2] <= data[[j]][relevantPoints,1] &
                         data[[j]][relevantPoints,1] <= params[[j]][3])
        outPoints <- relevantPoints[outPoints]
        timesOutside[j] <- sum(tempWeight[outPoints]) / 
          sum(tempWeight[relevantPoints])
      }
      if(any(timesOutside > sapply(params, "[", 4))){
        # if the outliers exceed their allowed limit in any parameter, the piece
        # is invalid
        cost[, i] <- Inf
      } else {
        cost[, i] <- timesOutside
      }
    }
  }
  
  return(list(cost = cost, ends = ends[[1]], correspondingEnds = correspondingEnds,
              nInvolvedPositions = nInvolvedPositions))
}

# cutOneOpt - knapsack strategy that optimizes the first parameter (that is the 
#             one that is at position 1 in the data list), but does not further 
#             optimize the others.
# Input:
#  data - list of dataframes (or matrices). Each dataframe corresponds to one 
#         quality parameter and has 2 columns. The first one is the measurement 
#         and the second one the position of this measurement.
#  params - list of numerics. Each vector corresponds to one quality parameter 
#           and includes its target value, lower specification limt, upper
#           specification limit and the relative amount of allowed outliers per
#           pieces.
#  l - numeric of length 1. The desired length of pieces.
#  keepDetails - logical. Wheter to return measurements and positions for each piece.
# Output:
# object of class pieceSelection including:
#  summary - numeric with a summary of the found pieces, including length, number,
#            sum of length, length of given material, waste of material, relative
#            amount of waste
#  piecePos - matrix with two columns. Each row corresponds to one found piece.
#             First column gives its start position, second column its end position.
#  pieceOutliers - matrix, relative amount of outliers per piece (row) and 
#                  parameter (column)
#  data - the input argument data
#  params - the input argument params
#  pieceData - only returned if keepDetails == TRUE. list of lists of dataframes.
#              Each is a part of the input argument data, but restrained on the
#              area of the pieces.
cutOneOpt <- function(data, params, l, keepDetails=TRUE){
  # transform the data into the optimzation problem
  pre <- makeCostMatrix(data, params, l)
  cost <- pre$cost
  ends <- pre$ends
  correspondingEnds <- pre$correspondingEnds
  nInvolvedPositions <- pre$nInvolvedPositions
  
  # compute the recursive formula by dynamic programming (filing up a matrix 
  # bottom up, see knapsack problem)
  # rows represent number of searched pieces, columns the ending positions
  rek <- matrix(Inf, nrow = 1, ncol = nrow(data[[1]]))
  row <- 1
  repeat{
    for(col in which(rowSums(nInvolvedPositions) != Inf)){
      if(row != 1){
        rek[row, col] <- min(rek[row, col - 1], 
                             cost[1, col] + rek[row - 1, col - nInvolvedPositions[col]])
      } else {
        # if row == 1, the row - 1 above will not work, therefore do it manually
        rek[row, col] <- min(rek[row, col - 1], cost[1, col])
      }
    }
    
    # if we were able to find the amount of pieces, add another row and repeat,
    # else delete the current row and stop
    if(all(rek[row, ] == Inf)){
      rek <- rek[-row, , drop = FALSE]
      break
    }
    
    row <- row + 1
    rek <- rbind(rek, Inf)
  }
  nPieces <- nrow(rek)
  
  # Search for the optimal selection of pieces. It is found by starting at the
  # bottom right edge of the matrix and going left until there is a shift in the
  # costs (see knapsack problem)
  pieceDetails <- vector("list", nPieces) 
  piecePositions <- matrix(nrow = nPieces, ncol = 2)
  colnames(piecePositions) <- c("start", "end")
  pieceOutliers <- matrix(nrow = nPieces, ncol = length(data))
  indPiece <- nPieces
  
  row <- dim(rek)[1]
  col <- dim(rek)[2]
  while(row > 0 && col > 0){
    if(rek[row, col] < rek[row, col - 1]){
      # the col-th element is the end of a piece, add it to the list
      pieceDetails[[indPiece]] <- list()
      for(j in 1:length(data)){
        relevantPoints <- seq(from = correspondingEnds[col,j] - nInvolvedPositions[col,j] + 1,
                              to = correspondingEnds[col,j], by = 1)
        pieceDetails[[indPiece]][[j]] <- 
          cbind("indData" = relevantPoints, 
                "pos" = data[[j]][relevantPoints,2], 
                "value" = data[[j]][relevantPoints,1])
      }
      piecePositions[indPiece, ] <- c(ends[max(pieceDetails[[indPiece]][[1]][,1])] - l, 
                                      ends[max(pieceDetails[[indPiece]][[1]][,1])])
      pieceOutliers[indPiece, ] <- cost[, col]
      
      indPiece <- indPiece - 1
      col <- col - nInvolvedPositions[col]
      row <- row - 1
    } else {
      col <- col - 1
    }
  }
  
  # Generate Output
  maxPos <- sapply(data, function(dat) max(dat[, 2])) 
  minPos <- sapply(data, function(dat) min(dat[, 2]))
  
  results <- list()
  results[["summary"]] <- c("length" = l, 
                            "nPieces" = nPieces, 
                            "goodMaterial" = l * nPieces, 
                            "allMaterial" = max(maxPos - minPos),
                            "badMaterial" = max(maxPos - minPos) - l * nPieces, 
                            "badMaterialPercent" = (1 - (l * nPieces / max(maxPos - minPos))) * 100)
  results[["piecePositions"]] <- piecePositions
  if(keepDetails) results[["pieceData"]] <- pieceDetails
  rownames(pieceOutliers) <- NULL
  results[["pieceOutliers"]] <-	pieceOutliers
  results[["data"]] <- data
  results[["params"]] <- params
  class(results) <- "pieceSelection"
  
  return(results)
}

# cutSeqOpt - slower approach, but it optimizes all given parameters in the
#             order of their occurence in the list. That means, if there are 
#             several possible selections of pieces that have the same opti-
#             mality value regarding the first parameter, the second parameter 
#             is evaluated on those selections. This yields a sequential opti-
#             mization.
# Input:
#  data - list of dataframes (or matrices). Each dataframe corresponds to one 
#         quality parameter and has 2 columns. The first one is the measurement 
#         and the second one the position of this measurement.
#  params - list of numerics. Each vector corresponds to one quality parameter 
#           and includes its target value, lower specification limt, upper
#           specification limit and the relative amount of allowed outliers per
#           pieces.
#  length - numeric of length 1. The desired length of pieces.
#  nChoose - numeric. Number of pieces to be searched for.
#  keepDetails - logical. Wheter to return measurements and positions for each piece.
# Output:
# object of class pieceSelection including:
#  summary - numeric with a summary of the found pieces, including length, number,
#            sum of length, length of given material, waste of material, relative
#            amount of waste
#  piecePos - matrix with two columns. Each row corresponds to one found piece.
#             First column gives its start position, second column its end position.
#  pieceOutliers - matrix, relative amount of outliers per piece (row) and 
#                  parameter (column)
#  data - the input argument data
#  params - the input argument params
#  pieceData - only returned if keepDetails == TRUE. list of lists of dataframes.
#              Each is a part of the input argument data, but restrained on the
#              area of the pieces.
cutSeqOpt <- function(data, params, l, nChoose, keepDetails=TRUE){
  # Have to catch this special case or else we will have an out of bound exception
  if(nChoose > 0){
    # transform the data into the optimzation problem
    pre <- makeCostMatrix(data, params, l)
    cost <- pre$cost
    ends <- pre$ends
    correspondingEnds <- pre$correspondingEnds
    nInvolvedPositions <- pre$nInvolvedPositions
    
    # Assume we cut out a piece of length l that ends at position endPos. Which 
    # positions are then cut out and which preceeding positions are still in the
    # non-cut-out part so that we can cut further pieces from them?
    # Compute for each piece which of the preceeding positions are allowed to be
    # ends of pieces as well while not overlapping the piece
    # array's first dimension: which predecessors are allowed
    #         second dim: for which position we are looking at
    #         third dim: if we have to choose this many pieces
    pred <- array(FALSE, dim = c(dim(data[[1]])[1], dim(data[[1]])[1], nChoose))
    for(endPos in seq(dim(pred)[2])){
      if(endPos - nInvolvedPositions[endPos, 1] >= 1)
        pred[1:(endPos - nInvolvedPositions[endPos, 1]), endPos, 1] <- TRUE
    }
    if(dim(pred)[3] > 1){
      for(costInd in 2:(dim(pred)[3]))
        pred[,,costInd] <- pred[,,1]
    }
    # Special case for picking the first piece: All are allowed as long as they
    # do not exceed the band's area:
    pred[,nInvolvedPositions[, 1] != Inf, 1] <- TRUE
    
    # Apply sequential optimization (see paper):
    for(costInd in seq(dim(cost)[1])){ # For all cost parameters
      ownCost <- matrix(Inf, ncol = dim(data[[1]])[1], nrow = nChoose)
      for(pieceCount in seq(nChoose)){
        for(endPos in seq(dim(data[[1]])[1])){
          if(any(pred[, endPos, pieceCount])){
            if(pieceCount == 1){
              ownCost[pieceCount, endPos] <- cost[costInd, endPos]
            } else {
              predCost <- min(ownCost[pieceCount - 1, pred[, endPos, pieceCount]])
              pred[, endPos, pieceCount] <- pred[, endPos, pieceCount] & 
                (ownCost[pieceCount - 1, ] == predCost)
              ownCost[pieceCount, endPos] <- cost[costInd, endPos] + predCost
            }
          }
        }
      }
      isLowest <- ownCost[nChoose, ] == min(ownCost[nChoose, ])
      pred[, !isLowest, nChoose] <- FALSE
      if(nChoose > 1){
        for(s in seq(nChoose - 1, 1, by = -1)){
          for(j in 1:dim(data[[1]])[1]){
            # delete all its preds if it has no succeeder
            if(!any(pred[j, ,s + 1])){
              pred[, j, s] <- FALSE
            }
          }
        }
      }
    }
    
    # See if any solution was found:
    if(min(ownCost[nChoose, ]) != Inf){
      # Walk through pred to give out the optimal solution
      # start with the one that has the lowest owncosts
      bestEnds <- integer(nChoose)
      bestEnds[nChoose] <- which.min(ownCost[nChoose, ])
      if(nChoose > 1){
        for(i in seq(nChoose, 2, by = -1)){
          bestEnds[i - 1] <- max(which(pred[, bestEnds[i], i]))
        }
      }
    } else {
      bestEnds <- numeric(0)
    }
  } else {
    bestEnds <- numeric(0)
  }
  
  # Grab all relevant piece data for the bestEnds
  pieceDetails <- list()
  piecePositions <- matrix(ncol = 2, nrow = 0)
  colnames(piecePositions) <- c("start", "end")
  pieceOutliers <- matrix(nrow = 0, ncol = length(data))
  
  indPiece <- 1
  for(col in bestEnds){
    pieceDetails[[indPiece]] <- list()
    # Gather information about all cost parameters j:
    for(j in 1:length(data)){
      relevantPoints <- seq(from = correspondingEnds[col,j] - nInvolvedPositions[col,j] + 1,
                            to = correspondingEnds[col,j], by = 1)
      pieceDetails[[indPiece]][[j]] <- 
        cbind("indData" = relevantPoints, 
              "pos" = data[[j]][relevantPoints,2], 
              "value" = data[[j]][relevantPoints,1])
    }
    # compute the position of the piece
    piecePositions <- rbind(piecePositions, c("start" = ends[max(pieceDetails[[indPiece]][[1]][,1])] - l, 
                                              "end" = ends[max(pieceDetails[[indPiece]][[1]][,1])]))
    pieceOutliers <- rbind(pieceOutliers, cost[, col])
    
    indPiece <- indPiece + 1
  }
  nPieces <- indPiece - 1
  
  # compute summary information
  maxPos <- sapply(data, function(dat) max(dat[, 2])) 
  minPos <- sapply(data, function(dat) min(dat[, 2]))
  
  results <- list()
  results[["summary"]] <- c("length" = l, 
                            "nPieces" = nPieces, 
                            "goodMaterial" = l * nPieces, 
                            "allMaterial" = max(maxPos - minPos),
                            "badMaterial" = max(maxPos - minPos) - l * nPieces, 
                            "badMaterialPercent" = (1 - (l * nPieces / max(maxPos - minPos))) * 100)
  results[["piecePositions"]] <- piecePositions
  if(keepDetails) results[["pieceData"]] <- pieceDetails
  results[["pieceOutliers"]] <-	pieceOutliers
  results[["data"]] <- data
  results[["params"]] <- params
  class(results) <- "pieceSelection"
  
  return(results)
}


# interpolate: internal help function for cutCombSearch. adds more discrete 
#              artificial data points for the knapsack optimizers.
# Input:
#  data - list of dataframes (or matrices). Each dataframe corresponds to one 
#         quality parameter and has 2 columns. The first one is the measurement 
#         and the second one the position of this measurement.
#  factor - numeric. new data will have factor * oldLength datapoints
#           Must be an uneven number in order to not alter the jump positions 
#           of the quality functions.
# Output:
#  data, but with the added points.
interpolate <- function(data, factor = 3){
  if(factor %% 2 == 0)
    stop("factor has to be uneven")
  
  cat("Increasing search grid size...")
  flush.console()
  
  newData <- as.data.frame(matrix(ncol = 2, nrow = factor * (dim(data)[1] - 1) + 1))
  colnames(newData) <- colnames(data)
  
  # Insert cutting points
  w <- seq(1, 0, by = -1 / factor)
  for(i in seq(along = data[-1,2])){
    newData[((i - 1) * factor + 1):(i * factor + 1), 2] <- w * data[i, 2] + (1 - w) * data[i + 1, 2]
    newData[((i - 1) * factor + 1):(i * factor + 1), 1] <- rep(data[c(i, i+1), 1], each = (factor + 1) / 2)
  }
  
  return(newData)
}

# cutCombSearch - combines cutNaive, cutOneOpt and cutSeqOpt. A fitting function
#                 is chosen depending on available system rescources and expected
#                 usage of system memory. Adds more cutting points if it fails to
#                 find an optimal result.
# Input:
#  data - list of dataframes (or matrices). Each dataframe corresponds to one 
#         quality parameter and has 2 columns. The first one is the measurement 
#         and the second one the position of this measurement.
#  params - list of numerics. Each vector corresponds to one quality parameter 
#           and includes its target value, lower specification limt, upper
#           specification limit and the relative amount of allowed outliers per
#           pieces.
#  length - numeric of length 1. The desired length of pieces.
#  nChoose - numeric. Number of pieces to be searched for.
#  maxInterpol - numeric, how many times should new cutting points be added before
#                stopping and returning a non optimal result.
# Output:
# object of class pieceSelection including:
#  summary - numeric with a summary of the found pieces, including length, number,
#            sum of length, length of given material, waste of material, relative
#            amount of waste
#  piecePos - matrix with two columns. Each row corresponds to one found piece.
#             First column gives its start position, second column its end position.
#  pieceOutliers - matrix, relative amount of outliers per piece (row) and 
#                  parameter (column)
#  data - the input argument data
#  params - the input argument params
cutCombSearch <- function(data, params, length, nPieces, maxInterpol = 2){
  interpolCount <- 0
  while(interpolCount <= maxInterpol){
    # If the problem requires less than 8GB RAM and has more than one 
    # parameter, use SeqOpt:
    if(length(data) > 1 & nrow(data[[1]])^2 * nPieces * 4 <= 8e+09){
      cat("Trying to optimize all parameters...")
      flush.console()
      res <- cutSeqOpt(data, params, length, nPieces)
    } else {
      cat("Trying to optimize only first parameter...")
      flush.console()
      res <- cutOneOpt(data, params, length)
    }
    
    # Check if the the required number of pieces was found:
    if(res[["summary"]][2] == nPieces){
      cat("Success!\n")
      flush.console()
      return(res)
    } else {
      # if they were not found, interpolate
      if(interpolCount < maxInterpol){
        data[[1]] <- interpolate(data[[1]])
      }
    }
    interpolCount <- interpolCount + 1
  }
  
  # if after the max number of interpolations the number of pieces was still not 
  # found, return naive optimization result
  cat("Failed! Returning valid, but non-optimal result.\n")
  flush.console()
  
  return(cutNaive(data, params, length))
}


# cutSplitSearch - uses cutNaive to look for possible points to divide the problems
#                  into subproblems without losing too much optimality. Then applies
#                  cutCombSearch on each of the subproblems and recombines the result.
# Input:
#  data - list of dataframes (or matrices). Each dataframe corresponds to one 
#         quality parameter and has 2 columns. The first one is the measurement 
#         and the second one the position of this measurement.
#  params - list of numerics. Each vector corresponds to one quality parameter 
#           and includes its target value, lower specification limt, upper
#           specification limit and the relative amount of allowed outliers per
#           pieces.
#  length - numeric of length 1. The desired length of pieces.
# Output:
# object of class pieceSelection including:
#  summary - numeric with a summary of the found pieces, including length, number,
#            sum of length, length of given material, waste of material, relative
#            amount of waste
#  piecePos - matrix with two columns. Each row corresponds to one found piece.
#             First column gives its start position, second column its end position.
#  pieceOutliers - matrix, relative amount of outliers per piece (row) and 
#                  parameter (column)
#  data - the input argument data
#  params - the input argument params
cutSplitSearch <- function(data, params, length){
  cat("Finding maximum number of pieces...\n")
  flush.console()
  
  resNaive <- cutNaive(data, params, length)
  
  nPieces <- resNaive[["summary"]][2]
  if(nPieces == 0){
    return(resNaive)
  }
  
  # Try to partition the problem (see paper) to reduce CPU and RAM load
  # .Machine$double.eps^0.5 due to numeric inaccuracies of float numbers
  beforeSkip <- which(abs(resNaive$`piecePositions`[-1 , 1] - resNaive$`piecePositions`[-nPieces, 2]) > .Machine$double.eps^0.5)
  nPiecePart <- diff(c(0, beforeSkip, nPieces))
  partitions <- c(min(data[[1]][, 2]), rep(resNaive$`piecePositions`[beforeSkip + 1, 1], each = 2), max(data[[1]][, 2]))
  partitions <- matrix(partitions, ncol = 2, byrow = TRUE)
  # partitions is now a matrix that includes start and end positions of the 
  # search area for the subproblems
  
  nPartitions <- dim(partitions)[1]
  if(nPartitions){
    cat("Partitioning problem into", nPartitions, "subproblems.\n")
    flush.console()
  }
  
  # For each partition, search for pieces
  resPart <- list()
  for(p in seq(nPartitions)){
    if(nPartitions > 1){
      cat("Solving", p, "/", nPartitions, ": ")
      flush.console()
    }
    
    # create a fitting sub-dataset for each partition:
    dataPart <- data
    partStart <- min(which(dataPart[[1]][, 2] >= partitions[p, 1]))
    partEnd <- max(which(dataPart[[1]][, 2] <= partitions[p, 2]))
    dataPart[[1]] <- dataPart[[1]][partStart:partEnd, ]
    
    # Add points at start and end of partition to actually search the whole 
    # partition area
    addFirst <- c(data[[1]][max(partStart - 1, 1), 1], partitions[p, 1])
    if(partStart > 1){
      # Add mid point between first point and point before
      midFirst <- c(data[[1]][partStart, 1], 
                    mean(data[[1]][c(partStart - 1, partStart), 2]))
      if(midFirst[2] > addFirst[2]){
        addFirst <- rbind(addFirst, midFirst)
        colnames(addFirst) <- colnames(dataPart[[1]]) # avoid problems with rbind
      }
    }
    addLast <- c(data[[1]][partEnd, 1], partitions[p, 2])
    # Avoid conflicts with rbind
    dataPart[[1]] <- rbind(addFirst, dataPart[[1]], addLast)
    
    # apply cutCombSearch
    resPart[[p]] <- cutCombSearch(dataPart, params, length, nPiecePart[p])
  }
  
  # Construct output
  results <- list()
  results[["summary"]] <- c("length" = length,
                            "nPieces" = sum(sapply(resPart, function(x) x$summary[2])), 
                            "goodMaterial" = sum(sapply(resPart, function(x) x$summary[3])), 
                            "allMaterial" = unname(resNaive$summary[4]), 
                            "badMaterial" = unname(resNaive$summary[4] - sum(sapply(resPart, function(x) x$summary[3]))), 
                            "badMaterialPercent" = 0)
  results[["summary"]][6] <- results[["summary"]][5] / results[["summary"]][4] * 100
  results[["piecePositions"]] <- do.call(rbind, lapply(resPart, function(x) x$'piecePositions'))
  results[["pieceOutliers"]] <-	do.call(rbind, lapply(resPart, function(x) x$'pieceOutliers'))
  results[["data"]] <- data
  results[["params"]] <- params
  class(results) <- "pieceSelection"
  
  return(results)
}
