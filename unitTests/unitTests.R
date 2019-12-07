# Unit Tests for all optimization algorithms

library(testthat)
library(readxl)
source("../optFunctions.R")

# arePiecePositionsOk - checks for an pieceSelection object whether all selected
#                       pieces lay within the data area and do not overlap
# Output:
#  boolean
arePiecePositionsOk <- function(x){
  # Check if all pieces are within data area:
  if(nrow(x$piecePositions) > 0){
    dataStart <- max(sapply(x$data, function(x) min(x[, 2], na.rm = TRUE)))
    dataEnd <- min(sapply(x$data, function(x) max(x[, 2], na.rm = TRUE)))
    if(any(x$piecePositions[, "start"] < dataStart |
           x$piecePositions[, "end"] > dataEnd)){
      return(FALSE)
    }
  }
  
  # Check if any pieces overlap
  if(nrow(x$piecePositions) > 1){
    x$piecePositions <- x$piecePositions[order(x$piecePositions[, "start"]), ]
    if(any(x$piecePositions[-1, "start"] < x$piecePositions[-nrow(x$piecePositions), "end"])){
      return(FALSE)
    }
  }
  
  return(TRUE)
}


# ------ realistic data test -----
simData <- as.data.frame(read_excel("Simulated_Values.xlsx"))
simData <- list(simData[, 1:2], simData[, 3:4])

testNaiveNormalData <- function(){
  res <- cutNaive(data = simData, l = 0.5, keepDetails = FALSE,
                  params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 4)
}
testNaiveNormalData()

testOneOptNormalData <- function(){
  res <- cutOneOpt(data = simData, l = 0.5, keepDetails = FALSE,
                   params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 4)
}
testOneOptNormalData()

testSeqOptNormalData <- function(){
  res <- cutSeqOpt(data = simData, l = 0.5, nChoose = 4,
                   params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 4)
}
testSeqOptNormalData()

testSplitSearchNormalData <- function(){
  res <- cutSplitSearch(data = simData, l = 0.5, 
                        params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 4)
}
testSplitSearchNormalData()

testCombSearchNormalData <- function(){
  res <- cutCombSearch(data = simData, l = 0.5, nPieces = 4,
                       params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 4)
}
testCombSearchNormalData()


# ----- All data is i.o., will any material be wasted? -----
# (only on Naive, CombSearch and SplitSearch, because these are the only 
# algorithms that guarantee finding the maximum number of pieces)
testNaiveAllSelected <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0, 0), pos = c(0, 0.5, 1, 1.5)))
  res <- cutNaive(data, l = 0.5, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 3)
}
testNaiveAllSelected()

testSplitSearchAllSelected <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0, 0), pos = c(0, 0.5, 1, 1.5)))
  res <- cutSplitSearch(data, l = 0.5, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 3)
}
testSplitSearchAllSelected()

testCombSearchAllSelected <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0, 0), pos = c(0, 0.5, 1, 1.5)))
  res <- cutCombSearch(data, l = 0.5, params = list(c(0, -1, 1, 0)), nPieces = 3)
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 3)
}
testCombSearchAllSelected()


# ----- check if maximum quality is reached -----
# (not tested on Naive, because it does not guarantee optimality)
optQ <- read.csv2("optimalQualityTest.csv")
optQ <- list(optQ[, 1:2], optQ[, 3:4])

testOneOptMaxQuality <- function(){
  res <- cutOneOpt(data = optQ, l = 0.5, keepDetails = FALSE,
                   params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 7)
  expect_equal(sum(res$pieceOutliers[, 1]), 0)
}
testOneOptMaxQuality()

testSeqOptMaxQuality <- function(){
  res <- cutSeqOpt(data = optQ, l = 0.5, nChoose = 7,
                   params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 7)
  expect_equal(sum(res$pieceOutliers[, 1]), 0)
  expect_equal(sum(res$pieceOutliers[, 2]), 0)
}
testSeqOptMaxQuality()

testSplitSearchMaxQuality <- function(){
  res <- cutSplitSearch(data = optQ, length = 0.5, 
                        params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 7)
  expect_equal(sum(res$pieceOutliers[, 1]), 0)
  expect_equal(sum(res$pieceOutliers[, 2]), 0)
}
testSplitSearchMaxQuality()

testCombSearchMaxQuality <- function(){
  res <- cutCombSearch(data = optQ, length = 0.5, nPieces = 7,
                       params = list(c(9.2, 9.11, 9.48, 0.05), c(117, 115, 119, 0.05)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 7)
  expect_equal(sum(res$pieceOutliers[, 1]), 0)
  expect_equal(sum(res$pieceOutliers[, 2]), 0)
}
testCombSearchMaxQuality()


# ----- how do the algs handle data that does not start at 0? -----
testNaiveStartBiggerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(3, 3.09, 4.1)))
  res <- cutNaive(data = data, l = 0.5, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testNaiveStartBiggerThanZero()

testNaiveStartSmallerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(-4, -3.91, -2.9)))
  res <- cutNaive(data = data, l = 0.5, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testNaiveStartSmallerThanZero()

testOneOptStartBiggerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(3, 3.09, 4.1)))
  res <- cutOneOpt(data = data, l = 0.5, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testOneOptStartBiggerThanZero()

testOneOptStartSmallerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(-4, -3.91, -2.9)))
  res <- cutOneOpt(data = data, l = 0.5, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testOneOptStartSmallerThanZero()

testSeqOptStartBiggerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(3, 3.09, 4.1)))
  res <- cutSeqOpt(data = data, l = 0.5, nChoose = 2, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testSeqOptStartBiggerThanZero()

testSeqOptStartSmallerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(-4, -3.91, -2.9)))
  res <- cutSeqOpt(data = data, l = 0.5, nChoose = 2, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testSeqOptStartSmallerThanZero()

testSplitSearchStartBiggerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(3, 3.09, 4.1)))
  res <- cutSplitSearch(data = data, l = 0.5, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testSplitSearchStartBiggerThanZero()

testSplitSearchStartSmallerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(-4, -3.91, -2.9)))
  res <- cutSplitSearch(data = data, l = 0.5, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testSplitSearchStartSmallerThanZero()

testCombSearchStartBiggerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(3, 3.09, 4.1)))
  res <- cutCombSearch(data = data, l = 0.5, nPieces = 2, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testCombSearchStartBiggerThanZero()

testCombSearchStartSmallerThanZero <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(-4, -3.91, -2.9)))
  res <- cutCombSearch(data = data, l = 0.5, nPieces = 2, params = list(c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 2)
}
testCombSearchStartSmallerThanZero()


# ----- not-matching position areas across variables -----
testNaiveNonMatchingPositions <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(1, 1.55, 2.1)),
               data.frame(measurement = c(0, 0, 0), pos = c(3, 3.55, 4.1)))
  res <- cutNaive(data = data, l = 0.5, params = list(c(0, -1, 1, 0), c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 0)
}
testNaiveNonMatchingPositions()

testOneOptNonMatchingPositions <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(1, 1.55, 2.1)),
               data.frame(measurement = c(0, 0, 0), pos = c(3, 3.55, 4.1)))
  res <- cutOneOpt(data = data, l = 0.5, params = list(c(0, -1, 1, 0), c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 0)
}
testOneOptNonMatchingPositions()

testSeqOptNonMatchingPositions <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(1, 1.55, 2.1)),
               data.frame(measurement = c(0, 0, 0), pos = c(3, 3.55, 4.1)))
  res <- cutSeqOpt(data = data, l = 0.5, nChoose = 1,
                   params = list(c(0, -1, 1, 0), c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 0)
}
testSeqOptNonMatchingPositions()

testSplitSearchNonMatchingPositions <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(1, 1.55, 2.1)),
               data.frame(measurement = c(0, 0, 0), pos = c(3, 3.55, 4.1)))
  res <- cutSplitSearch(data = data, l = 0.5, params = list(c(0, -1, 1, 0), c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 0)
}
testSplitSearchNonMatchingPositions()

testCombSearchNonMatchingPositions <- function(){
  data <- list(data.frame(measurement = c(0, 0, 0), pos = c(1, 1.55, 2.1)),
               data.frame(measurement = c(0, 0, 0), pos = c(3, 3.55, 4.1)))
  res <- cutCombSearch(data = data, l = 0.5, nPieces = 0, 
                       params = list(c(0, -1, 1, 0), c(0, -1, 1, 0)))
  
  expect_true(arePiecePositionsOk(res))
  expect_equal(as.numeric(res$summary["nPieces"]), 0)
}
testCombSearchNonMatchingPositions()

# TOOO:
# weighting test in case of varying measurement intervals
# Edge cases: only one or no measurement, some empty variables etc
# can we make the splitSearch run into problems?
#  -> most likely can for edge cases because it only uses a heuristic, not a 
#     splitting strategy which we can prove for that it does not lose any 
#     quality


# ----- all tests together: -----
allTests <- matrix(c(class(try(testNaiveNormalData())) != "try-error",
                     class(try(testOneOptNormalData())) != "try-error",
                     class(try(testSeqOptNormalData())) != "try-error",
                     class(try(testSplitSearchNormalData())) != "try-error",
                     class(try(testCombSearchNormalData())) != "try-error",
                     
                     class(try(testNaiveAllSelected())) != "try-error",
                     NA,
                     NA,
                     class(try(testSplitSearchAllSelected())) != "try-error",
                     class(try(testCombSearchAllSelected())) != "try-error",
                     
                     NA,
                     class(try(testOneOptMaxQuality())) != "try-error",
                     class(try(testSeqOptMaxQuality())) != "try-error",
                     class(try(testSplitSearchMaxQuality())) != "try-error",
                     class(try(testCombSearchMaxQuality())) != "try-error",
                     
                     class(try(testNaiveStartBiggerThanZero())) != "try-error",
                     class(try(testOneOptStartBiggerThanZero())) != "try-error",
                     class(try(testSeqOptStartBiggerThanZero())) != "try-error",
                     class(try(testSplitSearchStartBiggerThanZero())) != "try-error",
                     class(try(testCombSearchStartBiggerThanZero())) != "try-error",
                     
                     class(try(testNaiveStartSmallerThanZero())) != "try-error",
                     class(try(testOneOptStartSmallerThanZero())) != "try-error",
                     class(try(testSeqOptStartSmallerThanZero())) != "try-error",
                     class(try(testSplitSearchStartSmallerThanZero())) != "try-error",
                     class(try(testCombSearchStartSmallerThanZero())) != "try-error",
                     
                     class(try(testNaiveNonMatchingPositions())) != "try-error",
                     class(try(testOneOptNonMatchingPositions())) != "try-error",
                     class(try(testSeqOptNonMatchingPositions())) != "try-error",
                     class(try(testSplitSearchNonMatchingPositions())) != "try-error",
                     class(try(testCombSearchNonMatchingPositions())) != "try-error"),
                   ncol = 5, byrow = TRUE)
colnames(allTests) <- c("Naive", "OneOpt", "SeqOpt", "SplitSearch", "CombSearch")
rownames(allTests) <- c("NormalData", "AllSelected", "MaxQuality", "StartBiggerThanZero",
                        "StartSmallerThanZero", "NonMatchingPositions")

allTests

