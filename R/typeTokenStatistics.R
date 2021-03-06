### Script with functions to calculate type/token statistics ###

# method to get the type/token ratios in a vector
# from a folder
#' Function to get the type token ratios for a corpus
#'
#' This function cuts documents from a corpus to a fixed word count and
#' ignores the documents which are shorter than that word count.  The
#' accepted documents are then tokenized and the type/token ratios
#' for each are calculated and returned.
#' @param path, the path to the folder containing the corpus
#' @param minMaxWordCount, no documents with less tokens than indicated will be accepted and all documents longer than the spefified count will be cropped, defaults to 300.
#' @export
getTypeTokenRatios <- function(path, minMaxWordCount = 300) {

  # Get all of the unique tokens and their frequencies from a
  # specified folder
  wordFrequencyMatrix <- getFromFolderWF(path)

  # Calculate the type/token ratio for all of the documents

  # Create an empty vector to store the ratios in
  typeTokenRatios <- c(1:(length(wordFrequencyMatrix) / 2))
  # separate index for the vector
  currentIndex <- 1
  # Iterate through the matrix
  for (i in 1:length(wordFrequencyMatrix)) {
    # odd indexes hold unique tokens
    if (i %% 2 != 0) {
      typeTokenRatios[currentIndex] <- length(wordFrequencyMatrix[[i]])
    }
    # even indexes hold counts of unique tokens
    else {
      denominator <- sum(as.numeric(wordFrequencyMatrix[[i]]))
      denominator <- as.double(denominator)
      typeTokenRatios[currentIndex] <- typeTokenRatios[currentIndex] / denominator
      currentIndex <- currentIndex + 1
    }
  }
  return(typeTokenRatios)
}

# method to get the type/token ratios in a vector
# from an existing wordFreqencyMatrix
#' Function to calculate type token ratios
#'
#' This function takes a data structure created by this package
#' using methods such as getFromFolderWF and returns the type token
#' ratios.
#' @param wordFrequencyMatrix, a data strucutre generated by this package which contians the unique tokens and their counts
#' @keywords type token ratios, token, ratios
#' @export
getTypeTokenRatios <- function(wordFrequencyMatrix) {
  # Calculate the type/token ratio for all of the documents

  # Create an empty vector to store the ratios in
  typeTokenRatios <- c(1:(length(wordFrequencyMatrix) / 2))
  # separate index for the vector
  currentIndex <- 1
  # Iterate through the matrix
  for (i in 1:length(wordFrequencyMatrix)) {
    # odd indexes hold unique tokens
    if (i %% 2 != 0) {
      typeTokenRatios[currentIndex] <- length(wordFrequencyMatrix[[i]])
    }
    # even indexes hold counts of unique tokens
    else {
      denominator <- sum(as.numeric(wordFrequencyMatrix[[i]]))
      denominator <- as.double(denominator)
      typeTokenRatios[currentIndex] <- typeTokenRatios[currentIndex] / denominator
      currentIndex <- currentIndex + 1
    }
  }
  return(typeTokenRatios)
}

# Calculate some mesures of central tendency

# Arithmetic mean
#' Function to get the aritmetic mean
#'
#' This function takes the type token ratio data structure as an
#' argument and returns the arithmetic mean
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, ratios, mean
#' @export
arthMean <- function(typeTokenRatios) {
  temp <- sum(typeTokenRatios) / length(typeTokenRatios)
  return(temp)
}

# Median
#' Function to calculate the median
#'
#' This function takes a data structure containing type token ratios
#' created by this package and returns the median
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, ratios, median
#' @export
medTypeToken <- function(typeTokenRatios) {
  temp <- median(typeTokenRatios)
  return(temp)
}

# Mode
#' Function to calculate the mode
#'
#' This function takes a data structure containing type token ratios
#' created by this package and returns the mode
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, ratios, mode
#' @export
mode <- function(typeTokenRatios) {
  uniq <- unique(typeTokenRatios)
  uniq <- which.max(tabulate(match(typeTokenRatios, uniq)))
  uniq <- typeTokenRatios[[uniq]]
  return(uniq)
}

# Some measures of dispersion

# Range
#' Function to calculate the range
#'
#' This function takes a data structure containing type token ratios
#' created by this package and returns the range
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, ratios, range
#' @export
rangeTypeToken <- function(typeTokenRatios) {
  temp <- max(typeTokenRatios) - min(typeTokenRatios)
  return(temp)
}

# Variance
#' Function to calculate the variance
#'
#' This function takes a data structure containing type token ratios
#' created by this package and returns the variance
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, ratios, variance
#' @export
variance <- function(typeTokenRatios) {
  temp <- var(typeTokenRatios)
  return(temp)
}

# Standard Deviation
#' Function to calculate the standard deviation
#'
#' This function takes a data structure containing type token ratios
#' created by this package and returns the standard deviation
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, standard deviation
#' @export
standardDev <- function(typeTokenRatios) {
  temp <- sd(typeTokenRatios)
  return(temp)
}

# Interquartile Range
#' Function to calculate the interquartile range
#'
#' This function takes a data structure containing type token ratios
#' created by this package and returns the interquartile range
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, interquartile range
#' @export
interquartileRange <- function(typeTokenRatios) {
  temp <- IQR(typeTokenRatios)
  return(temp)
}

# Function to print all of the measures of central tendency
#' Function to print the measures of central tendency
#'
#' This function takes a data structure containing type token ratios
#' created by this package and prints the measures central tendency
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, central tendency
#' @export
printMeasuresOfCentralTendency <- function(typeTokenRatios) {
  print("Arithmetic Mean: ")
  print(arthMean(typeTokenRatios))

  print("Median: ")
  print(medTypeToken(typeTokenRatios))

  print("Mode: ")
  print(mode(typeTokenRatios))

}
# Function to print all of the measures of dispersion
#' Function to print the measures of dispersion
#'
#' This function takes a data structure containing type token ratios
#' created by this package and prints the measures of dispersion
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, dispersion
#' @export
printMeasuresOfDispersion <- function(typeTokenRatios) {
  print("Range: ")
  print(rangeTypeToken(typeTokenRatios))

  print("Variance")
  print(variance(typeTokenRatios))

  print("Standard Deviation")
  print(standardDev(typeTokenRatios))

  print("Interquartile Range")
  print(interquartileRange(typeTokenRatios))
}

# Funtion to plot the normal distrubtion
# and also the actual distribution
#' Function to plot the normal distribution and actual distribution
#'
#' This function takes a data structure containing type token ratios
#' created by this package plots the standard and actual definition
#' @param typeTokenRatios, the results from the type token ratio calculator
#' @keywords type token ratios, token, standard deviation
#' @export
plotDist <- function(typeTokenRatios) {
  q <- quantile(typeTokenRatios)
  stDev <- standardDev(typeTokenRatios)
  mean <- arthMean(typeTokenRatios)
  # function which returns the coordinates of a line describing
  # the normal distribution
  distFunc <- dnorm(q, mean = mean, sd = stDev)
  # Plotting functions
  plot(q, distFunc, type = "l", lty = 1, xlab = "Type Token Ratio", ylab = "Occurance", main = "Distribution of Type Token Ratios", ylim = c(0, 100), add = TRUE)
  hist(typeTokenRatios, col = alpha(cyan, 0.2), add = TRUE)
  legend('topright', c("normal distribution", "actual frequencies") , lty=1, col=c('black', 'cyan'), bty='n', cex=.75)
  return(distFunc)
}
