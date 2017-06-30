### Script for counting pronouns ###

# Pronoun type can be one of a set number of options
# 1 = first person singular pronoun (fpsp)
# 2 = first person plural pronoun (fppp)
# 3 = third person singular pronoun (tpsp)
# 4 = third person plural pronoun (tppp)
# 5 = second person pronouns (spp)
#' Function which returns the count of a specific kind of pronoun
#' 
#' This function returns the count of a specific kind of pronoun. 
#' The kind of pronouns in use can be specified using the following 
#' numbers:
#' 1 = first person singular pronoun (fpsp)
#' 2 = first person plural pronoun (fppp)
#' 3 = third person singular pronoun (tpsp)
#' 4 = third person plural pronoun (tppp)
#' 5 = second person pronouns (spp)
#' @param wordFreqMatrix, the output from getFromFolderWF() or processXMLwf()
#' @param pronounType, a number in [1,5] which maps to a list of pronouns in a manner described in the function description 
#' @keywords tokenize, word frequency, token, pronoun count, pronoun
#' @export

pronounCount <- function(wordFreqMatix, pronounType) {
  # all the pronoun options
  fpsp <- c('i', 'im', 'me', 'my', 'mine', 'myself', 'i m')
  fppp <- c('we', 'we re', 'us', 'our', 'ourselves')
  tpsp <- c('he', 'she', 'him', 'her', 'his', 'hers', 'himself', 'herself')
  tppp <- c('them', 'theirs', 'they', 'themselves')
  spp <- c('you', 'yourselves', 'yall', 'yalls', 'your', 'yours')
  
  # Empty variable to hold the selected vector
  pronounsInUse <- c()
  
  # If/else chain to determine which option was selected,
  # and to assign pronounsInUse accordingly
  if (pronounType < 3) {
    if (pronounType == 1) {
      pronounsInUse <- fpsp
    }
    else if (pronounType == 2) {
      pronounsInUse <- fppp
    }
  }
  else if (pronounType >= 3) {
    if (pronounType == 3) {
      pronounsInUse <- tpsp
    }
    else if (pronounType == 4) {
      pronounsInUse <- tppp
    }
    else {
      pronounsInUse <- spp
    }
  }
  
  # Find the count by calling the function getCount with a 
  # data structure which hold the unique tokens and the fequency
  # of their occurance and the string indicating the token requested,
  # here held inside pronounsInUse
  count <- 0
  for(i in 1:length(pronounsInUse)) {
    tempCount <- getCount(wordFreqMatix, pronounsInUse[i])
    if (!is.na(tempCount)) {
      count <- count + tempCount
    }
  }
  return(count)
}

# Function which returns the total count of all pronouns in the text
# Using all possible pronoun lists.
#' Function which returns the count of all pronouns in the document
#' 
#' This function returns the count of all the pronouns in the documents
#' @param wordFreqMatrix, the output from getFromFolderWF() or processXMLwf()
#' @keywords tokenize, word frequency, token, pronoun count, pronoun
#' @export

allPronounCount <- function(wordFreqMatrix) {
  allCount <- 0
  for (i in 1:5) {
    allCount <- allCount + pronounCount(wordFreqMatrix, i)
  }
  return(allCount)
}