### Script for tagging text into lexical categories ###
### and for anaylzing proportions of lexical categories ###

require("phrasemachine")

# Function which uses phrasemachine to part of speech tag documents
#' lexicalTaggging file function
#' 
#' This function allows you to part of speech tag your corpus and write
#' the resulting tag/token relationships to a csv file.  This specific
#' function also uses a path to a folder containing *.txt documents
#' as input, as opposed to a pre-processed string
#' @param pathToFolder, the path to the folder containing the corpus
#' @param pathToStorage, the path to the folder you would like the output to go
#' @param minMaxWordCount, no documents with less tokens than indicated will be accepted and all documents longer than the spefified count will be cropped Defaults to 300
#' @keywords lexical, tag, part of speach, phrasemachine
#' @export

lexTagFiles <- function(pathToFolder, pathToStorage, minMaxWordCount = 300) {
  if (!requireNamespace("phrasemachine", quietly = TRUE)) {
    stop("phrasemachine is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  documents <- getFromFolderString(pathToFolder, minMaxWordCount)
  tagged <- POS_tag_documents(documents)
  # return(tagged)
  writeLexTag(tagged, pathToStorage)
}

#' lexicalTaggging string function
#' 
#' This function allows you to part of speech tag your corpus and write
#' the resulting tag/token relationships to a csv file.  This specific
#' function uses a pre-processed vector of strings for input, as opposed
#' to gathering such a vector itself from a folder containing a corpus
#' @param documents, a vector or list containing the strings to be tagged
#' @param pathToStorage, the path to the folder you would like the output to go
#' @keywords lexical, tag, part of speach, phrasemachine
#' @export

lexTagString <- function(documents, pathToStorage) {
  tagged <- POS_tag_documents(documents)
  # return(tagged)
  writeLexTag(tagged, pathToStorage)
}

# Function which writes the tagged text to a file
#' lexicalTaggging file function
#' 
#' This function is indended to be called only from inside lexTagFile 
#' or lexTagString, but may be called externally if desired.  This
#' function only writes the tags/tokens to a file and is fairly rigid in
#' what it expects (outputs from other POS taggers may not be compatable)
#' @param taggedTokens, the output from either lexTagFile, lexTagString, or phrasemachine's POS_tag_documents()
#' @param pathToStorage, the path to the folder you would like the output to go; 
#' @keywords lexical, tag, part of speach, phrasemachine
#' @export

writeLexTag <- function(taggedTokens, pathToStorage) {
  oldWorkingDir <- getwd()
  setwd(pathToStorage)
  count <- 0
  for (i in 1:length(taggedTokens)) {
    fileName <- paste("labeled", count, sep = "")
    fileName <- paste(fileName, ".csv", sep = "")
    # Data frame which will be written to the file
    dfToWrite <- data.frame(matrix(nrow=length(taggedTokens[[i]][1]), ncol = 2))
    # for loop to fill it
    for (j in 1:length(taggedTokens[[i]]$tokens)) {
      dfToWrite[j,1] <- taggedTokens[[i]]$tokens[j]
      dfToWrite[j,2] <- taggedTokens[[i]]$tags[j]
    }
    # Dataframe written to the appropriate file
    write.csv2(dfToWrite, fileName)
    # updating the counter to ensure that each document has 
    # a unique name
    count <- count + 1
  }
  setwd(oldWorkingDir)
}

# Function to retrieve the tagged tokens from a csv file 
# written by writeLexTag and return a data structure
#' Retrieves the tagged tokens from a csv file and places them in a
#' data structure
#' 
#' This function goes into the directory specified and finds the output
#' from writeLexTag and puts it in a data structure
#' @param workingDirectory, the folder where the tag/token csv files are stored
#' @keywords lexical, tag, part of speach, phrasemachine
#' @export

getLexTag <- function(workingDirectory) {
  files <- list.files(workingDirectory, pattern = "*.csv")
  allTags <- c()
  for(i in 1:length(files)) {
    fileName <- paste(workingDirectory, files[i], sep = "")
    tempTag <- read.csv(fileName, stringsAsFactors = FALSE)
    allTags <- c(allTags, tempTag)
  }
  finalTags <- c()
  index <- 1
  for(i in 1:length(allTags)) {
    if (i %% 2 == 0) {
      temp <- data.frame(allTags[[i]], stringsAsFactors = FALSE)
      finalTags <- c(finalTags, temp)
      index <- index + 1
    }
    else if (i %% 3 == 0) {
      temp <- data.frame(allTags[[i]], stringsAsFactors = FALSE)
      finalTags <- c(finalTags, temp)
      index <- index + 1
    }
  }
  return(finalTags)
}

# Uses the penn treebank pos tags, listed below
# CC Coordinating conjunction, CD cardinal number, DT determiner
# EX existential there, FW foreign word, IN preposition or 
# subordinating conjunction, JJ adjective, JJR Adjective, superlative
# LS list item marker, MD modal, NN noun, singular or mass
# NNS noun, plural, NNP proper noun, singular, NNPS proper noun, plural
# PDT predeterminer, POS, possessive ending, PRP personal pronoun,
# PRP$ possessive pronoun, RB adverb, RBR adverb, comparative
# RBS adverb, superlative, RP particle, SYM symbol, TO to
# UH interjection VB, verb, base form, VBD, verb, past tense
# VBG, Verb gerund or present participle
# VBP verb non 3rd person singular present
# VBZ verb 3rd person singular present, WDT whdeterminer
# WP whpronoun, WP$ possessive whpronoun, WRB, whadverb

# option dictates whether it is for all documents or
# invidual documents, all, individual
#' Function to get the count of any given part of speach
#' 
#' This function returns the count(s) of a given part of speech either as
#' a cumulative total (option = "all", the default) or a vector containing
#' the counts for each corresponing documents.  Acceptable tag types are as follows:
#' CC Coordinating conjunction, CD cardinal number, DT determiner
#' EX existential there, FW foreign word, IN preposition or
#' subordinating conjunction, JJ adjective, JJR Adjective, superlative
#' LS list item marker, MD modal, NN noun, singular or mass
#' NNS noun, plural, NNP proper noun, singular, NNPS proper noun, plural
#' PDT predeterminer, POS, possessive ending, PRP personal pronoun,
#' PRP$ possessive pronoun, RB adverb, RBR adverb, comparative
#' RBS adverb, superlative, RP particle, SYM symbol, TO to
#' UH interjection VB, verb, base form, VBD, verb, past tense
#' VBG, Verb gerund or present participle
#' VBP verb non 3rd person singular present
#' VBZ verb 3rd person singular present, WDT whdeterminer
#' WP whpronoun, WP$ possessive whpronoun, WRB, whadverb
#' @param taggedData, the output from the retieval function, getLexTag
#' @param tagType, the couple letter code from the list in the description
#' @param option, all or anything else will be accepted, all is the default and will return a cumulative total for the whole corpus, any other option will return individual counts for each document in a vector
#' @keywords lexical, tag, part of speach, phrasemachine
#' @export

getTagCategoryCount <- function(taggedData, tagType, option = "all") {
  occurances <- 0
  if (option == "all") {
    for (i in 1:length(taggedData)) {
      if (i %% 2 == 0) {
        for (j in 1:length(taggedData[[i]])) {
          # checks to see if the any tags match the tag requested
          if(taggedData[[i]][j] == tagType) {
            occurances <- occurances + 1
          }
        }
      }
    }
  }
  # Does the same as above, except it makes a vector with the
  # counts for each individual document.
  else {
    occurances <- c()
    index <- 1
    for (i in 1:length(taggedData)) {
      if (i %% 2 == 0) {
        for (j in 1:length(taggedData[[i]])) {
          if(taggedData[[i]][j] == tagType) {
            occurances[index] <- occurances + 1
          }
        }
      }
      index <- index + 1
    }
  }
  return(occurances)
}