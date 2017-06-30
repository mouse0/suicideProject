### Script to read xml data ###
require(XML)
require(stringr)

#' Function to process XML data from a specific corpus into unique tokens and their frequencies
#' 
#' This function takes a .xml documents from a corpus of forum posts
#' and returns the unique tokens and their frequencies in one data frame.  Can perhaps
#' be used for other forum corpora which have a similar structure
#' @param pathToFolder, the path to the folder containing the corpus
#' @param minMaxWordCount, no documents with less tokens than indicated will be accepted and all documents longer than the spefified count will be cropped, defaults to 300.  If it is set to NULL documents will not be cut and will only be excluded if they contain no usable characters
#' @keywords xml, processing, cleaning, corpus
#' @export
XMLgetOneDataFrame <- function(pathToFolder, minMaxWordCount = 300) {
  if (!requireNamespace("XML", quietly = TRUE)) {
    stop("XML is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  files <- list.files(pathToFolder, pattern = "*.xml")
  documents <- ""
  print("Precent of documents processed: ")
  pb <- txtProgressBar(min = 0, max = length(files), initial = 1, char = "=", width = NA, style = 3, file = "")
  acceptedDocuments <- 0
  for(i in 1:length(files)) {
    fileName <- paste(pathToFolder, files[i], sep = "")
    data <- xmlParse(fileName)
    xml_data <- xmlToList(data)
    text <- tryCatch( {
      xml_data$message$body$text
    },
    error = function(cond) {
      NULL
    },
    warning = function(cond) {
      # do nothing
    })
    if (is.null(text)) {
      next
    }
    cleanText <- str_replace_all(text, "<[:print:]*?>", " ")
    cleanText <- str_replace_all(cleanText, "<//[:print:]*?\\[:print:]*?<", " ")
    cleanText <- str_replace_all(cleanText, "&nbsp[:punct:]", " ")
    cleanText <- iconv(cleanText, "latin1", "ASCII", sub="")
    
    if (grepl("^\\s*$", cleanText)) {
      next
    }
    if (is.null(minMaxWordCount)) {
      documents <- paste(documents, cleanText)
      acceptedDocuments <- acceptedDocuments + 1
    }
    else {
      cutText <- getFixedSampleSizeS(cleanText, minMaxWordCount)
      if (!is.null(cutText)) {
        documents <- paste(documents, text)
        acceptedDocuments <- acceptedDocuments + 1
      }
    }
    setTxtProgressBar(pb, i)
  }
  wordFrequencies <- tokenize(documents)
  print("Number of accepted documents: ")
  print(acceptedDocuments)
  print("Out of: ")
  print(length(files))
  return(wordFrequencies)
}
#' Function to process XML data from a specific corpus into unique tokens and their frequencies
#' 
#' This function takes a .xml documents from a corpus of forum posts
#' and returns the unique tokens and their frequencies.  Can perhaps
#' be used for other forum corpora which have a similar structure
#' @param pathToFolder, the path to the folder containing the corpus
#' @param minMaxWordCount, no documents with less tokens than indicated will be accepted and all documents longer than the spefified count will be cropped, defaults to 300.  If it is set to NULL documents will not be cut and will only be excluded if they contain no usable characters
#' @keywords xml, processing, cleaning, corpus
#' @export
processXMLwf <- function(pathToFolder, minMaxWordCount = 300) {
  if (!requireNamespace("XML", quietly = TRUE)) {
    stop("XML is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  files <- list.files(pathToFolder, pattern = "*.xml")
  wordFrequencies <- c()
  print("Precent of documents processed: ")
  pb <- txtProgressBar(min = 0, max = length(files), initial = 1, char = "=", width = NA, style = 3, file = "")
  acceptedDocuments <- 0
  for(i in 1:length(files)) {
    fileName <- paste(pathToFolder, files[i], sep = "")
    data <- xmlParse(fileName)
    xml_data <- xmlToList(data)
    text <- tryCatch( {
      xml_data$message$body$text
    },
    error = function(cond) {
      NULL
    },
    warning = function(cond) {
      # do nothing
    })
    if (is.null(text)) {
      next
    }
    cleanText <- str_replace_all(text, "<[:print:]*?>", " ")
    cleanText <- str_replace_all(cleanText, "<//[:print:]*?\\[:print:]*?<", " ")
    cleanText <- str_replace_all(cleanText, "&nbsp[:punct:]", " ")
    cleanText <- iconv(cleanText, "latin1", "ASCII", sub="")
    
    if (grepl("^\\s*$", cleanText)) {
      next
    }
    if (is.null(minMaxWordCount)) {
      wordFrequency <- tokenize(cleanText)
      wordFrequencies <- c(wordFrequencies, wordFrequency)
      acceptedDocuments <- acceptedDocuments + 1
    }
    else {
      wordFrequency <- getFixedSampleSizeWF(cleanText, minMaxWordCount)
      if (!is.null(wordFrequency)) {
        wordFrequencies <- c(wordFrequencies, wordFrequency)
        acceptedDocuments <- acceptedDocuments + 1
      }
    }
    setTxtProgressBar(pb, i)
  }
  print("Number of accepted documents: ")
  print(acceptedDocuments)
  print("Out of: ")
  print(length(files))
  return(wordFrequencies)
}

#' Function to process XML data from a specific corpus into a vector of strings
#' 
#' This function takes a .xml documents from a corpus of forum posts
#' and returns a vector of strings.  Can perhaps
#' be used for other forum corpora which have a similar structure
#' @param pathToFolder, the path to the folder containing the corpus
#' @param minMaxWordCount, no documents with less tokens than indicated will be accepted and all documents longer than the spefified count will be cropped Defaults to 300
#' @keywords xml, processing, cleaning, corpus
#' @export
processXMLstring <- function(pathToFolder, minMaxxWordCount = 300) {
  if (!requireNamespace("XML", quietly = TRUE)) {
    stop("XML is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  files <- list.files(pathToFolder)
  documents <- c()
  print("Precent of documents processed: ")
  pb <- txtProgressBar(min = 0, max = length(files), initial = 1, char = "=", width = NA, style = 3, file = "")
  acceptedDocuments <- 0
  for(i in 1:length(files)) {
    fileName <- paste(pathToFolder, files[i], sep = "")
    data <- xmlParse(fileName)
    xml_data <- xmlToList(data)
    text <- tryCatch( {
      xml_data$message$body$text
    },
    error = function(cond) {
      NULL
    },
    warning = function(cond) {
      # do nothing
    })
    if (is.null(text)) {
      next
    }
    
    cleanText <- str_replace_all(text, "<[:print:]*?>", " ")
    cleanText <- str_replace_all(cleanText, "<//[:print:]*?\\[:print:]*?<", " ")
    cleanText <- str_replace_all(cleanText, "&nbsp[:punct:]", " ")
    cleanText <- iconv(cleanText, "latin1", "ASCII", sub="")
    
    temp <- getFixedSampleSizeS(cleanText, minMaxWordCount)
    if(!is.null(temp)) {
      documents <- c(documents, temp)
      acceptedDocuments <- acceptedDocuments + 1
    }
    setTxtProgressBar(pb, i)
  }
  print("Number of accepted documents: ")
  print(acceptedDocuments)
  print("Out of: ")
  print(length(files))
  return(documents)
}

#' Function for coping and sorting files
#' 
#' which takes a tab seperated value file containing the ids of files
#' which have already been tagged as belonging to a given category and
#' copies them into a specified folder so they can be processed seperately
#' @param pathToTrainingLabels, path to the tab separtated value file
#' @param pathToTrainingData, path to the data which is described by the tsv file
#' @param destinationFolder, folder where the files will be copied, 
#' @param type, the label used in the tsv to describe the requested files it is also assumed to be the name of the folder inside the requested directory where the files will go
#' @keywords xml, processing, cleaning, corpus
#' @export
processXMLwithTrainingLabels <- function(pathToTrainingLabels, pathToTrainingData, destinationFolder, type) {
  trainingLabels <- read.table(pathToTrainingLabels, sep = "\t")
  crisisIDs <- c()
  for(i in 1:nrow(trainingLabels)) {
    if(grepl(type, trainingLabels[i, 2])) {
      crisisIDs <- c(crisisIDs, trainingLabels[i, 1])
    }
  }
  for(i in 1:length(crisisIDs)) {
    fileName <- paste(pathToTrainingData, "post-", sep = "")
    fileName <- paste(fileName, crisisIDs[[i]], sep = "")
    fileName <- paste(fileName, ".xml", sep = "")
    destination <- paste(destinationFolder, type, sep = "")
    destination <- paste(destination, "/", sep = "")
    file.copy(fileName, destination)
  }
}
