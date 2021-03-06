% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lexicalTagger.R
\name{writeLexTag}
\alias{writeLexTag}
\title{lexicalTaggging file function}
\usage{
writeLexTag(taggedTokens, pathToStorage)
}
\arguments{
\item{taggedTokens, }{the output from either lexTagFile, lexTagString, or phrasemachine's POS_tag_documents()}

\item{pathToStorage, }{the path to the folder you would like the output to go;}
}
\description{
This function is indended to be called only from inside lexTagFile 
or lexTagString, but may be called externally if desired.  This
function only writes the tags/tokens to a file and is fairly rigid in
what it expects (outputs from other POS taggers may not be compatable)
}
\keyword{lexical,}
\keyword{of}
\keyword{part}
\keyword{phrasemachine}
\keyword{speach,}
\keyword{tag,}
