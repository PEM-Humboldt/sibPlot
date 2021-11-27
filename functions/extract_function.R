extractFunction <- function(file, nameFunction, endMarker= "^#{3,} *END *#{3,}", commentBefore = T)
{
  allFile <- readLines(file)
  lineBegin <- which(grepl(paste0("\<",nameFunction,"\>"), allFile) & grepl("<- ?function",allFile))
  stopifnot(length(lineBegin) == 1)
  lineEnds <- grep(endMarker,allFile)
  stopifnot(length(lineEnds) > 0)
  lineEnd <- lineEnds[lineEnds > lineBegin][1] - 1
  stopifnot(length(lineEnd) == 1)
  # Include comments before function
  if (commentBefore & lineBegin > 1) {
    counter <- lineBegin
    while (grepl("^ *#",allFile[counter - 1]))
    {
      counter <- counter - 1
    }
    lineBegin <- counter
  }
  return(paste(allFile[lineBegin:lineEnd],collapse = "\n"))
}