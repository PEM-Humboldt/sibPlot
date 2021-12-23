sqlizeNames <- function(x)
{
  gsub("^[-_.0-9]*","",gsub("\\_?([A-Z]{1,3})","_\\L\\1",gsub("^([A-Z]+)","\\L\\1",x,perl=T),perl=T))
}
##############END#############
