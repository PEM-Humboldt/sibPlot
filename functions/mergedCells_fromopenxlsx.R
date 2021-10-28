require(openxlsx)

readUTF8 <- function(x)
{
  readLines(x, warn = FALSE, encoding = "UTF-8")
}

removeHeadTag <- function (x) 
{
  x <- paste(x, collapse = "")
  if (any(grepl("<\\?", x))) {
    x <- gsub("<\\?xml [^>]+", "", x)
  }
  x <- gsub("^>", "", x)
  x
}
getRId <- function (x) 
{
  regmatches(x, gregexpr("(?<= r:id=\")[0-9A-Za-z]+", x, perl = TRUE))
}


replaceXMLEntities <- function (v) 
{
  v <- gsub("&amp;", "&", v, fixed = TRUE)
  v <- gsub("&quot;", "\"", v, fixed = TRUE)
  v <- gsub("&apos;", "'", v, fixed = TRUE)
  v <- gsub("&lt;", "<", v, fixed = TRUE)
  v <- gsub("&gt;", ">", v, fixed = TRUE)
  return(v)
}

getSharedStringsFromFile <- function (sharedStringsFile, isFile) 
{
  sharedStrings <- openxlsx:::get_shared_strings(xmlFile = sharedStringsFile, 
                                      isFile = isFile)
  Encoding(sharedStrings) <- "UTF-8"
  z <- tolower(sharedStrings)
  sharedStrings[z == "true"] <- "TRUE"
  sharedStrings[z == "false"] <- "FALSE"
  z <- NULL
  sharedStrings <- replaceXMLEntities(sharedStrings)
  return(sharedStrings)
}

mergeCell2mapping <- function (x) 
{
  refs <- regmatches(x, regexpr("(?<=ref=\")[A-Z0-9:]+", x, 
                                perl = TRUE))
  refs <- strsplit(refs, split = ":")
  rows <- lapply(refs, function(r) {
    r <- as.integer(gsub(pattern = "[A-Z]", replacement = "", 
                         r, perl = TRUE))
    seq(from = r[1], to = r[2], by = 1)
  })
  cols <- lapply(refs, function(r) {
    r <- convertFromExcelRef(r)
    seq(from = r[1], to = r[2], by = 1)
  })
  refs <- do.call("rbind", lapply(seq_along(rows), function(i) {
    tmp <- expand.grid(cols = cols[[i]], rows = rows[[i]])
    tmp$ref <- paste0(openxlsx:::convert_to_excel_ref(cols = tmp$cols, 
                                           LETTERS = LETTERS), tmp$rows)
    tmp$anchor_cell <- tmp$ref[1]
    return(tmp[, c("anchor_cell", "ref", "rows")])
  }))
  refs <- refs[refs$anchor_cell != refs$ref, ]
  return(refs)
}


# From read.xlsx.default
getCellMerge <- function(xlsxFile,sheet)
{
  na.strings<-"NA"
  rows<-NA
  xmlDir <- file.path(tempdir(), paste0(sample(LETTERS, 10), collapse = ""), "_excelXMLRead")
  xmlFiles <- unzip(xlsxFile, exdir = xmlDir)
  on.exit(unlink(xmlDir, recursive = TRUE), add = TRUE)
  sharedStringsFile <- grep("sharedStrings.xml$", xmlFiles, perl = TRUE, value = TRUE)
  workbook <- grep("workbook.xml$", xmlFiles, perl = TRUE, value = TRUE)
  workbookRelsXML <- grep("workbook.xml.rels$", xmlFiles, perl = TRUE, value = TRUE)
  workbookRelsXML <- paste(readUTF8(workbookRelsXML), collapse = "")
  workbookRelsXML <- openxlsx:::getChildlessNode(xml = workbookRelsXML, tag = "Relationship")
  workbook <- unlist(readUTF8(workbook))
  sheets <- unlist(regmatches(workbook, gregexpr("(?<=<sheets>).*(?=</sheets>)", workbook, perl = TRUE)))
  sheets <- unlist(regmatches(sheets, gregexpr("<sheet[^>]*>", sheets, perl = TRUE)))
  sheets <- grep("r:id=\"[[:blank:]]*\"", sheets, invert = TRUE, value = TRUE)
  sheetrId <- unlist(getRId(sheets))
  sheetNames <- unlist(regmatches(sheets, gregexpr("(?<=name=\")[^\"]+", sheets, perl = TRUE)))
  sheetNames <- replaceXMLEntities(sheetNames)
  nSheets <- length(sheetrId)
  file_name <- sapply(sheetrId, function(rId) {
    txt <- grep(sprintf("Id=\"%s\"", rId), workbookRelsXML, fixed = TRUE, value = TRUE)
    regmatches(txt, regexpr("(?<=Target=\").+xml(?=\")", txt, perl = TRUE))
  })
  sheetNames <- openxlsx:::replaceXMLEntities(sheetNames)
  sheetInd <- which(sheetNames == sheet)
  sheet <- file_name[sheetInd]
  worksheet <- xmlFiles[grepl(tolower(sheet), tolower(xmlFiles), fixed = TRUE)]
  
  # Not quite sure what is the goal of "sharedStrings"
  if (length(sharedStringsFile) > 0) {
    sharedStrings <- getSharedStringsFromFile(sharedStringsFile = sharedStringsFile, isFile = TRUE)
    if (!is.null(na.strings)) {
      sharedStrings[is.na(sharedStrings) | sharedStrings %in% na.strings] <- "openxlsx_na_vlu"
    }
  }else {
    sharedStrings <- ""
  }
  
  startRow<-1
  
  rows <- NA
  skipEmptyRows <- FALSE
  detectDates<- FALSE
  
  cell_info <- openxlsx:::getCellInfo(xmlFile = worksheet, sharedStrings = sharedStrings, skipEmptyRows = skipEmptyRows, startRow = startRow, rows = rows, getDates = detectDates)
  
  regex="^<mergeCell ref=\"([A-Z]{1,4}[0-9]{1,7}:[A-Z]{1,4}[0-9]{1,7})\"/>$"
  stopifnot(all(grepl(regex,cell_info$cellMerge)))
  cellMerge<-sub(regex,"\\1",cell_info$cellMerge)
  return(cellMerge)
}

xlAd2rowCol<-function(xlAd)
{
  regex_a<-"^([A-Z]*)([0-9]*)"
  col_let<-sub(regex_a,"\\1",xlAd)
  row<-as.integer(sub(regex_a,"\\2",xlAd))
  n_let<-max(nchar(col_let))
  ref_let<-LETTERS
  if(n_let>1){
    for(i in 2:n_let){
    ref_let<-c(ref_let,apply(
      expand.grid(
        lapply(1:i,function(x)return(LETTERS))
        )[i:1]
      ,1,function(x)paste0(x,collapse = "",sep="")))
    }}
  col<- match(col_let,ref_let)
  return(data.frame(xlAd,col_let,col,row))
}

# From... me!
treatCellRange<-function(cellRange)
{
   splitRange<-strsplit(cellRange,":")
   mat<-matrix(unlist(splitRange),ncol=2,byrow = T,dimnames = list(NULL,c("from","to")))
   from_num<-xlAd2rowCol(mat[,"from"])
   to_num<-xlAd2rowCol(mat[,"to"])
   return(data.frame(
     from=mat[,"from"],
     to=mat[,"to"],
     from_row=from_num$row,
     from_col=from_num$col,
     to_row=to_num$row,
     to_col=to_num$col,
     nrow=(to_num$row-from_num$row)+1,
     ncol=(to_num$col-from_num$col)+1
   ))
}



xl_fillMerged<-function(fileXl,getWorkbook=F,writeFile=!is.na(dos_unMerged),dos_unMerged=NA,overwrite=T)
{
  stopifnot(length(fileXl)==1)
  regex_file<-"(.*)\\.xlsx$"
  if(writeFile)
  {
    fileXl_base<-basename(fileXl)
    dos_ori<-dirname(fileXl)
    if(is.na(dos_unMerged))
    {
      dos_unMerged<-dos_ori
    }
    if(!file.exists(dos_unMerged))
    {
      dir.create(dos_unMerged)
    }
  file_unmerged<-paste(dos_unMerged,sub(regex_file,"\\1_unmerged.xlsx",fileXl_base))
  }
  stopifnot(grepl(regex_file,fileXl_base))
  sheetNames<-openxlsx::getSheetNames(fileXl)
  wb<-openxlsx::loadWorkbook(fileXl)
  data_wb<-lapply(sheetNames,openxlsx::read.xlsx,xlsxFile=wb, colNames=F, detectDates=T, skipEmptyRows=F, skipEmptyCols=F)
  names(data_wb)<-sheetNames
  mergedCells<-lapply(sheetNames,getCellMerge,xlsxFile=fileXl)
  names(mergedCells)<-sheetNames
  mergedCells_t<-lapply(mergedCells,function(x)
    if(length(x)>0)
    {
      treatCellRange(x)
    }else{
      NULL
    })
  mergedCells_v<-mapply(function(x,y)
  {
    if(length(y)>0)
    {
      res<-y
      res$valueOri<-x[cbind(row=y$from_row,col=y$from_col)]
      return(res)
    }else{return(NULL)}
  },data_wb,mergedCells_t)
  names(mergedCells_v)<-sheetNames
  mergedCells_v<-lapply(mergedCells_v,function(x)
    if(is.null(x)){return(NULL)}else{
      return(x[!is.na(x$valueOri),])
    })
  for(i in 1:length(mergedCells_v))
  {
    if(length(mergedCells_v[[i]])==0){next}
    sheet<-names(mergedCells_v)[i]
    for(j in 1:nrow(mergedCells_v[[i]]))
    {
      dat_replace<-matrix(mergedCells_v[[i]][j,"valueOri"],ncol=mergedCells_v[[i]][j,"ncol"],nrow=mergedCells_v[[i]][j,"nrow"])
      data_wb[[sheet]][mergedCells_v[[i]][j,"from_row"]:mergedCells_v[[i]][j,"to_row"],mergedCells_v[[i]][j,"from_col"]:mergedCells_v[[i]][j,"to_col"]]<-mergedCells_v[[i]][j,"valueOri"]
      if(getWorkbook|writeFile){
        openxlsx::removeCellMerge(wb,sheet,
                                  cols=mergedCells_v[[i]][j,"from_col"]:mergedCells_v[[i]][j,"to_col"],
                                  rows=mergedCells_v[[i]][j,"from_row"]:mergedCells_v[[i]][j,"to_row"]
                                  )
        openxlsx::writeData(wb,sheet,dat_replace,
                            startCol = mergedCells_v[[i]][j,"from_col"],
                            startRow = mergedCells_v[[i]][j,"from_row"],
                            colNames = F,
                            rowNames = F
                            )
      }
    }
  }
  if(writeFile)
  {
    openxlsx::saveWorkbook(wb,file=file_unmerged,overwrite=overwrite)
  }
  if(getWorkbook)
  {
    return(wb)
  }else{
    return(invisible(data_wb))
  }
  
}


