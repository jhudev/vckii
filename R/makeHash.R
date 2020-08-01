#' Makes hash text file
#'
#' This function generates a hash file for a dataset. Also, returns a hash.
#' @param data Working dataset used to make the hash file.
#' @param file A character string naming the file and (path) directory to write the hash file.
#' @param addHash Logical, should the hash be included in file name? (default: F)
#' @param colName Logical, should return hash include hash for column names? (default: T)
#' @param rowName Logical, should return hash include hash for row names? (default: T)
#' @param suppress.output Logical, should the output be suppressed? (default: F)
#' @keywords hash, key, make
#' @export
#' @examples
#' test <- iris #< Set working dataset
#'
#' #Create hash file and export data
#' makeHash(test,"test_hash.txt") #Create hash file, returns "overall" hash
#' makeHash(test,"test_hash.txt",addHash=TRUE) #Option: Add "overall" hash to file name
#' #Note: Using default, colName=TRUE and rowName=F as we only export column names
#' # > Hashes are created separately for the data and both row and column names
#' write.csv(test,"test.csv",row.names=FALSE)
#'
#' # Data gets transferred, etc....
#'
#' # Import data and check hash
#' test2 <- read.csv("test.csv")
#' checkHash(test2,"test_hash.txt",simple=TRUE) #< check if imported data matches hash, returns T/F
#' #If simple==TRUE (default), it will only check three hashes: data, colNames, rowNames
#' #Can set to ignore colNames and rowNames (default values: colName.ignore=F,rowName.ignore=TRUE)
#'
#' #One can do manual checking/debugging using fetchHash.data() and fetchHash.file()
#' fetchHash.data(test2,t="cell+col") # < Fetches the a hash from data
#' fetchHash.file("test_hash.txt",t="cell+col") # < Fetches the a hash from hash file
#' #Note: t parameter for determines type of hash to fetch (options and explanation listed below)
#' # 'all' - returns 26char hash for data, column and row names (cell-colName-rowName)
#' # 'cell' - returns 8char hash for data only
#' # 'cell+col' - (default) returns 17 digit hash for data and column names (cell-colName)
#' # 'cell+row' - returns 19 digit hash for data and row names (cell-x-rowName)
#' # 'col' OR 'row' - returns single 8char hash for combined column OR row
#' # 'colValue' OR 'rowValue' - returns list of 8char hashes for each column OR row
#' # 'colName' OR 'rowName' - returns 8char hash for column OR row names
#'
#' # GET SUMMARY INFORMATION for checkHash()--------------
#' checkHash(test2,"test_hash.txt",summary=TRUE) #< OPTION, provide additional info on errors
#' # Example output (with explanation)
#' # >SUMMARY: Hash Check (summary==TRUE):
#' # >Hash match: All        : [T/F] < if the overall hash of the data match
#' # >Hash match: Column name: [T/F] < if the col names/header match (colName.ignore=TRUE)
#' # >Hash match: Column     : [T/F] < if the hashes by column match
#' # >Hash match: Row name   : [T/F] < if the row names match (can be ignored, rowName.ignore=F)
#' # >Hash match: Row        : [T/F] < if the hashes by row match
#' # >Check result.......... : Pass/Fail < return TRUE/FALSE
#'
#'
#' # Simulated error
#' test2_Error <- test2
#' #Error: change a single value
#' test2_Error[[1,2]] <- 4
#' #Error: shift values in one column
#' test2_Error[90:150,4] <- c(test2_Error[150,4],test2_Error[90:149,4])
#' fetchHash.data(test2_Error)
#' checkHash(test2_Error,"test_hash.txt")
#'
#' #' # GET ADDITIONAL INFORMATION for checkHash()--------------
#' checkHash(test2_Error,"test_hash.txt",summary=TRUE,details=TRUE) #< OPTION, provide additional info
#' # Example output (with explanation)
#' # >DETAILS: Additional Info (details==TRUE)
#' # >HashFile format match  : [T/F] < if formatting of the hash file is correct
#' # >HashFile length match  : [T/F] < if length of hashes match dim. of data
#' # ># columns match        : [T/F] < if # columns match hashes
#' # ># rows match           : [T/F] < if # rows match hashes
#' # >List mismatched Columns (below): < Lists column # of data with mismatched hash
#' # >List mismatched Rows (below): < Lists row # of data with mismatched hash
#'
#' #Plot mismatched hashes  (details will automatically set to TRUE)
#' checkHash(test2_Error,"test_hash.txt",plot=TRUE) #< OPTION, plot rows/columns with error
#' #Get mismatched hashes (details will automatically set to TRUE)
#' checkHash(test2_Error,"test_hash.txt",returnError=TRUE) #< OPTION, return rows/column mismatches


makeHash <- function(data,file,addHash=FALSE,colName=TRUE,rowName=TRUE,suppress.output=FALSE){
  #Check if inputs are correct
  if(!is.data.frame(data)){stop("'data' is not a proper data.frame.")}
  if(nrow(data)==0){stop("'data' is empty.")}
  if(!(addHash %in% c(TRUE,FALSE))){stop("'addHash' must be either TRUE(T)/FALSE(F).")}
  if(!(colName %in% c(TRUE,FALSE))) stop("'colName' must be either TRUE(T)/FALSE(F).")
  if(!(rowName %in% c(TRUE,FALSE))) stop("'rowName' must be either TRUE(T)/FALSE(F).")
  if(!(suppress.output %in% c(TRUE,FALSE))) stop("'suppress.output' must be either TRUE(T)/FALSE(F).")

  #Convert all factors into characters
  data <- factorToChar(data)

  #HASH GEN++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Create hashes
  hashID_gen <- .pFetchHash_all(data) #< "All" entire dataset
  hashID_colNames <- .pFetchHash_colName(data) # Hash for column names/headers
  hashID_rowNames <- .pFetchHash_rowName(data) # Hash for row names
  hashIDList_col <- .pFetchHash_col(data) # Hash each column
  hashID_col <- .pFetchHash_list(hashIDList_col) #Single hash for cols
  hashIDList_row <- .pFetchHash_row(data) # Hash each row
  hashID_row <- .pFetchHash_list(hashIDList_row) #Single hash for rows
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #Create hash id output (the hash outputed or written on file)
  if(colName & rowName){hashAddText <- paste(hashID_gen,hashID_colNames,hashID_rowNames,sep="-")
  }else if(colName & !rowName){hashAddText <- paste(hashID_gen,hashID_colNames,"X",sep="-")
  }else if(!colName & rowName){hashAddText <- paste(hashID_gen,"X",hashID_rowNames,sep="-")
  }else{hashAddText <- hashID_gen}

  #Add hash to filename, if selected
  if(addHash){
    fileExtLoc <- gregexpr("\\.",file)[[1]]
    fileExtLoc <- fileExtLoc[length(fileExtLoc)]
    file <- paste0(substr(file,1,fileExtLoc-1),"(h-",hashAddText,")",substr(file,fileExtLoc,nchar(file)))
  }

  #Make hash check file
  fileConn<-file(file)
  writeLines(c(paste(length(hashIDList_col),length(hashIDList_row),sep=" "),
               paste(hashID_gen,hashID_colNames,hashID_rowNames,sep=" "),
               paste(hashID_col,hashID_row,sep=" "),
               "col-hashId",hashIDList_col,
               "row-hashId",hashIDList_row), fileConn)
  close(fileConn)

  #Return file hash id
  if(suppress.output==FALSE){return(hashAddText)}
}
