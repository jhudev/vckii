#' Check hash file with working dataset
#'
#' This function checks if a hash file generated from makeHash()
#' fits with hashes of the current working dataset. If not, it provides
#' debugging tools for locating inconsistencies.
#' @param data Working dataset used to check with hash key file. If not already a data.frame, it will be converted into a data.frame.
#' @param file A character string naming the file and (path) directory of the hash key.
#' @param simple Logical, simplified comparison (faster), return T/F (default: T)
#' @param colName.ignore Logical, should column name (header) hashes be ignored? (default: F)
#' @param rowName.ignore Logical, should row name hashes be ignored? (default: T)
#' @param summary Logical, do you want a summary of the check? (default: F)
#' @param details Logical, do you want a details of the check? (default: F)
#' @param plot Logical, if there are discrepencies, do you want to plot them? (default: F)
#' @param returnError Logical, do you want mismatched row/col ids returned? (default: F)
#' @keywords hash, key, check
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


checkHash <- function(data,file,simple=TRUE,colName.ignore=FALSE,rowName.ignore=TRUE,summary=FALSE,details=FALSE,plot=FALSE,returnError=FALSE){
  #Check if inputs are correct
  if(!is.data.frame(data)) stop("'data' is not a proper data.frame.")
  if(nrow(data)==0) stop("'data' is empty.")
  if(!file.exists(file)) stop("'file' does not exist.")
  if(!(colName.ignore %in% c(TRUE,FALSE))) stop("'colName.ignore' must be either TRUE(T)/FALSE(F).")
  if(!(rowName.ignore %in% c(TRUE,FALSE))) stop("'rowName.ignore' must be either TRUE(T)/FALSE(F).")
  if(!(summary %in% c(TRUE,FALSE))) stop("'summary' must be either TRUE(T)/FALSE(F).")
  if(!(details %in% c(TRUE,FALSE))) stop("'details' must be either TRUE(T)/FALSE(F).")
  if(!(plot %in% c(TRUE,FALSE))) stop("'plot' must be either TRUE(T)/FALSE(F).")

  #Convert all factors into characters
  data <- factorToChar(data)

  #Fetch hash ids from hash key file----------------------------
  con <- file(file,"r")
  hashChkStats <- readLines(con,n=3)
  close(con)
  hashChkStats_main <- strsplit(hashChkStats[2],split=" ")[[1]] #< fetch data, colNames, rowNames hash (2 row)
  hashChkDim <- as.numeric(strsplit(hashChkStats[1],split=" ")[[1]]) # <- fetch dimension (1 row)

  #HASH GEN++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Create hashes
  hashID_gen <- .pFetchHash_all(data) #< "All" entire dataset
  hashID_colNames <- .pFetchHash_colName(data) # Hash for column names/headers
  hashID_rowNames <- .pFetchHash_rowName(data) # Hash for row names
  if(simple & !(summary) & !(details) & !(plot) & !(returnError)){ # If only comparison of main overall hash id is requested, then stop here
    simpStatus <- c(FALSE,FALSE,FALSE) #Only give pass if all checks pass
    if(hashID_gen==hashChkStats_main[1]) simpStatus[1] <- TRUE
    if(colName.ignore | hashID_colNames==hashChkStats_main[2]) simpStatus[2] <- TRUE
    if(rowName.ignore | hashID_rowNames==hashChkStats_main[3]) simpStatus[3] <- TRUE
    return(all(simpStatus))
  }
  hashIDList_col <- .pFetchHash_col(data) # Hash each column
  hashID_col <- .pFetchHash_list(hashIDList_col) #Single hash for cols
  hashIDList_row <- .pFetchHash_row(data) # Hash each row
  hashID_row <- .pFetchHash_list(hashIDList_row) #Single hash for rows
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #Check if header, column, and row (hashes match with compared data)
  hashChkComp <- ifelse(c(hashChkStats_main,strsplit(hashChkStats[3],split=" ")[[1]])== #< fetch col and row single hash (3 row)
                          c(hashID_gen,hashID_colNames,hashID_rowNames,hashID_col,hashID_row),TRUE,FALSE)
  hashStatus <- hashChkComp
  if(colName.ignore) hashStatus[2] <- TRUE #< If ignore, change to T
  if(rowName.ignore) hashStatus[3] <- TRUE #< If ignore, change to T
  hashStatus <- all(hashStatus)


  #Provide SUMMARY of comparison: If selected
  if(summary){
    cat("SUMMARY: Hash Check (summary==T): \n")
    cat("Hash match: All        :",hashChkComp[1],"\n")
    cat("Hash match: Column name:",hashChkComp[2],"\n")
    cat("Hash match: Column     :",hashChkComp[4],"\n")
    cat("Hash match: Row name   :",hashChkComp[3],"\n")
    cat("Hash match: Row        :",hashChkComp[5],"\n")
    cat("Check result.......... :",ifelse(hashStatus==T,"Pass","Fail"),"\n")
  }

  #Provide DETAILED SUMMARY of comparison: If selected
  if(details | plot | returnError){
    #Fetch row and column hash ids from file
    con <- file(file,"r")
    hashChkAll <- readLines(con)
    close(con)
    #Get hash ids for rows and columns
    hashChk_col <- hashChkAll[5:(4+hashChkDim[1])]
    hashChk_row <- hashChkAll[(4+hashChkDim[1]+2):(4+hashChkDim[1]+1+hashChkDim[2])]
    #Check and print detailed error list
    cat(paste("DETAILS: Additional Info (details==T) \n"))
    # Check ID placeholder is present at right spot
    cat("HashFile format match  :",hashChkAll[4]=="col-hashId" & hashChkAll[(4+hashChkDim[1]+1)]=="row-hashId","\n")
    cat("HashFile length match  :",(4+hashChkDim[1]+1+hashChkDim[2])==length(hashChkAll),"\n")
    cat("# columns match        :",length(hashIDList_col)==length(hashChk_col),"\n")
    cat("# rows match           :",length(hashIDList_row)==length(hashChk_row),"\n")
    #List Col mismatches---------------------------
    cat("List mismatched Columns (below): \n")
    preValue <- -1
    ListError_col <- c() #< save mismatched ids
    for(colCount in 1:length(hashIDList_col)){
      if(hashIDList_col[colCount]!=hashChk_col[colCount]){
        if(preValue!=colCount-1){cat("Col #",colCount,"-",sep="")}
        preValue <- colCount
        ListError_col <- c(ListError_col,colCount)
      }else if(preValue==colCount-1) cat(colCount-1,"\n")
    }
    if(preValue==colCount){cat(colCount,"\n")}

    #List Row mismatches--------------------------
    cat("List mismatched Rows: (below) \n")
    preValue <- -1
    ListError_row <- c()  #< save mismatched ids
    for(rowCount in 1:length(hashIDList_row)){
      if(hashIDList_row[rowCount]!=hashChk_row[rowCount]){
        if(preValue!=rowCount-1){cat("Row #",rowCount,"-",sep="")}
        preValue <- rowCount
        ListError_row <- c(ListError_row,rowCount)
      }else if(preValue==rowCount-1) cat(rowCount-1,"\n")
    }
    if(preValue==rowCount){cat(rowCount,"\n")}
    #Plot the row and figures with issues if selected
    if(plot & (length(ListError_col)>0 | length(ListError_row)>0)){
      print(graphics::plot(1, type="n", xlab="columns", ylab="rows", xlim=c(0,hashChkDim[1]), ylim=c(hashChkDim[2],0)))
      print(graphics::abline(v=ListError_col,col=grDevices::rgb(1,0,0,alpha=0.3)))
      print(graphics::abline(h=ListError_row,col=grDevices::rgb(1,0,0,alpha=0.3)))
    }
  }

  #Return final decision of comparison
  if(returnError) return(list(status=hashStatus,colError=ListError_col,rowError=ListError_row))
  return(hashStatus)
}
