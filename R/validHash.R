#' Checks the validity of hash file
#'
#' This function checks if a hash file generated from makeHashKey() is valid.
#' @param file A character string naming the file and (path) directory of the hash file
#' @keywords hash, key, validity
#' @export
#' @examples
#' #Step 1: User 1 exporting dataset “iris”
#' makeHash(iris,"iris_hash.txt",addHash=TRUE) #< write hashfile
#' write.csv(iris, "iris.csv",row.names=FALSE) #< export data
#' hashFile("iris.csv",addHash=TRUE) #< gets hash of data file
#'
#' #Step 2: Transferring data and hash files from User 1 to User 2
#'
#' #Step 3: User 2 importing dataset “iris.csv” and validating with “iris_hash.txt”
#' hashFile("iris(h-f3c14350).csv") #< checks hash of data file
#' validHash("iris_hash(h-65257ac8-ab891a64-d8663379).txt") #< checks if valid hash file
#' iris_2 <- read.csv("iris(h-f3c14350).csv") #< import data
#' checkHash(iris_2, "iris_hash(h-65257ac8-ab891a64-d8663379).txt") #< check hash

validHash <- function(file){
  #Check if inputs are correct
  if(!file.exists(file)) stop("'file' does not exist.")
  if(substr(file,nchar(file)-3,nchar(file))!=".txt") stop("'file' is not a valid text file.")

  #Fetch hash ids from hash key file----------------------------
  con <- file(file,"r")
  hashChkAll <- readLines(con)
  close(con)

  hashChkDim <- as.numeric(strsplit(hashChkAll[1],split=" ")[[1]]) # <- fetch dimension (1 row)
  hashChkStats_main <- strsplit(hashChkAll[2],split=" ")[[1]] #< fetch data, colNames, rowNames hash (2 row)
  hashChkStats_sec <- strsplit(hashChkAll[3],split=" ")[[1]] #< fetch col and row single hash (3 row)
  hashChk_col <- hashChkAll[5:(4+hashChkDim[1])]
  hashChk_row <- hashChkAll[(4+hashChkDim[1]+2):(4+hashChkDim[1]+1+hashChkDim[2])]
  hashChk_placeHold_col <- hashChkAll[4]
  hashChk_placeHold_row <- hashChkAll[(4+hashChkDim[1]+1)]

  hashID_col <- .pFetchHash_list(hashChk_col) #Single hash for cols
  hashID_row <- .pFetchHash_list(hashChk_row) #Single hash for rows

  if(length(hashChkDim)!=2){return(FALSE)
  }else if(any(hashChkDim%%1!=0)){return(FALSE)
  }else if(length(hashChkStats_main)!=3){return(FALSE)
  }else if(any(nchar(hashChkStats_main)!=8)){return(FALSE)
  }else if(length(hashChkStats_sec)!=2){return(FALSE)
  }else if(any(nchar(hashChkStats_sec)!=8)){return(FALSE)
  }else if(hashChk_placeHold_col!="col-hashId"){return(FALSE)
  }else if(hashChk_placeHold_row!="row-hashId"){return(FALSE)
  }else if(length(hashChkAll)!=(5+hashChkDim[1]+ hashChkDim[2])){return(FALSE)
  }else if(hashChkStats_sec[1]!=hashID_col){return(FALSE)
  }else if(hashChkStats_sec[2]!=hashID_row){return(FALSE)
  }else{return(TRUE)}
}
