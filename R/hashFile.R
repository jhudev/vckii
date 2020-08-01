#' Determine checksum hash of a file
#'
#' This function determines the checksum hash of a file using CRC32 (digest).
#' @param file A character string naming the file and (path) directory to write the hash key file.
#' @param addHash Logical, should the hash be added to the file name? (default: F)
#' @keywords hash, file, crc32
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

hashFile <- function(file,addHash=FALSE){
  #Return file hash id
  hash <- digest::digest(file=file,algo="crc32")

  #Add hash to filename, if selected
  if(addHash){
    fileExtLoc <- gregexpr("\\.",file)[[1]]
    fileExtLoc <- fileExtLoc[length(fileExtLoc)]
    fileRename <- paste0(substr(file,1,fileExtLoc-1),"(h-",hash,")",substr(file,fileExtLoc,nchar(file)))
    file.rename(file,fileRename)
  }
  return(hash)
}
