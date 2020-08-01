.pFetchHash_all <- function(data){
  tmp <- digest::digest(paste0(unlist(data),collapse="%"),algo="crc32")
  return(tmp)
}

.pFetchHash_col <- function(data){
  tmp <- lapply(1:length(data),
                function(e){
                  digest::digest(paste0(data[[e]],collapse="%"),algo="crc32")
                })
  return(unlist(tmp))
}

.pFetchHash_row <- function(data){
  tmp <- lapply(1:nrow(data),
                function(e){
                  digest::digest(paste0(data[e,],collapse="%"),algo="crc32")
                })
  return(unlist(tmp))
}

.pFetchHash_colName <- function(data){
  tmp <- digest::digest(paste0(names(data),collapse="%"),algo="crc32")
  return(tmp)
}

.pFetchHash_rowName <- function(data){
  tmp <- digest::digest(paste0(row.names(data),collapse="%"),algo="crc32")
  return(tmp)
}

.pFetchHash_list <- function(data){
  tmp <- digest::digest(paste0(data,collapse="%"),algo="crc32")
  return(tmp)
}
