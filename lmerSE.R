lmerCI <- function(Y,Z){
  frame <- matrix(nrow = nrow(Y), ncol = 2*ncol(Z))
  for(j in 1:ncol(Y)){
    for(i in 1:nrow(Z)){
      frame[i,j] <- -2*Z[i,j] + Y[i,j]
      frame[i,j+ncol(Y)] <- 2*Z[i,j] + Y[i,j]
    }
  }
  frame <- as.data.frame(frame)
  for(k in 1:ncol(Y)){
    col.name <- colnames(Y)[k]
    colnames(frame)[k] <- paste0(col.name, ".", "lo")
    colnames(frame)[k + ncol(Y)] <- paste0(col.name, ".", "hi")
  }
  frame <- cbind(Y, frame)
  return(frame)
}