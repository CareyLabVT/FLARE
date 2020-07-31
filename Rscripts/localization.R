localization <- function(mat,nstates,modeled_depths,num_wq_vars,wq_start,wq_end){
  
  distance_matrix <- matrix(NA,nrow = nstates, ncol = nstates)
  
  for(i in 1:dim(distance_matrix)[1]){
    index <- 1
    for(j in 1:dim(distance_matrix)[2]){
      distance_matrix[i,j] <- modeled_depths[index]
      index <- index + 1
      if(index > length(modeled_depths)){
        index <- 1
      }
    }
  }
  
  distance_differ_matrix <- distance_matrix - diag(distance_matrix)
  
  for(i in 1:dim(distance_matrix)[1]){
  distance_differ_matrix[i, ] <- exp(-(distance_differ_matrix[i, ]/1)^2)
  }
  
  
  mat_new <- mat * distance_differ_matrix
  

  return(mat_new)
  
}