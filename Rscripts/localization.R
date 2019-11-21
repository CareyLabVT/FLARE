localization <- function(mat,nstates,modeled_depths,num_wq_vars,wq_start,wq_end){
  
  mat_new <- mat
  mat_new[,] <- 0.0
  diag(mat_new) <- diag(mat)
  mat_new[1:nstates,1:nstates] <- 0.0
  
  for(s in 1:length(modeled_depths)){
    
    depth_diff <- abs(modeled_depths[s] - modeled_depths)
    depth_diff_index <- which(depth_diff <= localization_distance)
    #index <- c(s,s+1, s + 2, s + 3)
    index <- depth_diff_index[which(depth_diff_index > 0 & depth_diff_index <= nstates)]
    mat_new[s, index] <- mat[s, index]
    mat_new[index, s] <- mat[s, index]
  }
  
  if(include_wq){
    for(wq in 1:num_wq_vars){
      for(s in 1:length(modeled_depths)){
        depth_diff <- abs(modeled_depths[s] - modeled_depths)
        depth_diff_index <- which(depth_diff <= localization_distance)
        depth_diff_index <- wq_start[wq] + depth_diff_index - 1
        wq_index <- wq_start[wq] + s -1 
        index <- depth_diff_index[which(depth_diff_index > 0 & depth_diff_index <= wq_end[wq])]
        mat_new[wq_index, index] <- mat[wq_index, index] #Keep covariances within the localization distance
        mat_new[index, wq_index] <- mat[wq_index, index] #Ensure that covariance matrix is symetric
        mat_new[wq_index, s] <- mat[wq_index, s] #Keep covariance between temperature and water quality variable at that depth
        mat_new[s, wq_index] <- mat[wq_index, s]
        all_wq_at_same_depth <- wq_start + s - 1
        for(wq2 in 1:length(all_wq_at_same_depth)){  #Keep covariance between all other water quality variables at that depth
          mat_new[wq_index, c(all_wq_at_same_depth[wq2])] <- mat[wq_index, c(all_wq_at_same_depth[wq2])]
        }
      }
    }
  }
  
  return(mat_new)
  
}