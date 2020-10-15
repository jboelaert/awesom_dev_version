library(cluster)

ok.pam_clust.function <- function(ok.som, input_kohSuperclass){
  return(pam(ok.som$codes[[1]], input_kohSuperclass))
}




ok.hclust.function <- function(ok.som){
    hclust(dist(ok.som$codes[[1]]), "ward.D2")
}