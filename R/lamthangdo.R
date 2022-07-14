lamthangdo <- function(obs,m,n,p,sig){
  a <-rep(c(1:5),c(0.05*obs,0.15*obs,0.2*obs,0.25*obs,0.35*obs))
  tt <-rep(c(4:5),round(obs/2))
  stt <- 1:obs
  so <- runif(obs)
  dlieu10 <- cbind(stt,so)
  for (j in LETTERS[1:m]) {
    doi <- replace( a,rbinom(obs,1,p)==1,tt)
    assign(paste0(j), doi ) ->tam
    for (i in 1:n){
      chuan <-replace( tam ,rbinom(obs,1,sig)==1,tt)
      assign(paste0(j, i), chuan) ->tam2
      tam2 <-data.frame(tam2)
      colnames(tam2) <- paste0(j,i)
      dlieu10 <- cbind(dlieu10,tam2)
    }
  }
}

