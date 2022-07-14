lamthangdo <- function(obs,m,n,p,sig){
  cong <- round(0.05*obs) + round(0.15*obs) + round(0.25*obs) + round(0.25*obs)
  a <-rep(c(1:5),c(round(0.05*obs),round(0.15*obs),round(0.25*obs),round(0.25*obs),obs-cong))
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

