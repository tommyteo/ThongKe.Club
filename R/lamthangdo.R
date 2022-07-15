lamthangdo <- function(obs,m,n,p,sig){
  set.seed(123)
  cong <- round(0.05*obs) + round(0.15*obs) + round(0.25*obs) + round(0.25*obs)
  a <-rep(c(1:5),c(round(0.05*obs),round(0.15*obs),round(0.25*obs),round(0.25*obs),obs-cong))
  tt <-rep(c(4:5),round(obs/2))
  stt <- 1:obs
  so <- runif(obs)
  dlieu10 <- cbind(stt,so)
  nhoj <-LETTERS[1:m]
  nhoi <- c(1:n)

for (j in nhoj) {
doi <- replace( a,rbinom(obs,1,p)==1,tt)
assign(paste0(j), doi ) ->tam
    for (i in nhoi){
    chuan <-replace( tam ,rbinom(obs,1,1-sig)==1,tt) 
    chuan <-data.frame(chuan)
    colnames(chuan) <- paste0(j,i)
    dlieu10 <- cbind(dlieu10,chuan)
    dlieu10 <-data.frame(dlieu10)
    }
   }
  return(dlieu10)
}
