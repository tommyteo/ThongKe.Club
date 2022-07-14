them_phantram <- function(input) {
    if(!is.numeric(input)) {
            cat("Hãy nhập dữ liệu là số vào: ")

          }else if (any(abs(input<=1))) {
               lamtron <- round(input * 100, digits = 1)
               output <- paste0(lamtron, "%")
          }else if (any(abs(input>1))) {
                   output <- paste0(input,"%")
                   }
    return(output)
 }
