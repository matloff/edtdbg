source("reduceonly.r")

test <- function() {
   fp <- function(x,y) return(x+y)
   u <- 1:8   
   cat("output = ",reduce(fp,u),"\n")
   cat("done\n")  
}
