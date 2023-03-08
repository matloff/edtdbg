# adding a functional programming reduce op to R; apply f to v

reduce <- function(f,v) {
   rslt <- v[1]
   for (vj in v[-1]) {
      rslt <- f(rslt,vj)
   }
   return(rslt)
 } 

test <- function() {
   fp <- function(x,y) return(x+y)
   u <- 1:8   
   cat("output = ",reduce(fp,u),"\n")
   cat("done\n")  
}
