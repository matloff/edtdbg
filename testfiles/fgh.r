
f <- function() {
   a <- 1
   return(g(a)+a)
}

g <- function(aa) {
   b <- 2
   aab <- h(aa+b)
   return(aab)
}

h <- function(aaa) {
   c <- 3
   return(aaa+c)
}
