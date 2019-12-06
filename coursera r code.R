M <- function(x = matrix()) {
  fanzhuan <- NULL
  shezhi <- function(y){
    x <<- y
    fanzhuan <<- NULL
  }
  de <- function() x
  shefan <- function(solveMatrix) fanzhuan <<- solveMatrix
  defan <- function() fanzhuan
  list(shezhi = shezhi, de = de, shefan = shefan, defan = defan)
}

S <- function(x, ...) {
  fanzhuan <- x$defan()
  if(!is.null(fanzhuan)){
    message("getting cached data")
    return(fanzhuan)
  }
  data <- x$de()
  fanzhuan <- solve(data)
  x$shefan(fanzhuan)
  fanzhuan      
}