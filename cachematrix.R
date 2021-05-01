makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y){
    x <<- y
    t <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) {t <<- inverse}
  getInverse <- function() t 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  t <- x$getInverse()
  if(!is.null(t)){
    message("getting cached data")
    return(t)
  }
  mat <- x$get()
  t <- solve(mat,...)
  x$setInverse(t)
  t
}