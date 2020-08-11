## Put comments here that give an overall description of what your
## functions do

## This function make matrix which is able to be cached.
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function either uses cached matrix (producing the result and relevant message) or solves matrix ann then cached it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
