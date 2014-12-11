## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix would make a Matrix for get the cached solve Matrix
## makeCacheMatrix(your Matrix here) -> myMatrix to make one

makeCacheMatrix <- function(x = matrix()) {
    M <- NULL
    set <- function(y) {
        x <<- y
        M <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) M <<- Inverse
    getInverse <- function() M
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve would calculate the  of the myMatrix by solve() function
## if the inverse matrix of the myMatrix has been calculated that would be cached
## if there was a cached inverse matrix of the myMatrix, that would get the cached directely

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  M <- x$getInverse()
  if(!is.null(M)) {
      message("getting cached Inverse matrix")
      return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setInverse(M)
  M		
}
