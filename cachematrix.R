## Put comments here that give an overall description of what your
## functions do


# The first function makeCacheMatrix creates a matrix of variable size when the number of rows and columns are added to the function.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



# This function is used to get the inverse of the matrix which was previously created by makeCacheMatrix
# If the inverse of the matrix has already been calculated then "getting cached data" will appear with the inverse of the original matrix below.
# the inverse of the original matrix is the same as the matrix produced by cacheSolve.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}  
  ## Return a matrix that is the inverse of 'x'

