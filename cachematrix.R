## Programming Assignment 2
## Caching the inverse matrix

## creates a special "matrix", which contains functions to:
## 1. set the value of matrix (setMat())
## 2. get the value of matrix (getMat())
## 3. set the value of inverse matrix (setInv())
## 4. get the value of inverse matrix (getInv())

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  setMat <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  getMat <- function() x
  setInv <- function(inverse) inv_matrix <<- inverse 
  getInv <- function() inv_matrix  
  list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## calculate and return the inverse matrix of the special "matrix"
## created by the function "makeCacheMatrix()": 
## first check if the inverse matrix has already been calculated
## if so, gets the inverse matrix from the cache and skip the computation
## if not, calculates the inverse matrix of the data and set the inverse matrix
## in cache via the setInv() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInv()
  if(!is.null(inv_matrix)) {
    message("getting cached inverse matrix")
    return(inv_matrix)
  }
  data <- x$getMat()
  inv_matrix <- solve(data, ...)
  x$setInv(inv_matrix)
  inv_matrix
}
