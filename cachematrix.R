## A pair of functions that cache the inverse of a matrix to save 
# computation time if the inverse of a given matrix has to be computed repeatedly.

## The following function creates a special "matrix" object that can cache its inverse. 
# It stores functions to set the value of the  matrix, to get the value of the matrix,
# to set the value of the inverse of the matrix and to get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


##  The following function calculates the inverse of a given matrix. 
# First it check whether the inverse was already calculated, and if so,
# it uses the calculated inverese matrix from the cache rather than calculating it again. 
# If the inverse was not cached before, it performs the calculation
# and saves the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i  ## Return a matrix that is the inverse of 'x'
}


#some testing code

test <- matrix(3:9, nrow = 3, ncol = 3)
test
solve(test)

test_list <- makeCacheMatrix(test)
test_list
cacheSolve(test_list)

test_list <- makeCacheMatrix(matrix(1:6, nrow = 2, ncol = 3))
test_list
cacheSolve(test_list) #not sqare matrix
