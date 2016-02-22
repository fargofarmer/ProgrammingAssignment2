## makeCacheMatrix function creates a special matrix object that can cache its inverse
## cacheSolve function computes the inverse of the matrix object created by makeCacheMatrix

## makeCacheMatrix function will
##set the matrix 
##get the matirx
##set the inverse
##get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  The cacheSolve function computes the inverse of the matrix object created by above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinv()
     if (!is.null(m)) {
     ##If the inverse of the matrix has already been calculate, the function will throw the 
     ##the following message.
         message("getting cached data")
         return(m)
       }
       ##if the inverse of the inverse of the matrix has not been calculated, it will get the matrix from above 
       ##function and calculates the inverse of the matrix and prints it.
    matrix <- x$get()
     m <- solve(matrix, ...)
     x$setinv(m)
     m
   }      
}
