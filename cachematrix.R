# Taking an inversion of a matrix is usually costly computation, it may not considerable in small matrices. 
# But it is a major concern incase of larger matrices like nxn matrix(n can be any large number).
# Caching the inverse of the matrix is one solution rather than computing it repeatedly.
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function in the end to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # Args:
  #   x: input square matrix to which inverse has to be cached.
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  } 
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve function returns inverse of the given matrix.
# It checks if the inverse of the given matrix has been computed or not.
# If it has already computed then it will return the cached inverse
# If not then it will compute the inverse and sets the computed value through setInverse function.
# Note: It can only compute the inverse assuming that the matrix is inversible.

cacheSolve <- function(x, ...) {
  # Args:
  #   x: Input square matrix to which inverse has to be computed.
  #
  # Returns:
  #   inverse of the square matrix 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
          message("getting cached data..")
          return(inverse)
        }
        squareMatrix <- x$get()
        inverse <- solve(squareMatrix)
        x$setInverse(inverse)
        inverse
}


# Sample test run:
# matrix <- rbind(c(1,-2), c(3, -2.5))
# matrixObject <- makeCacheMatrix(matrix)
# matrixObject$get()
#       [,1] [,2]
# [1,]    1 -2.0
# [2,]    3 -2.5

## inverse computed in first run and returned
# cacheSolve(matrixObject)
#        [,1]      [,2]
# [1,] -0.7142857 0.5714286
# [2,] -0.8571429 0.2857143
## second time it will retun cached version of inverse
# cacheSolve(matrixObject)
#        [,1]      [,2]
# [1,] -0.7142857 0.5714286
# [2,] -0.8571429 0.2857143
#
#Note: inverse of 
#       [,1] [,2]
# [1,]    a   c
# [2,]    b   d
# is mathematically computed as  
#                      [,1] [,2]
# 1/(axd - bxc) * [1,]    d  -c
#                 [2,]   -b   a