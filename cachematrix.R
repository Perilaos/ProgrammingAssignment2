## File cachematrix.R contains two functions: "makeCacheMatrix" and "cacheSolve"

# ----- Function "makeCacheMatrix" ----- 
# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

# Input: Square matrix x
# Output: list object
makeCacheMatrix <- function(x = matrix()) {
    M <- NULL
    set <- function(y) {
      x <<- y
      M <<- NULL
    }
    get <- function() x
    setInverse <- function(myInverse) M <<- myInverse
    getInverse <- function() M
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
# -------------------------------------

# ----- Function "cacheSolve" ----- 
## The second function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmean function.
# Input: Matrix in "list form" created via makeCacheMatrix
# Output: Inverse matrix
cacheSolve <- function(x, ...) {
    M <- x$getInverse()
    if(!is.null(M)) {
      message("getting cached data")
      return(M)
    }
    data <- x$get()
    M <- solve(data, ...)
    x$setInverse(M)
    M
  }
# -------------------------------------
