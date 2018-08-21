# The first function handles the matrix and its inverse and maintains a cache status
# The second function cheques if the cached matrix exists and accordingly solves for the inverse

# This takes and input of a matrix 
# This has 4 sub functions, to get and set the matrix and its inverse.
# set() function takes advantage of lexical scoping and checks if cache is available
# The return value is a list of 4 functions which are used by the object in the next step

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The second function takes an input as a function, specifically an object of the function makeCacheMatrix
# It checks for the existence of cache and accordingly computes or imports the inverse
# This mainly serves as a calling function while the previous function does the data handling

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}