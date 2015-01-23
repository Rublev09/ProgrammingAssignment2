
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL          #inversion is stored
  set <- function(y) {
    x <<- y
    inverse <<- NULL       #initialises inverse to null
  }
  get <- function() x 
  # setinv sets the inv variable
  setinv <- function(inv) inverse <<- inv 
  # getinv get the cached inverse
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()  # inversed matrix from object x
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  # return calculated inversion
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  # return solved result
}


