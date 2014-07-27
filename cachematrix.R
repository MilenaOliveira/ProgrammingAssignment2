## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## initializes the matrix to NULL during the first call to makeCacheMatrix
  m <- NULL
  
  ## sets a new value for the variable below
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get function for the matrix below
  get <- function() x
  
  ## sets the inverse matrix of the matrix `x` (`solve()` funcion)
  setInvMat <- function(solve) m <<- solve
  
  ## returns the inverse of the matrix `x`
  getInvMat <- function() m
  
  ## creates a list of exposed/public functions/variables
  ## that can be read latter on
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed yet!), then the cachesolve retrieves the inverse
## from the cache. Returna a matrix that is the inverse of `x`
cacheSolve <- function(x, ...) {
  
  ## gets the inverse matrix of the matrix `x`
  m <- x$getInvMat()
  
  ## checks if the inverse matrix has already been computed
  ## if it has been computed, returns its cached version
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  ## then, if the inverse matrix has NOT been computed: 
  ## calls `get()` to get the matrix below
  data <- x$get()
  
  ## computes the inverse matrix
  m <- solve(data, ...)
  
  ## caches the invert matrix in `m` and prints it
  x$setInvMat(m)
  m
}