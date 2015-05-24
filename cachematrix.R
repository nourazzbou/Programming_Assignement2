## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m_inv <- NULL
  ## function that sets x value to y and m_inv to null
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  ## function that returns the x value
  get <- function() x
  ## function that sets the m_inv value to inv_matrix
  setinv <- function(inv_matrix) m_inv <<- inv_matrix
  ## fuction that returns the m_inv value
  getinv <- function() m_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinv()
  ## check if the value is already computed then return it
  if(!is.null(m_inv)) {
    message("getting cached inverse")
    return(m_inv)
  }
  ## get the data from the special matrix structure and compute the inverse
  data <- x$get()
  m_inv <- solve(data, ...)
  ## set the m_inv value in order to cache it
  x$setinv(m_inv)
  m_inv
}
