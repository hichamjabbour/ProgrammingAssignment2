## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix  <- function(x = matrix()) {
  m <- NULL ## This matrix will contain the inverse of my matrix x
  
  ## This function assign the values of a given matrix y to the matrix x
  set <- function(y) {
    x<<-y 
    m <<- NULL
  }
  
  ## This function return the matrix x
  get <- function() x
  
  ## This function set the values of a given matrix inverse to the matrix m
  setinverse <- function(inverse) m<<-inverse 
  
  ## This function return the matrix m
  getinverse <- function() m
  
  ## The function makeCacheMatrix return a list of 4 objects:
  ## The first one is a setter for the matrix x
  ## The second one is a getter of the matrix x
  ## The 3rd one is a setter for the matrix m
  ## The 4th one is a getter of the matrix m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## This matrix will contain the inverse of my matrix x$get()
  if(!is.null(m)) { ## If the inverse of x$get() is already computed (which means that it is cached),get it from the cache
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## If x$get() is not found in the cache and it is a square matrix than compute the value of the inverse and store it in the matrix m
  if(nrow(data)==ncol(data))
  m <- solve(data)
  x$setinverse(m) ## Call the method x$setinverse(m) to set the inverse of the matrix x as the matrix m
  m
}
