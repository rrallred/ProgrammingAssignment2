## Returns chached inversed matrix value, if already exists uses cached value


## makeCacheMatrix: provides the original matrix and the functions to be consumed and executed by cacheSolve
makeCacheMatrix <- function(x = matrix()) 
{
  getmatrix<-function() x
  getinverse <- function() im_globe
  setinverse <- function(im) im_globe <<- im
  list(getmatrix=getmatrix, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: determines if cached inversed matrix exist, if not it returns the inversed matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(exists("im_globe"))
  {  im <- x$getinverse() }
  else {im_globe<-NULL}
  if(!is.null(im_globe)) {
    message("getting cached data")
    return(im_globe)
  }
  data <- x$getmatrix()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
