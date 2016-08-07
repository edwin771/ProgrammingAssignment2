##        x <- matrix(1:4, nrow=2, ncol=2)
## makeCacheMatrix set a matrix 
##      matx <- makeCacheMatrix(x)
##  
##  get() is to get the matrix
##      matx$get()
##  setinversematrix() is to set an inverse matrix
##      matx$setinversematrix(solve(matx$get()))
##  getinversematrix() is to obtain the inverse matrix
##      matx$getinversematrix()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) m <<- inversematrix
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## cacheSolve function check if the inverser function has been set 
##    cacheSolve()
## If the inverse matrix has been set, the set inverse matrix will be returned and the message "getting cached data" will be printed. 
## If not, this function will calcultate and return the calculated inverse matrix


cacheSolve <- function(x, ...) {
  
  m <- x$getinversematrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinversematrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
  
}
