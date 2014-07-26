#Function for initializing and assigning computed values

makeCacheMatrix <- function(x = matrix()) {
  #initializing the values 
  m<- NULL
  
  #for assigning  values to the function for testing
  #values inside the function uses special operator to assign new values
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## getting the matrix values
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




#takes the values if already cached, else assigns the value in the cache for future ref

cacheSolve <- function(x, ...) {
  
  # inverse of the matrix        
  m <- x$getinverse()
  
  ## if matrix exists then return it else find inverse and assign to cache
  if(!is.null(m)) {
    message("getting cached data...")
    return(m)
  }
  ## if not: get the inverse of the matrix   
  data <- x$get()
  m <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(m)
  m
}
