## At overall level, these functions use lexical scoping concepts to save 
## computing time by storing the inverse matrix in cache after 1st iteration
## and going forward, it refers to the existing cache instead of calculating
## the inverse again

## Write a short comment describing this function
## This function creates a vector which has a list of funstions to 
## set a matrix, call a matrix, set inverse of matrix and get matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve(x)
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## Write a short comment describing this function

#The following function calculates the mean of the special "vector" created
#with the above function. However, it first checks to see if the mean has 
#already been calculated. If so, it gets the mean from the cache and skips 
#the computation. Otherwise, it calculates the mean of the data and sets the
#value of the mean in the cache via the setmean function.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
  
}
