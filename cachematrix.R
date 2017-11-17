## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## This function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inversematrix <<- i
  getinverse <- function() i
  list(setmatrix = set, getmatrix = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!all(is.na(inversematrix))) {
    message("getting cached inverse matrix")
    return(inversematrix)
  }
  
  newmatrix <- x$get()
  inversematrix <- solve(newmatrix)
  x$setinverse(inversematrix)
  return(inversematrix)
  
}
