## The first function, `makeCacheMatrix` creates a special "Matrix", which is
## really a list containing a function to

## 1.  set the value of the Matrix
## 2.  get the value of the Matrix
## 3.  set the value of the Inverse Matrix
## 4.  get the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y){
      x <<- y
      inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_matrix <<- inverse
    getinverse <- function() inv_matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will calculate the inverse of a matrix returned by makecachematrix function.
## and will return the result from the cache if the inverse has already been claculated.


cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
}

#Testing the functions
x <- matrix(data = c(4,5,6,2,8,12,1,3,4),3,3)
x
y <- solve(x)
y
CacheMatrix <- makeCacheMatrix(x)
CacheMatrix$get()
CacheMatrix$getinverse()
cacheSolve(CacheMatrix)
