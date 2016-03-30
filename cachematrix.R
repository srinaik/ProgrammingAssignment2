## makeCacheMatrix function creates a special matrix that can cache its inverse
## cacheSolve function returns the inverse of the matrix if the inverse is already computed else
## it will compute the inverse and then return it

## The makeCacheMatrix function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  } 
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function returns the inverse of the matrix if the inverse is computed already else 
## it will compute the inverse and then return it

cacheSolve <- function(x, ...) {
        ## Returns the inverse of the matrix 'x'
        inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr,...)
  x$setinverse(inv)
  inv
}
