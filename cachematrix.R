## A pair of functions which store on the cache a matrix and its inverse

## This function creates an object which contains: a matrix and a place to
## store its inverse (if not calculated yet, it's empty; otherwise it
##stores the inverse )

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z){
          x <<-z
          inv<<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}




## This function calculates the inverse of x, prints it, and stores it
##in the object created by the first function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("Accesing cached data")
          return(inv)
        }
        d <- x$get()
        inv <- solve(d,...)
        x$setinv(inv)
        inv
}
