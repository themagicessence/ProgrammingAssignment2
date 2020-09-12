## A pair of functions which store on the cache a matrix and its inverse

## This function creates an object which contains: a matrix and a place to
## store its inverse (if not calculated yet, it's empty; otherwise it
##stores the inverse )

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Creates a variable to store the inverse
  set <- function(z){ #This function cleans the cached inverse when we change the matrix
          x <<-z
          inv<<- NULL
  }
  get <- function() x #A getter for the object x
  setinv <- function(inverse) inv <<- inverse #A setter for inv, lets us change the value of inv inside this special matrix
  getinv <- function() inv #A getter for inv, retrieves inv
  list(set = set, get = get, #This list contains all the info of the functions and their values
         setinv = setinv,
         getinv = getinv)
    
}




## This function calculates the inverse of x, prints it, and stores it
##in the object created by the first function

cacheSolve <- function(x, ...) { #a function which receives the object the other function created
        inv <- x$getinv() #We access the value of inv stored in the special matrix object the other function created
        if(!is.null(inv)) { #If it has already stored the info of its inverse, it prints it
          message("Accesing cached data") #It tells us we already had stored the inverse
          return(inv)
        }
        d <- x$get() #if not, we retrieve the matrix we want
        inv <- solve(d,...) #We calculate its inverse
        x$setinv(inv) #We keep the inverse in the object we first created
        inv #We print the inverse
}
