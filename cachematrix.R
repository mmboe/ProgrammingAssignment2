## Put comments here that give an overall description of what your
## functions do.
#
#First create a function that specifies a matrix object 
#that can cache its contents and sets up the infrastructure so that 
#you can then create a function that can solve
#the inverse of any matrix (that isn't singular - they don't all work as I have
#discovered)
#
## Write a short comment describing this function


  
  makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
         x <<- y
         minv <<- NULL
       }
     get <- function() x
    setInverse <- function(inverse) {minv <<- inverse}
    getInverse <- function() {minv}
    list(set = set, get = get,
                    setInverse = setInverse,
                    getInverse = getInverse)
   }
  cacheSolve <- function(x, ...) {
    minv <- x$getInverse()
    if(!is.null(minv)) {
      message("getting cached data")
      return(minv)
      }
    data <- x$get()
    minv <- solve(data, ...)
    x$setInverse(minv)
    minv
    }
  
     
   
    

