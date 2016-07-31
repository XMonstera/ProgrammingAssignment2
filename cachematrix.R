## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a list for:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 


## Write a short comment describing this function
## 1. to return the inverse of the matrix. 
## a. checks if the inverse computation is compelted. 
##    b. If completed, it gets the result and skips the computation. 
##    c. If not, it computes the inverse, sets the value in the cache in setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() 
        if(!is.null(inv)) { 
          message("getting cached data.") 
          return(inv) 
        } 
        data <- x$get() 
        inv <- solve(data) 
        x$setinverse(inv) 
        inv 

  }
