## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # inv stores the inverse of the matrix x, if it has been calculated,
  # NULL otherwise
  inv <- NULL
  
  # set: changes the value of the stored matrix
  # and resets the value of its not yet calculated inverse to NULL
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  
  # get retrieves the stored value of the matrix
  get <- function() x
  
  # get.inv returns the value of the matrix inverse, if it has been previously calculated.
  # otherwise it calculates the inverse value before returning it.
  get.inv <- function() {
    if (is.null(inv)) {
      inv <<- solve(x)
    }
    return(inv)
  }
  
  list(  set = set
       , get = get
       , get.inv = get.inv
       )
}


# cacheSolve: computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above.
# 
# If the inverse has already been calculated (and the matrix has not changed),
# then `cacheSolve` should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'

# NOTE: given that the logic to conditionally calculate and return the
# value of the inverse is already part of the previous function,
# -- I found to be an easier implementation --,
# this becomes a trivial function

cacheSolve <- function(x, ...) {
  x$get.inv()
}

# test code, below

test <- function() {
  m <- makeCacheMatrix(
    matrix(
      nrow=3
      , ncol=3
      , data=
        c(1,0,0,0,2,0,0,0,3)
    )
  )
  
  print(m$get())
  
  print("first time:")
  system.time(print(cacheSolve(m)))
  
  print("second time:")
  system.time(print(cacheSolve(m)))
  
  m$set(
    matrix(
      nrow=3
      , ncol=3
      , data=
        c(4,0,0,0,5,0,0,0,6)
    )
  )
  
  print(m$get())
  
  print("third time:")
  system.time(print(cacheSolve(m)))
  
  print("fourth time:")
  system.time(print(cacheSolve(m)))
  
}
