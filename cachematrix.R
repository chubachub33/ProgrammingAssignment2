## First Function - ## Create matrix to cache inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  # inverse value to NULL
  inv <- NULL
  
  # set value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # retrieve value
  get <- function() x
  
  # set inverse
  setinv <- function(inv) inv <<- inv
  
  # get inverse
  getinv <- function() inv
  
  # return list 
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}

##Second Function - Computes the inverse.

cacheSolve <- function(x, ...) {

# get inverse  
  
  inv <- x$getinv()

# check if already cached inverse and return inverse 
  
  if(!is.null(inv)) {
    message("finding that data")
    return(inv)
  }
  
# if not, retrieve matrix
  data <- x$get()

# make inverse
inv <- solve(data)
x$setinv(inv)

#inverse
inv
  
}
