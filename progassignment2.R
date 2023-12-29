## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function

#create a special matrix
makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL #create inv as empty to hold the value of the inverse of the matrix later
  set <- function(y) { #set the value of the matrix
    x <<- y
    invr <<- NULL
  }
  get <- function() x #get the value of the matrix
  setinvr <- function(inverse) invr <<- inverse #set the value of the inverse matrix
  getinvr <- function() invr #get the value of the inverse matrix
  list(set = set, get = get,
       setinvr = setinvr,
       getinvr = getinvr)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invr <- x$getinvr()
  if(!is.null(invr)){ #if the inverse matrix has already been calculated, return its value and skip the calculations
    message("getting cached data")
    return(invr)
  }
  
  data <- x$get() #otherwise, calculate the inverse of the matrix
  invr <- solve(data, ...) #use solve() to calculate the inverse of the matrix
  x$setinvr(invr)
  invr
}

