## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

CacheMatrix <- function(x = matrix()) {
  
  xinverse <- NULL # return result
  set <- function(y) {
    x <<- y
    xinverse <<- NULL # initiatlize the value to null
  }
  
  get <- function() x # return the input matrix
  setI <- function(inverse) xinverse <<- inverse # set the inversed matrix
  getI <- function() xinverse # return the inversed matrix
  # return a list that contains these functions, so that we can use
  ## set the value of the vector
  ## get the value of the vector
  ## set the value of the mean
  ## get the value of the mean
  list(set = set, get = get,
       setI = setI,
       getI = getI)
}
cacheSolve <- function(x, ...) {
  m <- x$getI() # get the inversed matrix object x
  if(!is.null(m)) { # check for inverseion result
    message("getting cached data")
    return(m) # return the calculated value
  }
  data <- x$get() # else get the matrix object
  m <- solve(data) # solve the data
  x$setI(m) # set to the object
  m # return answer
}

#test
#> m$setI(matrix(seq(1:10),2))
#> m$get()
#     [,1]
#[1,]   NA
#> m$getI()
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    1    3    5    7    9
#[2,]    2    4    6    8   10



