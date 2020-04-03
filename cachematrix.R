## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The makeCacheMatrix function takes a matrix as an argument and creates a list with functions which can be used to cache a variable by another function, in this case cacheSolve.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

##Goes to the list created by makeCacheMatrix and assign the getinv function to m. m is a local variable in the cacheSolve environment.
##Checks if m is null. If m is not null it returns m. This means that the inverse matrix has been calculated previosly.
##If matrix is null (which it will always be the first time the function is executed) it goes into the list created by makeCacheMatrix function
## and get the matrix originally passed as an argument to makeCacheSolve and store it as a local variable called data.
##The function then takes the inverse of the matrix and assign it to m. 
##The setinv function in makeCacheMatrix is then called which assigns the inverse matrix to m in makeCacheSolve environment.
##In the end the function return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinv()                   
  if(!is.null(m)) {                 
    message("getting cached data")
    return(m)
  }
  data <- x$get()                   
  m <- solve(data, ...)            
  x$setinv(m)                      
  m                                 
  
  
}
