
## # The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

##@x: a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {  
  inv <-NULL
  set<-function(y){
    
    x<<-y
    inv<<-NULL
    
  }
  get<-function() x
  
  setinv<-function(inverse)
    inv <<-inverse
  
  getinv<-function() inv
  
  list(set=set, get=get, setinv =setinv, getinv=getinv) 
  
  
}


## Write a short comment describing this function

##   ## @x: output of makeCacheMatrix()


#cacheSolve: This function computes the inverse of the special "matrix" , @x
## return: inverse of the original matrix (@x) input to makeCacheMatrix()
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  # if the inverse has already been calculated
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse
  data<-x$get()
  inv<-solve(data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  return(inv)
  
}