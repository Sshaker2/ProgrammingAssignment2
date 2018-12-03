## makeCachematrix creates a matrix x
## returns a list containing funtions below as input to cacheSolve()
## - set matrix, get matrix, set inverse, get inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<- function(inverse) inv <<- inverse
  getinverse<- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  

}


## Output of makeCacheMatrix()
## returns inverse of original matrix input to makeCacheMatrix()
## if inverse has already been calculated, gets it from cache and skips computing


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  new_m<-x$get()
  inv<- solve(new_m)
  x$setinverse(inv)
  return(inv)
       
}
