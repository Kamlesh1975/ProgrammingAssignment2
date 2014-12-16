## Computing the inverse of a square matrix 

makeCacheMatrix <- function(x = matrix()) {
 m1<-NULL
  set<-function(y){
    x<<-y
    m1<<-NULL
  }
  get<-function() x
  set_matrix<-function(solve) m1<<- solve
  get_matrix<-function() m1
  list(set=set, get=get,
       setmatrix=set_matrix,
       getmatrix=get_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m1<-x$getmatrix()
  if(!is.null(m1)){
    message("getting cached data")
    return(m1)
  }
  matrix<-x$get()
  m1<-solve(matrix, ...)
  x$setmatrix(m1)
  m1
}
