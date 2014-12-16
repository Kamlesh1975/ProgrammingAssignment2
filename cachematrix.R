## to find the inverse of a matrix and cache it using a free floating variable

## It contains four function: get, set, get_matrix, set_matrix & special vector

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


## It return the inverse matrix

cacheSolve <- function(x, ...) {
        m1<-x$getmatrix()
        if(!is.null(m1)){
                message("cached data available")
                return(m1)
        }
        matrix<-x$get()
        m1<-solve(matrix, ...)
        x$setmatrix(m1)
 
        ## Return a matrix that is the inverse of 'x'
        m1
}
