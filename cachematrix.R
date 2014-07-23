## The following are two functions that together work to create a special
## object that stores a matrix and solves for its inverse via the 'solve' function


## this first function creates a special "matrix", which is really a list containing
##a function to 1) set the value of the matrix 2)get the value of the matrix 3) set 
##the value of the inverse (solve) 4)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)
    {
    x<<-y
    m<<-NULL
    }
  get<-function()x
  setsolve<-function(solve)m<<-solve
  getsolve<-function()m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function solves for the inverse of the special "matrix" created with the above
##above function. However, it first checks to see if the inverse has already been calculated
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates
##the inverse of the data and sets the value of the inverse in the cache via the setsolve function
cacheSolve <- function(x, ...) 
  {
  m<-x$getsolve()
  if(!is.null(m))
    {
    message("getting cached data")
    return(m)
    }
  data<-x$get()
  m<-solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
  }
