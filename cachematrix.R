## Set of functions to 1.create a special matrix object that stores
## it's own inverse in cache 2.read if cached data is present,
## and either return the cached value, or calculate and return it.

# Create a special matrix object that can cache it's own solve()

makeCacheMatrix <- function(x) { # x not defined as matrix, since that's the main
                                 # assumption for the assignment: invertible matrix
  
  if (is.matrix(x)==TRUE & nrow(x)==ncol(x)) { #check if object is a square matrix
    s <- NULL        #clear cache variable: s
    set <- function(y) 
    {
      x <<- y      #set a new object
      s <<- NULL   #clear cache
    }
    
    get <- function() x
    setSolve <- function(Solve) s <<- Solve
    getSolve <- function() s
    list(set = set,           #create list of defined functions 
         get = get,           #that will be used by cacheSolve function
         setSolve = setSolve, #
         getSolve = getSolve) #
  }
  else message("not square matrix object")
}


#########################################################################

## Return value of cached makeCacheMatrix object inverse, 
## if NULL -> calculate solve() and set new cached value to the object

cacheSolve <- function(x) {
  s <- x$getSolve()    #check makeCasheMatrix object for cached result
  if(!is.null(s)) 
  {
    message("getting cached data")
    return(s)          #if cache value is present, return it
  }
  s <- solve(x$get())  #if cache is not present, calculate solve
  x$setSolve(s)        #set solve to cache in makeCacheMatrix object
  return(s)
}
