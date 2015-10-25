## Put comments here that give an overall description of what your
## functions do

#############################################################################################
# Simply put...I have two functions ripped straight from the vector example that does the 
# same thing except it takes in matrix (not a vector) and finds it's inverse (not the mean of
# of the vector).  
#
# The first function takes in a matrix or sets up a default null matrix in order as a filler
# and also sets up the needed functions that are only available for the cacheable matrix.
# Those functions can then be used within the second function which, will solve and find the
# inverse of the cached matrix.  HOWEVER, it is entirely possible to bypass the second 
# function entirely by just using the functions attached to the invertable, cached matrix. 
#############################################################################################

makeCacheMatrix <- function(x = matrix()) {
  im<-NULL                             # Default value
  set<-function(y){                    # Nice function that allows for resetting of the matrix AFTER makeCacheMatrix fnct. is sourced
    x<<-y                              # Assings/caches the matrix to the global environment,   
    im<<-NULL                          # NULL is just a placeholder/default value for the value hasn't been set yet...
  }
  get<-function() x                       # From assignment example; used in cacheSolve function to get the passed in matrix
  setinverse<-function(solve) im<<- solve # Simple function to be used to find the inverse of the square, full-ranked matrix
  getinverse<-function() im                # From assignment example; used in cacheSolve function to get the invertible matrix
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  im<-x$getinverse()                   A# 
  if(!is.null(im)){                    # Checks to see if inverse matrix has been calculated; if so it returns
    message("getting cached data")     #    the cached matrix stored in the global environment.  
    return(im)                         # Returns the already inverted matrix forbidding further line implementation
  }
  matrix<-x$get()                      # In case matrix is not inverted already, it gets the cached matrix...
  im<-solve(matrix, ...)               # ...then gets the inverse
  x$setinverse(im)                     # ...caches it in the global environment
  im                                   # ...and returns it for good measure even though, it doesn't need to be...
}

