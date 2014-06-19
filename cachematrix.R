################################################################################
##  
##    cachematrix.R
##
## DESCRIPTION:
##    This R function provides functions to construct a cached matrix "class". 
##      This class will allow for the caching of the matrices inverse, so that 
##      it only needs to be computed once.
##
##    This function was written as part of the requirements for the R 
##      Programming course on Coursera [1]. If you are in this class or any
##      future iteration of this class, please do NOT use this code and instead
##      write it yourself.
##
## HISTORY:
##    2014-06-19: 
##    - Initial Version
##    2014-06-20:
##    - Finished up documentation
##
## AUTHORS:
##    Paul Romanczyk
##
## PUBLIC REPOSITORY:
##    https://github.com/pavdpr/rprog-004-assignment2
##
## SAMPLE USAGE:
##    m <- makeCacheMatrix( matrix( c( 1, 3, 3, 1 ), 2, 2 ) )
##    m$get() # gets the matrix
##    m$getInverse() # show that the inverse has not been computed
##    cacheSolve( m ) # invert the matrix
##    cacheSolve( m ) # invert the matrix using the cached answer
##    m$set( matrix( c( 4, 1, 1, 4 ), 2, 2 ) ) # update the matrix 
##
## TODO:
##    
## KNOWN BUGS:
##
## REFERENCES:
##    [1] https://class.coursera.org/rprog-004/
##
################################################################################



################################################################################
##
##    makeCacheMatrix <- function( x = matrix() )
##
## DESCRIPTION:
##    This function defines a "class" which allows for the inverse of a matrix
##      to be cached. It has get and set methods for the matrix and 
##      getInverse and setInverse methods for its inverse.
##
## INPUTS:
##    x: a matrix
##
## OUTPUTS:
##    a Cached Matrix object
##    
## METHODS:
##    get(): returns the matrix
##    set( x ): sets the matrix to x, will wipe out any cache
##    getInverse(): returns the inverse of the matrix
##    setInverse( x_inv ): set the inverse of the matrix
##
## NOTES:
##    Until the inverse is set (use cacheSolve), NULL will be returned for the 
##      inverse
##
## ATTRIBUTES:
##    x: the matrix
##    inv: the inverse of the matrix
##
################################################################################
makeCacheMatrix <- function( x = matrix() ) {
  # set the an inverse to NULL
  inv <- NULL
  
  # define the set function
  set <- function( y ) {
    # make x equal to the input
    x <<- y
    # set the inverse equal to NULL
    inv <<- NULL
  }
  
  # define the get function
  get <- function() {
    # return the matrix
    x
  }
  
  # define a set inverse function
  setInverse <- function( inverse ) {
    # set the inverse
    inv <<- inverse
  }
  
  # define a get inverse function
  getInverse <- function() {
    # return the inverse
    inv
  }
  
  # make (and return) a list containing all of the attributes of the 
  # this "class"
  list( set = set, 
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse )
}

################################################################################
##
##    cacheSolve <- function( x, ... ) 
##
## DESCRIPTION:
##    This function will return the cached inverse, if it exists. Otherwise it
##    will compute the inverse, set the inverse of the cacheMatrix object, and
##    return the inverse.
##
## INPUTS:
##    x: a cached matrix object
##    ...: not really sure why this is needed, it will not be used in current 
##      implementation
##
## OUTPUTS:
##    the inverse of x
##
## MODIFIES:
##    inv: if the inverse of x is not cached, it will set it
##
################################################################################
cacheSolve <- function( x, ... ) {
  ## Return a matrix that is the inverse of 'x'
  
  # HMM..., I never make use of the ..., should I?
  
  # try to get the inverse of x
  inv <- x$getInverse()
  
  # check to see if we already have a cached inverse
  if( !is.null( inv ) ) {
    # we have a cached inverse
    
    # # uncomment next line to prove that the cached version is returned
    # message( "getting cached data" ) 
    # # I dislike printing things like this in functions since writing to stdout
    # # is a slow process
    
    # return the cached version
    return( inv )
  }
  
  # if we get this far, we do not have a cached inverse
  # get the matrix x
  data <- x$get()
  
  # compute the inverse
  inv <- solve( data )
  
  # cache the inverse
  x$setInverse( inv )
  
  # return the inverse
  inv
}