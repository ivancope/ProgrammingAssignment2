# ------------------- Start of file. -----------------------------------
## Overall description of functions.
##  The objective is to save time by caching inverted matrices 
##  so that they can be retrieved, 
##  thus saving the time of repeated inversion.

## The timesaving can be great if the inverted matrix is used many times,
##       if the matrix is large and near singular, 
##       so that inversion takes a long time.

## These functions calculate the inverse if it hasn't been done before
##      and cache it. In future calls to cacheSolve the cahed inverse is
##      retrieved, saving the time required for inversion.
# ------------------------------------------------------------------------
#  Layout of Rest of File
#   1) Function makeCacheMatrix is documented and listed.
#   2) Function cacheSolve is documented and listed.
#   3) using a 2*2 square test matrix it is shown that 
#        function solve inverts the test matrix correctly
#        the first run of cacheSolve causes the inverse to be calculated,
#        the second run retrieves the inverse from the cache, and
#        xxx, the inverse produced, is the same as the output of 
#          function solve, z (given above).

#   This proves that the programme works and the objective has been met.
#   [ The proof was not requested, but it is felt to be essential.
#     When presenting work to a client this shows 
#     evidence of good workmanship.]
# -------------------------------------------------------------------------
## 1)  Function makeCacheMatrix creates a "special" matrix,
##      which is really a list containing a function to
##           set the value of the matrix
##           get the value of the matrix
##           set the value of the inverse
##           get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# ------------------------------------------------------------------------
## 2)  Function cacheSolve returns a matrix that is the inverse 
##       of the matrix used in the makeCacheMatrix function call.

##  If the matrix has been previously inverted,
##     it retrieves the inverse from cache, without inverting again.
##     (This is where the time is saved, especially 
##          if the inverse is required many times.)

cacheSolve <- function(x, ...) {
  ## Here argument x is the output from the makeCacheMatrix function call
  ## Function cacheSolve returns a matrix that is the inverse
  ##  of the matrix input to function makeCacheMatrix.
  
  m <- x$getsolve()
  if(!is.null(m)) {
       message("getting cached inverse")
       return(m)
  }
  data <- x$get()
  message("calculating inverse")
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

# ------------------------------------------------------------------
#  Proof that the programme works. 
#  This is mainly a reproduction of the R output.

#  1) Proof that solve inverts a matrix.
# > x <- matrix(1:4, 2, 2)  # x is test 2*2 square matrix
# > x
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# > z <- solve(x)   # showing that function solve works .....
# > z
#      [,1] [,2]           x*z =      [,1] [,2] 
# [1,]   -2  1.5                [1,]    1    0
# [2,]    1 -0.5                [2,]    0    1 

#  ..... because the matrix product, x*z gives the identity matrix, 
#  where: - * is used to denote matrix multiplication, 
#         - in the matrix product of n by n matrices, x*Z = p, 
#              p is also an n by n matrix with elements p.ij in row i, column j 
#                   for i,j in the range [1,n],
#              and p.ij = x.i1*z.1j + x.i2*z.2j + .... + x.in*z.nj, and
#         - an identity matrix is a square matrix with 1's on the main diagonal
#              and zeroes everywhere else - see above for 2 by 2 identity matrix.

#  Reference:   http://mathworld.wolfram.com/MatrixInverse.html
#     (The inverse of a square matrix A is a matrix A.inv 
#     such that  A*A.inv = I, where I is the identity matrix.)
     
#   2) We now present the output of 
#          [i] running makeCacheMatrix once, followed by
#         [ii] running cacheSolve twice, 
#   proving that
#   i) the first run of cacheSolve causes the inverse to be calculated,
#  ii) the second (and subsequent) runs retrieve the inverse from the cache, and
# iii) xxx, the inverse produced, is the same as the output of 
#          function solve, z (given above).

#           # [i]  makeCacheMatrix is run once & xx is printed
# > xx <- makeCacheMatrix(x)   
# > xx
#  $set
# function (y) 
# {
#   x <<- y
#   m <<- NULL
# }
# <environment: 0x05dbbc08>
 
#  $get
# function () 
#   x
# <environment: 0x05dbbc08>

#   $setsolve
# function (solve) 
#   m <<- solve
# <environment: 0x05dbbc08>
  
#   $getsolve
# function () 
#   m
# <environment: 0x05dbbc08>

#          # [ii]  cacheSolve is run twice & xxx is printed
# > xxx <- cacheSolve(xx)   # i)   first run calculates the inverse
# calculating inverse       
# > xxx <- cacheSolve(xx)   # ii)  second run retrieves inverse from cache  ****
# getting cached inverse
# > xxx                     # iii) cached inverse xxx is the same as z,   
#       [,1] [,2]           #      produced by solve above.
# [1,]   -2  1.5
# [2,]    1 -0.5  

# **** subsequent xxx <- cacheSolve(xx) calls will also retrieve the cache inverse.

# ----------- End of proof that programme works ---------------------------
# ----------- End of file. ------------------------------------------------