\name{listKnownIlarCodes}
\alias{listKnownIlarCodes}

\title{
    List all known ILAR codes.
}

\description{
    This function returns a vector of ILAR codes of all
    IMPC centers stored in an internal database.
}

\usage{
    listKnownIlarCodes()
}

\details{
    ILAR codes of IMPC centers are stored in an internal
    database (environment serving as a hash table), which is used to 
    provide the results of this function.
}

\value{
    An vector of class \code{character} containing a ILAR codes 
    of IMPC centers known to this library.
}

\author{Josef Spidlen}

\seealso{
    \code{\link{ilarCodeDescription}}
}

\examples{
    ## Checking internal dictionary
    ## Codes are case sensitive
    listKnownIlarCodes()
    ilarCodeDescription(listKnownIlarCodes())
}

\keyword{methods}
