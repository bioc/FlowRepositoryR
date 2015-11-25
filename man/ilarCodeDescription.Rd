\name{ilarCodeDescription}
\alias{ilarCodeDescription}

\title{
    Get the name of a lab from it's ILAR code
}

\description{
    This function returns the name of a lab based on its ILAR code,
    see \code{http://dels.nas.edu/global/ilar/Lab-Codes}.
    This function works only with a very limited set of ILAR codes defined
    for IMPC centers. A NULL value will be returned for all unknown 
    centers.
}

\usage{
    ilarCodeDescription(ilarCode)
}

\arguments{
    \item{ilarCode}{ILAR code of an IMPC centre, or a list of ILAR codes, 
        or a vector of ILAR codes. The code (or codes) will be looked
        in an internal database of ILAR codes, which contains labs from
        IMPC centers only. The codes are case sensitive.}
}

\details{
    ILAR codes of IMPC centers are stored in an internal
    database (environment serving as a hash table), which is used to 
    lookup the results of this function.
    The ILAR codes are case sensiteve. The function can handle a single code
    as well as a list (or vector) of codes.
}

\value{
    An object of class \code{character} containing a human readable name
    of the lab corresponding to this ILAR code. If a list (or vector) of 
    multiple codes is submitted, then a vector of multiple descriptions 
    will be returned. The length of the output will match the length of 
    the input.
}

\author{Josef Spidlen}

\seealso{
    \code{\link{listKnownIlarCodes}}
}

\examples{
    ## Checking internal dictionary
    ## Codes are case sensitive
    ilarCodeDescription("H")
    ilarCodeDescription(listKnownIlarCodes())
}

\keyword{methods}
