\name{impcCodeDescription}
\alias{impcCodeDescription}

\title{
    Get a human readable description of an IMPC code.
}

\description{
    This function returns a human readable description of an IMPC code
    based on \code{https://www.mousephenotype.org/impress/protocol/174/7}
    as of November 6, 2015. This includes IMPC parameters as
    well as IMPC metadata.
}

\usage{
    impcCodeDescription(code)
}

\arguments{
    \item{code}{Either a single IMPC code, or a list of IMPC
        codes, or a vector of IMPC codes. The code (or codes) will be looked
        in an internal database of IMPC codes. 
        The codes are not case sensitive.}
}

\details{
    IMPC parameter and metadata codes are stored in an internal
    database (environment serving as a hash table), which is used to 
    lookup the results of this function.
    The codes are not case sensitive. The function can handle a single code
    as well as a list (or vector) of codes.
}

\value{
    An object of class \code{character} containing a human readable description
    of the IMPC code. If a list (or vector) of multiple codes is submitted,
    then a vector of multiple human readable descriptions will be returned.
    The length of the output will match the length of the input.
}

\references{
    Spidlen Josef. FlowRepository Resources for Developers.\cr
    http://flowrepository.org/developers
}

\author{Josef Spidlen}

\seealso{
    \code{\link{impcResultsCopy}}
}

\examples{
    ## Checking IMPC dictionary
    ## codes are not case sensitive
    impcCodeDescription("impc_imm_010_001")
    impcCodeDescription("IMPC_IMM_013_001")
    impcCodeDescription("ImpC_iMm_019_001")
    impcCodeDescription(c("impc_imm_022_001", "IMPC_IMM_028_001"))
}

\keyword{methods}
