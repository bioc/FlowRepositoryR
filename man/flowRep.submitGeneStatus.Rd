\name{flowRep.submitGeneStatus}
\alias{flowRep.submitGeneStatus}

\title{
    Submit information about an IMPC gene analysis results to FlowRepository.
}

\description{
    This function connects to FlowRepository via a programming interface
    and submits results of the analysis of a specified IMPC gene.
    These results shall contain information about whether the gene has 
    been found phenodeviant. In addition, a comment may be specified, and
    information about whether the analysis of this gene is completed.
}

\usage{
    flowRep.submitGeneStatus(mgiGeneId, geneSymbol, isPhenodeviant, 
        comment=NULL, isProcessed=TRUE)
}

\arguments{
    \item{mgiGeneId}{MGI gene ID of the gene that the results are for.}
    \item{geneSymbol}{Gene symbol of the gene that the results are for. 
        This shall be consistent with the MGI gene ID.}
    \item{isPhenodeviant}{An object of type logicle (i.e, TRUE or FALSE)
        indicating whether the gene has been found to couse the specimens to
        be phenodeviant.}
    \item{comment}{An optional comment about the analysis of this gene.}
    \item{isProcessed}{An object of type logicle (i.e, TRUE or FALSE)
        indicating whether the gene analysis has been completed for this gene.}
}

\details{
    An HTTP POST request sends the results to FlowRepository.
    You need to have your credentials set (see 
    \code{\link{setFlowRepositoryCredentials}}) and you as the user need to
    have permissions to subnit IMPC data analysis results.
    In addition, the gene needs to be found in FlowRepository, i.e., a
    specimen with this gene needs to be known to FlowRepository; both the
    MGI gene ID and the gene symbol need to match.
}

\value{
    A list of 2 objects named \code{response} and \code{status}. The
    response contains a text of the response from the server, such as
    "Results submitted." if the results have been accepted. The status
    will contain the HTTP status code of the response, such as "200 OK" if
    everything was successful. An example if an unacceptable response
    is "Bad results (unknown gene)" if you try to specify a gene that
    is not known to FlowRepository. The "400 Bad Request" status 
    is to be expected in this case.
}

\references{
    Spidlen Josef. FlowRepository Resources for Developers.\cr
    http://flowrepository.org/developers
}

\author{Josef Spidlen}

\seealso{
    \code{\link{setFlowRepositoryCredentials}},
    \code{\link{flowRep.submitImpcResults}},
    \code{\link{flowRep.get}}
}

\examples{
    ## This will not submit results as the public FlowRepository is not
    ## accepting those at this point.
    tryCatch(
        {
            mgiGeneId <- "geneX1"
            geneSymbol <- "gXr1"
            comment <- "This will not submit anything due to no credentials."
            flowRep.submitGeneStatus(
                mgiGeneId = mgiGeneId, 
                geneSymbol = geneSymbol, 
                isPhenodeviant = TRUE, 
                comment = comment, 
                isProcessed = TRUE)
        },
        interrupt = function(ex) { NULL; },
        error = function(ex) { NULL; }
    )
}

\keyword{methods}
