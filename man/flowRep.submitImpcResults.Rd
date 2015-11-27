\name{flowRep.submitImpcResults}
\alias{flowRep.submitImpcResults}

\title{
    Submit IMPC data analysis results to FlowRepository.
}

\description{
    This function connects to FlowRepository via a programming interface 
    and submits results of the analysis of a specified IMPC experiment.
    These results shall contain counts for IMPC defined cell populations.
}

\usage{
    flowRep.submitImpcResults(gatedByIlar, impcExpId, results)
}

\arguments{
    \item{gatedByIlar}{The ILAR code of the laboratory that perforned the
        data analysis. See \code{http://dels.nas.edu/global/ilar/Lab-Codes}
        for more info about ILAR codes. The laboratory needs to be listed
        among those allowed to submit IMPC data analysis results to
        FlowRepository}
    \item{impcExpId}{The IMPC experiment id of the experiment that the 
        results are being submitted for.}
    \item{results}{The results that are being submitted. This shall be a list
        of objects with names corresponding to IMPC parameter codes as defined
        in \code{https://www.mousephenotype.org/impress/protocol/174/7}
        (e.g., IMPC_IMM_011_001 to state the number of CD4 NKT cells in 
        IMPC panel A). See also \code{\link{impcResultsCopy}}.}
}

\details{
    An HTTP POST request sends the results to FlowRepository.
    You need to have your credentials set (see 
    \code{\link{setFlowRepositoryCredentials}}) and you as the user need to
    have permissions to subnit IMPC data analysis results.
    In addition, the laboratory needs to be listed among those allowed 
    to submit IMPC data analysis results to FlowRepository. You 
    may need to contact your FlowRepository administrators if you are not
    allowed to submit IMPC results. In addition, note that at this point,
    the public FlowRepository (available at FlowRepository.org) does
    not contain IMPC data and is not accepting these sorts of results.
    There are private FlowRepository instances only for IMPC data at this
    point. Also, note that some basic consistency checks are in place
    and FlowRepository may refuse results that appear to be wrong.
}

\value{
    A list of 2 objects named \code{response} and \code{status}. The
    response contains a text of the response from the server, such as
    "Results submitted." if the results have been accepted. The status
    will contain the HTTP status code of the response, such as "200 OK" if
    everything was successful. An example if an unacceptable response
    is "Bad results (impc_imm_049_001 out of range, value should be 
    between 0 and 100)" if you try to specify the percentage of live gated 
    events in Panel B outside of the 0-100 range. The "400 Bad Request"
    status is to be expected in this case.
}

\references{
    Spidlen Josef. FlowRepository Resources for Developers.\cr
    http://flowrepository.org/developers
}

\author{Josef Spidlen}

\seealso{
    \code{\link{setFlowRepositoryCredentials}},
    \code{\link{impcResultsCopy}},
    \code{\link{flowRep.submitGeneStatus}},    
    \code{\link{flowRep.get}}
}

\examples{
    ## This will not submit results as the public FlowRepository is not
    ## accepting those at this point, "H" is probably not allowed to submit
    ## IMPC results, "impcEx0001" is a made up IMPC experiment ID,
    ## and finally, the user did not set her credentials...
    tryCatch(
        {
            centreILAR <- "H"
            expId <- "impcEx0001"
            results <- list(
                impc_imm_001_001 = 0.12, 
                impc_imm_026_001 = 1000000, 
                impc_imm_002_001 = 87.345, 
                impc_imm_027_001 = 1000000, 
                impc_imm_049_001 = 91.567, 
                impc_imm_002_001 = 543213)
            flowRep.submitImpcResults(centreILAR, expId, results)
        },
        interrupt = function(ex) { NULL; },
        error = function(ex) { NULL; }
    )
}

\keyword{methods}
