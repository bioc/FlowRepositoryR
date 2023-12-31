\name{flowRep.ls}
\alias{flowRep.ls}

\title{
    List identifiers of datasets available in FlowRepository
}

\description{
    This function connects to FlowRepository (flowrepository.org) 
    via an XML-based API and retrieves a vector of identifiers of 
    available datasets.
}

\usage{
    flowRep.ls(
        include.private = FALSE, 
        impc.only = FALSE,
        impc.unanalyzed.only = FALSE,
        impc.centre = NULL, 
        impc.date.from = NULL, impc.date.to = NULL,
        impc.specimen.geneKO.mgiId=NULL,
        impc.specimen.geneKO.geneSymbol=NULL,
        impc.specimen.baseline=NULL,
        impc.specimen.minCount=NULL
    )
}

\arguments{

    \item{include.private}{Whether to include private experiments 
        in the list. This only works if credentials to access 
        FlowRepository have been provided. By default, only 
        public experiments will be listed.}
        
    \item{impc.only}{Whether to return only datasets that are
        related to IMPC experiments and IMPC specimen.}
        
    \item{impc.unanalyzed.only}{Whether to return only datasets that are
        related to an IMPC experiment and that do not have any automated
        analysis results associated with.}
    
    \item{impc.centre}{Whether to return only datasets that are
        related to an IMPC experiment performed by a specified IMPC centre.
        If this is not NULL then it shall be an ILAR code of a known IMPC
        centre; see also \code{\link{listKnownIlarCodes}}.
        The impc.only will be set to TRUE automatically if impc.centre is
        provided.}

    \item{impc.date.from}{Whether to return only datasets that are
        related to an IMPC experiment performed on or after the specified date.
        If this is not NULL then it shall be a value of the Date class,
        e.g., \code{as.Date("2015-11-24", "\%Y-\%m-\%d")}.
        The impc.date.to will be set to TRUE automatically if impc.centre is
        provided.}
        
    \item{impc.date.to}{Whether to return only datasets that are
        related to an IMPC experiment performed on or before the specified date.
        If this is not NULL then it shall be a value of the Date class,
        e.g., \code{as.Date("2015-11-24", "\%Y-\%m-\%d")}.
        The impc.date.to will be set to TRUE automatically if impc.centre is
        provided.}
        
    \item{impc.specimen.geneKO.mgiId}{Whether to return only datasets
        related to an IMPC experiment that involves a specimen with a specified 
        gene knockout (KO). In this argument, the KO gene is referenced by 
        its MGI gene id. 
        The impc.date.to will be set to TRUE automatically if impc.centre is
        provided.}
        
    \item{impc.specimen.geneKO.geneSymbol}{Whether to return only datasets
        related to an IMPC experiment that involves a specimen with a specified 
        gene knockout (KO). In this argument, the KO gene is referenced by 
        its gene symbol. 
        The impc.date.to will be set to TRUE automatically if impc.centre is
        provided.}
        
    \item{impc.specimen.baseline}{Whether to return only datasets
        related to an IMPC experiment that involves a specimen with a specified 
        boolean baseline attribute, the value of which shall match the value
        of this argument. That means, if impc.specimen.baseline is set
        to TRUE then only IMPC specimen marked as baseline will be considered.
        If impc.specimen.baseline is set to FALSE then only IMPC specimen 
        that are not marked as baseline will be considered. If 
        impc.specimen.baseline is left as NULL then both baseline and not 
        baseline specimen will be considered.
        The impc.date.to will be set to TRUE automatically if impc.centre is
        provided.}
        
    \item{impc.specimen.minCount}{Whether to return only datasets
        related to IMPC experiments that together involve at least the 
        specified minimum number of specimen.
        The impc.date.to will be set to TRUE automatically if impc.centre is
        provided.}

}

\details{
    An HTTP GET request retrieves an XML from FlowRepository, and this XML 
    is parsed to extract the list of identifiers of public experiments.
}

\value{
    A vector of objects of class \code{character} containing strings 
    with FlowRepository dataset identifiers available.
}

\references{
    Spidlen Josef. FlowRepository Resources for Developers.\cr
    http://flowrepository.org/developers
}

\author{Josef Spidlen}

\seealso{
    \code{\link{flowRep.get}}, 
    \code{\link{setFlowRepositoryCredentials}}
    \code{\link{listKnownIlarCodes}}
    \code{\link{ilarCodeDescription}}
}

\examples{
    flowRep.ls()
}

\keyword{methods}
