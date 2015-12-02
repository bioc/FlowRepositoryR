###############################################################################
## Copyright (c) 2015 Josef Spidlen, Ph.D.
##
## License
## The software is distributed under the terms of the 
## Artistic License 2.0
## http://www.r-project.org/Licenses/Artistic-2.0
## 
## Disclaimer
## This software and documentation come with no warranties of any kind.
## This software is provided "as is" and any express or implied 
## warranties, including, but not limited to, the implied warranties of
## merchantability and fitness for a particular purpose are disclaimed.
## In no event shall the  copyright holder be liable for any direct, 
## indirect, incidental, special, exemplary, or consequential damages
## (including but not limited to, procurement of substitute goods or 
## services; loss of use, data or profits; or business interruption)
## however caused and on any theory of liability, whether in contract,
## strict liability, or tort arising in any way out of the use of this 
## software.    
###############################################################################


flowRep.ls <- function(
    include.private=FALSE,
    impc.only=FALSE,
    impc.unanalyzed.only=FALSE,
    impc.centre=NULL,
    impc.date.from=NULL,
    impc.date.to=NULL,
    impc.specimen.geneKO.mgiId=NULL,
    impc.specimen.geneKO.geneSymbol=NULL,
    impc.specimen.baseline=NULL,
    impc.specimen.minCount=NULL)
{
    if (!haveFlowRepositoryCredentials()) include.private <- FALSE
    destfile <- tempfile(pattern="FlowRepository.DatasetList", 
        tmpdir=tempdir(), fileext=".xml")
    h <- getCurlHandle(cookiefile="")

    myUrl <- paste0(getFlowRepositoryURL(), 
        'list?client=', URLencode(getFlowRepositoryClientID()))
    
    if(length(impc.only) > 1 || !is.logical(impc.only))
    {
        warning("impc.only shall be a single logical value, TRUE or FALSE")
        return(NULL)
    }
    
    if(length(impc.unanalyzed.only) > 1 || !is.logical(impc.unanalyzed.only))
    {
        warning(paste("impc.unanalyzed.only shall be a single logical value,", 
            "TRUE or FALSE"))
        return(NULL)
    }
    
    if(impc.unanalyzed.only) 
    {
        myUrl <- paste0(myUrl, "&unanalyzedonly=1")
        impc.only <- TRUE
    }
    
    if (!is.null(impc.centre))
    {
        if (is.character(impc.centre) && (length(impc.centre) == 1))
        {
            if (is.null(ilarCodeDescription(impc.centre))) 
            {
                warning(paste0("This impc.centre was not recognized; use an ILAR code of a known IMPC center, one of ", paste0(listKnownIlarCodes(), collapse = ", ")), ".")
                return(NULL)
            } 
            else 
            {
                myUrl <- paste0(myUrl, "&impccentre=", URLencode(impc.centre, reserved=TRUE))
                impc.only <- TRUE
            }
        }
        else 
        {
            warning("impc.centre shall be a single string of characters")
            return(NULL)
        }
    }
    
    if (!is.null(impc.date.from)) 
    {
        if ((class(impc.date.from) != "Date") || (length(impc.date.from) != 1)) 
        {
            warning(paste('impc.date.from shall be a single Date value, ',
                'e.g., as.Date("2015-10-01", "%Y-%m-%d")'))
            return(NULL)
        }
        else
        {
            myUrl <- paste0(myUrl, "&datafrom=", 
                URLencode(strftime(impc.date.from, "%Y%m%d"), reserved=TRUE))
            impc.only <- TRUE
        }
    }
    
    if (!is.null(impc.date.to)) 
    {
        if ((class(impc.date.to) != "Date") || (length(impc.date.to) != 1)) 
        {
            warning(paste('impc.date.to shall be a single Date value, ',
                'e.g., as.Date("2015-10-01", "%Y-%m-%d")'))
            return(NULL)
        }
        else
        {
            myUrl <- paste0(myUrl, "&datato=", 
                URLencode(strftime(impc.date.to, "%Y%m%d"), reserved=TRUE))
            impc.only <- TRUE
        }
    }
    
    if (!is.null(impc.specimen.geneKO.mgiId))
    {
        if (is.character(impc.specimen.geneKO.mgiId) && 
            (length(impc.specimen.geneKO.mgiId) == 1))
        {
            myUrl <- paste0(myUrl, "&mgigeneid=", 
                URLencode(impc.specimen.geneKO.mgiId, reserved=TRUE))
            impc.only <- TRUE
        }
        else 
        {
            warning("impc.specimen.geneKO.mgiId shall be a single string of characters")
            return(NULL)
        }
    }
    
    if (!is.null(impc.specimen.geneKO.geneSymbol))
    {
        if (is.character(impc.specimen.geneKO.geneSymbol) && 
                (length(impc.specimen.geneKO.geneSymbol) == 1))
        {
            myUrl <- paste0(myUrl, "&genesymbol=", 
                URLencode(impc.specimen.geneKO.geneSymbol, reserved=TRUE))
            impc.only <- TRUE
        }
        else 
        {
            warning("impc.specimen.geneKO.geneSymbol shall be a single string of characters")
            return(NULL)
        }
    }
    
    if(!is.null(impc.specimen.baseline))
    {
        if(length(impc.specimen.baseline) > 1 || !is.logical(impc.specimen.baseline))
        {
            warning(paste("impc.specimen.baseline shall be NULL or a single", 
                          "logical value, TRUE or FALSE"))
            return(NULL)
        } 
        else 
        {
            impc.only <- TRUE
            if(impc.specimen.baseline) 
            {
                myUrl <- paste0(myUrl, "&baseline=1")
            }
            else
            {
                myUrl <- paste0(myUrl, "&baseline=0")
            }
        }
    }
    
    if(!is.null(impc.specimen.minCount))
    {
        if(length(impc.specimen.minCount) > 1 || !is.numeric(impc.specimen.minCount))
        {
            warning("impc.specimen.minCount shall be NULL or a numeric value")
            return(NULL)
        }
        else
        {
            myUrl <- paste0(myUrl, "&minmice=", 
                URLencode(as.character(impc.specimen.minCount), reserved=TRUE))
            impc.only <- TRUE
        }
    }    
    
    if (impc.only) 
    {
        myUrl <- paste0(myUrl, "&impconly=1")
    }
    
    if (include.private) flowRep.login(h)
    f <- CFILE(destfile, mode="wb")
    response <- curlPerform(url=myUrl, writedata=f@ref, curl=h, 
        .opts=list(ssl.verifypeer=FALSE))
    close(f)
    if (include.private) flowRep.logout(h)

    myEnv <- new.env()
    myEnv[['datasetIDs']] <- list()
    parseFlowRepositoryXML(xmlRoot(smartTreeParse(destfile)), myEnv)
    try(file.remove(destfile), silent=TRUE)
    unlist(myEnv[['datasetIDs']])
}


flowRep.get <- function(id, use.credentials=TRUE, impc.details=FALSE) {
    if (!is.character(id)) 
        stop('Please specify a dataset identifier as a character string.')
    if (!haveFlowRepositoryCredentials()) use.credentials <- FALSE
    destfile <- tempfile(pattern="FlowRepository.Dataset", tmpdir=tempdir(), 
        fileext=".xml")
    h <- getCurlHandle(cookiefile="")

    impcreq <- ""
    if (impc.details) impcreq <- "&impc=details"
    if (use.credentials) flowRep.login(h)
    f <- CFILE(destfile, mode="wb")
    response <- curlPerform(url=paste0(getFlowRepositoryURL(), 
        'list/', as.character(id), '?client=', getFlowRepositoryClientID(), 
        impcreq), 
        writedata=f@ref, curl=h, .opts=list(ssl.verifypeer=FALSE))
    close(f)
    if (use.credentials) flowRep.logout(h)

    myEnv <- new.env()
    myEnv[[id]] <- list()
    parseFlowRepositoryXML(xmlRoot(smartTreeParse(destfile)), myEnv)
    try(file.remove(destfile), silent=TRUE)
    unlist(myEnv[[id]])
}

flowRep.search <- function(query.string) {
    if((!is(query.string, "character")) || nchar(query.string) == 0)
        stop("query.string shall be a non-empty string", call.=FALSE)
    query.string <- URLencode(query.string, reserved=TRUE, repeated=TRUE)

    ## This is just getting ready for when the API supports this, at this point
    ## public datasets only are being searched.
    include.private <- FALSE
    if (!haveFlowRepositoryCredentials()) include.private <- FALSE
    destfile <- tempfile(pattern="FlowRepository.DatasetList", 
        tmpdir=tempdir(), fileext=".xml")
    h <- getCurlHandle(cookiefile="")

    if (include.private) flowRep.login(h)
    f <- CFILE(destfile, mode="wb")
    response <- curlPerform(url=paste0(getFlowRepositoryURL(), 
        'apisearch?client=', getFlowRepositoryClientID(), '&query_term=',
        query.string), 
        writedata=f@ref, curl=h, .opts=list(ssl.verifypeer=FALSE))
    close(f)
    if (include.private) flowRep.logout(h)

    myEnv <- new.env()
    myEnv[['datasetIDs']] <- list()
    parseFlowRepositoryXML(xmlRoot(smartTreeParse(destfile)), myEnv)
    try(file.remove(destfile), silent=TRUE)
    unlist(myEnv[['datasetIDs']])
}

flowRep.submitImpcResults <- function(gatedByIlar, impcExpId, results, final=TRUE) {
    if (!is.list(results)) stop("results shall be a list", call.=FALSE)
    if ((!is.character(gatedByIlar)) || 
        (nchar(gatedByIlar) == 0) || (nchar(gatedByIlar) > 5))
        stop(paste("gatedByIlar shall be a code from the International",
                   "Laboratory Code Registry list"), call.=FALSE)
    if (!is.character(impcExpId))
        stop("impcExpId shall be an IMPC experiment identifier", call.=FALSE)
    if (!haveFlowRepositoryCredentials()) 
        stop("credentials need to be set before you can submit IMPC results", 
             call.=FALSE)
    if (length(final) > 1 || !is.logical(final))
    {
        stop("final need to be a single logical value, TRUE or FALSE", 
             call.=FALSE)
    }
    if (final) final <- "1"
    else final <- "0"
    
    credentials <- getFlowRepositoryCredentials()
    resultsJson <- toJSON(results)
    headfunc  <- basicTextGatherer()
    writefunc <- basicTextGatherer()
    response <- postForm(
        paste0(getFlowRepositoryURL(), "impc/results/submit"),
        email=credentials[1], pass=credentials[2], final=final,
        gated_by=gatedByIlar, impc_exp_id=impcExpId, results=resultsJson,
        .opts=list(ssl.verifypeer=FALSE, 
            headerfunction=headfunc$update, writefunc=writefunc$update))
    
    response <- writefunc$value()
    header <- preprocessHttpHeader(headfunc$value())
    list(response=response, status=header)
}


flowRep.submitGeneStatus <- function(
    mgiGeneId, geneSymbol, isPhenodeviant, comment=NULL, isProcessed=TRUE) 
{
    if (!is.character(mgiGeneId) || is.null(mgiGeneId) || 
        length(mgiGeneId) != 1 || nchar(mgiGeneId) < 1)
        stop("mgiGeneId shall be a valid MGI Gene identifier", call.=FALSE)
    if (!is.character(geneSymbol) || is.null(geneSymbol) || 
        length(geneSymbol) != 1 || nchar(geneSymbol) < 1)
        stop("geneSymbol shall be a valid gene symbol", call.=FALSE)
    if (!is.logical(isPhenodeviant) || length(isPhenodeviant) != 1)
        stop("isPhenodeviant shall be TRUE or FALSE", call.=FALSE)
    if (!is.logical(isProcessed) || length(isProcessed) != 1)
        stop("isProcessed shall be TRUE or FALSE", call.=FALSE)
    if (!is.null(comment) && (!is.character(comment) || 
        length(comment) != 1 || nchar(comment) < 1))
        stop("comment shall be a single character string or NULL", call.=FALSE)

    if (!haveFlowRepositoryCredentials()) stop(
        "credentials need to be set before you can submit gene status results",
        call.=FALSE)
    
    credentials <- getFlowRepositoryCredentials()
    
    headfunc  <- basicTextGatherer()
    writefunc <- basicTextGatherer()
    response <- postForm(
        paste0(getFlowRepositoryURL(), "impc/results/submitgene"),
        email=credentials[1], pass=credentials[2],
        mgi_gene_id=mgiGeneId, gene_symbol=geneSymbol, 
        is_phenodeviant=isPhenodeviant, is_processed=isProcessed,
        comment=comment,
        .opts=list(ssl.verifypeer=FALSE, 
                   headerfunction=headfunc$update, writefunc=writefunc$update))
    
    response <- writefunc$value()
    header <- preprocessHttpHeader(headfunc$value())
    list(response=response, status=header)
}


