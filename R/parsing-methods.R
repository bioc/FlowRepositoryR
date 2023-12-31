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

setGeneric("parseFlowRepositoryXML", def=function(object, myEnv, ...)
    standardGeneric("parseFlowRepositoryXML"),
    useAsDefault=function(object, myEnv, ...)
    {
        stop(cat("Unrecognized response:", class(object)))
    }
)

setMethod("parseFlowRepositoryXML", "FlowRepository",
    function(object, myEnv, ...) 
    {
        lapply(xmlChildren(object), identifyNode, myEnv, ...)
    }
)

setGeneric("identifyNode",
    def=function(object, myEnv, ...) standardGeneric("identifyNode"),
    useAsDefault=function(object, myEnv, ...)
    {
        stop(paste("Not a supported XML node in FlowRepository API Response:", 
            paste(object, collapse=", "), sep=" "))
    }
)

setMethod("identifyNode", "api.version",
    function(object, myEnv, ...) 
    {
        myEnv[['.api.version']] <- getElValAsChar(object)
    }
)

setMethod("identifyNode", "public.experiments",
    function(object, myEnv, ...) 
    {
        lapply(xmlChildren(object), identifyNode, myEnv, ...)
    }
)

setMethod("identifyNode", "experiment",
    function(object, myEnv, ...) 
    {
        id <- NULL
        public.url <- NULL
        public <- NULL
        name <- NULL
        primary.researcher <- NULL
        primary.investigator <- NULL
        uploader <- NULL
        purpose <- NULL
        conclusion <- NULL
        comments <- NULL
        funding <- NULL
        experiment.dates <- NULL
        qc.measures <- NULL
        miflowcyt.score <- NULL
        keywords <- list()
        publications <- list()
        organizations <- list()
        fcs.files <- list()
        attachments <- list()
        impc.experiments <- list()
    
        for (node in xmlChildren(object))
        {
            if (node$name == "id") 
                id <- getElValAsChar(node)
            else if (node$name == "public-url") 
                public.url <- getElValAsChar(node)
            else if (node$name == "public") 
                public <- getElValAsLogicle(node)
            else if (node$name == "name") 
                name <- getElValAsChar(node)
            else if (node$name == "primary-researcher") 
                primary.researcher <- getElValAsChar(xmlChildren(node)$name)
            else if (node$name == "primary-investigator") 
                primary.investigator <- getElValAsChar(xmlChildren(node)$name)
            else if (node$name == "uploader") 
                uploader <- getElValAsChar(xmlChildren(node)$name)
            else if (node$name == "experiment-dates") 
                experiment.dates <- 
                    c(getElValAsChar(xmlChildren(node)[['start-date']]), 
                        getElValAsChar(xmlChildren(node)[['end-date']]))
            else if (node$name == "purpose") 
                purpose <- getElValAsChar(node)
            else if (node$name == "conclusion") 
                conclusion <- getElValAsChar(node)
            else if (node$name == "comments") 
                comments <- getElValAsChar(node)
            else if (node$name == "funding") 
                funding <- getElValAsChar(node)
            else if (node$name == "quality-control-measures") 
                qc.measures <- getElValAsChar(node)
            else if (node$name == "miflowcyt-score") 
                miflowcyt.score <- getElValAsNum(node)
            else if (node$name == "keywords") 
                keywords <- extractKeywords(node)
            else if (node$name == "related-publications") 
                publications <- extractPublications(node)
            else if (node$name == "organizations") 
                organizations <- extractOrganizations(node)
            else if (node$name == "fcs-files") 
                fcs.files <- extractFCSFilesInfo(node)
            else if (node$name == "attachments")
                attachments <- extractAttachmentsInfo(node)
            else if (node$name == "impc_experiments")
                impc.experiments <- extractImpcInfo(node)
        }
    
        if (!is.null(id)) {
            myEnv[[id]] <- flowRepData(
                id=id, public.url=public.url, name=name, public=public,
                primary.researcher=primary.researcher, 
                primary.investigator=primary.investigator, uploader=uploader, 
                purpose=purpose, conclusion=conclusion, comments=comments,
                funding=funding, experiment.dates=experiment.dates, 
                qc.measures=qc.measures,
                miflowcyt.score=miflowcyt.score, keywords=keywords, 
                publications=publications, organizations=organizations, 
                fcs.files=fcs.files, attachments=attachments,
                impc.experiments <- impc.experiments)
        }
    }
)

setMethod(
    "identifyNode", "public.datasets",
    function(object, myEnv, ...) 
    {
        myEnv[['datasetIDs']] <- tryCatch(
            { unlist(strsplit(getElValAsChar(object), split=",")); },
            interrupt = function(ex) { NULL; },
            error = function(ex) { NULL; }
        )
    }
)

setMethod(
    "identifyNode", "result.datasets",
    function(object, myEnv, ...)
    {
        myEnv[['datasetIDs']] <- tryCatch(
            { unlist(strsplit(getElValAsChar(object), split=",")); },
            interrupt = function(ex) { NULL; },
            error = function(ex) { NULL; }
        )
    }
)

# Ignoring these elements
setMethod("identifyNode", "response.with.details", 
    function(object, myEnv, ...) { } 
)

setMethod(
    "identifyNode", 
    "error", 
    function(object, myEnv, ...) 
    {
        stop(try(paste0("An error received: ", names(object), ' - ', 
            object[[1]][['text']])[[6]], silent=TRUE))
    } 
)

extractPublications <- function(object)
{
    ret <- list()
    for (node in xmlChildren(object))
    {
        if (node$name == "publication") 
        {
            publ <- NULL
            for (childnode in xmlChildren(node))
            {
                if (childnode$name == "pubmed-id") 
                    publ=paste0("PMID:", getElValAsChar(childnode))
                else if (childnode$name == "pmc-id") 
                    publ=paste0("PMCID:", getElValAsChar(childnode))
            }
            if (!is.null(publ) && nchar(publ) > 6)
            {
                ret <- c(ret, publ)
            }
        }
    }
    ret
}


extractKeywords <- function(object) 
{
    ret <- list()
    for (node in xmlChildren(object))
    {
        if (node$name == "keyword") 
        {
            kw <- getElValAsChar(node)
            if (!is.null(kw) && nchar(kw) > 0)
            {
                ret <- c(ret, kw)
            }
        }
    }
    ret
}

extractOrganizations <- function(object) 
{
    ret <- list()
    for (node in xmlChildren(object))
    {
        if (node$name == "organization") 
        {
            name <- NULL
            street <- NULL
            city <- NULL
            zip <- NULL
            state <- NULL
            country <- NULL
            for (childnode in xmlChildren(node))
            {
                if (childnode$name == "name") name=getElValAsChar(childnode)
                else if (childnode$name == "address")
                {
                    for (subchildnode in xmlChildren(childnode))
                    {
                        if (subchildnode$name == "street") 
                            street <- getElValAsChar(subchildnode)
                        else if (subchildnode$name == "city") 
                            city <- getElValAsChar(subchildnode)
                        else if (subchildnode$name == "zip") 
                            zip <- getElValAsChar(subchildnode)
                        else if (subchildnode$name == "state") 
                            state <- getElValAsChar(subchildnode)
                        else if (subchildnode$name == "country") 
                            country <- getElValAsChar(subchildnode)
                    }
                }
            }
            
            if (!is.null(name))
            {
                ret <- c(ret, flowRepOrganization(name=name, street=street, 
                    city=city, zip=zip, state=state, country=country))
            }
        }
    }
    ret
}


extractFCSFilesInfo <- function(object) 
{
    ret <- list()
    for (node in xmlChildren(object))
    {
        if (node$name == "fcs-file") 
        {
            file.name <- NULL
            file.url <- NULL
            file.version <- NULL
            file.size <- 0
            file.md5sum <- NULL
            for (childnode in xmlChildren(node))
            {
                if (childnode$name == "file-name") 
                    file.name <- getElValAsChar(childnode)
                else if (childnode$name == "url") 
                    file.url <- getElValAsChar(childnode)
                else if (childnode$name == "fcs-version") 
                    file.version <- getElValAsChar(childnode)
                else if (childnode$name == "md5sum")
                    file.md5sum <- getElValAsChar(childnode)
                else if (childnode$name == "file-size") 
                    file.size <- getElValAsNum(childnode)
            }
            if (!is.null(file.name) && !is.null(file.url))
            {
                ret <- c(ret, fcsProxy(name=file.name, url=file.url, 
                    size=as.numeric(file.size), md5sum=file.md5sum, 
                    fcs.version=file.version, localpath=NULL))
            }
        }
    }
    ret
}

extractAttachmentsInfo <- function(object) 
{
    ret <- list()
    for (node in xmlChildren(object))
    {
        if (node$name == "attachment") 
        {
            file.name <- NULL
            file.url <- NULL
            file.description <- NULL
            file.size <- 0
            file.md5sum <- NULL
            for (childnode in xmlChildren(node))
            {
                if (childnode$name == "file-name") 
                    file.name <- getElValAsChar(childnode)
                else if (childnode$name == "url") 
                    file.url <- getElValAsChar(childnode)
                else if (childnode$name == "description") 
                    file.description <- getElValAsChar(childnode)
                else if (childnode$name == "md5sum") 
                    file.md5sum <- getElValAsChar(childnode)
                else if (childnode$name == "file-size") 
                    file.size <- getElValAsNum(childnode)
            }
            if (!is.null(file.name) && !is.null(file.url))
            {
                ret <- c(ret, attachmentProxy(name=file.name, url=file.url, 
                    size=as.numeric(file.size), md5sum=file.md5sum, 
                    description=file.description, localpath=NULL))
            }
        }
    }
    ret
}

extractImpcInfo <- function(object)
{
    ret <- list()
    for (node in xmlChildren(object))
    {
        if (node$name == "impc_experiment")  {
            impc_experiment_code <- xmlGetAttr(node, "impc_experiment_code")
            details <- NULL
            impc_specimen_code <- NULL
            impc_specimen_fcs_files <- list()
            impc_specimen_details <- NULL
            impc_metadata_sets <- list()
            impc_parameter_sets <- list()
            
            for (childnode in xmlChildren(node))
            {
                if (childnode$name == "details")
                    details <- tryCatch(fromJSON(xmlGetAttr(childnode, "json")), 
                        warning=function(w){}, error=function(e){}, finally={})
                else if (childnode$name == "impc_specimen") 
                {
                    impc_specimen_code <- xmlGetAttr(childnode, 
                        "impc_specimen_code")
                    for (grandchildnode in xmlChildren(childnode)) 
                    {
                        if (grandchildnode$name == "details")
                        {
                            impc_specimen_details <- tryCatch(
                                fromJSON(xmlGetAttr(grandchildnode, "json")), 
                                warning=function(w){}, error=function(e){}, 
                                finally={}
                            )
                        }
                        else if (grandchildnode$name == "fcs_files")
                        {
                            for (g2childnode in xmlChildren(grandchildnode)) 
                            {
                                if (g2childnode$name == "fcs_file")
                                {
                                    f <- xmlGetAttr(g2childnode, "filename")
                                    p <- xmlGetAttr(g2childnode, "panel")
                                    incV <- xmlGetAttr(
                                        g2childnode, "incrementValue")
                                    s <- xmlGetAttr(g2childnode, "size")
                                    m <- xmlGetAttr(g2childnode, "md5sum")
                                    
                                    impc_specimen_fcs_files <- c(
                                        impc_specimen_fcs_files, 
                                        list(list(
                                            filename = f,
                                            panel = p,
                                            incrementValue = incV,
                                            size = s,
                                            md5sum = m)))
                                }
                            }
                        }
                    }
                }
                else if (childnode$name == "impc_metadata_sets") 
                {
                    for (grandchildnode in xmlChildren(childnode))
                    {
                        if (grandchildnode$name == "impc_metadata")
                        {
                            impc_metadata_set <- NULL
                            for (g2childnode in xmlChildren(grandchildnode)) 
                            {
                                if (g2childnode$name == "details")
                                    impc_metadata_set <- tryCatch(
                                        fromJSON(
                                            xmlGetAttr(g2childnode, "json")), 
                                        warning=function(w){}, 
                                        error=function(e){}, 
                                        finally={}
                                    )
                            }
                            impc_metadata_set <- tryCatch(
                                impc_metadata_set$impc_metadata, 
                                warning=function(w){}, error=function(e){}, 
                                finally={})
                            impc_metadata_sets <- c(
                                impc_metadata_sets, list(impc_metadata_set))
                        }
                    }
                }
                else if (childnode$name == "impc_parameter_sets") 
                {
                    for (grandchildnode in xmlChildren(childnode))
                    {
                        if (grandchildnode$name == "impc_parameters")
                        {
                            gated_by <- xmlGetAttr(grandchildnode, "gated_by")
                            impc_parameter_set <- NULL
                            for (g2childnode in xmlChildren(grandchildnode)) 
                            {
                                if (g2childnode$name == "details")
                                    impc_parameter_set <- tryCatch(
                                        fromJSON(
                                            xmlGetAttr(g2childnode, "json")), 
                                        warning=function(w){}, 
                                        error=function(e){}, 
                                        finally={}
                                    )
                            }
                            impc_parameter_set <- tryCatch(
                                impc_parameter_set$impc_parameter, 
                                warning=function(w){}, error=function(e){}, 
                                finally={})
                            impc_parameter_set$gated_by <- gated_by
                            impc_parameter_sets <- c(
                                impc_parameter_sets, list(impc_parameter_set))
                        }
                    }
                    
                }
            }
            
            details <- tryCatch(details$impc_experiment, 
                     warning=function(w){}, error=function(e){}, finally={})
            impc_specimen_details <- tryCatch(
                impc_specimen_details$impc_specimen, 
                warning=function(w){}, error=function(e){}, finally={})
            
            impc_experiment <- list(
                "impc_experiment_code" = impc_experiment_code,
                "details" = details,
                "impc_specimen_code" = impc_specimen_code,
                "impc_specimen_fcs_files" = impc_specimen_fcs_files,
                "impc_specimen_details" = impc_specimen_details,
                "impc_metadata_sets" = impc_metadata_sets,
                "impc_parameter_sets" = impc_parameter_sets
                )
            ret <- c(ret, list(impc_experiment))
        }
    }
    ret
}
