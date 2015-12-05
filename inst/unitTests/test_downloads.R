test_downloads <- function() {
    myDataset <- flowRep.get("FR-FCM-ZZJ7")
    
    ## First, the dataset is not downloaded
    checkTrue(!is.downloaded(myDataset))
    
    ## The first attachment is not downloaded, download it and check
    ## again. It should be downloaded, but the whole dataset still isn't.
    checkTrue(is.null(myDataset@attachments[[1]]@localpath))
    myDataset@attachments[[1]] <- download(myDataset@attachments[[1]])
    checkTrue(!is.null(myDataset@attachments[[1]]@localpath))
    checkTrue(is.downloaded(myDataset@attachments[[1]]))
    checkTrue(!is.downloaded(myDataset))

    ## The first fcs file is not downloaded, download it and check
    ## again. It should be downloaded, but the whole dataset still isn't.
    checkTrue(is.null(myDataset@fcs.files[[1]]@localpath))
    myDataset@fcs.files[[1]] <- download(myDataset@fcs.files[[1]])
    checkTrue(!is.null(myDataset@fcs.files[[1]]@localpath))
    checkTrue(is.downloaded(myDataset@fcs.files[[1]]))
    checkTrue(!is.downloaded(myDataset))
    
    ## Finally, let's just download the whole dataset (i.e., all remaining
    ## FCS files and attachments). The dataset should be all downloaded
    ## afterwards.
    myDataset <- download(myDataset)
    checkTrue(is.downloaded(myDataset))
    
    checkTrue(id(myDataset) == "FR-FCM-ZZJ7")
    checkTrue(length(fcs.files(myDataset)) >=2)
    checkTrue(length(attachments(myDataset)) >= 2)
    checkTrue(length(impc.experiments(myDataset)) == 0)
    checkTrue(nchar(localpath(attachments(myDataset)[[1]])) > 5)
    checkTrue(nchar(localpath(fcs.files(myDataset)[[1]])) > 5)
    checkTrue(length(organizations(myDataset)) >= 1)
    
    checkTrue(FlowRepositoryR:::verify.integrity(fcs.files(myDataset)[[1]]))
    
    tmpfile <- tempfile(pattern="FRRUnitTst", tmpdir=tempdir(), fileext=".tmp")
    sink(tmpfile)
    summary(myDataset)
    sink()
    checkTrue(
        grepl('flowRepData', readChar(tmpfile, file.info(tmpfile)$size), 
            fixed=TRUE)
    )
    file.remove(tmpfile)

    tmpfile <- tempfile(pattern="FRRUnitTst", tmpdir=tempdir(), fileext=".tmp")
    sink(tmpfile)
    summary(fcs.files(myDataset)[[2]])
    sink()
    checkTrue(
        grepl('fileProxy', readChar(tmpfile, file.info(tmpfile)$size),
            fixed=TRUE)
    )
    file.remove(tmpfile)
    
    tmpfile <- tempfile(pattern="FRRUnitTst", tmpdir=tempdir(), fileext=".tmp")
    sink(tmpfile)
    summary(organizations(myDataset)[[1]])
    sink()
    checkTrue(
        grepl('BC Cancer Agency', readChar(tmpfile, file.info(tmpfile)$size),
            fixed=TRUE)
    )
    file.remove(tmpfile)
    
    myError <- tryCatch({fcs.files(myDataset) <- "foo"}, 
        error = function(x) paste0(x))
    checkTrue(myError == "Error: fcs.files shall be a list\n")
    
    fcs.files(myDataset) <- c(fcs.files(myDataset), fcs.files(myDataset))
    checkTrue(length(fcs.files(myDataset)) == 4)

    myError <- tryCatch({attachments(myDataset) <- "foo"}, 
        error = function(x) paste0(x))
    checkTrue(myError == "Error: attachments shall be a list\n")
    
    attachments(myDataset) <- c(attachments(myDataset), attachments(myDataset))
    checkTrue(length(attachments(myDataset)) == 4)
    
    myError <- tryCatch({impc.experiments(myDataset) <- "foo"}, 
        error = function(x) paste0(x))
    checkTrue(myError == "Error: impc.experiments shall be a list\n")
    
    impc.experiments(myDataset) <- c(impc.experiments(myDataset), 
                                     impc.experiments(myDataset))
    checkTrue(length(impc.experiments(myDataset)) == 0)
    
}
