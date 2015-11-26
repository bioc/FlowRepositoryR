test_generics_download <- function() {
    myError = tryCatch({download("foo")}, error = function(x) paste0(x))
    checkTrue(
        myError == 
        paste("Error in download(\"foo\"): The download",
            "method is not supported on object type: character\n")
        )
}

test_generics_is_downloaded <- function() {
    myError = tryCatch({is.downloaded("foo")}, error = function(x) paste0(x))
    checkTrue(
        myError == 
            paste("Error in is.downloaded(\"foo\"): The is.downloaded",
                  "method is not supported on object type: character\n")
    )
}

test_generics_verify_integrity <- function() {
    myError <- tryCatch({FlowRepositoryR:::verify.integrity("foo")}, 
        error = function(x) paste0(x))
    checkTrue(
        myError == 
            paste("Error in FlowRepositoryR:::verify.integrity(\"foo\"): The",
                  "verify.integrity method is not supported on character\n")
    )
}

test_generics_impcResultsCopy <- function() {
    myError = tryCatch({impcResultsCopy("foo")}, error = function(x) paste0(x))
    checkTrue(
        myError == 
            paste("Error in impcResultsCopy(\"foo\"): The impcResultsCopy",
                  "method is not supported on character\n")
    )
}


