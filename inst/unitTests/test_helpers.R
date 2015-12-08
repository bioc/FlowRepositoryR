library(Rcurl)
test_login_logout <- function() {
    h <- getCurlHandle(cookiefile="")
    myError <- tryCatch({FlowRepositoryR:::flowRep.login(h)}, 
        error = function(x) paste0(x))
    checkTrue(grepl('Unauthorized', myError, fixed=TRUE))
    resp <- FlowRepositoryR:::flowRep.logout(h)
    checkTrue(grepl('flowrepository.org/login', resp, fixed=TRUE))
}

test_preprocessHttpHeader <- function() {
    checkTrue(FlowRepositoryR:::preprocessHttpHeader("foo") == "foo")
    checkTrue(FlowRepositoryR:::preprocessHttpHeader
        ("foo 401 Unauthorized\n\r foo") 
        == "401 Unauthorized")
    checkTrue(FlowRepositoryR:::preprocessHttpHeader
        ("foo 403 Forbidden\n\r foo") 
        == "403 Forbidden")
    checkTrue(FlowRepositoryR:::preprocessHttpHeader
        ("foo 500 Internal Server Error\n\r foo") 
        == "500 Internal Server Error")
    checkTrue(FlowRepositoryR:::preprocessHttpHeader
        ("foo 404 Not Found\n\r foo") 
        == "404 Not Found")
    checkTrue(FlowRepositoryR:::preprocessHttpHeader
        ("foo 400 Bad Request\n\r foo") 
        == "400 Bad Request")
}