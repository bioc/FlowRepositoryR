test_url_settings <- function() {
    myUrl = getFlowRepositoryURL()
    checkTrue(myUrl == "https://flowrepository.org/")
    setFlowRepositoryURL("http://localhost")
    ## The slash at the end should be added automatically
    checkTrue(getFlowRepositoryURL() == "http://localhost/")
    setFlowRepositoryURL("https://127.0.0.1/")
    ## Now the slash is already at the end and should not be added
    checkTrue(getFlowRepositoryURL() == "https://127.0.0.1/")
    ## Finally, let's set it back to the default
    setFlowRepositoryURL(myUrl)
}

test_credentials_settings <- function() {
    ## Setting and than forgetting credentials
    checkTrue(!FlowRepositoryR:::haveFlowRepositoryCredentials())
    setFlowRepositoryCredentials(email="boo@gmail.com", password="foo123456")
    checkTrue(FlowRepositoryR:::haveFlowRepositoryCredentials())
    forgetFlowRepositoryCredentials()
    checkTrue(!FlowRepositoryR:::haveFlowRepositoryCredentials())
}

test_impcCodeDescription <- function() {
    checkTrue(impcCodeDescription('impc_imm_088_001') == 
        'Cell counting equipment model')
    
    resVect <- impcCodeDescription(c('IMPC_IMM_002_001', 'IMPC_Imm_004_001')) ==
        c('Percentage of live gated events in Panel A', 'NKT cells (panel A)')
    checkTrue(resVect[1])
    checkTrue(resVect[2])
    checkTrue(is.null(impcCodeDescription('foo')))
}

test_ilarCodeDescription <- function() {
    checkTrue(ilarCodeDescription('Rbri') == "BC Cancer Agency")
    checkTrue(is.null(ilarCodeDescription('RBri')))
    resVect <- ilarCodeDescription(c('Rbri', 'H')) == 
        c('BC Cancer Agency', 'MRC Harwell')
    checkTrue(resVect[1])
    checkTrue(resVect[2])
}

test_listKnownIlarCodes <- function() {
    tmp <- listKnownIlarCodes()
    checkTrue(length(tmp) >= 11)
}