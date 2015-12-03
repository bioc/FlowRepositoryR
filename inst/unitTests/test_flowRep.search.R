test_flowRep.search <- function() {
    checkTrue(length(flowRep.search("Spidlen")) >= 1)
    checkTrue(length(flowRep.search("CD11b")) >= 1)   
}