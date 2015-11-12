test_impc_dictionary <- function() {
    ## Checking IMPC dictionary
    ## lowercase
    checkTrue(FlowRepositoryR:::impcCodeDescription("impc_imm_010_001") == "DP T cells")
    ## Uppercase
    checkTrue(FlowRepositoryR:::impcCodeDescription("IMPC_IMM_013_001") == "DN NKT cells")
    ## Mixedcase
    checkTrue(FlowRepositoryR:::impcCodeDescription("ImpC_iMm_019_001") == "DN CD25- T cells")
    ## Works with lists as well
    x <- FlowRepositoryR:::impcCodeDescription(c("impc_imm_022_001", "IMPC_IMM_028_001"))
    checkTrue(length(x) == 2)
    checkTrue(x[1] == "CD8 CD25+ NKT cells")
    checkTrue(x[2] == "CD4 CD44+CD62L- T cells")
}
