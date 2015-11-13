\name{impcResultsCopy}
\alias{impcResultsCopy}
\alias{impcResultsCopy,flowRepData,impcResultsCopy-method}

\title{Retrieve a copy of IMPC results associated with a flowRepData object}

\description{
    This method retrieves a copy of the results associated with
    a parameter set of an IMPC experiment related to a
    \code{\linkS4class{flowRepData}} object (i.e., to a FlowRepository 
    dataset).
}

\usage{
    impcResultsCopy(object, \dots)
}

\arguments{
    \item{object}{Object of class \code{\linkS4class{flowRepData}}.}
    \item{\dots}{Additional options, see the details section.}
}

\details{
    This method will return an object of class \code{list} with a
    copy of IMPC analysis results. This is what can typically be found in
    \code{object@impc.experiments[[experimentIndex]]$
        impc_parameter_sets[[resultIndex]]}
    but without the \code{updated_at}, \code{created_at}, \code{id},
    \code{gated_by}, \code{gated_by_id}, and \code{impc_experiment_id} items.
    You can specify the following additional options:
    \describe{
        \item{experimentIndex}{Index of the IMPC experiment that shall be used.
            Usually, there is no more than 1 IMPC experiment related to a 
            \code{\linkS4class{flowRepData}} object, but this argument is 
            available for the odd case where there would be multiple IMPC 
            experiments related to a single FlowRepository dataset.}
        \item{resultIndex}{Index of which IMPC parameter set of the IMPC 
            experiment shall be used. When new IMPC results are submitted to 
            FlowRepository, those will create a new paramater set associated 
            with the particular IMPC experiment (i.e., previous results will 
            not be overwritten). This argument can be used to specify which 
            results are supposed to be coppied.}
    }
}

\value{
    A list representing the results of IMPC gating. This list will usually
    include:
        \code{impc_imm_001_001},
        \code{impc_imm_002_001},
        \code{impc_imm_003_001},
        \code{impc_imm_004_001},
        \code{impc_imm_005_001},
        \code{impc_imm_006_001},
        \code{impc_imm_007_001},
        \code{impc_imm_008_001},
        \code{impc_imm_009_001},
        \code{impc_imm_010_001},
        \code{impc_imm_011_001},
        \code{impc_imm_012_001},
        \code{impc_imm_013_001},
        \code{impc_imm_014_001},
        \code{impc_imm_015_001},
        \code{impc_imm_016_001},
        \code{impc_imm_017_001},
        \code{impc_imm_018_001},
        \code{impc_imm_019_001},
        \code{impc_imm_020_001},
        \code{impc_imm_021_001},
        \code{impc_imm_022_001},
        \code{impc_imm_023_001},
        \code{impc_imm_024_001},
        \code{impc_imm_025_001},
        \code{impc_imm_026_001},
        \code{impc_imm_027_001},
        \code{impc_imm_028_001},
        \code{impc_imm_029_001},
        \code{impc_imm_030_001},
        \code{impc_imm_031_001},
        \code{impc_imm_032_001},
        \code{impc_imm_033_001},
        \code{impc_imm_034_001},
        \code{impc_imm_035_001},
        \code{impc_imm_036_001},
        \code{impc_imm_037_001},
        \code{impc_imm_038_001},
        \code{impc_imm_039_001},
        \code{impc_imm_040_001},
        \code{impc_imm_041_001},
        \code{impc_imm_042_001},
        \code{impc_imm_043_001},
        \code{impc_imm_044_001},
        \code{impc_imm_045_001},
        \code{impc_imm_046_001},
        \code{impc_imm_047_001},
        \code{impc_imm_048_001},
        \code{impc_imm_049_001},
        \code{impc_imm_050_001},
        \code{impc_imm_051_001},
        \code{impc_imm_052_001},
        \code{impc_imm_053_001},
        \code{impc_imm_054_001},
        \code{impc_imm_055_001},
        \code{impc_imm_056_001},
        \code{impc_imm_057_001},
        \code{impc_imm_058_001},
        \code{impc_imm_059_001},
        \code{impc_imm_060_001},
        \code{impc_imm_061_001},
        \code{impc_imm_062_001},
        \code{impc_imm_063_001},
        \code{impc_imm_064_001},
        \code{impc_imm_065_001},
        \code{impc_imm_066_001},
        \code{impc_imm_067_001},
        \code{impc_imm_068_001},
        \code{impc_imm_069_001},
        \code{impc_imm_070_001},
        \code{impc_imm_071_001},
        \code{impc_imm_072_001},
        \code{impc_imm_073_001},
        \code{impc_imm_074_001},
        \code{impc_imm_075_001},
        \code{impc_imm_076_001}.
    Those are IMPC codes defined at 
    \code{https://www.mousephenotype.org/impress/protocol/174/7}
}

\author{Josef Spidlen}

\examples{
    ## Get dataset from FlowRepository
    mySmallDataset <- flowRep.get("FR-FCM-ZZ3X", impc.details = TRUE)
    impcResultsCopy(mySmallDataset, experimentIndex = 1, resultIndex = 1)
    ## The result will be NULL since the "FR-FCM-ZZ3X" does not include
    ## any IMPC details. Right now, IMPC details are only in private instances
    ## of FlowRepository. This example will be updated to a better once once
    ## IMPC data is also included and public in the main public FlowRepository
    ## (flowrepository.org).
}

\keyword{methods}
