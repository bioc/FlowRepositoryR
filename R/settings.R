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


.FlowRepository.env <- new.env()

## .FlowRepository.env$.FlowRepository.URL <- 'http://localhost/'
## .FlowRepository.env$.FlowRepository.URL <- 'http://flowrepository.org/'
.FlowRepository.env$.FlowRepository.URL <- 'https://flowrepository.org/'

## This clientID is specific for FlowRepositoryR. If you need a clientID
## for a different tool to work with FlowRepository API, please contact
## FlowRepository administrators at flowrepository-admin@isac-net.org
.FlowRepository.env$.FlowRepository.ClientID <- 'FlowRepositoryRyzJl74CNkUp1Kpb'

# Dictionary for IMPC codes
# Based on https://www.mousephenotype.org/impress/protocol/174/7
# as of November 6, 2015
.FlowRepository.env$.dict <- new.env()
.FlowRepository.env$.dict$impc_imm_001_001 <- "Spleen weight in grams"
.FlowRepository.env$.dict$impc_imm_026_001 <- "Total number of acquired events in Panel A"
.FlowRepository.env$.dict$impc_imm_002_001 <- "Percentage of live gated events in Panel A"
.FlowRepository.env$.dict$impc_imm_003_001 <- "T cells (panel A)"
.FlowRepository.env$.dict$impc_imm_004_001 <- "NKT cells (panel A)"
.FlowRepository.env$.dict$impc_imm_005_001 <- "NK cells (panel A)"
.FlowRepository.env$.dict$impc_imm_006_001 <- "Others"
.FlowRepository.env$.dict$impc_imm_007_001 <- "CD4 T cells"
.FlowRepository.env$.dict$impc_imm_008_001 <- "CD8 T cells"
.FlowRepository.env$.dict$impc_imm_009_001 <- "DN T cells"
.FlowRepository.env$.dict$impc_imm_010_001 <- "DP T cells"
.FlowRepository.env$.dict$impc_imm_011_001 <- "CD4 NKT cells"
.FlowRepository.env$.dict$impc_imm_012_001 <- "CD8 NKT cells"
.FlowRepository.env$.dict$impc_imm_013_001 <- "DN NKT cells"
.FlowRepository.env$.dict$impc_imm_014_001 <- "CD4 CD25+ T cells"
.FlowRepository.env$.dict$impc_imm_015_001 <- "CD4 CD25- T cells"
.FlowRepository.env$.dict$impc_imm_016_001 <- "CD8 CD25+ T cells"
.FlowRepository.env$.dict$impc_imm_017_001 <- "CD8 CD25- T cells"
.FlowRepository.env$.dict$impc_imm_018_001 <- "DN CD25+ T cells"
.FlowRepository.env$.dict$impc_imm_019_001 <- "DN CD25- T cells"
.FlowRepository.env$.dict$impc_imm_020_001 <- "CD4 CD25+ NKT cells"
.FlowRepository.env$.dict$impc_imm_021_001 <- "CD4 CD25- NKT cells"
.FlowRepository.env$.dict$impc_imm_022_001 <- "CD8 CD25+ NKT cells"
.FlowRepository.env$.dict$impc_imm_023_001 <- "CD8 CD25- NKT cells"
.FlowRepository.env$.dict$impc_imm_024_001 <- "DN CD25+ NKT cells"
.FlowRepository.env$.dict$impc_imm_025_001 <- "DN CD25- NKT cells"
.FlowRepository.env$.dict$impc_imm_028_001 <- "CD4 CD44+CD62L- T cells"
.FlowRepository.env$.dict$impc_imm_029_001 <- "CD4 CD44+CD62L+ T cells"
.FlowRepository.env$.dict$impc_imm_030_001 <- "CD4 CD44-CD62L+ T cells"
.FlowRepository.env$.dict$impc_imm_031_001 <- "CD4 CD44-CD62L- T cells"
.FlowRepository.env$.dict$impc_imm_032_001 <- "CD8 CD44+CD62L- T cells"
.FlowRepository.env$.dict$impc_imm_033_001 <- "CD8 CD44+CD62L+ T cells"
.FlowRepository.env$.dict$impc_imm_034_001 <- "CD8 CD44-CD62L+ T cells"
.FlowRepository.env$.dict$impc_imm_035_001 <- "CD8 CD44-CD62L- T cells"
.FlowRepository.env$.dict$impc_imm_036_001 <- "DN CD44+CD62L- T cells"
.FlowRepository.env$.dict$impc_imm_037_001 <- "DN CD44+CD62L+ T cells"
.FlowRepository.env$.dict$impc_imm_038_001 <- "DN CD44-CD62L+ T cells"
.FlowRepository.env$.dict$impc_imm_039_001 <- "DN CD44-CD62L- T cells"
.FlowRepository.env$.dict$impc_imm_040_001 <- "CD4 CD44+CD62L- NKT cells"
.FlowRepository.env$.dict$impc_imm_041_001 <- "CD4 CD44+CD62L+ NKT cells"
.FlowRepository.env$.dict$impc_imm_042_001 <- "CD4 CD44-CD62L+ NKT cells"
.FlowRepository.env$.dict$impc_imm_043_001 <- "CD8 CD44+CD62L- NKT cells"
.FlowRepository.env$.dict$impc_imm_044_001 <- "CD8 CD44+CD62L+ NKT cells"
.FlowRepository.env$.dict$impc_imm_045_001 <- "CD8 CD44-CD62L+ NKT cells"
.FlowRepository.env$.dict$impc_imm_046_001 <- "DN CD44+CD62L- NKT cells"
.FlowRepository.env$.dict$impc_imm_047_001 <- "DN CD44+CD62L+ NKT cells"
.FlowRepository.env$.dict$impc_imm_048_001 <- "DN CD44-CD62L+ NKT cells"
.FlowRepository.env$.dict$impc_imm_027_001 <- "Total number of acquired events in Panel B"
.FlowRepository.env$.dict$impc_imm_049_001 <- "Percentage of live gated events in Panel B"
.FlowRepository.env$.dict$impc_imm_050_001 <- "Neutrophils"
.FlowRepository.env$.dict$impc_imm_051_001 <- "Monocytes"
.FlowRepository.env$.dict$impc_imm_052_001 <- "Eosinophils"
.FlowRepository.env$.dict$impc_imm_053_001 <- "NK Cells (panel B)"
.FlowRepository.env$.dict$impc_imm_054_001 <- "NK Subsets (Q1)"
.FlowRepository.env$.dict$impc_imm_055_001 <- "NK Subsets (Q2)"
.FlowRepository.env$.dict$impc_imm_056_001 <- "NK Subsets (Q3)"
.FlowRepository.env$.dict$impc_imm_057_001 <- "NK Subsets (Q4)"
.FlowRepository.env$.dict$impc_imm_058_001 <- "NKT Cells (panel B)"
.FlowRepository.env$.dict$impc_imm_059_001 <- "NKT Subsets (Q1)"
.FlowRepository.env$.dict$impc_imm_060_001 <- "NKT Subsets (Q3)"
.FlowRepository.env$.dict$impc_imm_061_001 <- "T Cells (panel B)"
.FlowRepository.env$.dict$impc_imm_062_001 <- "T Subset"
.FlowRepository.env$.dict$impc_imm_063_001 <- "B Cells"
.FlowRepository.env$.dict$impc_imm_064_001 <- "B1B Cells"
.FlowRepository.env$.dict$impc_imm_065_001 <- "B2B Cells"
.FlowRepository.env$.dict$impc_imm_066_001 <- "Follicular B Cells"
.FlowRepository.env$.dict$impc_imm_067_001 <- "Follicular B Cells (CD21/35+)"
.FlowRepository.env$.dict$impc_imm_068_001 <- "pre-B Cells"
.FlowRepository.env$.dict$impc_imm_069_001 <- "pre-B Cells (CD21/35 low)"
.FlowRepository.env$.dict$impc_imm_070_001 <- "MZB"
.FlowRepository.env$.dict$impc_imm_071_001 <- "MZB (CD21/35 high)"
.FlowRepository.env$.dict$impc_imm_072_001 <- "cDCs"
.FlowRepository.env$.dict$impc_imm_073_001 <- "cDCs CD11b Type"
.FlowRepository.env$.dict$impc_imm_074_001 <- "pDCs"
.FlowRepository.env$.dict$impc_imm_075_001 <- "RP Macrophage (F4/80+)"
.FlowRepository.env$.dict$impc_imm_076_001 <- "RP Macrophage (CD19- CD11c-)"
.FlowRepository.env$.dict$impc_imm_107_001 <- "Panel A FCS file(s)"
.FlowRepository.env$.dict$impc_imm_108_001 <- "Panel B FCS file(s)"
.FlowRepository.env$.dict$impc_imm_077_001 <- "Equipment name"
.FlowRepository.env$.dict$impc_imm_078_001 <- "Equipment manufacturer"
.FlowRepository.env$.dict$impc_imm_079_001 <- "Equipment model"
.FlowRepository.env$.dict$impc_imm_080_001 <- "CS&T Bead lot"
.FlowRepository.env$.dict$impc_imm_081_001 <- "Anesthesia"
.FlowRepository.env$.dict$impc_imm_082_001 <- "Cell digestion"
.FlowRepository.env$.dict$impc_imm_083_001 <- "Cell digestion agent"
.FlowRepository.env$.dict$impc_imm_084_001 <- "Cell digestion agent manufacturer"
.FlowRepository.env$.dict$impc_imm_085_001 <- "Cell digestion agent catalog number"
.FlowRepository.env$.dict$impc_imm_086_001 <- "Cell counting"
.FlowRepository.env$.dict$impc_imm_087_001 <- "Cell counting equipment manufacturer"
.FlowRepository.env$.dict$impc_imm_088_001 <- "Cell counting equipment model"
.FlowRepository.env$.dict$impc_imm_089_001 <- "Cell counting equipment name"
.FlowRepository.env$.dict$impc_imm_090_001 <- "Cell lysis buffer manufacturer"
.FlowRepository.env$.dict$impc_imm_091_001 <- "Cell lysis buffer catalog number"
.FlowRepository.env$.dict$impc_imm_092_001 <- "Date and time of sacrifice"
.FlowRepository.env$.dict$impc_imm_093_001 <- "Date and time of sample preparation"
.FlowRepository.env$.dict$impc_imm_094_001 <- "Sample storage temperature until analysis (in Celsius)"
.FlowRepository.env$.dict$impc_imm_095_001 <- "FCS repository reference (URL/ID)"
.FlowRepository.env$.dict$impc_imm_096_001 <- "Balanced salt solution type"
.FlowRepository.env$.dict$impc_imm_097_001 <- "Balanced salt solution manufacturer"
.FlowRepository.env$.dict$impc_imm_098_001 <- "Balanced salt solution catalog number"
.FlowRepository.env$.dict$impc_imm_099_001 <- "RPMI manufacturer"
.FlowRepository.env$.dict$impc_imm_100_001 <- "RPMI catalog number"
.FlowRepository.env$.dict$impc_imm_101_001 <- "DNAse I manufacturer"
.FlowRepository.env$.dict$impc_imm_102_001 <- "DNAse I catalog number"
.FlowRepository.env$.dict$impc_imm_103_001 <- "Dead cell exclusion dye"
.FlowRepository.env$.dict$impc_imm_104_001 <- "Dead cell exclusion dye manufacturer"
.FlowRepository.env$.dict$impc_imm_105_001 <- "Dead cell exclusion dye catalog number"
.FlowRepository.env$.dict$impc_imm_106_001 <- "Cell digestion temperature"
.FlowRepository.env$.dict$impc_imm_109_001 <- "Automated analysis: Yes or No"
.FlowRepository.env$.dict$impc_imm_110_001 <- "Collection buffer manufacturer"
.FlowRepository.env$.dict$impc_imm_111_001 <- "Collection buffer catalog number number"
.FlowRepository.env$.dict$impc_imm_112_001 <- "FACS buffer manufacturer"
.FlowRepository.env$.dict$impc_imm_113_001 <- "FACS buffer catalog number"
.FlowRepository.env$.dict$impc_imm_114_001 <- "Enzyme buffer manufacturer"
.FlowRepository.env$.dict$impc_imm_115_001 <- "Enzyme buffer catalog number"

## Exported
getFlowRepositoryURL <- function() {
    .FlowRepository.env$.FlowRepository.URL
}

## Exported
setFlowRepositoryURL <- function(url) {
    if(substring(url, nchar(url)) != "/") url <- paste0(url, "/")
    .FlowRepository.env$.FlowRepository.URL <- url
}

impcCodeDescription <- function(code) {
    .FlowRepository.env$.dict[[tolower(code)]]
}


## NOT Exported
getFlowRepositoryClientID <- function() {
    .FlowRepository.env$.FlowRepository.ClientID
}

.FlowRepository.env$.FlowRepository.User.Email <- ''
.FlowRepository.env$.FlowRepository.User.Password <- ''

## Exported
setFlowRepositoryCredentials <- function(filename=NULL, email=NULL, 
    password=NULL) 
{
    if(!is.null(filename)) {
        con=file(filename, open="r")
        line=readLines(con, 2) 
        if (length(line) >= 2) {
            email <- line[1]
            password <- line[2]
        }
        close(con)
    } else {
        if (is.null(email) && interactive()) 
            email <- readline("Please enter your email:    ")
        if (is.null(password) && interactive()) 
            password <- readline("Please enter your password: ")
    }

    if (!is.null(email) && !is.null(password) && nchar(password) > 0 && 
        nchar(email) > 0) 
    {
        .FlowRepository.env$.FlowRepository.User.Email <- email
        .FlowRepository.env$.FlowRepository.User.Password <- password
    } else {
        stop(paste("Please provide either a file with email and password,",
            "or your email and password directly as arguments."))
    }
}

# Exported
forgetFlowRepositoryCredentials <- function() {
    .FlowRepository.env$.FlowRepository.User.Email <- ''
    .FlowRepository.env$.FlowRepository.User.Password <- ''
}

# NOT Exported
getFlowRepositoryCredentials <- function() {
    c(.FlowRepository.env$.FlowRepository.User.Email, 
        .FlowRepository.env$.FlowRepository.User.Password)
}

# NOT Exported
haveFlowRepositoryCredentials <- function() {
    !is.null(.FlowRepository.env$.FlowRepository.User.Email) && 
        (nchar(.FlowRepository.env$.FlowRepository.User.Email) > 0) && 
        !is.null(.FlowRepository.env$.FlowRepository.User.Password) && 
        (nchar(.FlowRepository.env$.FlowRepository.User.Password) > 0)
}
