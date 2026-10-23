### Unit tests for externalFunctions functions

library(splitTypeR)
library(testthat)

data("expNormalCountsDemo")
data("signaturesDemo")

#############################################################################
### Tests runSubtypingBimodal() results
#############################################################################

test_that("runSubtypingBimodal() must return error when geneLists is a list with 1 entry", {
    
    error_message <- "The \'geneLists\' object must be a list with at least 2 entries."
    
    gList <- list()
    gList[["test"]] <- c("ABC1", "ABC2")
    
    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrix=matrix(data=rep(3, 6), nrow = 2), 
        permRatio=0.8, permNbr=10, upscaleNbr=4), error_message)
})

test_that("runSubtypingBimodal() must return error when geneLists is a character vector", {
    
    error_message <- "The \'geneLists\' object must be a list with at least 2 entries."
    
    gList <- "test001"
    
    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=matrix(data=rep(3, 6), nrow = 2), 
        permRatio=0.8, permNbr=10, upscaleNbr=4), error_message)
})

test_that("runSubtypingBimodal() must return error when geneLists is a character vector", {
    
    error_message <- "The \'expectedCountsMatrixLog\' object must be a matrix."
    
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- data.frame(P1=c(2, 3, 4), P2=c(3, 4, 5))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.8, permNbr=10, 
        upscaleNbr=4), error_message)
})

test_that("runSubtypingBimodal() must return error when geneLists is a character vector", {
    
    error_message <- paste0("None of the genes in the signatures are present", 
        " in the row names of the \'expectedCountsMatrixLog\' matrix.")
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(2, 3, 4, 3, 4, 5), byrow = FALSE, nrow=3)
    rownames(exCounts) <- c("TT1", "TT2", "TT3")
    colnames(exCounts) <- c("P1", "P2")

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, 
        permRatio=0.8, permNbr=10), error_message)
})

test_that("runSubtypingBimodal() must return error when geneLists is a character vector", {
    
    error_message <- paste0("For at least one gene signature, none of the ", 
        "genes are present in the row names of the", 
        " \'expectedCountsMatrixLog\' matrix.")
    
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(2, 3, 4, 3, 4, 5), byrow = FALSE, nrow=3)
    rownames(exCounts) <- c("ABC2", "TT2", "TT3")
    colnames(exCounts) <- c("P1", "P2")

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.8, permNbr=10, 
        upscaleNbr=3), error_message)
})


test_that("runSubtypingBimodal() must return error when geneLists is a character vector", {
    
    error_message <- paste0("For at least one gene signature, none of the ", 
        "genes are present in the row names of ", 
        "the \'expectedCountsMatrixLog\' matrix.")
    
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(2, 3, 4, 3, 4, 5), byrow = FALSE, nrow=3)
    rownames(exCounts) <- c("ABC2", "TT2", "TT3")
    colnames(exCounts) <- c("P1", "P2")

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.8, permNbr=10), 
        error_message)
})

test_that("runSubtypingBimodal() must return error when genes not present for one signatures", {
    
    error_message <- paste0("For at least one gene signature, none of the ", 
        "genes are present in the row names of ", 
        "the \'expectedCountsMatrixLog\' matrix.")

    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(rep(2:17, 3), byrow=TRUE, nrow=4)
    rownames(exCounts) <- c("ABC11", "KRAS", "ABC22", "FYN")
    colnames(exCounts) <- c(paste0("P", 1:12))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=-0.8, permNbr=10, 
        upscaleNbr=5), error_message)
})


test_that("runSubtypingBimodal() must return error when 1 gene present for one signatures", {
    
    error_message <- paste0("For at least one gene signature, only one gene ", 
        "is present in the row names of the 'expectedCountsMatrixLog' ", 
        "matrix.")

    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(rep(2:17, 3), byrow=TRUE, nrow=4)
    rownames(exCounts) <- c("ABC1", "KRAS", "ABC22", "FYN")
    colnames(exCounts) <- c(paste0("P", 1:12))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, 
        permRatio=-0.8, permNbr=10, upscaleNbr=3), error_message)
})

test_that("runSubtypingBimodal() must return error when permRatio is a numeric vector", {
    
    error_message <- "The \'permRatio\' must be a numeric between 0 and 1."

    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(rep(2:7, 8), byrow = FALSE, nrow=4)
    rownames(exCounts) <- c("ABC1", "KRAS", "FYN", "ABC2")
    colnames(exCounts) <- c(paste0("P", 1:12))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, 
        permRatio=c(0.3, 0.4), permNbr=10, upscaleNbr=3), error_message)
})

test_that("runSubtypingBimodal() must return error when permNbr is a character vector", {
    
    error_message <- paste0("The \'permNbr\' must be an integer higher", 
        " than or equal to 5.")
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(2, 3, 4, 3, 4, 5, 12, 11), byrow = FALSE, nrow=4)
    rownames(exCounts) <- c("KRAS", "ABC1", "ABC2", "FYN")
    colnames(exCounts) <- c("P1", "P2")

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, 
        permRatio=0.8, permNbr="toto", upscaleNbr=3), error_message)
})

test_that("runSubtypingBimodal() must return error when permNbr is 4", {
    
    error_message <- paste0("The \'permNbr\' must be an integer higher", 
        " than or equal to 5.")
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(2, 3, 4, 3, 4, 5, 12, 11), byrow = FALSE, nrow=4)
    rownames(exCounts) <- c("KRAS", "ABC1", "ABC2", "FYN")
    colnames(exCounts) <- c("P1", "P2")

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.8, permNbr=4, 
        upscaleNbr=5), error_message)
})

test_that("runSubtypingBimodal() must return error when permRatio is a character vector", {
    
    error_message <- "The \'permRatio\' must be a numeric between 0 and 1."
    
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(rep(2:13, 4), byrow = FALSE, nrow=4)
    rownames(exCounts) <- c("KRAS", "ABC2", "FYN", "ABC1")
    colnames(exCounts) <- c(paste0("P", 1:12))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio="TEST", permNbr=10, 
        upscaleNbr=10), error_message)
})

test_that("runSubtypingBimodal() must return error when permRatio is too close to one", {
    
    error_message <- paste0("The \'permRatio\' is too close to one. All", 
        " samples are selected for the permutation step rather than a subset.")
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(rep(2:7, 10)), byrow = TRUE, nrow=6)
    rownames(exCounts) <- c("KRAS", "ABC2", "FYN", "ABC1", "TP53", "MUC12")
    colnames(exCounts) <- c(paste0("P", 1:10))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.99, permNbr=10), 
        error_message)
})

test_that("runSubtypingBimodal() must return error when permNbr is too high for the number of samples", {
    
    error_message <- paste0("The \'permNbr\' is too high for the number of", 
        " samples. The number of unique combinations for selecting 8 out of ", 
        "10 samples is 45.")
    
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(rep(2:7, 10)), byrow = TRUE, nrow=6)
    rownames(exCounts) <- c("KRAS", "ABC2", "FYN", "ABC1", "TP53", "MUC12")
    colnames(exCounts) <- c(paste0("P", 1:10))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.8, permNbr=200), 
        error_message)
})

test_that("runSubtypingBimodal() must return a warning when not enough samples", {
    
    message <- paste0("! A minimum of 10 samples is recommended to run ", 
        "a GSVA analysis.")
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(rep(2:7, 5)), byrow = TRUE, nrow=6)
    rownames(exCounts) <- c("KRAS", "ABC2", "FYN", "ABC1", "TP53", "MUC12")
    colnames(exCounts) <- c(paste0("P", 1:5))

    set.seed(1211)

    expect_warning(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.8, permNbr=5, 
        upscaleNbr=10), message)
})

test_that("runSubtypingBimodal() must return an error when upscaleNbr is string", {
    
    message <- paste0("The \'upscaleNbr\' must be an integer equal to or ", 
                "higher than 2.")
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(rep(2:7, 12)), byrow = TRUE, nrow=6)
    rownames(exCounts) <- c("KRAS", "ABC2", "FYN", "ABC1", "TP53", "MUC12")
    colnames(exCounts) <- c(paste0("P", 1:12))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.8, permNbr=5, 
        upscaleNbr="info"), message)
})

test_that("runSubtypingBimodal() must return an error when upscaleNbr is 1", {
    
    message <- paste0("The \'upscaleNbr\' must be an integer equal to or ", 
                "higher than 2.")
    gList <- list()
    gList[["test1"]] <- c("ABC1", "ABC2")
    gList[["test2"]] <- c("KRAS", "FYN")
    
    exCounts <- matrix(c(rep(2:7, 12)), byrow = TRUE, nrow=6)
    rownames(exCounts) <- c("KRAS", "ABC2", "FYN", "ABC1", "TP53", "MUC12")
    colnames(exCounts) <- c(paste0("P", 1:12))

    expect_error(runSubtypingBimodal(geneLists=gList, 
        expectedCountsMatrixLog=exCounts, permRatio=0.8, permNbr=5, 
        upscaleNbr=1), regexp=message)
})

test_that("runSubtypingBimodal() must return expected results", {
    
    ## Basal and classical gene list signatures
    gList <- list()
    gList[["classical"]] <- 
        signaturesDemo$`Classical`
    gList[["basal"]] <- 
        signaturesDemo$`Basal-like`
    
    ## Fix seed
    set.seed(12221)
    
    res <- runSubtypingBimodal(geneLists=gList, 
            expectedCountsMatrixLog=log2(expNormalCountsDemo + 1), permNbr=6, 
            permRatio=0.75, upscaleNbr=10)
    
    expect_true(inherits(res, "SplitTypeRdata"))
    expect_equal(rownames(gsvaResults(res)), c("classical", "basal"))
    expect_equal(colnames(gsvaResults(res)), paste0("Patient_", 1:30))
    expect_equal(unname(gsvaResults(res)[1, 2:3]), c(0.3662256, -0.7589744), 
                    tolerance=1e-5)
    expect_equal(unname(gsvaResults(res)[1, 12:14]), c(-0.1877486, 0.3554295, 
                    0.2706283), tolerance=1e-5)
    expect_equal(unname(gsvaResults(res)[2, 24:27]), c(-0.4008772, 0.9533679, 
                    0.6351190, -0.8144757), tolerance=1e-5)
    expect_equal(unname(gsvaResults(res)[2, 5:7]), c(0.8258160, -0.6158120, 
                    0.1218935), tolerance=1e-5)
    
    expect_equal(names(permutations(res)), c("classical", "basal"))
    expect_true(is.matrix(permutations(res)$basal))
    expect_true(is.matrix(permutations(res)$classical))
    expect_equal(permutations(res)$basal[4, ], c(-0.2704338, -0.5520513, 
            NA, -0.1547619, NA, NA), tolerance=1e-5)
    expect_equal(permutations(res)$basal[23, ], c(NA, -0.6482759, -0.5258528, 
            -0.4140927, -0.3792350, -0.6996835), tolerance=1e-5)
    expect_equal(permutations(res)$classical[14, ], c(0.2266473, NA, NA, 
            0.1748530, 0.2382375, NA), tolerance=1e-5)
    expect_equal(permutations(res)$classical[25, ], c(NA, -0.8651168, 
            -0.8868652, NA, -0.8846154, -0.8859022), tolerance=1e-5)
    ## TODO more validations
    expect_equal(names(threshold(res)), c("classical", "basal"))
    for (i in c("classical", "basal")) {
        posMin <- which.min(model(res)[[i]]$mu)
        expect_equal(threshold(res)[[i]], qnorm(p=0.95, 
            mean=model(res)[[i]]$mu[posMin], sd=model(res)[[i]]$sigma[posMin]))
        ## Samples above the threshold are assigned to the signature
        expect_equal(classification(res)[[i]]$classification == i, 
            classification(res)[[i]]$GSVA_score > threshold(res)[[i]])
    }
    
    expect_equal(names(classification(res)), c("classical", "basal"))
    expect_equal(colnames(classification(res)$basal), c("GSVA_score", 
                                                        "classification"))
    expect_equal(colnames(classification(res)$classical), c("GSVA_score", 
                                                       "classification"))
    expect_equal(rownames(classification(res)$basal), paste0("Patient_", 1:30))
    expect_equal(rownames(classification(res)$classical), 
                 paste0("Patient_", 1:30))
    
})

#############################################################################
### Tests getGeneSignaturesNames() results
#############################################################################

test_that("getGeneSignaturesNames() must return the expected signature names", {
    
    expResults <- c("Tiriac_PDAC_PDO_Basal_like", 
        "Tiriac_PDAC_PDO_Classical", "Moffitt_Basal_like", "Moffitt_Classical")

    results <- getGeneSignaturesNames()

    expect_equal(results, expResults)
})

#############################################################################
### Tests getGeneSignatures() results
#############################################################################

test_that("getGeneSignatures() must return the expected signatures", {
    
    expNames <- c("Tiriac_PDAC_PDO_Basal_like",
        "Tiriac_PDAC_PDO_Classical", "Moffitt_Basal_like", "Moffitt_Classical")
    results <- getGeneSignatures()

    expect_equal(names(results), expNames)
    expect_equal(length(results[[expNames[2]]]), 62L)
    expect_equal(results[[expNames[2]]][1:5], 
            c("MUC13", "GPR35", "USH1C", "BAIAP2L2", "GAL3ST1"))
    expect_equal(results[[expNames[2]]][46:49], 
            c("GATA6", "GPX2", "CA2", "ST6GALNAC1"))
    expect_equal(length(results[[expNames[1]]]), 26L)
    expect_equal(results[[expNames[1]]][1:5], 
            c("ANO1", "ADORA2B", "S100A2", "CSF2", "ANXA1"))
    expect_equal(results[[expNames[1]]][21:24], 
            c("SPRR1B",  "BCAR3", "TTC9", "PTGES"))
})

test_that("getGeneSignatures() must return error when nameList is a number", {
    
    error_message <- paste0("The \'nameList\' parameter must be a vector of ", 
        "characters representing the selected signature names or \'NULL\'.")

    expect_error(getGeneSignatures(nameList=33), regexp=error_message)
})

test_that("getGeneSignatures() must return error when not all names in nameList are present", {
    
    error_message <- paste0("At least one signature is not available.")

    expect_error(getGeneSignatures(
        nameList=c("Tiriac_PDAC_PDO_Classical", "Chateau")), 
        regexp=error_message)
})

test_that("getGeneSignatures() must return expected results when nameList is one entry", {
    
    results <- getGeneSignatures(
                nameList=c("Tiriac_PDAC_PDO_Classical"))
    
    expect_true(is.list(results))
    expect_equal(length(results), 1L)
    expect_equal(results[[1]][1:5], 
            c("MUC13", "GPR35", "USH1C", "BAIAP2L2", "GAL3ST1"))
    expect_equal(results[[1]][46:49],c("GATA6", "GPX2", "CA2", "ST6GALNAC1"))
    expect_equal(results[[1]][55:58], c("HKDC1", "IYD", "DOK4", "CYP2C18"))
})