### Unit tests for internalValidationFunctions functions

library(splitTypeR)
library(testthat)

#############################################################################
### Tests validateRunSubtyping() results
#############################################################################

test_that("validateRunSubtyping() must return TRUE when all parameters are valid", {

    testSignature <- list()
    testSignature[["signatureA"]] <- c("SMAD4", "KRAS", "CDC")
    testSignature[["signatureB"]] <- c("ABC", "FYN", "DHC")

    testMatrix <- matrix(data=rep(3:6, 12), nrow = 4)
    rownames(testMatrix) <- c("ABC", "KRAS", "SMAD4", "FYN")
    colnames(testMatrix) <- paste0("Sample_", 1:12)

    expect_true(splitTypeR:::validateRunSubtyping(geneLists=testSignature, 
        expectedCountsMatrix=testMatrix, permRatio=0.8, permNbr=10, 
        upscaleNbr=11))
})

#############################################################################
### Tests validateRunSubtypingSubSection() results
#############################################################################

test_that("validateRunSubtypingSubSection() must return TRUE when all parameters are valid", {

    expect_true(splitTypeR:::validateRunSubtypingSubSection(nbAll=20,
        permRatio=0.8, permNbr=10, upscaleNbr=30))
})

test_that("validateRunSubtypingSubSection() must return TRUE when permNbr and upscaleNbr are integers at the minimum value", {

    expect_true(splitTypeR:::validateRunSubtypingSubSection(nbAll=20,
        permRatio=0.8, permNbr=5L, upscaleNbr=2L))
    expect_true(splitTypeR:::validateRunSubtypingSubSection(nbAll=20,
        permRatio=0.8, permNbr=5, upscaleNbr=2))
})

test_that("validateRunSubtypingSubSection() must return an error when permNbr is lower than 5", {

    error_message <- "The \'permNbr\' must be an integer higher than or equal to 5."

    for (value in list(4, 4L, 2L, 0L, -3L)) {
        expect_error(splitTypeR:::validateRunSubtypingSubSection(nbAll=20,
            permRatio=0.8, permNbr=value, upscaleNbr=10), error_message,
            fixed=TRUE)
    }
})

test_that("validateRunSubtypingSubSection() must return an error when permNbr is not a whole number", {

    error_message <- "The \'permNbr\' must be an integer higher than or equal to 5."

    expect_error(splitTypeR:::validateRunSubtypingSubSection(nbAll=20,
        permRatio=0.8, permNbr=5.5, upscaleNbr=10), error_message,
        fixed=TRUE)
})

test_that("validateRunSubtypingSubSection() must return an error when upscaleNbr is lower than 2", {

    error_message <- paste0("The \'upscaleNbr\' must be an integer equal ",
        "to or higher than 2.")

    for (value in list(1, 1L, 0L, -3L)) {
        expect_error(splitTypeR:::validateRunSubtypingSubSection(nbAll=20,
            permRatio=0.8, permNbr=10, upscaleNbr=value), error_message,
            fixed=TRUE)
    }
})

test_that("validateRunSubtypingSubSection() must return an error when upscaleNbr is not a whole number", {

    error_message <- paste0("The \'upscaleNbr\' must be an integer equal ",
        "to or higher than 2.")

    expect_error(splitTypeR:::validateRunSubtypingSubSection(nbAll=20,
        permRatio=0.8, permNbr=10, upscaleNbr=2.5), error_message,
        fixed=TRUE)
})

