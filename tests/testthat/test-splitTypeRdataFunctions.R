### Unit tests for splitTypeRdata class functions

library(splitTypeR)

#############################################################################
### Tests splitTypeRdata class
#############################################################################

context("splitTypeRdata class results")


test_that("create a splitTypeRdata class with all default parameters should return an object", {
    
    ## New splitTypeRdata with all default values
    paramTest <- new("SplitTypeRdata")
    
    ## Test signatures
    expect_true(is.null(paramTest@signatures))
    
    ## Test gsvaResults
    expect_true(is.null(paramTest@gsvaResults))
    
    ## Test permutations
    expect_true(is.null(paramTest@permutations))
    
    ## Test standardDeviation
    expect_true(is.null(paramTest@standardDeviation))
    
    ## Test upscaling
    expect_true(is.null(paramTest@upscaling))
    
    ## Test model
    expect_true(is.null(paramTest@model))
    
    ## Test threshold
    expect_true(is.null(paramTest@threshold))
    
    ## Test classification
    expect_true(is.null(paramTest@classification))
})

test_that("create a splitTypeRdata() function with all default parameters should return an object", {
    
    ## New splitTypeRdata with all default values
    paramTest <- SplitTypeRdata()
    
    ## Test signatures
    expect_true(is.null(paramTest@signatures))
    
    ## Test gsvaResults
    expect_true(is.null(paramTest@gsvaResults))
    
    ## Test permutations
    expect_true(is.null(paramTest@permutations))
    
    ## Test standardDeviation
    expect_true(is.null(paramTest@standardDeviation))
    
    ## Test upscaling
    expect_true(is.null(paramTest@upscaling))
    
    ## Test model
    expect_true(is.null(paramTest@model))
    
    ## Test threshold
    expect_true(is.null(paramTest@threshold))
    
    ## Test classification
    expect_true(is.null(paramTest@classification))
})


test_that("create a SplitTypeRdata class with integer for signatures parameter should generate an error", {
    
    expect_error(new("SplitTypeRdata", signatures=33), 
                 "got class \"numeric\", should be or extend class \"ListOrNULL\"")
})

test_that("create a SplitTypeRdata class with integer for gsvaResults parameter should generate an error", {
    
    expect_error(new("SplitTypeRdata", gsvaResults=33), 
                 "got class \"numeric\", should be or extend class \"MatrixOrNULL\"")
})

test_that("create a SplitTypeRdata class with integer for permutations parameter should generate an error", {
    
    expect_error(new("SplitTypeRdata", permutations=33), 
                 "got class \"numeric\", should be or extend class \"ListOrNULL\"")
})

test_that("create a SplitTypeRdata class with integer for sd parameter should generate an error", {
    
    expect_error(new("SplitTypeRdata", standardDeviation=33), 
                 "got class \"numeric\", should be or extend class \"ListOrNULL\"")
})

test_that("create a SplitTypeRdata class with integer for upscaling parameter should generate an error", {
    
    expect_error(new("SplitTypeRdata", upscaling=33), 
                 "got class \"numeric\", should be or extend class \"ListOrNULL\"")
})

test_that("create a SplitTypeRdata class with integer for model parameter should generate an error", {
    
    expect_error(new("SplitTypeRdata", model=33), 
                 "got class \"numeric\", should be or extend class \"ListOrNULL\"")
})

test_that("create a SplitTypeRdata class with integer for threshold parameter should generate an error", {
    
    expect_error(new("SplitTypeRdata", threshold=33), 
                 "got class \"numeric\", should be or extend class \"ListOrNULL\"")
})

test_that("create a SplitTypeRdata class with integer for classification parameter should generate an error", {
    
    expect_error(new("SplitTypeRdata", classification=33), 
                 "got class \"numeric\", should be or extend class \"ListOrNULL\"")
})

test_that("create a SplitTypeRdata class with signatures setter and getter should return an object", {
    
    exp <- list("SignatureA"=c("Gene1", "Gene2", "Gene3"), 
                        "SignatureB"=c("Gene10", "Gene12"))
    
    paramTest <- new("SplitTypeRdata")
    signatures(paramTest) <- exp
    
    expect_equal(signatures(paramTest), exp)
    expect_error(signatures(paramTest) <- 33L)
    expect_equal(signatures(paramTest), exp)
})

test_that("create a SplitTypeRdata class with gsvaResults setter and getter should return an object", {
    
    sign <- list("SignatureA"=c("Gene1", "Gene2", "Gene3"), 
                "SignatureB"=c("Gene10", "Gene12"))
    
    expResults <- matrix(data=c(0.23772165, -0.71262458, 0.42328775, 
                                  -0.65149268, 0.18324268, 0.007158662, 
                                  0.263418803, 0.169815921, -0.708384551, 
                                  0.440403512), nrow=2, ncol=5, byrow=FALSE)
    colnames(expResults) <- paste0("Patient_", 1:5)
    rownames(expResults) <- c("SignatureA", "SignatureB")
    
    paramTest <- new("SplitTypeRdata", signatures=sign, 
                        gsvaResults=NULL)
    
    gsvaResults(paramTest) <- expResults
    
    expect_equal(gsvaResults(paramTest), expResults)
    expect_error(gsvaResults(paramTest) <- 33L)
    expect_equal(gsvaResults(paramTest), expResults)
})

test_that("create a SplitTypeRdata class with permutations setter and getter should return an object", {
    
    sign <- list("SignatureA"=c("Gene1", "Gene2", "Gene3"), 
                 "SignatureB"=c("Gene10", "Gene12"))
    
    expResults1 <- matrix(data=c(0.23772165, -0.71262458, 0.42328775, 
                                -0.65149268, 0.18324268, 0.007158662, 
                                0.263418803, 0.169815921, -0.708384551, 
                                0.440403512), nrow=2, ncol=5, byrow=FALSE)
    rownames(expResults1) <- paste0("Patient_", 1:2)
    expResults2 <- matrix(data=c(0.29772165, 0.21262458, 0.42218775, 
                                 0.65133268, 0.18324268, -0.017158662, 
                                 0.263418803, 0.269805921, 0.708384551, 
                                 0.240403512), nrow=2, ncol=5, byrow=FALSE)
    rownames(expResults2) <- paste0("Patient_", 1:2)
    
    expResults <- list()
    expResults[[names(sign)[1]]] <- expResults1
    expResults[[names(sign)[2]]] <- expResults2
    
    paramTest <- new("SplitTypeRdata", signatures=sign, 
                     permutations=NULL)
    
    permutations(paramTest) <- expResults
    
    expect_equal(permutations(paramTest), expResults)
    expect_error(permutations(paramTest) <- 33L)
    expect_equal(permutations(paramTest), expResults)
})

test_that("create a SplitTypeRdata class with model setter and getter should return an object", {
    
    sign <- list("SignatureA"=c("Gene1", "Gene2", "Gene3"), 
                 "SignatureB"=c("Gene10", "Gene12"))
    set.seed(121)
    modelExp <- normalmixEM(c(sample(1:25, size=12, replace=T), 
                    sample(23:45, size=12, replace=T)), k=2, verb=FALSE)
    
    expResults <- list()
    expResults[[names(sign)[1]]] <- modelExp
    expResults[[names(sign)[2]]] <- modelExp
    
    paramTest <- new("SplitTypeRdata", signatures=sign, 
                     model=NULL)
    
    model(paramTest) <- expResults
    
    expect_equal(model(paramTest), expResults)
    expect_error(model(paramTest) <- 33L)
    expect_equal(model(paramTest), expResults)
})


test_that("create a SplitTypeRdata class with threshold setter and getter should return an object", {
    
    sign <- list("SignatureA"=c("Gene1", "Gene2", "Gene3"), 
                 "SignatureB"=c("Gene10", "Gene12"))
    
    expResults <- list("SignatureA"=0.1234, "SignatureB"=-0.2345)
    
    paramTest <- new("SplitTypeRdata", signatures=sign, threshold=NULL)
    
    threshold(paramTest) <- expResults
    
    expect_equal(threshold(paramTest), expResults)
    expect_error(threshold(paramTest) <- 33L)
    expect_equal(threshold(paramTest), expResults)
})

test_that("create a SplitTypeRdata object with threshold parameter in SplitTypeRdata() should return an object", {
    
    expResults <- list("SignatureA"=0.1234, "SignatureB"=-0.2345)
    
    paramTest <- SplitTypeRdata(threshold=expResults)
    
    expect_equal(threshold(paramTest), expResults)
})

test_that("create a SplitTypeRdata class with standardDeviation setter and getter should return an object", {
    
    sign <- list("SignatureA"=c("Gene1", "Gene2", "Gene3"), 
                 "SignatureB"=c("Gene10", "Gene12"))
    
    expResults <- list()
    expResults[[names(sign)[1]]] <- c(0.01748988, 0.08992500, 0.11781061, 
                                            0.0747440)
    expResults[[names(sign)[2]]] <- c(0.0205218364, 0.0847276766, 
                                            0.1478349058, 0.1009854961)
    
    paramTest <- new("SplitTypeRdata", signatures=sign, 
                     model=NULL)
    
    standardDeviation(paramTest) <- expResults
    
    expect_equal(standardDeviation(paramTest), expResults)
    expect_error(standardDeviation(paramTest) <- 33L)
    expect_equal(standardDeviation(paramTest), expResults)
})

test_that("create a SplitTypeRdata class with upscaling setter and getter should return an object", {
    
    sign <- list("SignatureA"=c("Gene1", "Gene2", "Gene3"), 
                 "SignatureB"=c("Gene10", "Gene12"))
    
    expResults <- list()
    expResults[[names(sign)[1]]] <- matrix(data=c(0.0174988, 0.0892500, 0.1178101, 
                0.0744401, 0.0654692, 0.2546492), nrow=2, ncol=3, byrow=FALSE)
    expResults[[names(sign)[2]]] <- matrix(data=c(0.3749488, 0.4894500, 0.2118101, 
                0.4444401, 0.4154692, 0.8543392), nrow=2, ncol=3, byrow=FALSE)
    
    paramTest <- new("SplitTypeRdata", signatures=sign, 
                     upscaling=NULL)
    
    upscaling(paramTest) <- expResults
    
    expect_equal(upscaling(paramTest), expResults)
    expect_error(upscaling(paramTest) <- 33L)
    expect_equal(upscaling(paramTest), expResults)
})
#############################################################################
### Tests show() method
#############################################################################

test_that("show() must display a summary of an empty SplitTypeRdata object", {
    
    paramTest <- SplitTypeRdata()
    
    expect_output(show(paramTest), "A SplitTypeRdata object")
    expect_output(show(paramTest), "Signatures: none")
    expect_output(show(paramTest), "Completed steps: none")
    expect_output(res <- withVisible(show(paramTest)))
    expect_false(res$visible)
    expect_null(res$value)
})

test_that("show() must display the signatures and the classification summary", {
    
    sign <- list("SignatureA"=c("Gene1", "Gene2", "Gene3"), 
                 "SignatureB"=c("Gene10", "Gene12"))
    
    classRes <- list()
    classRes[["SignatureA"]] <- data.frame(GSVA_score=c(0.45, -0.32, 0.12),
        classification=c("SignatureA", "unclassified", "unclassified"),
        row.names=c("Sample_1", "Sample_2", "Sample_3"))
    classRes[["SignatureB"]] <- NA
    
    gsvaRes <- matrix(data=c(0.45, -0.32, 0.12, -0.51, 0.38, 0.29), nrow=2, 
        byrow=TRUE, dimnames=list(c("SignatureA", "SignatureB"), 
        c("Sample_1", "Sample_2", "Sample_3")))
    
    paramTest <- SplitTypeRdata(signatures=sign, gsvaResults=gsvaRes, 
        classification=classRes)
    
    expect_output(show(paramTest), 
        "Signatures \\(2\\): SignatureA \\(3 genes\\), SignatureB \\(2 genes\\)")
    expect_output(show(paramTest), "Samples: 3")
    expect_output(show(paramTest), 
        "Completed steps: gsvaResults, classification")
    expect_output(show(paramTest), "SignatureA: 1 assigned, 2 unclassified")
    expect_output(show(paramTest), "SignatureB: mixture model not available")
})

test_that("show() must use the singular for a signature with one gene", {
    
    paramTest <- SplitTypeRdata(signatures=list("SignatureA"=c("Gene1"), 
        "SignatureB"=c("Gene10", "Gene12")))
    
    expect_output(show(paramTest), 
        "SignatureA \\(1 gene\\), SignatureB \\(2 genes\\)")
})
