### Unit tests for externalFunctions functions

library(splitTypeR)
library(testthat)

data("expNormalCountsDemo")
data("signaturesDemo")


#############################################################################
### Tests validatePlotDensityWithUpscalingSamplesBimodal() results
#############################################################################

test_that("validatePlotDensityWithUpscalingSamplesBimodal() must return TRUE when all parameters valid", {
    
    set.seed(121)
    
    results <- runSubtypingBimodal(geneLists=signaturesDemo, 
                    expectedCountsMatrix=expNormalCountsDemo, 
                    permRatio=0.75, permNbr=10, upscaleNbr=5)
    
    expect_true(splitTypeR:::validatePlotDensityWithUpscalingSamplesBimodal(x=results,
        signature="Basal-like", breaks=12L, colorSignature="blue", 
        colorAlternative="gray", colorBorderSamples="orange3", 
        colorFillingSamples="orange"))
})


#############################################################################
### Tests validatePlotPermutationSamplesNormal() results
#############################################################################

test_that("validatePlotPermutationSamplesNormal() must return TRUE when all parameters valid", {
    
    set.seed(121)
    
    results <- runSubtypingBimodal(geneLists=signaturesDemo, 
                    expectedCountsMatrix=expNormalCountsDemo, 
                    permRatio=0.75, permNbr=10, upscaleNbr=5)
    
    expect_true(splitTypeR:::validatePlotPermutationSamplesNormal(x=results, 
        signature="Basal-like", samples=c("Patient_9", "Patient_25"), 
        pointColor="blue"))
})
    