#' @title Validate parameters for the plotPermutationSamplesNormal() function
#'
#' @description This function validates the parameters for the 
#' plotPermutationSamplesNormal function. When a parameter is incorrect, a 
#' message is sent to the user before quitting the program.
#'
#' @param x a \code{list} of class "splitTypeResults", the output object 
#' from \code{runSubtyping}function, to be graphed.
#'
#' @param signature a \code{character} string representing the signature 
#' that will be used to create the graph. The signature must 
#' be present in the object.
#' 
#' @param samples a \code{list} of \code{character} string representing 
#' the names of the samples that will be used to create the graph. The samples 
#' must be present in the object.
#' 
#' @param pointColor a \code{character} string representing the color of the 
#' dots representing the enrichment scores for the sampled values, obtained 
#' during the upscaling step, in the graph.
#' 
#' @return \code{TRUE} when all parameters are valid
#' 
#' @examples
#' 
#' ## Loading signatures
#' data("signaturesDemo")
#' 
#' ## Load demo normalized expected counts for 30 patients
#' data("expNormalCountsDemo")
#' 
#' ## Fix seed for reproducibility
#' set.seed(1221)
#' 
#' ## Run classification on the 30 patients using 10 permutations on 75% of 
#' ## the dataset, and 10 points per patient for the up-scaling step
#' results <- runSubtypingBimodal(geneLists=signaturesDemo, 
#'     expectedCountsMatrix=log2(expNormalCountsDemo + 1), 
#'     permRatio=0.75, permNbr=10, upscaleNbr=5)
#'     
#' ## Validation should return TRUE when all parameters are valid
#' splitTypeR:::validatePlotPermutationSamplesNormal(x=results, 
#'     signature="Basal-like", samples=c("Patient_9", "Patient_25"))
#' 
#' 
#' @author Astrid Deschênes
#' @encoding UTF-8
#' @keywords internal
validatePlotPermutationSamplesNormal <- function(x, signature, samples, 
        pointColor) {
    
    if (!inherits(x, "SplitTypeRdata")) {
        stop("The x object must be of class \'SplitTypeRdata\'.")
    }
    
    if (!signature %in% names(permutations(x))) {
        stop("The signature \'", signature, "\' must be present in the ", 
                "\'SplitTypeRdata\' object.")
    }
    
    if (!is.vector(samples) || !is.character(samples) || length(samples) == 0 || 
        !all(samples %in% rownames(permutations(x)[[signature]]))) {
        stop("All the samples must all be present in the ", 
                "\'SplitTypeRdata\' object.")
    }
    
    return(TRUE)
}


#' @title Validate parameters for the 
#' plotDensityWithUpscalingSamplesBimodal() function
#'
#' @description This function validates the parameters for the 
#' plotDensityWithUpscalingSamplesBimodal function. When a parameter is 
#' incorrect, a message is sent to the user before quitting the program.
#' 
#' @param x a "SplitTypeRdata" object, the output object 
#' from \code{runSubtyping}function, to be graphed.
#'
#' @param signature a \code{character} string representing the signature 
#' that will be used to create the graph. The signature must 
#' be present in the object.
#' 
#' @param breaks a single \code{integer} value giving the number of cells 
#' for the histogram,
#' 
#' @param colorSignature a \code{character} string representing the color of 
#' the signature distribution curve. 
#' 
#' @param colorAlternative a \code{character} string representing the color of 
#' the alternative distribution curve. 
#' 
#' @param colorBorderSamples a \code{character} string representing the color 
#' of the histogram border for the sample distribution. 
#' 
#' @param colorFillingSamples a \code{character} string representing the 
#' color of the histogram filling for the sample distribution. 
#'
#' @return \code{TRUE} when all parameters are valid
#' 
#' @examples
#'
#' ## Loading signatures
#' data("signaturesDemo")
#'
#' ## Load demo normalized expected counts for 30 patients
#' data("expNormalCountsDemo")
#' 
#' ## Fix seed for reproducibility
#' set.seed(1221)
#' 
#' ## Run classification on the 30 patients using 10 permutations on 75% of 
#' ## the dataset, and 10 points per patient for the up-scaling step
#' results <- runSubtypingBimodal(geneLists=signaturesDemo, 
#'     expectedCountsMatrix=expNormalCountsDemo, 
#'     permRatio=0.75, permNbr=10, upscaleNbr=5)
#'     
#' ## Validation should return TRUE when all parameters are valid
#' splitTypeR:::validatePlotDensityWithUpscalingSamplesBimodal(x=results, 
#'     signature="Classical", colorSignature="blue", colorAlternative="red", 
#'     colorBorderSamples="gray20", colorFillingSamples="black")
#'
#' 
#' @author Astrid Deschênes
#' @encoding UTF-8
#' @keywords internal
validatePlotDensityWithUpscalingSamplesBimodal <- function(x, signature, 
    breaks, colorSignature, colorAlternative, colorBorderSamples, 
    colorFillingSamples) {
    
    if (!inherits(x, "SplitTypeRdata")) {
        stop("The x object must be of class \'SplitTypeRdata\'.")
    }
    
    if (is.null(model(x))) {
        stop("The x object must contain a distribution model (model slot).")
    }
    
    if (!signature %in% names(model(x)) || 
        !signature %in% names(upscaling(x))) {
        stop("The signature \'", signature, "\' must be present in the ", 
                "\'SplitTypeRdata\' object.")
    }
    
    return(TRUE)
}

    