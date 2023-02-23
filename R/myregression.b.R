


myRegressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "myRegressionClass",
    inherit = myRegressionBase,
    private = list(
        .regtab=NULL,
        
        .init = function() {
            
            private$.regtab<-SmartTable$new(self$results$coefficients)
            private$.regtab$initTable()
        },
        .run = function() {
            
            data     <- self$data
            dep      <- self$options$dep
            covs     <- self$options$covs
            
            if (!is.something(dep) | !is.something(covs))
                return()
            
            formula  <- jmvcore::composeFormula(dep,covs)
            model    <- stats::lm(formula,data=data)
            .summary  <- summary(model)
            coeffs   <- .summary$coefficients
            coeffs   <-  as.data.frame(coeffs)
            names(coeffs)  <- c("coef","se","t","p")
            coeffs$var <-  rownames(coeffs) 
            
            private$.regtab$runSource <- coeffs
            private$.regtab$runTable()
            
            
        }
    )
)