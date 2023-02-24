myRegressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "myRegressionClass",
    inherit = myRegressionBase,
    private = list(
        .runner=NULL,
        .regtab=NULL,
        .init = function() {
            
            private$.runner<-Runner$new(self)
            private$.regtab<-SmartTable$new(self$results$coefficients,private$.runner)
            private$.regtab$initTable()
            
        },
        .run = function() {
            
            data     <- self$data
            dep      <- self$options$dep
            covs     <- self$options$covs
            
            if (!is.something(dep) | !is.something(covs))
                return()
            
            private$.regtab$runTable()
            
            
        }
    )
)