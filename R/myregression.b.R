myRegressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "myRegressionClass",
    inherit = myRegressionBase,
    private = list(
        .runner=NULL,
        # this is a list that contains all the SmartTables
        .tables=list(),
        .init = function() {
            
            if (!is.something(self$options$dep) | !is.something(self$options$covs))
                return()
            # define the runner
            private$.runner<-Runner$new(self)
            
            # set up the coefficients SmartTable
            atable<-SmartTable$new(self$results$coefficients,private$.runner)
            # put the SmartTable in the list
            private$.tables[[length(private$.tables)+1]]<-atable
            # set up the anova SmartTable
            atable<-SmartTable$new(self$results$anova,private$.runner)
            # put the SmartTable in the list
            private$.tables[[length(private$.tables)+1]]<-atable
            # set up the effects SmartTable
            atable<-SmartTable$new(self$results$effects,private$.runner)
            # put the SmartTable in the list
            private$.tables[[length(private$.tables)+1]]<-atable
            
            # init all tables
            lapply(private$.tables,function(x) x$initTable())
            
        },
        .run = function() {
            
            if (!is.something(self$options$dep) | !is.something(self$options$covs))
                return()
            # estimate the model             
            private$.runner$estimate()
            # execute all SmartTable run functions             
            lapply(private$.tables,function(x) x$runTable())
            
        }
    )
)
