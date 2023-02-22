myRegressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "myRegressionClass",
    inherit = myRegressionBase,
    private = list(
        .runner=NULL,
        # this is a list that contains all the SmartTables
        .tables=list(),
        .time=NULL,
        .init = function() {
            ginfo("MODULE:  #### init phase start ####")
            private$.time<-Sys.time()
            
            if (!is.something(self$options$dep) | !is.something(self$options$covs))
                return()
            # define the table
            private$.runner<-Runner$new(self)
            
            # set up the coefficients SmartTable
            atable<-SmartTable$new(self$results$main$coefficients,private$.runner)
            # put the SmartTable in the list
            private$.tables[[length(private$.tables)+1]]<-atable
            
            # set up the means SmartArray
            obj<-SmartArray$new(self$results$means,private$.runner)
            # put the SmartArray in the list
            private$.tables[[length(private$.tables)+1]]<-obj
            
            # set up the means SmartArray
            obj<-SmartArray$new(self$results$sig$means,private$.runner)
            # put the SmartArray in the list
            private$.tables[[length(private$.tables)+1]]<-obj
            
            # set up the anova SmartTable
            atable<-SmartTable$new(self$results$main$anova,private$.runner)
            atable$hideOn <- list(df1=Inf)
            # put the SmartTable in the list
            private$.tables[[length(private$.tables)+1]]<-atable
            
            # set up the effects SmartTable
            atable<-SmartTable$new(self$results$additional$effects,private$.runner)
            atable$spaceBy <- "var"
            atable$superTitle <- list(es_ci_lower="95% confidence interval",es_ci_upper="95% confidence interval")
            # put the SmartTable in the list
            private$.tables[[length(private$.tables)+1]]<-atable


            # set up the means SmartTable
            obj<-SmartTable$new(self$results$correlations,private$.runner)
            obj$expandOnInit  <- TRUE
            obj$expandFrom    <- 2
            private$.tables[[length(private$.tables)+1]]<-obj
            
            # init all tables
            lapply(private$.tables,function(x) x$initTable())
            ginfo("MODULE:  #### init phase end ####")
            
        },
        .run = function() {
            
            if (!is.something(self$options$dep) | !is.something(self$options$covs))
                return()
            ginfo("MODULE:  #### run phase start ####")
            
            # estimate the model             
            private$.runner$estimate()
            # execute all SmartTable run functions             
            lapply(private$.tables,function(x) x$runTable())
            
            ginfo("MODULE:  #### run phase end ####")
            now<-Sys.time()
            ginfo("TIME:",now-private$.time," secs")
            
        }
    )
)

