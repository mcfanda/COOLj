Runner <- R6::R6Class(
    "Runner",
    class=TRUE, 
    cloneable=FALSE,
    inherit = Scaffold,
    public=list(
        analysis=NULL,
        initialize=function(obj) {
            self$analysis<-obj
        },
        run_coefficients=function() {
            
            formula  <- jmvcore::composeFormula(self$analysis$options$dep,self$analysis$options$covs)
            model    <- stats::lm(formula,data=self$analysis$data)
            .summary  <- summary(model)
            coeffs   <- .summary$coefficients
            coeffs   <-  as.data.frame(coeffs)
            names(coeffs)  <- c("coef","se","t","p")
            coeffs$var <-  rownames(coeffs) 
            coeffs
        }
    ) # end of public
) # end of class
