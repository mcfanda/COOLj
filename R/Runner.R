Runner <- R6::R6Class(
  "Runner",
  class=TRUE, 
  cloneable=FALSE,
  inherit = Scaffold,
  public=list(
    analysis=NULL,
    model=NULL,
    dispatcher=NULL,
    
    initialize=function(obj) {
      self$analysis<-obj
      self$dispatcher<-Dispatch$new(obj$results)
    },
    
    estimate=function() {
      
      formula  <- jmvcore::composeFormula(self$analysis$options$dep,self$analysis$options$covs)
      self$model    <- stats::lm(formula,data=self$analysis$data)
    },

    run_main_coefficients=function() {
      
      .summary         <-   summary(self$model)
      coeffs           <-  .summary$coefficients
      coeffs           <-   as.data.frame(coeffs)
      names(coeffs)    <-   c("coef","se","t","p")
      coeffs$var       <-  rownames(coeffs) 
      return(coeffs)
    },

    run_main_anova=function() {
      
      .anova          <-  as.data.frame(car::Anova(self$model,type=3))
       names(.anova)  <-  c("nothing","df1","test","p")
      .anova$df2      <-  self$model$df.residual
      .anova$var      <-  rownames(.anova) 
      return(.anova)
       
      },
    

    run_additional_effects=function() {

      eps       <-  effectsize::epsilon_squared(self$model)
      eps_df    <-  data.frame(var=eps$Parameter,index="Epsilon^2",value=eps$Epsilon2)
      eta       <-  effectsize::eta_squared(self$model)
      eta_df    <-  data.frame(var=eta$Parameter,index="Eta^2",value=eta$Eta2)
      omega     <-  effectsize::omega_squared(self$model)
      omega_df  <-  data.frame(var=eta$Parameter,index="Omega^2",value=omega$Omega2)
      
      tab       <-  as.data.frame(rbind(eps_df,eta_df,omega_df))
      tab       <-  tab[order(tab$var),]
      
      return(tab)
    }


    
  ) # end of public
) # end of class
    