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
      self$dispatcher$warnings<-list(topic="main_coefficients",message="This comes from estimate")
    },

    init_main_coefficients=function() {
      
      covs<-self$analysis$options$covs
      tab<-data.frame(id=1:(length(covs)+1))
      tab$var<-c("(Intercept)",covs)
      return(tab)
    },
    
    run_main_coefficients=function() {
      
      .summary         <-   summary(self$model)
      coeffs           <-  .summary$coefficients
      coeffs           <-   as.data.frame(coeffs)
      names(coeffs)    <-   c("coef","se","t","p")
      warning("something fishy is going on")
      
      return(coeffs)
    },
    init_main_anova=function() {
      
      covs             <-self$analysis$options$covs
      tab              <-data.frame(id=1:(length(covs)+2))
      tab$var          <-c("(Intercept)",covs,"Residuals")
      self$dispatcher$warnings<-list(topic="main_coefficients",message="This may take a while",initOnly=TRUE)
      
      return(tab)
    },
    
    run_main_anova=function() {
      
      .anova          <-  as.data.frame(car::Anova(self$model,type=3))
       names(.anova)  <-  c("nothing","df1","test","p")
      .anova$df2      <-  self$model$df.residual
      self$dispatcher$warnings<-list(topic="main_coefficients",message="This comes from anova")
      

      return(.anova)
       
      },
    
    init_additional_effects=function() {

      covs      <- self$analysis$options$covs
      es        <- c('\u03b5','\u03b7','\u03c9') ## greek UTF 
      es        <- paste(es,'\u00B2',sep="")
      tab       <- as.data.frame(expand.grid(covs,es))
      names(tab)<- c("var","index")
      tab       <- tab[order(tab$var),]
      return(tab)
      
      },
    run_additional_effects=function() {

      eps       <-  effectsize::epsilon_squared(self$model)
      eps_df    <-  data.frame(var=eps$Parameter,value=eps$Epsilon2)
      eta       <-  effectsize::eta_squared(self$model)
      eta_df    <-  data.frame(var=eta$Parameter,value=eta$Eta2)
      omega     <-  effectsize::omega_squared(self$model)
      omega_df  <-  data.frame(var=eta$Parameter,value=omega$Omega2)
      
      tab       <-  as.data.frame(rbind(eps_df,eta_df,omega_df))
      tab       <-  tab[order(tab$var),]
      
      return(tab)
    },
    init_means=function() {
      
      covs<-self$analysis$options$covs
      lapply(covs,function(x) list(var=x))
      
    },
    run_means=function() {
      ## select the covs ##
      results      <-   as.data.frame(summary(self$model)$coefficients)
      results      <-   results[-1,]
      whichcovs    <-   rownames(results)
  
  ## make the tables
      
      
      tabs<-  lapply(whichcovs, function (x) {
                 m  <- mean(self$analysis$data[[x]],na.rm=TRUE)
                 s  <- sd(self$analysis$data[[x]],na.rm=TRUE)
                 data.frame(var=x,mean=m,sd=s)
  })
      
  attr(tabs,"keys")<- whichcovs
  warning("something happened in the run_means function")
  return(tabs)
},


    init_sig_means=function() {
  
        covs<-self$analysis$options$covs
        tabs<-lapply(covs,function(x) data.frame(var=x))
        attr(tabs,"keys")<- covs
        tabs
   },

    run_sig_means=function() {

      ## select the covs ##
      results      <-   as.data.frame(summary(self$model)$coefficients)
      results      <-   results[-1,]
      whichcovs    <-   rownames(results[results[,4]<.05,])

      ## make the tables
      tabs     <-  lapply(whichcovs, function (x) {
                   m  <- mean(self$analysis$data[[x]],na.rm=TRUE)
                   s  <- sd(self$analysis$data[[x]],na.rm=TRUE)
                   data.frame(var=x,mean=m,sd=s)
      })
      attr(tabs,"keys")<- whichcovs
      self$dispatcher$warnings<-list(topic="sig_means_1",message="In particular to the first")
      
      warning("something happens in the sig means")
      
      return(tabs)
    },
    init_correlations=function() {
      
      vars<-c(self$analysis$options$dep,self$analysis$options$covs)
      tab<-as.data.frame(matrix(".",nrow = length(vars),ncol=length(vars)))
      names(tab)  <- vars
      tab$var     <- vars
      return(tab)
      
    },
    run_correlations=function() {
      
      vars<-c(self$analysis$options$dep,self$analysis$options$covs)
      tab<-as.data.frame(cor(self$analysis$data[,vars]))
      return(tab)
      self$dispatcher$warnings<-list(topic="means",message="This come from correlations means for all subtables")
      
    }
    
    
    
    
  ) # end of public
) # end of class
    