Runner <- R6::R6Class(
  "Runner",
  class=TRUE, 
  cloneable=FALSE,
  inherit = Scaffold,
  public=list(
    analysis=NULL,
    model=NULL,
    estimate=function() {
      
      formula  <- jmvcore::composeFormula(self$analysis$options$dep,self$analysis$options$covs)
      self$model    <- stats::lm(formula,data=self$analysis$data)
    #  Sys.sleep(3) uncomment this to articially slow down the estimation
      
    },
    init_main_coefficients=function() {
      
      covs      <- self$analysis$options$covs
      terms     <- c("(Intercept)",covs)
      tab       <- lapply(terms, function(x) list(var=x))
      return(tab)
    },
    
    run_main_coefficients=function() {
      
      .summary         <-   summary(self$model)
      coeffs           <-  .summary$coefficients
      coeffs           <-   as.data.frame(coeffs)
      names(coeffs)    <-   c("coef","se","t","p")
      coeffs$var       <-  rownames(coeffs) 
      return(coeffs)
      
    },

    init_main_anova=function() {
      
      covs      <- self$analysis$options$covs
      terms     <- c("(Intercept)",covs,"Residuals")
      tab       <- lapply(terms, function(x) list(var=x))
      return(tab)
    },
    
    run_main_anova=function() {

            
      .anova          <-  as.data.frame(car::Anova(self$model,type=3))
      names(.anova)  <-  c("nothing","df1","test","p")
      .anova$df2      <-  self$model$df.residual
      .anova$var      <-  rownames(.anova)
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
      if (self$options$show_ci) {
                      x<-rnorm(10)
                      y<-rnorm(10)
                      dat<-effectsize::epsilon_squared(self$model,ci = .95, alternative="two.sided")
                      eps_df$es_ci_lower<-dat$CI_low
                      eps_df$es_ci_upper<-dat$CI_high
                      dat<-effectsize::eta_squared(self$model,ci = .95, alternative="two.sided")
                      eta_df$es_ci_lower<-dat$CI_low
                      eta_df$es_ci_upper<-dat$CI_high
                      dat<-effectsize::omega_squared(self$model,ci = .95, alternative="two.sided")
                      omega_df$es_ci_lower<-dat$CI_low
                      omega_df$es_ci_upper<-dat$CI_high
                      
      }
      tab       <-  as.data.frame(rbind(eps_df,eta_df,omega_df))
      tab       <-  tab[order(tab$var),]
      
      return(tab)
    },

    init_means=function() {
      
      covs      <- self$analysis$options$covs
      tab       <- lapply(covs, function(x) list(var=x))
      return(tab)
    },
    
    run_means=function() {
        
        tabs  <-  lapply(self$analysis$options$covs, function (x) {
            m  <- mean(self$analysis$data[[x]],na.rm=TRUE)
            s  <- sd(self$analysis$data[[x]],na.rm=TRUE)
            data.frame(var=x,mean=m,sd=s)
        })
        return(tabs)
    },
    run_sig_means=function() {
        jinfo("RUNNER: run_sig_mean run")
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
        attr(tabs,"keys")<-whichcovs
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
      
    }
    
    
    


    
  ) # end of public
) # end of class
    