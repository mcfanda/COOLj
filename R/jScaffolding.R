Scaffold <- R6::R6Class("Scaffold",
                          cloneable=FALSE,
                          class=FALSE,
                          public=list(
                            options=NULL,
                            dispatcher=NULL,
                            initialize=function(options,dispatcher) {
                              
                              self$options<-options
                              self$dispatcher<-dispatcher
                              
                            },
                            option=function(val,spec=NULL) {
                              
                              res<-utils::hasName(self$options,val)
                              if (res) {
                                if (is.logical(self$options[[val]]))
                                  res<-self$options[[val]]
                                else
                                  res<-is.something(self$options[[val]])
                                
                                
                                if (!is.null(spec))
                                  res<-any(spec %in% self$options[[val]])
                              }
                              res      
                              
                            },
                            optionValue=function(val) {
                              
                              test<-utils::hasName(self$options,val)
                              if (test) 
                                return(self$options[[val]])
                              else
                                return(NULL)
                            }
                          ), ## end of public
                          active=list(
                            
                            storage=function(obj) {

                              if (missing(obj))
                                 return(private$.storage)
                              
                              private$.storage<-obj
                              
                            }
                          ), #end of active
                          private=list(
                            .storage=NULL
                          ) #end of private
) ## end of class
