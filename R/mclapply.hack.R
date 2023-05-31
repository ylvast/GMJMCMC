##
## mclapply.hack.R
##
## Nathan VanHoudnos
## nathanvan AT northwestern FULL STOP edu
## July 14, 2014
##
## A script to implement a hackish version of
## parallel:mclapply() on Windows machines.
## On Linux or Mac, the script has no effect
## beyond loading the parallel library.

## Define the hack
mclapply.hack <- function(...) {
    ## Create a cluster
    size.of.list <- length(list(...)[[1]])
    cl <- makeCluster( min(size.of.list, detectCores()) )
    ## Find out the names of the loaded packages
    loaded.package.names <- c(
        ## Base packages
        sessionInfo()$basePkgs,
        ## Additional packages
        names( sessionInfo()$otherPkgs ))
    tryCatch( {
       ## Copy over all of the objects within scope to
       ## all clusters.
       this.env <- environment()
       while( identical( this.env, globalenv() ) == FALSE ) {
           clusterExport(cl,
                         ls(all.names=TRUE, env=this.env),
                         envir=this.env)
           this.env <- parent.env(environment())
       }
       clusterExport(cl,
                     ls(all.names=TRUE, env=globalenv()),
                     envir=globalenv())

       ## Load the libraries on all the clusters
       ## N.B. length(cl) returns the number of clusters
       parLapply(cl, seq_along(cl), function(xx){
           lapply(loaded.package.names, function(yy) {
               require(yy , character.only=TRUE)})
       })

       ## Run the lapply in parallel
       return( parLapply( cl, ...) )
    }, finally = {
       ## Stop the cluster
       stopCluster(cl)
    })
}

## If the OS is Windows, set mclapply to the
## the hackish version. Otherwise, leave the
## definition alone.
mclapply.new <- switch( Sys.info()[['sysname']],
   Windows = { mclapply.hack },
   Linux   = { mclapply },
   Darwin  = { mclapply })
## end mclapply.hack.R