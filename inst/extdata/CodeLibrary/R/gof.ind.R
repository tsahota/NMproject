## Description: Function template: Ind plots Depends on: output.data.R Run
## interactively: FALSE Key words: gof, template, function Example use:
## gof2(37,log=TRUE)

gof.ind.plot <- function(run.no, primary.key = "ORD", models.dir = ".", time.var = "TIME", 
    id.var = "ID", subset, label, log = FALSE) {
    
    ######################################## load packages and source functions here
    
    library(ggplot2)
    library(gridExtra)
    library(plyr)
    library(dplyr)
    
    source("Scripts/output.data.R")
    
    ######################################## main script here
    
    ## individual PK plots
    d <- output.data(run.no, primary.key = primary.key, models.dir = models.dir)
    d$TIME <- d[, time.var]
    d$ID <- d[, id.var]
    
    # if(!missing(subset)) { x <- substitute(subset) d <- with(d,eval(x) }
    
    d <- d[!(d$EVID == 2 & d$MDV == 1), ]
    
    if (log == TRUE) 
        d$PRED <- exp(d$PRED)
    
    pl <- dlply(d, c("ID"), function(d) {
        p <- ggplot(d, aes_string(x = "TIME", y = "DV")) + theme_bw()
        p <- p + geom_point()
        p <- p + geom_line(aes(y = IPRED), colour = "red")
        p <- p + geom_line(aes(y = PRED), colour = "blue")
        p <- p + scale_x_continuous()
        if (!log) 
            p <- p + scale_y_continuous()
        if (log) 
            p <- p + scale_y_log10()
        p
    })
    
    log.lbl <- ""
    if (log) 
        log.lbl <- "log"
    
    pdf(paste0("Results/explore.nm1.pkind1.", log.lbl, ".run.", run.no, ".", label, 
        ".pdf"), width = 12, height = 7)
    marrangeGrob(pl, ncol = 3, nrow = 2)
    dev.off()
    
    
}
