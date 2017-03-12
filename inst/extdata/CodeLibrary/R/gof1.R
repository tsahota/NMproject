## Description: Function template: Xpose GOF Instructions: source() this file, then
## run.  Run interactively: FALSE Key words: function, template E.g. for sdtab37, run
## with gof1(37)

gof1 <- function(run.no, model.dir = ".") {
    
    ## assumes existance of 'plots' directory in main working directory (plots.dir)
    
    ## assumes the existance of an sdtab $TABLE file e.g.  $TABLE ID TIME IPRED IWRES
    ## IRES CWRES NPDE FILE=sdtab[run.no] NOPRINT ONEHEADER FORMAT=tF13.4
    
    library(xpose4, lib.loc = "/home/kbtr119/R/x86_64-unknown-linux-gnu-library/3.1/")
    
    xpdb <- xpose.data(run.no, directory = paste0(model.dir, "/"))
    
    ### Make any changes to xpdb, e.g. to change the independent variable
    ### change.xvardef(xpdb,var='idv') <- 'TRLD'
    
    pdf(file.path("Results", paste("gof1.run.", run.no, ".xpose.basic.pdf", sep = "")))
    
    ## Delete/Modify the following as needed
    print(dv.vs.pred(xpdb, smooth = NULL, type = "p", main = NULL, logy = F, logx = F, 
        ylb = "Plasma concentration (units)", ))
    print(dv.vs.ipred(xpdb, smooth = NULL, type = "p", main = NULL, logy = F, logx = F, 
        ylb = "Plasma concentration (units)", ))
    print(cwres.vs.pred(xpdb, smooth = NULL, type = "p", main = NULL))
    print(cwres.vs.idv(xpdb, smooth = NULL, type = "p", main = NULL, xlb = "Time (units)"))
    print(ind.plots(xpdb, layout = c(2, 2), ylb = "Plasma concentration (units)", xlb = "Time (units)"))
    
    dev.off()
    
}
