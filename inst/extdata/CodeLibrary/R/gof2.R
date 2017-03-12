## Description: Function template: ggplot GOF Instructions: source() this file, then
## run.  Depends on: output.data.R Key words: function, template

gof2 <- function(run.no, logdv = FALSE, DV = "DV", TIME = "TIME", primary.key = "ORD", 
    model.dir = ".") {
    
    d <- output.data(run.no, primary.key = primary.key)
    if (logdv) 
        d$PRED <- exp(d$PRED)
    
    pl <- list()
    p <- ggplot(d, aes_string(x = "PRED", y = DV)) + theme_bw() + geom_abline(slope = 1) + 
        geom_point()
    pl[[length(pl) + 1]] <- p
    pl[[length(pl) + 1]] <- p + scale_x_log10() + scale_y_log10()
    
    p <- ggplot(d, aes_string(x = "IPRED", y = DV)) + theme_bw() + geom_abline(slope = 1) + 
        geom_point()
    pl[[length(pl) + 1]] <- p
    pl[[length(pl) + 1]] <- p + scale_x_log10() + scale_y_log10()
    
    maxCWRES <- max(abs(d$CWRES), na.rm = TRUE)
    p <- ggplot(d, aes_string(x = "PRED", y = "CWRES")) + theme_bw() + geom_hline(yintercept = 0) + 
        geom_point() + scale_y_continuous(limits = c(-1.05 * maxCWRES, 1.05 * maxCWRES))
    pl[[length(pl) + 1]] <- p
    
    p <- ggplot(d, aes_string(x = TIME, y = "CWRES")) + theme_bw() + geom_hline(yintercept = 0) + 
        geom_point() + scale_y_continuous(limits = c(-1.05 * maxCWRES, 1.05 * maxCWRES))
    pl[[length(pl) + 1]] <- p
    
    pdf(file.path("Results", paste("gof2.run.", run.no, ".pdf", sep = "")))
    print(pl)
    dev.off()
    
}
