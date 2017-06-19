read_ext0 <- function(ext.file){
  ## Raw function to read in and format an ext.file.
  s <- scan(ext.file,what="character",sep="\n",quiet = TRUE)
  tab.rows <- grep("TABLE",s)
  cut.points <- c(tab.rows,length(s)+1)

  headings <- s[tab.rows]
  headings <- gsub("^TABLE NO.\\s+[0-9]+:\\s","",headings)
  headings <- gsub(": Goal.*","",headings)

  dlist <- lapply(seq_along(tab.rows),function(i){
    if((cut.points[i] + 1)>(cut.points[i + 1] - 1)) return(data.frame())
    d <- s[(cut.points[i]+1):(cut.points[i+1]-1)]
    tmp <- file()
    writeLines(d,tmp)
    d <- utils::read.table(tmp,header=TRUE)
    d$EST.NO <- i
    d$EST.NAME <- headings[i]
    names(d)[names(d)%in%"SAEMOBJ"] <- "OBJ"
    d$OBJ <- as.numeric(as.character(d$OBJ))
    d$TYPE <- NA
    d$TYPE[d$ITERATION>=0] <- "ITER"
    d$TYPE[d$ITERATION>-1000000000 & d$ITERATION<0] <- "BURN"
    d$TYPE[d$ITERATION==-1000000000] <- "FINAL"
    d$TYPE[d$ITERATION==-1000000001] <- "SE"
    d$EVALUATION <- grepl("Evaluation",d$EST.NAME)
    close(tmp)
    d
  })
  do.call(rbind,dlist)
}

read_ext <- function(r,trans=FALSE){
  d <- read_ext0(r$output$psn.ext)
  if(!trans) return(d)

  p_info <- param_info(r$output$psn.mod)
  ## combine d with p
  for(i in seq_len(nrow(p_info))){
    pi <- p_info[i,]
    names(d)[names(d) %in% pi$Parameter] <- pi$Name
    if(pi$trans %in% c("LOG","LOGODDS")){
      d[,pi$Name][d$ITERATION>-1000000000] <- exp(d[,pi$Name][d$ITERATION>-1000000000])
    } else if (pi$trans %in% "LOGIT") {
      d[,pi$Name][d$ITERATION>-1000000000] <- stats::plogis(d[,pi$Name][d$ITERATION>-1000000000])
    }
  }
  d
}
