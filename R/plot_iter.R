read_ext0 <- function(ext.file){
  ## Raw function to read in and format an ext.file.
  s <- scan(ext.file,what="character",sep="\n")
  tab.rows <- grep("TABLE",s)
  cut.points <- c(tab.rows,length(s)+1)

  headings <- s[tab.rows]
  headings <- gsub("^TABLE NO.\\s+[0-9]+:\\s","",headings)
  headings <- gsub(": Goal.*","",headings)

  dlist <- lapply(seq_along(tab.rows),function(i){
    d <- s[(cut.points[i]+1):(cut.points[i+1]-1)]
    tmp <- file()
    writeLines(d,tmp)
    d <- read.table(tmp,header=TRUE)
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

read_ext <- function(ext.file,p_info=NULL){
  d <- read_ext0(ext.file)
  if(is.null(p_info)) return(d)

  ## combine d with p
  for(i in seq_len(nrow(p_info))){
    pi <- p_info[i,]
    names(d)[names(d) %in% pi$Parameter] <- pi$Name
    if(pi$trans %in% c("LOG","LOGODDS")){
      d[,pi$Name][d$ITERATION>=0] <- exp(d[,pi$Name][d$ITERATION>=0])
    } else if (pi$trans %in% "LOGIT") {
      d[,pi$Name][d$ITERATION>=0] <- plogis(d[,pi$Name][d$ITERATION>=0])
    }
  }
  d
}

plot_iter0 <- function(ext.file,p_info=NULL,skip=0,yvar="OBJ"){

  d <- read_ext(ext.file = ext.file,p_info = p_info)
  print(paste("last.modified",file.info(ext.file)$mtime))

  d <- d[d$TYPE %in% c("ITER","BURN"), ]
  d$EST.NAME2 <- paste("$EST",d$EST.NO,":",d$EST.NAME,sep=" ")

  ####################################
  ## find "estimation" est.no
  ## will this work with FOCE evaluation?
  if(length(unique(d$EST.NO))>1){
    est.no <- max(unique(d$EST.NO[!grepl("Eval",d$EST.NAME)]))
  } else est.no <- max(unique(d$EST.NO))
  ####################################
  d <- d[d$EST.NO %in% est.no, ]

  par.names <- names(d)[2:(match("OBJ",names(d))-1)]
  ## remove fixed parameters
  for(i in par.names) if(length(unique(d[,i]))==1) d[,i] <- NULL

  ## TODO: would like to remove all duplicated IOV ETAs

  ## move up the NBURN to immediately before NITER
  d <- by(d,d$EST.NO,function(d){

    d <- d[d$ITERATION>=min(d$ITERATION)+skip,]  ## skip first few iterations on non-evaluation methods.

    if(!"BURN"%in%d$TYPE | length(which(d$TYPE%in%"BURN"))==1)return(d)
    max.burn <- sort(d$ITERATION[d$TYPE%in%"BURN"],decreasing=TRUE)[1]
    max.burn2 <- sort(d$ITERATION[d$TYPE%in%"BURN"],decreasing=TRUE)[2]
    burn.interval <- max.burn-max.burn2
    d$ITERATION[d$TYPE%in%"BURN"] <- d$ITERATION[d$TYPE%in%"BURN"] - max.burn - burn.interval
    d
  })
  d <- do.call(rbind,d)

  par.names <- c("OBJ",names(d)[names(d) %in% par.names])

  d <- tidyr::gather_(data = d,key = "variable",value = "value",
                      gather_cols = par.names)

  p <- ggplot2::ggplot(d,ggplot2::aes_string(x="ITERATION",y="value"))
  p <- p + ggplot2::geom_line(ggplot2::aes_string(colour="TYPE"))
  p <- p + ggplot2::geom_point(ggplot2::aes_string(colour="TYPE"))
  p <- p + ggplot2::facet_wrap(~variable,scale="free")
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::ggtitle(unique(d$EST.NAME))
  p <- p + ggplot2::theme_bw()
  p
}

#' Plot iterations vs parameters/OBJ
#'
#' @param x object of class nm.execute
#' @export

plot_iter <- function(nm_obj,trans=TRUE,...){

  if(!trans) return(plot_iter0(ext.file = nm_obj$psn.ext,...))

  ctl <- readLines(nm_obj$ctl)
  ctl <- ctl_nm2r(ctl)
  p <- theta_nm2r(ctl$THETA)

  plot_iter0(ext.file = nm_obj$psn.ext,p_info=p,...)

}

