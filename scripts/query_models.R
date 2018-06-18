
library(ggplot2)

source_dir <- 'BMA_development/models'
files <- list.files(source_dir)

smpars <- c('beta[1]','beta[2]',"taub")


# convert the file names to data
temp <- strsplit(gsub(files, pa=".rData", repl=""),"_")
metadata <- data.frame(raw = files, 
                       group=sapply(temp, function(x) x[[1]]),
                       knots=sapply(temp, function(x) x[[2]]),
                       scfac=sapply(temp, function(x) x[[3]]))

group <- 'specialist'

output <- sapply(files, 
#output <- sapply(files[grepl(group, files)], 
            simplify = FALSE,
            USE.NAMES = TRUE,
            function(modelfile){
              load(file.path(source_dir, modelfile)) # -> model
              x <- model$summary
              logI <- x[grepl("logI", dimnames(x)[[1]]),]
              logI2 <- x[grepl("logI2", dimnames(x)[[1]]),]
              smoothpars <- x[dimnames(x)[[1]] %in% smpars,]
              out <- rbind(logI, logI2, smoothpars)[,c(1,2,3,7,8)]
              out <- as.data.frame(out)
              out$param <- rownames(out)
              return(out)
            })

output <- melt(output, id=1:6)
output <- merge(output, metadata, by.x="L1", by.y='raw')

output$precision <- 1/(output$sd^2)
output$width <- output[,"97.5%"] - output[,"2.5%"]

output$knots <- as.numeric(as.character(output$knots))
output$scfac <- as.numeric(as.character(output$scfac))



####################### smooth params

smp <- subset(output, param %in% smpars)
(qp <- qplot(y=width, x=factor(knots, ordered=TRUE), 
      data=smp, col=factor(scfac), shape=group) + 
  facet_wrap(~param, scales='free') +
  xlab('number of knots') +
  ylab('width of 95% credible intervals')
)
ggsave(qp, file='BMA_development/plots/width_of_smoothing_params.png')

(qp <- qplot(y=Rhat, x=factor(knots, ordered=TRUE), 
      data=smp, col=factor(scfac), shape=group) + 
  facet_wrap(~param, scales='free') +
  xlab('number of knots')
)
ggsave(qp, file='BMA_development/plots/width_of_smoothing_params.png')

####################### compare I vs I2
