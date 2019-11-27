#### Named output list is ttm.lst

##### PROVINCIAL DATA #####
setwd("./data-raw")
provs <- c('IN', 'NB', 'ON', 'QC')
ttm.lst <- list()
cup.lst <- list()
lst.nms <- vector()
for (p in provs) {
  files <- list.files(pattern=paste0(p,'_'))
  for (f in files) {
    r <- read.csv(f, header = T)
    g1 <- grep('.5', names(r), fixed = T)
    if (length(g1) > 0) {
      extra <- r[,g1]
      extra <- as.matrix(extra, nr = nrow(r))
      w.normal <- which(apply(extra, 1, function(x) {all(x == '' | is.na(x))}))
      r <- r[w.normal,]
    }
    deg <- as.numeric(strsplit(strsplit(f, '_')[[1]][3], 'deg')[[1]][1])
    w <- which(names(r) %in% c('L2', 'L3', 'L4', 'L5', 'L6', 'Pupa', 'Sex'))
    g <- grep('Days_', names(r))
    s <- sort(c(w, g))
    ns <- names(r)[s]
    g2 <- grep('.5', ns, fixed = T)
    if (length(g2) > 0) {s <- s[-g2]}
    new <- r[,s]
    a <- which(apply(new, 1, function(x) {all(x != '')}))
    new <- new[a,]
    cups <- r$Cup..[a]
    cup.lst <- append(cup.lst, list(cups))
    new.f <- lapply(new[,(1:ncol(new) - 1)], function(x) {
      as.Date(as.character(x), format = '%d-%m-%Y')
    })
    new.df <- data.frame(new.f, 'Sex' = new$Sex)
    val <- sprintf('%s.%s.F1', p, deg)
    assign(val, new.df)
    ttm.lst <- append(ttm.lst, list(new.df))
    lst.nms <- c(lst.nms, val)
  }
}
names(ttm.lst) <- lst.nms
ttm.prov <- ttm.lst

##### IPU DATA #####
format.dates <- function(data, cols) {
  cut <- data[,cols]
  for (i in 1:ncol(cut)) {
    cut[,i] <- as.Date(sapply(as.character(cut[,i]), function(x) {substr(x, 3, nchar(x))}),
                       format = '%d-%m-%y')
  }
  data[,cols] <- cut
  return(data)
}

format.dates2 <- function(data, cols) {
  cut <- data[,cols]
  for (i in 1:ncol(cut)) {
    cut[,i] <- as.Date(cut[,i], format = '%d-%m-%Y')
  }
  data[,cols] <- cut
  return(data)
}
gens <- c('F0', 'F1')
ttm.lst <- list()
lst.nms <- vector()
for (g in gens) {
  files <- list.files(pattern=paste0('IPU_',g))
  for (f in files) {
    r <- read.csv(f, header = T)
    g0 <- grep('Days_L', names(r), fixed = T)
    if (length(g0) > 0) {r <- r[,-g0]}
    g1 <- grep('skippers', names(r))
    if (length(g1) > 0) {
      extra <- r[,g1]
      extra <- as.matrix(extra, nr = nrow(r))
      w.normal <- which(apply(extra, 1, function(x) {all(x == '' | is.na(x))}))
      r <- r[w.normal,]
    }
    deg <- strsplit(strsplit(f, '_')[[1]][3], '.c')[[1]][1]
    deg <- as.numeric(deg)
    w <- which(names(r) %in% c('L2', 'L3', 'L4', 'L5', 'L6', 'Pupa', 'Sex'))
    g2 <- grep('Days_', names(r))
    s <- sort(c(w, g2))
    ns <- names(r)[s]
    new <- r[,ns]
    a <- which(apply(new, 1, function(x) {all(x != '')}))
    new <- new[a,]
    if (g == 'F0') {
      new.f <- format.dates(new, names(new)[-length(names(new))])
    }
    else if (g == 'F1') {
      if (deg == 20) {new.f <- format.dates(new, names(new)[-length(names(new))])}
      else {new.f <- format.dates2(new, names(new)[-length(names(new))])}
    }
    val <- sprintf('IPU.%s.%s',deg, g)
    ttm.lst <- append(ttm.lst, list(new.f))
    lst.nms <- c(lst.nms, val)
  }
}
names(ttm.lst) <- lst.nms
ttm.ipu <- ttm.lst

#####
DevTimeSBW <- append(ttm.prov, ttm.ipu)
#####
usethis::use_data(DevTimeSBW,overwrite = TRUE)
