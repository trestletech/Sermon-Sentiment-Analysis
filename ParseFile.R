
readFile <- function(filename){
  library(stringr);
  con <- file(filename);
  pars <- readLines(con);
  pars <- pars[-grep("^(\\s+)?$", pars)];
  
  sentences <- list();  
  for (i in 1:length(pars)){
    sentIndex <- 1;
    sents <- strsplit(pars[i], ".", fixed=TRUE)[[1]];
    sentences[[i]] <- array();
    #sents <- paste(sents, " ", sep="");
    for (s in 1:length(sents)){
      #get sentiment analysis for this sentence
      
      strlen <- str_length(paste(sents[s], ""));
      if (strlen > 6){
        sentences[[i]][sentIndex] <- sents[s];
        sentIndex <- sentIndex + 1;
      }
    }
  }
  
  close(con);
      
  return(sentences);    
}



addSermon <- function(title, date){    
  library(stringr)
  load(file="sentiments.Rda");
  
  sermonID <- title;
  sermDate <- date;
  
  
  sermonSpeaker <- substr(sermonID, 0, str_locate(sermonID, "-")[1,1]-1)
  sermonSpeaker <- str_trim(sermonSpeaker);
  
  sermonTitle <- substr(sermonID, str_locate(sermonID, "-")[1,1]+1, str_length(sermonID));
  sermonTitle <- str_trim(sermonTitle);
          
  serm <- readFile(paste(sermonID,".txt",sep=""));
  sent <- serm;
  for (p in 1:length(serm)){
    print (paste(p,"/", length(serm)));
    par <- serm[[p]];
    for (s in 1:length(par)){
      serm[[p]][s] <- str_replace_all(serm[[p]][s], "\"", "\\\"");
      #print(paste(p,s,":",serm[[p]][s]));    
      
      thisSent <- -Inf;
      try (thisSent <- getSentiment(serm[[p]][s]));      
      
      sent[[p]][s] <- thisSent;
      
      #sometimes the GET just fails randomly. So crash gracefully.      
      if (thisSent == -Inf){
        warning(paste("Couldn't get a sentiment at paragraph", p,", sentence", s));
        sent[[p]][s] <- 0;
      }
      
      
    }
  }    
     
    
    
    sentIndex <- 1;
    if (exists("sentiments")){
      sentIndex <- length(sentiments)+1;
    }  else{
      sentiments <- list();
      sentIndex <- 1;
    }
    sentiments[[sentIndex]] <- list();
    sentiments[[sentIndex]]$title <- sermonTitle;
    sentiments[[sentIndex]]$author <- sermonSpeaker;
  sentiments[[sentIndex]]$date <- sermDate;  
  sentiments[[sentIndex]]$sent <- sent;
    sentiments[[sentIndex]]$serm <- serm;
    
    
  save(sentiments, file="sentiments.Rda");
}


         
myCloud <- function (w, col, yspace = 1.3, xspace = 0.1, minh = 0, add=FALSE, xmin = 0, xmax = 0.98, ystart=0.95, ...) 
{
    if (missing(col)) 
        col <- "#000000"
    omar <- par("mar")
    par(mar = c(0, 0, 0, 0))
    if (!add){
      plot(xmin:xmax, 0:ystart, type = "n", axes = FALSE)
    }
    x = xmin
    y = ystart
    xch = minh
    cm = 3/max(w)
    . <- lapply(1:length(w), function(i) {
        cex = w[i] * cm
        ctw = strwidth(names(w[i]), cex = cex)
        cth = strheight(names(w[i]), cex = cex)
        if (cth > xch) 
            xch <<- cth
        if (x + ctw > xmax) {
            x <<- xmin
            y <<- y - (yspace * xch)
            xch <<- minh
        }
        text(x, y, names(w[i]), cex = cex, adj = c(0, 0.5), col = col[i])
        x <<- x + ctw + xspace
    })
    par(mar=omar)
    invisible(TRUE)
}
 

getWordTable <- function (words, maxWords = 0){
  words <- words[-grep("[\\)\\(,;:\\.\\'\\\"]", words)]
  words <- words[-grep("^\\d+$", words)]
  words <- words[grep("^\\w+$", words)]
  words <- words[!words %in% stopwords()]
  #words <- words[!words %in% customStopwords]
  words <- words[grep("\\w", words)]
  wt <- table(words)
  wt <- wt[wt > 1]
  
  if (maxWords > 0){
    countThreshold <- 2;    
    while (length(wt) > maxWords){      
      #we'd trim too many if we cut here.
      if (sum(wt > countThreshold) < maxWords){
        
        # introduce some noise so that we can randomly remove some of the words with the feweset counts
        wt <- wt + runif(length(wt), min=-.1, max=.1)
        
        #trim the smallest one until we're down to the desired size.
        while (length(wt) > maxWords){
          #cut the smallest
          wt <- wt[wt > min(wt)];
        }

        #round back to the original value
        wt <- round(wt);
      } else{ # we can cut safely
        wt <- wt[wt > countThreshold]    
        wt <- wt - 1;        
      }
      countThreshold <- countThreshold + 1;
    }
  }
  
  return(wt);
}