#' Reads in a file and chops it into sentences which can be analyzed by sentiment analysis individually
#' @param filename the file to read
#' @return the sentences making up the file given
#' @author Jeff Allen \email{jeff.allen@@trestletechnology.net}
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


#' Add a sermon to sentiments.Rda
#' 
#' Reads in the given sermon, processes it, and adds the results to the list stored in Sentiments.Rda
#' 
#' @param title the name of the file in which the sermon is stored, also will be stored as the "title" of the sermon
#' @param date the date on which the sermon was given -- meta information which will be stored alongside this sermon
#' @return 
#' @author Jeff Allen \email{jeff.allen@@trestletechnology.net}
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


   
 
#' Create a table from the individual words given
#' 
#' Make a table out of the words given, computing the number of instance each is used.
#' 
#' @param words the words from which to construct the table
#' @param maxWords the maximum number for each word
#' @return a table representing the words given
#' @author Jeff Allen \email{jeff.allen@@trestletechnology.net}
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