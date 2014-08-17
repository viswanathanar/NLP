Sys.setenv(language = 'en')
#setwd('C:\\Learning\\NLP')
setwd('S:\\Komma\\Task It\\')
source("S:\\Komma\\Task It\\AuxiliaryFunctions.r")
#install.packages("openNLP")
#install.packages('openNLPmodels.en',repos = 'http://datacube.wu.ac.at/',type="source")
#install.packages('SnowballC')
#install.packages('tm')
#install.packages('tau')
#install.packages('wordnet')
#install.packages("rjson")

library('openNLPmodels.en')
library("NLP")
library("openNLP")
library("SnowballC")
library('tm')
library(tau)
library("rjson")
#library('wordnet')

options( java.parameters = "-Xmx4g" ) ##to address Java heap space error
#options(java.parameters = "-Xms4096m -Xmx7000m", "-XX:-UseGCOverheadLimit")
##Path directory for wordnet instance

#pathData <- 'C:\\WordNet-3.0\\dict'
#setDict(pathData)
#initDict <- initDict(pathData)
#dict<-getDictInstance()

#Sys.setenv(WNHOME = "C:\\Program Files (x86)\\WordNet\\2.1\\")
#setDict <- setDict("C:\\Program Files (x86)\\WordNet\\2.1\\")
#initDict <- initDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")
#dict<-getDictInstance()

## Loading the location entity annotator
entity_annotator_Loc <- Maxent_Entity_Annotator(language = "en", 
                                                kind = "location", 
                                                probs = FALSE, 
                                                model = NULL)
## Loading the datetime entity annotator
entity_annotator_DateTime <- Maxent_Entity_Annotator(language = "en", 
                                                     kind = "date", 
                                                     probs = FALSE, 
                                                     model = NULL)

#Loading input file containing tasks\texts
filename <- 'Input Tasks'
text <- read.table(filename, sep = '\n')
unlink("Results", recursive = FALSE, force = FALSE)
for(i in 1:nrow(text))
{
  ## Getting Location entity
  Loc <- getEntity(text[i,], entity_annotator_Loc)
  if(Loc!=0){Loc <- paste("Location:",Loc)}
  
  Date <- getEntity(text[i,], entity_annotator_DateTime)
  if(Date!=0){Date <- paste("Datetime:",Date)}
  
  if(Loc==0 && Date==0)
  {
    msg <- "No entities are present"
  }else if(Loc!=0 && Date!=0)
  {
    msg <- c(Loc,Date)
  }else if(Loc!=0)
  {
    msg <- Loc
  }else if(Date!=0)
  {
    msg <- Date
  }
  result <- toJSON(msg)
  write(result, file = "Results",append = TRUE, sep = "\n")
}

# for(i in 1:nrow(text))
# {
  #	print(modifyPOS_tags(stemText(text[i,])))
  # POS <- modifyPOS_tags(stemText(text[i,]))
  # Semantic Similarity #1
  # Calling the function to get the meaning from wordnet
  # sent <- getMeaning(POS)  
  # sent <- gsub("\\(", "", sent)
  # sent <- gsub("\\)", "", sent)
  # print(sent)
# }