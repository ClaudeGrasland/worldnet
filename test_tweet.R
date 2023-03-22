
library(dplyr)
library(quanteda)
library(tidytext)
library(data.table)


### Import
store <- "corpus/tweets/"
media <- "tweets_raw_ger_1000"
type = ".csv"
fic <- paste(store,media,type,sep="")

df<-read.csv(fic,
             sep=";",
             header=T,
             encoding = "UTF-8",
             stringsAsFactors = F)

### Clean

# eliminate hashtags
df$text<-gsub("#","",df$text)
df$text<-gsub('@',' ',df$text)

# standardize hyphens
df$text<-gsub('&#8211;','-',df$text)

# Remove quotation marks
df$text<-gsub('&#171;&#160;','',df$text)
df$text<-gsub('&#160;&#187;','',df$text)
df$text<-gsub('&#8220;','',df$text)
df$text<-gsub('&#8221;','',df$text)
df$text<-gsub('&#8216;','',df$text)
df$text<-gsub('&#8243;','',df$text)


#### Create Quanteda corpus

qd<-corpus(df,docid_field = "status_id",text_field = "text")
print(head(qd))

# Select docvar fields and rename media
qd$date <-as.Date(qd$created_at)
qd$source <-qd$name
docvars(qd)<-docvars(qd)[,c("source","date")]


# Add global meta
meta(qd,"meta_source")<-"Twitter"
meta(qd,"meta_author")<-"Elaborated by Laura Schuhn"
meta(qd,"project")<-"ANR-DFG Project IMAGEUN"

summary(qd,3)


#### Load dict
dict<-read.table("dict/Imageun_world_geo_dict_V1.csv", 
                 header=T, 
                 sep=";",
                 encoding = "UTF-8",
                 quote = '"')

dict<-dict[dict$lang=="de",]

#### Extract tags
extract_tags <- function(qd = qd,                      # the corpus of interest
                         lang = "fr",                  # the language to be used
                         dict = dict,                  # the dictionary of target 
                         code = "id" ,                  # variable used for coding
                         split  = c("'","’","-"),       # split list
                         tolow = FALSE  ,                # Tokenize text
                         comps = c("Afrique du sud")  # compounds
)
{ 
  
  
  
  # Tokenize  
  x<-as.character(qd)
  
  
  if(length(split) > 0) { reg<-paste(split, collapse = '|')
  x <- gsub(reg," ",x)}  
  if(tolow) { x <- tolower(x)} 
  toks<-tokens(x)
  
  # compounds
  if(length(split) > 0) { reg<-paste(split, collapse = '|')
  comps<- gsub(reg," ",comps)}  
  if(tolow)       {comps <- tolower(comps)}  
  toks<-tokens_compound(toks,pattern=phrase(comps))
  
  
  # Load dictionaries and create compounds
  
  ## Target dictionary
  dict<-dict[dict$lang==lang & is.na(dict$label)==F,]
  target<-dict[ntoken(dict$label)>1,]
  labels <-dict$label
  if(length(split) > 0) { reg<-paste(split, collapse = '|')
  labels<- gsub(reg," ",labels)}  
  if(tolow)       {labels <- tolower(labels)}  
  toks<-tokens_compound(toks,pattern=phrase(labels))
  
  # create quanteda dictionary
  keys <-gsub(" ","_",labels)
  qd_dict<-as.list(keys)
  names(qd_dict)<-dict[[code]]
  qd_dict<-dictionary(qd_dict,tolower = FALSE)
  
  # Identify geo tags (states or reg or org ...)
  toks_tags <- tokens_lookup(toks, qd_dict, case_insensitive = F)
  toks_tags <- lapply(toks_tags, unique)
  toks_tags<-as.tokens(toks_tags)
  list_tags<-function(x){res<-paste(x, collapse=' ')}
  docvars(qd)[["tags"]]<-as.character(lapply(toks_tags,FUN=list_tags))
  docvars(qd)[["nbtags"]]<-ntoken(toks_tags)
  
  
  
  # Export results
  return(qd)
}
       


qd<-corpus_subset(qd,duplicated(as.character(qd))==FALSE)

decomps <- c("Europa League")
qd <- extract_tags (qd = qd,
                    lang="de",
                    dict = dict,
                    code = "code",
                    comps = decomps,
                    split = c("'","’","-"),
                    tolow = FALSE)

td<-tidy(qd)


write.table(td,"corpus/tweets/tweets_raw_ger_1000_tagged.csv", sep=";", fileEncoding = "UTF-8",row.names = F)
