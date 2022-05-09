# RTVSloveniaScraping
Using Rvest library for scraping content from Slovenian national television website; Creating a bimodal network that visualizes the post id's and hashtags used in them.

The document is in construction, don't kill me.

#### Required libraries
```
library(tidyverse)
library(tidyr)
library(tidytext)
library(tibble)
library(rvest)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)
```
#### Writing a function

```
crawler<- function(Link){

  body<-read_html(Link)
  elements_titles<-html_elements(body, 
                                 css="#main-container a:nth-child(4) , #main-container a:nth-child(3), #main-container a:nth-child(2)") 
                         
  Titles<- html_text(elements_titles) #titles

  links<- html_attr(elements_titles, "href") #link elements from titles
  links<-links[!links%in%""] #just some cleaning in case something goes wrong

  BaseLink<-"https://www.rtvslo.si" 

  for (i in 1:length(links)){  
  
    PostLink<-paste(BaseLink, links[i], sep="") 
    PostBody<-read_html(PostLink) 
    PostElements<-html_elements(PostBody, css=".tag") 
    PostTags<-html_text(PostElements) 
  
    IdPost<-gsub(".*(\\d{6})$","\\1",PostLink) 
  
    tab<-cbind(IdPost, PostTags) 
    write.table(tab, file="PostTags.tab", sep="\t", col.names=F,row.names=F, append=T, quote=F)
  }
}
```

#### Applying the function to a set number of pages of the website using the URL

```
all_links<- lapply(paste0('https://www.rtvslo.si/kljucne-besede?c_mod=search&q=covid-19&page=', 0:3), crawler)
```

#### Loading and transforming the obtained scraped id's and hashtags into a binary matrix needed for plotting a network

```
TagsLongTable<-read.table("PostTags.tab", sep="\t")

TagsWideTable<-dcast(TagsLongTable, V1~V2)

rownames(TagsWideTable)<-TagsWideTable[,1] # column 1 are  assigned as column names

TagsWideTable<-TagsWideTable[,-1]  # now the column 1 is deleted
TagsWideTable[!is.na(TagsWideTable)]<-1 
TagsWideTable[ is.na(TagsWideTable)]<-0 # transforming into binary values
TagsWideTable[] <- lapply(TagsWideTable, as.numeric) 
TagsWideTable<-as.matrix(TagsWideTable) #creating a binary matrix

```
#### Plotting a bimodal network

```
g<-graph.data.frame(TagsLongTable, directed=F)
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "darkgray"

library(ggraph)
ggraph(g, layout = "stress") +
  geom_edge_link(arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                 end_cap = circle(1, "mm"), color= "darkgray") +
  geom_node_point(color = ifelse(V(g)$type, "lightblue", "salmon"), 
                  size = 3, 
                  shape= ifelse(V(g)$type, "circle", "square")) +
  geom_node_text(aes(label = name), color= "black", size = 4) +
  theme_void()
```

#### Clearing the obtained PostTags.tab file to make it usable for new scraping

```
cat("", file="PostTags.tab")
```
