#   Necessary libraries.
library(rvest)
library(dplyr)
library(igraph)
library(ggraph)

###   Scraping N1

#   Base link for the website

linkN1<- "https://rs.n1info.com/"

#   Reading the website html

bodyN1<- read_html(linkN1)

#   Getting the list of titles. The css used in this while code has been acquired by using Chrome's SelectorGadget.

elements_titlesN1<- html_elements(bodyN1,
                                  css = ".uc-block-post-grid-title-link")
titlesN1<- html_text(elements_titlesN1)
titlesN1

#   Getting the links for each post on the website from the titles.

linksN1<- html_attr(elements_titlesN1, "href")
linksN1
#linksN1<-linksN1[!linksN1%in%""]
#linksN1


#   Scraping the tags and titles for every post reached trough the scraped links, 
#   and storing them in a tab separated file as a data table.

for (i in 1:length(linksN1)){  
  
  PostBody<-read_html(linksN1[i]) 
  
  PostElements<-html_elements(PostBody, 
                              css =".tags-wrapper a") 
  PostTags<-html_text(PostElements) 
  
  
  PostTitle<-html_elements(PostBody,
                           css = ".main-head .entry-title")
  PostTitle_text<- html_text(PostTitle)
  
  tab<-cbind(PostTitle_text, PostTags) 
  
  
  write.table(tab, file="PostTagsN1.tab", sep="\t", col.names=F,row.names=F, append=T, quote=F)
}


#   Importing the scraped data table.

TagsLongTableN1<-read.csv("PostTagsN1.tab", sep="\t") 

#   The column names of imported data are messy, so we will name them V1 and V2.
#   V1 Is the column containing post names; V2 is the column containing tags.

colnames(TagsLongTableN1)<- c("V1","V2")


#   Since missing values, blanks as well as rows where post name and tag name are identical cause errors in creating a network,
#   I wrote the code below to remove these problematic rows.

TagsLongTableN1_1<- TagsLongTableN1[!(is.na(TagsLongTableN1$V2) | 
                                        TagsLongTableN1$V2=="" | 
                                        TagsLongTableN1$V1 == TagsLongTableN1$V2), ] 

#   Importing the 'igraph' library to turn the table into a bipartite network.

library(igraph)

g<-graph.data.frame(TagsLongTableN1_1, directed=F)
V(g)$type <- bipartite_mapping(g)$type

#   Importing 'ggraph' library to plot the network.
#   Note: I have purposefuly minimized the size of the names of post names (plotted as red squares) because the sheer size of
#   the text makes the network messy. The tags are more interesting, but if you want the network to show the names of posts instead
#   just change the sizes in the code (the 'size =ifelse(V(g)$type, 2.8, 0' part).

library(ggraph)
ggraph(g, layout = "stress") +
  geom_edge_link(arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                 end_cap = circle(1, "mm"), color= "darkgray") +
  geom_node_point(color = ifelse(V(g)$type, "lightblue", "salmon"),
                  shape= ifelse(V(g)$type, "circle", "square"),
                  size = 3) +
  geom_node_text(aes(label = name), color= "black", size =ifelse(V(g)$type, 2.8, 0)) +
  theme_void() 

#   Some network centrality measures.

sort(degree(g))
sort(betweenness(g))

#   Emptying the Tags file so that it is ready for future use.
cat("", file="PostTagsN1.tab")








###   Scraping Kurir
#   Kurir website is structured differently and the acquired for each of the posts need additional attention and cleaning.
#   For this reason, though the code is largely the same as for N1, additional there are some changes.


#   Base link for the website

linkKurir<- "https://www.kurir.rs/"

#   Reading the website html

bodyKurir<- read_html(linkKurir)

#   Getting the list of titles. 

elements_titlesKurir<- html_elements(bodyKurir,
                                  css = ".title a")
elements_titlesKurir
titlesKurir<- html_text(elements_titlesKurir)
titlesKurir

#   Getting the links for each post on the website from the titles.

linksKurir<- html_attr(elements_titlesKurir, "href")
linksKurir

#   The first difference compared to N1 scraping is the structure and type of links leading to posts on the website.

#   On N1, each post title had a full link that leads to the post.
#   On Kurir, however, only the part of the link after the base link is included.
#   For example,'/stars/rijaliti/3937963/zola-urnisao-miljanu' without 'https://www.kurir.rs' that is the part of the full link.

#   For this reason, the loop for scraping tags and titles needs to first 
#   paste the base link with the scraped links for them to be useful (more on that soon).

#   In addition, the links scraped from the cover page of Kurir also contain links leading to external websites
#   and there is apparently no way to exclude them from scraping. These links also happen to be the only ones that start
#   with 'https', so I wrote the code below using regex in order to clean the list of scraped links from these.

linksKurir1<- gsub("^https:.+", "", linksKurir)
linksKurir1

#   Removing the blanks.
linksKurir1<-linksKurir1[!linksKurir1%in%""]
linksKurir1

#   Base link that will be used during scraping
baseLinkKurir<- "https://www.kurir.rs"

#   Scraping the tags and titles for every post reached trough the scraped links, 
#   and storing them in a tab separated file as a data table.

for (i in 1:length(linksKurir1)){  
  
  PostLink<-paste(baseLinkKurir, linksKurir1[i], sep="") 
  PostBody<-read_html(PostLink) 
  
  
  PostElements<-html_elements(PostBody, 
                              css =".lnk") 
  PostTags<-html_text(PostElements) 
  
  
  PostTitle<-html_elements(PostBody,
                           css = ".singleWrap .titleWrap .title")
  PostTitle_text<- html_text(PostTitle)
  
  tab<-cbind(PostTitle_text, PostTags) 
  
  
  write.table(tab, file="PostTagsKurir.tab", sep="\t", col.names=F,row.names=F, append=T, quote=F)
}

#   Importing the scraped data table.

TagsLongTableKurir<-read.csv("PostTagsKurir.tab", sep="\t") 

#   Naming the columns.

colnames(TagsLongTableKurir)<- c("V1","V2")

#   Removing missing values, blanks and rows with identical V1 and V2 values in order to avoid errors.
#   Note: Kurir has many more blanks compared to N1.

TagsLongTableKurir1<- TagsLongTableKurir[!(is.na(TagsLongTableKurir$V2) | 
                                             TagsLongTableKurir$V2=="" | 
                                             TagsLongTableKurir$V1 == TagsLongTableKurir$V2), ]

#   Turning the table into a bipartite network.

g<-graph.data.frame(TagsLongTableKurir1, directed=F)
V(g)$type <- bipartite_mapping(g)$type

#   Plotting the network.

ggraph(g, layout = "stress") +
  geom_edge_link(arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                 end_cap = circle(1, "mm"), color= "darkgray") +
  geom_node_point(color = ifelse(V(g)$type, "lightblue", "salmon"), 
                  size = 3, 
                  shape= ifelse(V(g)$type, "circle", "square")) +
  geom_node_text(aes(label = name), color= "black", size =ifelse(V(g)$type, 2.5, 0)) +
  theme_void()

#   Some network centrality measures

sort(degree(g))
sort(betweenness(g))

#   Emptying the Tags file so that it is ready for future use.
cat("", file="PostTagsKurir.tab")

