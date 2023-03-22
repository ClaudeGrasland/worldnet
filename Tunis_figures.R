library(data.table)
library(knitr)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(questionr)
library(tidyr)


# Load data

hc<-readRDS("hypercube/hc_sta_reg_day.RDS")

lab<-read.table("dict/Imageun_world_geo_def_V1.csv",
                sep=";",
                header=T, 
                encoding = "UTF-8",
                quote='"')
lab<-lab %>% filter(duplicated(lab[,c("code","lang")])==F) 
lablang<-lab %>% pivot_wider(id_cols = code,values_from = label,names_from = lang,values_fill = NA)





# Add country source name
hc$who_where<-as.factor(substr(hc$who,4,6))
levels(hc$who_where)<-c("4.DEU","6.DZA","1.FRA","2.GBR","5.IRL","5.IRL","3.TUR")
hc$who_where<-as.factor(as.character(hc$who_where))
table(hc$who_where)

hc[,.(nb=sum(news)), .(who_where)]

# check top / full corpus

top<-hc[,.(nb=min(.N,1)),.(where2,when)]
top2<-top[,.(nbd=sum(nb)),.(where2)] %>% 
         rename(code=where2) %>% 
          right_join(lablang) %>%
          arrange(-nbd)

kable(head(top2,20))

write.table(top2,"res/topreg.csv",row.names = F,fileEncoding = "UTF-8", sep=";")

# check top / by country or place

top<-hc[,.(nb=min(.N,1)),.(where2,when,who_where)]
top2<-top[,.(nbd=sum(nb)),.(who_where,where2)] %>% 
  mutate(code=where2) %>%
  right_join(lablang[,c(1,4)]) %>%
  rename(place=who_where) %>%
  group_by(place) %>%
  mutate(rnk=rank(-nbd,ties.method = "first"), index=100*nbd/max(nbd)) %>%
  select(place,rnk,nbd,index,region=en, code) %>%
  arrange(place,rnk)%>%
  filter(is.na(place)==F)
top2$type=as.factor(substr(top2$code,1,2))
levels(top2$type)<-c("Continent","Cultural area","Land body","Organisation","Water body")


tab<-top2 %>% filter( rnk<10) 
ggplot(tab,aes(x=rnk,y=index,label=region,  fill=typ)) +
  geom_bar(stat="identity") + 
  geom_text_repel(col="black",nudge_x=2, nudge_y=5, cex=2, max.overlaps = 8) +
  theme_light() +
  scale_y_continuous("Index 100 = maximum")+
  scale_x_continuous("rank") +
  ggtitle("Top world regions mentionned in newspapers (1.1.2019 to 30.06.2021)") +
  scale_fill_manual(values=c("green","pink","orange","red","lightblue")) +
  facet_wrap(~place)

#######  Dual classification
hc$region<-as.factor(as.character(hc$region))
hc$media<-as.factor(as.character(hc$who))
levels(hc$media) <-c("Al Nahar (DZA)", "Al Khabr (DZA",
                     "FAZ (DEU)", "Süddeu. Zeit (DEU)",
                     "Guardian (GBR)","Daily Tel. (GBR)",
                     "Irish Time (IRL)","Belfat Tel. (NIR)",
                     "Le Figaro (FRA)", "Le Monde (FRA)",
                     "Cumhuriyet (TUR)", "Yeni Savak (TUR)")
top<-hc[!(region %in% c("_no_","LA_amazon" )),.(nb=min(.N,1)),.(region,when,media)]
tab<-top[,.(nbd=sum(nb)),.(media,region)] %>% 
  rename(code=region) %>%
  inner_join(lablang[,c(1,4)]) %>%
  mutate(region=as.character(en)) %>% 
  dcast(formula = region~media, value.var = "nbd",fill = 0,fun.aggregate = sum) 

tab$region[tab$region=="Commonwealth of Nations"]<-"Commonwealth"
# Matrix

mat<-as.matrix(tab[,2:13])
row.names(mat)<-tab$region

# Filter ambiguous units
#mat<-mat[row.names(mat) != "Sahara",]
##mat<-mat[row.names(mat) != "Asia Minor",]
#mat<-mat[row.names(mat) != "Southern Africa",]
#mat<-mat[row.names(mat) != "Europe",]
#mat<-mat[row.names(mat) != "European Union",]


# Select units 
sel<-mat[apply(mat,1,sum)>40,]

# Exclude units mentionned by less than 3 media
sel <- sel[apply(sel>3,1,sum)>2,]



library(pheatmap)

x<-chisq.test(t(sel))
res<-as.matrix(x$residuals)
exp<-as.matrix(x$expected)
res[exp<2]<-0
res[res>3]<-3
res[res< -3]<- -3

pheatmap(res, cutree_rows = 6,cutree_cols = 7)
