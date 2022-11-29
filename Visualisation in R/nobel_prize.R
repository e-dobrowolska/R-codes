#starter
setwd("C:/Users/ewado/OneDrive/Dokumenty/Studia/R-mat/AViR")

library(ggplot2) 
library(scales)
library(dplyr)
library(rgdal)
library(jpeg)
library(ggimg)
library(treemapify)
library(leaflet)
library(ggChernoff)
library(ggiraph)
library(ggforce)
library(ggparallel)
library(RColorBrewer)

# The source of the dataset: https://www.kaggle.com/datasets/bahramjannesarr/nobel-prize-from-1901-till-2020

nobel<-read.csv("nobel_final.csv")

#1 Barplot: month of birth/discipline

nobel$stem = ifelse(nobel$category %in% c("physics", "chemistry", "mecicine"), "Science", "Social science")

pl<-nobel[nobel$born_country_code=="PL",]

pl2<- pl %>% 
  group_by(born_month) %>% 
  summarise(N = n()) %>% 
  mutate(born_month = factor(born_month))

ggplot(nobel, aes(x=born_month, fill=stem))+
  geom_bar(position="dodge", stat = 'count', aes(y = ..count../sum(..count..))) +
  geom_point(data=pl2, aes(x=born_month, y=N/dim(nobel)[1], colour='Polish scientists'), fill="firebrick2", size=2.5, pch=23) +
  scale_y_continuous(labels = percent)+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  xlab("Birthday month")+
  ylab("Percent of laureates")+
  ggtitle("Birthday month of the laureates")+
  scale_fill_manual(values=c("dodgerblue4", "forestgreen"), name="Discipline:")+
  scale_colour_manual(values="firebrick2", name="")+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5), legend.position="bottom", axis.text.x=element_text(angle=45))

#2 Top universities

uni<-data.frame(name_of_university=unique(nobel$name_of_university))

for(i in 2000:2019){
  uni2<- nobel[nobel$year<i,] %>% 
    group_by(name_of_university) %>% 
    summarise(Prizes = n())
  
  colnames(uni2)[2]<-paste0("Prizes", i)
  uni<-merge(uni, uni2, by="name_of_university", all.x=TRUE)
  }

uni<-uni[-1,]
ranking<-data.frame(Year = rep(2000:2019, each=15),rank=rep(1:15, 20), name_of_university=0)

for(i in 2:ncol(uni)){
  uni<-uni[order(-uni[,paste0("Prizes", 1998+i)]),]
  ranking$name_of_university[ranking$Year == 1998+i] <- uni$name_of_university[1:15]
}

ggplot(data = ranking, aes(col=name_of_university, x = Year, y = rank, group = name_of_university)) +
  geom_line(size = 2) +
  geom_point(size = 2.3, shape = 21, fill = 'white') +
  geom_text(data = ranking %>% filter(Year == "2019", rank <= 15),
            aes(label = name_of_university, x = Year) , hjust = -.15, color = "grey40", size = 2.5) + 
  geom_text(data = ranking %>% filter(Year == "2000", rank <= 15),
            aes(label = name_of_university, x = Year) , hjust = 1.15, color = "grey40", size = 2.5) +
  scale_x_discrete(expand = c(.5, .5)) +
  scale_y_reverse(breaks = seq(1, 15)) +
  labs(title = "Universities with the most winners", x = "Year", y = "Rank") +
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

#3 Year - age, when received the prize. Divided by category and gender

nobel$is_pl<-ifelse(nobel$born_country_code=="PL", "Yes", "No")

ggplot(nobel, aes(x=year, y=age_get_prize))+
  geom_point(aes(col=is_pl, size=is_pl), alpha=0.5)+
  geom_smooth(se=FALSE, size=1.5, col="blue")+
  xlab("Year")+
  ylab("Age, when received the Nobel Prize")+
  scale_color_manual(values=c("black", "red"), name="Is from Poland?")+
  scale_size_manual(values=c(2,4), name="Is from Poland?")+
  ggtitle("At what age do scientists get the Prize?")+
  facet_wrap(~category)+
  theme(plot.background=element_rect(fill="whitesmoke"),
        panel.background=element_rect(fill="white", color="grey70"),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_line(color='grey80', linetype = 'dashed', size=0.5),
        legend.position="bottom",
        legend.background=element_rect(fill="whitesmoke"))


#4 Category and prize share
nobel$change<-ifelse(nobel$born_country_code==nobel$died_country_code, "No", "Yes")

ggplot(nobel, aes(x=share, fill=change))+
  geom_bar()+
  theme_minimal()+
  ylab("Number of laureates")+
  xlab("Prize share")+
  ggtitle("Prize share among laureates")+
  theme(plot.background=element_rect(fill="whitesmoke"),
        panel.background=element_rect(fill="white", color="grey70"),
        plot.title=element_text(hjust=0.5),
        panel.grid.major = element_line(color='grey80', linetype = 'dashed', size=0.5),
        legend.position="top")+
  coord_flip()+
  scale_fill_manual(values=c("firebrick", "orange", "khaki"), name="Country change?")+
  facet_wrap(~category)

#5 Economics: pie plot – countries

eco<-nobel[nobel$category=="economics",]

eco.grouped<-eco %>% 
  group_by(born_country_code) %>% 
  summarise(N = n()) 

eco.grouped$born_country_code<-ifelse(eco.grouped$N>2, eco.grouped$born_country_code, "Other")
eco.ag<-aggregate(eco.grouped$N, by=list(eco.grouped$born_country_code), sum)
colnames(eco.ag)<-c("Country_code", "Value")
eco.ag$Country<-c("Canada", "France", "Great Britain", "Norway", "Other", "Russia", "United States")

ggplot(eco.ag, aes(x="", y=Value, fill=Country))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("goldenrod3", "forestgreen", "brown3", "darkcyan", "deeppink3", "deepskyblue3", "dodgerblue4"), name="Country:")+
  theme_minimal()+
  ggtitle("Home country of the winners in the economic sciences category")+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(hjust=0.5)
  ) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = percent(Value/84,1)),
             position = position_stack(vjust = 0.5), color="grey80", fontface="bold") 

#6 Treemap - economics & American universities

eco_usa<-eco[eco$country_of_university=="USA",]

eco_usa.grouped <- eco_usa %>% 
  group_by(name_of_university) %>% 
  summarise(Nobel_winners = n())

eco_usa.ag = aggregate(eco_usa$age_get_prize, by=list(eco_usa$name_of_university), mean)
colnames(eco_usa.ag)=c("name_of_university", "Mean_age")

eco_usa_final<-merge(eco_usa.grouped, eco_usa.ag, by="name_of_university")

ggplot(eco_usa_final, aes(area = Nobel_winners, fill=Mean_age, label = name_of_university)) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", size=10, colour = "white", place = "top",
                    grow = F, reflow=T) +
  geom_treemap_text(fontface = "bold", colour = "white", place="bottomright",
                    grow = F, alpha=0.5, aes(label=percent(Nobel_winners/47,1))) + 
  labs(title = "American Universities, which had a Nobel Prize Laureate in economics") +
  scale_fill_gradient(high="dodgerblue4", low="lightblue", name="Mean age:", guide = guide_colorbar(barheight = 20))+
  theme(plot.title=element_text(hjust=0.5))
  

#7 Scatterplot with images: polish physicists
pl$person<-c(paste0(pl$firstname," ", pl$surname))

pl.a<- pl %>% 
  group_by(category, gender) %>% 
  summarise(number = length(category))

pl.b <- pl.a %>% 
  group_by(category) %>% 
  mutate(label_sum = cumsum(number)) 

pl.b$gender<-factor(pl.b$gender, levels = c("male", "female"))

ggplot(pl.b, aes(x=category, y=number))+
  geom_bar(aes(fill=gender), position = "stack", stat = "identity")+
  xlab("Category")+
  ylab("Number od scientists")+
  ggtitle("Polish Nobel Prize Laureates")+
  scale_fill_manual(values=c( "cornflowerblue", "hotpink"), name="Gender:")+
  theme_minimal()+
  geom_text(aes(label = label_sum), position = position_stack(),
            vjust =2, color = "white", fontface="bold", size = 4) +
  theme(plot.title=element_text(hjust=0.5))

pl_fiz<-pl[pl$category=="physics",]
pl_fiz$IMG<-c("w_Maria_Curie.jpg", "w_Albert_Michelson.jpg", "w_Otto_Stern.jpg", "w_isodor_rabi.jpg",
              "w_Max_Born.jpg", "w_Maria_Goeppert-Mayer.jpg", "w_klitzing.jpg", "w_charpak.jpg")

ggplot(pl_fiz, aes(x=year, y=age_get_prize))+
  geom_point_img(aes(img=IMG), size=1.2)+
  ggtitle("Physicists born in Poland")+
  xlab("Year")+
  ylab("Age, when received the Prize")+
  scale_y_continuous(limits=c(30, 80))+
  scale_x_continuous(limits=c(1900, 2000))+
  geom_label(aes(label=person), vjust=2)+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5))

#8 Interactive map

world_spdf <- readOGR(".","TM_WORLD_BORDERS_SIMPL-0.3",
                      verbose = FALSE)

world.df<-as.data.frame(world_spdf)
world.df$ID<-c(1:246)
world.df<-world.df %>% rename(Country = ISO2)

noblists <- nobel %>% 
  group_by(Country = born_country_code) %>% 
  summarize(N_laureates = n())

world.df<-merge(world.df, noblists, by="Country", all.x=TRUE, sort=TRUE)
world.df$N_laureates[is.na(world.df$N_laureates)]<-0
world.df<-world.df[order(world.df$ID),]
rownames(world.df)<-c(0:245)

world.sp<-as(world_spdf, "SpatialPolygons")
world2<-SpatialPolygonsDataFrame(world.sp, world.df, match.ID=TRUE)
choropleth(world2, world2$N_laureates)

mytext <- paste("<strong>", 
  world2@data$NAME, 
  "</strong><br />Nobel Prize Laureates: ", world2@data$N_laureates) %>%
  lapply(htmltools::HTML)

brks<-c(0, 1, 2, 5, 30, 50, 100, 300)
cols<-brewer.pal("Blues", n=8)[-1]

leaflet(world2) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = cols[findInterval(world2$N_laureates, brks)], 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) 

#8 Parallel plot
dict.df<-data.frame(country_of_university = c("Germany", "the Netherlands", "France", "United Kingdom", "USA", "Sweden",
                                              "Denmark", "India", "Austria","Italy", "Japan", "Ireland", "USSR (now Russia)",
                                              "Canada", "Switzerland", "Czechoslovakia (now Czech Republic)", "Norway",
                                              "Argentina", "Spain", "Portugal", "China", "Israel", "Hungary", "Australia", "Russia",
                                              "Tunisia", "Belgium", "Germany (now France)"),
  uni_country_code = c("DE", "NL", "FR", "GB", "US", "SE", "DK", "IN", "AT", "IT", "JP", "IE",
                                          "RU", "CA", "CH", "CZ", "NO", "AR", "ES", "PT", "CN", "IL", "HU", "AU",
                                          "RU", "TN", "BE", "DE"))

nobel<-merge(nobel, dict.df, by="country_of_university", all=TRUE)

nobel$change_for_uni<-ifelse(nobel$born_country_code==nobel$uni_country_code, "No", "Yes")
nobel$change_for_uni[is.na(nobel$change_for_uni)]<-"No university"

cols <- c(brewer.pal(7, "Paired")[-1], brewer.pal(4, "Reds")[-1],  brewer.pal(3, "Greens")[-1])

ggparallel(list("category", "change_for_uni", "gender", "category"), order = -1, nobel, text.angle=0, width=0.45) +
  scale_fill_manual(values=cols, guide = "none") +
  scale_colour_manual(values=cols, guide = "none") +
  ylab("Number of laureates")+
  theme(axis.text=element_text(color="purple"))+
  theme_void()

#9 line plot
ggplot(eco, aes(x=year, y=age_get_prize))+
  geom_point(aes(col=gender), size=3)+
  geom_line(color="dodgerblue4", alpha=0.7)+
  scale_color_manual(values=c("hotpink", "cornflowerblue"), name="Gender:")+
  geom_line(y=mean(eco$age_get_prize), linetype="dotted")+
  geom_smooth(se=F, col="firebrick", alpha=0.01)+
  xlab("Year")+
  ylab("Age, when received the Prize")+
  ggtitle("Laureates in economic sciences")+
  scale_x_continuous(breaks=seq(1966, 2020, by=2))+
  facet_zoom(x=year>2000)+
  theme(plot.title=element_text(hjust=0.5),
        panel.background=element_rect(fill="lightsteelblue1"),
        panel.grid.major = element_line(color='white', linetype = 'dotted', size=0.5),
        axis.text.x=element_text(angle=30, vjust=0.5))

eco$change2<-ifelse(eco$change=="Yes", 0, 1) 
eco$person<-c(paste0(eco$firstname," ", eco$surname))

plot<-ggplot(data=eco[eco$year>2000,], aes(x=year, y=age_get_prize, smile=change2, fill=change2))+
  geom_chernoff()+
  geom_point_interactive(aes(tooltip = person, data_id = person), size=7, color="white", alpha=0.01)+
  xlab("Year")+
  ylab("Age, when received the prize")+
  ggtitle("Nobel Prize winners in economics since 2020")+ 
  scale_fill_gradient(low="goldenrod", high="cornflowerblue", name="University change?", breaks=c(0,1))+
  scale_smile_continuous(name="University change?", breaks=c(0,1))+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5))+
  guides(fill="none")


ggiraph(ggobj=plot)
