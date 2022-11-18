library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(arsenal)

bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')
speed_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv')

speed_index%>%
  mutate(Date = ymd(date))%>%
  data.frame()->speed

speed%>%
  select(c(client,p50,Date))->speedf

speedf$client[speedf$client =="mobile"] <- "Mobile"
speedf$client[speedf$client =="desktop"] <- "Desktop"

speedf%>%
  filter(client=="Mobile")->SpeedMobile

speedf%>%
  filter(client=="Desktop")->SpeedDesktop

colnames(SpeedDesktop)<-c("Client","Speed","Date")
SpeedDesktop%>%
  select(Date,Speed)->SD

colnames(SpeedMobile)<-c("Client","Speed","Date")
SpeedMobile%>%
  select(Date,Speed)->SM


bytes_total%>%
  mutate(Date = ymd(date))%>%
  data.frame()->bytes

bytes%>%
  select(c(client,p50,Date))->bytesf

bytesf$client[bytesf$client =="mobile"] <- "Mobile"
bytesf$client[bytesf$client =="desktop"] <- "Desktop"

bytesf%>%
  filter(client=="Mobile")->BytesMobile

bytesf%>%
  filter(client=="Desktop")->BytesDesktop

colnames(BytesDesktop)<-c("Client","Bytes","Date")
BytesDesktop%>%
  select(Date,Bytes)->BD

colnames(BytesMobile)<-c("Client","Bytes","Date")
BytesMobile%>%
  select(Date,Bytes)->BM

cbind(SD,BD)
comparedf(SD$Date,BD$Date)
comparedf(SD,BD)

SD%>%
  arrange(Date)%>%
  group_by(Date)%>%
  mutate(Month=month(Date))%>%
  mutate(Year=year(Date))%>%
  data.frame()->SD

SD%>%
  group_by(Year,Month)%>%
  arrange(Date)%>%
  slice_head()%>%
  data.frame()->SDF

BD%>%
  arrange(Date)%>%
  group_by(Date)%>%
  mutate(Month=month(Date))%>%
  mutate(Year=year(Date))%>%
  data.frame()->BD

BD%>%
  group_by(Year,Month)%>%
  arrange(Date)%>%
  slice_head()%>%
  data.frame()->BDF

BDF%>%
  filter(Year>=2016)->BDF1


SDF[-c(13,40),]->SDF1
cbind(SDF1,BDF1)->FDF1

colnames(FDF1)<-c("Date","Speed","Month","Year","Date1","Bytes","Month1","Year1")
FDF1%>%
  select(-c(Date1,Month1,Year1,Month,Year))->FDFF

FDFF%>%
  ggplot(aes(x=Date)) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Speed", limits=c(2,7),breaks=c(2,3,4,5,6,7),
    sec.axis = sec_axis(trans=~.*350,name="Weight in Bytes")
  )+
  geom_line(aes(y=Speed),colour="#0047AB")+
  geom_line(aes(y=Bytes/333),colour="#E30B5C")+
  scale_x_date(date_labels = "%Y", limit=c(as.Date("2016-01-01"),as.Date("2022-10-01")),date_breaks = "1 year")+
  labs(colour=" ")+
  theme(plot.margin=unit(c(0.5,1.5,0.5,1.5),"cm"),
        plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(colour="gray30",size=10),
        axis.title.y =element_text(colour="gray30",size=10,margin=margin(r=15)),
        axis.title.y.right = element_text(colour="gray30",size=10,margin=margin(l=15)),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray",
                                          size = 0.1,
                                          linetype = 1),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="black",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="black",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="black",hjust=0,margin=margin(t=40)))+
  labs(title="WEIGHT OF A WEB PAGE VERSUS ITS SPEED ON A DESKTOP",
       subtitle=str_wrap("Here's a comparison of how the 50th percentile of the weight and consequently the speed of a web page on a desktop have changed since 2016",120),
       caption = str_wrap("Source: httparchive.org by way of Data is Plural for Tidy Tuesday. Analysis and design: @annapurani93",70))+
annotate(geom="text", x=as.Date("2018-01-01"), y=4,
         label="Speed",color="#0047AB",fontface="bold")+
  annotate(geom="text", x=as.Date("2022-01-01"), y=6.2, 
           label="Weight",color="#E30B5C",fontface="bold")->plot

ggsave("desktopsvw.png",plot,width=10,height=7.14)   

  
  
  
  
