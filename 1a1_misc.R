
library(dplyr)
library(gtable)
seed <- read.csv("C:/Users/YK Huang/Documents/UIC2work/seed_porject_2018summer/dummy/seed_dummy_data.csv",header=T)

s2 <- subset(seed,k2==0)
s3 <- subset(seed,k2==1)

s3 %>% lm(no2~db,data=.) %>% summary()

rr1 <- as.data.frame(as.character(raw_1_CM$Title))
colnames(rr1) <- "V1"

seed <- read.csv("C:/Users/YK Huang/Downloads/toxnet(9).csv",header=F)

rr2 <- as.data.frame(as.character(seed$V1))
colnames(rr2) <- "V1"

s3 <- rbind(rr1,rr2)
s4 <- unique(s3)

#new venn d
venn.plot <- draw.pairwise.venn(
       area1 = 18,
       area2 = 8,
       cross.area = 2,
       category = c("14dy", "28dy"),
       fill = c("blue", "red"),
       lty = "blank",
       cex = 2,
       cat.cex = 2,
   );

#try bar chart
a <- function(indf,expo,health){
  c(expo,health,paste(expo,health,sep="+"),
  indf %>% subset(.,.$Year %in% c(1994:2007) ) %>% dim() %>% .[1],
  indf %>% subset(.,.$Year %in% c(2008:2012) ) %>% dim() %>% .[1],
  indf %>% subset(.,.$Year %in% c(2013:2017) ) %>% dim() %>% .[1]
  )
}

a2 <- function(b){
  c(as.numeric(b[1,4])/as.numeric(b[3,4]),as.numeric(b[1,5])/as.numeric(b[3,5]),as.numeric(b[1,6])/as.numeric(b[3,6]),
  as.numeric(b[2,4])/as.numeric(b[3,4]),as.numeric(b[2,5])/as.numeric(b[3,5]),as.numeric(b[2,6])/as.numeric(b[3,6]))
}

b <- rbind( a(df_1A,"no2","cvd"),a(df_1B,"no2","diabetes"),a(df_1CM,"no2","cardiometabolic"),
            a(df_2A,"pm2.5","cvd"),a(df_2B,"pm2.5","diabetes"),a(df_2CM,"pm2.5","cardiometabolic"),
            a(df_3A,"noise","cvd"),a(df_3B,"noise","diabetes"),a(df_3CM,"noise","cardiometabolic")  )

c <- c(
       rbind( a(df_1A,"no2","cvd"),a(df_1B,"no2","diabetes"),a(df_1CM,"no2","cardiometabolic")) %>% a2,
       rbind( a(df_2A,"pm2.5","cvd"),a(df_2B,"pm2.5","diabetes"),a(df_2CM,"pm2.5","cardiometabolic")) %>% a2,
       rbind( a(df_3A,"noise","cvd"),a(df_3B,"noise","diabetes"),a(df_3CM,"noise","cardiometabolic")) %>% a2
    ) %>% cbind(.,c(rep("no2",6),rep("pm2.5",6),rep("noise",6))) %>% 
    cbind(., rep(c(rep("cvd",3),rep("diabetes",3)),3)) %>%
    cbind(., rep(c("1994-2007","2008-2012","2013-2017"),3)) %>% as.data.frame()

colnames(c) <- c("percent","expo","health","period")
c$e_f = factor(c$expo, levels=c("no2","pm2.5","noise"))
#p <- ggplot(data=c, aes(y=as.numeric(as.character(percent)),x=period,fill=health)) + geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~e_f)

labels <- c(no2 = "NO2", pm2.5 = "PM2.5", noise= "Noise")
p <- ggplot(data=c, aes(y=as.numeric(as.character(percent)),x=period,fill=health)) + geom_bar(stat="identity", position=position_dodge()) + facet_grid(. ~ e_f, labeller=labeller(e_f = labels))
p <- p+labs(y="percent",x="time period") #title="a"
#p
p2 <- p+
  scale_fill_manual(name = "health outcomes",
                      labels = c("cardiovascular","diabetes"),
                      values = c("gray44","gray3"))
p2 <- p2 + theme_bw()
p2
ggsave("4_r_output/out2.png",p2,width = 8)

#
#2. try bar chart
#find single, multiple pollutants (two/three)
su_only1A <- df_1A[df_1A$EID %in% (setdiff(df_1A$EID,union(df_2A$EID,df_3A$EID))), ] 
su_only2A <- df_2A[df_2A$EID %in% (setdiff(df_2A$EID,union(df_1A$EID,df_3A$EID))), ] 
su_only3A <- df_3A[df_3A$EID %in% (setdiff(df_3A$EID,union(df_1A$EID,df_2A$EID))), ] 

su_only1B <- df_1B[df_1B$EID %in% (setdiff(df_1B$EID,union(df_2B$EID,df_3B$EID))), ] 
su_only2B <- df_2B[df_2B$EID %in% (setdiff(df_2B$EID,union(df_1B$EID,df_3B$EID))), ] 
su_only3B <- df_3B[df_3B$EID %in% (setdiff(df_3B$EID,union(df_1B$EID,df_2B$EID))), ] 

su_only1CM <- df_1CM[df_1CM$EID %in% (setdiff(df_1CM$EID,union(df_2CM$EID,df_3CM$EID))), ] 
su_only2CM <- df_2CM[df_2CM$EID %in% (setdiff(df_2CM$EID,union(df_1CM$EID,df_3CM$EID))), ] 
su_only3CM <- df_3CM[df_3CM$EID %in% (setdiff(df_3CM$EID,union(df_1CM$EID,df_2CM$EID))), ] 


#org
a3 <- function(indf,expo,health){
  c(expo,health,paste(expo,health,sep="+"),
    indf %>% subset(.,.$Year %in% c(1994:2008) ) %>% dim() %>% .[1],
    indf %>% subset(.,.$Year %in% c(2009:2017) ) %>% dim() %>% .[1],
    indf %>% subset(.,.$Year %in% c(2009:2017) ) %>% dim() %>% .[1]
  )
}

a4 <- function(b){
  c(
  c(as.numeric(b[1,4]),as.numeric(b[2,4]),as.numeric(b[3,4]),as.numeric(b[4,4]),as.numeric(b[5,4])  )/ sum(as.numeric(b[,4])),
  c(as.numeric(b[1,5]),as.numeric(b[2,5]),as.numeric(b[3,5]),as.numeric(b[4,5]),as.numeric(b[5,5])  )/ sum(as.numeric(b[,5])),
  c(as.numeric(b[1,6]),as.numeric(b[2,6]),as.numeric(b[3,6]),as.numeric(b[4,6]),as.numeric(b[5,6])  )/ sum(as.numeric(b[,6]))
  )
}

c2 <- c(
  rbind( a3(su_only1CM,"just no2","cvd"),a3(su_only2CM,"just pm","cvd"),a3(su_only3CM,"just noise","cvd"),
         a3(df_joint_no2pm25_CM,"no2+pm2.5","diabetes"),a3(df_joint_wnoise_CM,"M w noise","cardiometabolic")
  ) %>% a4,
  rbind( a3(su_only1A,"just no2","cvd"),a3(su_only2A,"just pm","cvd"),a3(su_only3A,"just noise","cvd"),
         a3(df_joint_no2pm25_A,"no2+pm2.5","diabetes"),a3(df_joint_wnoise_A,"M w noise","cardiometabolic")
  ) %>% a4,
  rbind( a3(su_only1B,"just no2","cvd"),a3(su_only2B,"just pm","cvd"),a3(su_only3B,"just noise","cvd"),
         a3(df_joint_no2pm25_B,"no2+pm2.5","diabetes"),a3(df_joint_wnoise_B,"M w noise","cardiometabolic")
  ) %>% a4  
  ) %>% cbind(.,c(rep("CM",15),rep("cvd",15),rep("diabetes",15))) %>% 
  cbind(., rep(c("single-no2","single-pm2.5","single-noise","mulit-no2+pm2.5","mulit-with noise"),3)) %>%
  cbind(., rep(c(rep("1994-2008",5),rep("2008-2017",5),rep("2008-2017",5)),3)) %>% as.data.frame()

colnames(c2) <- c("percent","health","expo","period")

c2$e_f = factor(c2$expo, levels=c("single-no2","single-pm2.5","single-noise","mulit-no2+pm2.5","mulit-with noise"))
#org
#org- in color
#p <- ggplot(data=c2, aes(y=as.numeric(as.character(percent)),x=period,fill=e_f)) + geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~health)
#p <- p+labs(y="percent",x="time period") #title="a"
#p




#modinto big function with set time peroid#
#org
moda3a4 <- function(t1,t2,t3,t4){
a3 <- function(indf,expo,health){
  c(expo,health,paste(expo,health,sep="+"),
    indf %>% subset(.,.$Year %in% c(t1:t2-1) ) %>% dim() %>% .[1],
    indf %>% subset(.,.$Year %in% c(t2:t3-1) ) %>% dim() %>% .[1],
    indf %>% subset(.,.$Year %in% c(t3:t4) ) %>% dim() %>% .[1]
  )
}

a4 <- function(b){
  c(
    c(as.numeric(b[1,4]),as.numeric(b[2,4]),as.numeric(b[3,4]),as.numeric(b[4,4]),as.numeric(b[5,4])  )/ sum(as.numeric(b[,4])),
    c(as.numeric(b[1,5]),as.numeric(b[2,5]),as.numeric(b[3,5]),as.numeric(b[4,5]),as.numeric(b[5,5])  )/ sum(as.numeric(b[,5])),
    c(as.numeric(b[1,6]),as.numeric(b[2,6]),as.numeric(b[3,6]),as.numeric(b[4,6]),as.numeric(b[5,6])  )/ sum(as.numeric(b[,6]))
  )
}

c2 <- c(
  rbind( a3(df_1CM,"just no2","cvd"),a3(df_2CM,"just pm","cvd"),a3(df_3CM,"just noise","cvd"),
         a3(df_joint_no2pm25_CM,"no2+pm2.5","diabetes"),a3(df_joint_wnoise_CM,"M w noise","cardiometabolic")
  ) %>% a4,
  rbind( a3(df_1A,"just no2","cvd"),a3(df_2A,"just pm","cvd"),a3(df_3A,"just noise","cvd"),
         a3(df_joint_no2pm25_A,"no2+pm2.5","diabetes"),a3(df_joint_wnoise_A,"M w noise","cardiometabolic")
  ) %>% a4,
  rbind( a3(df_1B,"just no2","cvd"),a3(df_2B,"just pm","cvd"),a3(df_3B,"just noise","cvd"),
         a3(df_joint_no2pm25_B,"no2+pm2.5","diabetes"),a3(df_joint_wnoise_B,"M w noise","cardiometabolic")
  ) %>% a4  
) %>% cbind(.,c(rep("CM",15),rep("cvd",15),rep("diabetes",15))) %>% 
  cbind(., rep(c("single-no2","single-pm2.5","single-noise","mulit-no2+pm2.5","mulit-with noise"),3)) %>%
  cbind(., rep(c(rep(paste(t1,t2-1,sep="-"),5),rep(paste(t2,t3-1,sep="-"),5),rep(paste(t3,t4,sep="-"),5)),3)) %>% as.data.frame()

colnames(c2) <- c("percent","health","expo","period")

c2$e_f = factor(c2$expo, levels=c("single-no2","single-pm2.5","single-noise","mulit-no2+pm2.5","mulit-with noise"))
#org
c2
}
#moe end
#make it bw+change faccet name
labels <- c(CM = "cardiometabolic", cvd = "cardiovascular", diabetes= "diabetes")
p <- moda3a4(1994,2008,2013,2017) %>% ggplot(data=., aes(y=as.numeric(as.character(percent)),x=period,fill=e_f)) + geom_bar(stat="identity", position=position_dodge()) + facet_grid(. ~ health, labeller=labeller(health = labels))
p <- p+labs(y="percent",x="time period") #title="a"
p
p2 <- p+
  scale_fill_manual(name = "exposures",
                    labels = c("single-NO2","single-PM2.5","single-Noise","co-NO2+PM2.5","co-with noise"),
                    values = c("gray78","gray61","gray45","gray22","gray0"))
p2 <- p2 + theme_bw() +theme(axis.text=element_text(size=8),
                             axis.title=element_text(size=14,face="bold"))
p2

ggsave("4_r_output/out3.png",p2,width = 8)

#out3 byline
rbind(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% subset(.,Year %in% c(1994:2007)) %>% dim %>% .[1]
rbind(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% subset(.,Year %in% c(2008:2012)) %>% dim %>% .[1]
rbind(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% subset(.,Year %in% c(2013:2017)) %>% dim %>% .[1]
rbind(df_1A,df_2A,df_3A,df_joint_no2pm25_A,df_joint_wnoise_A) %>% subset(.,Year %in% c(1994:2007)) %>% dim %>% .[1]
rbind(df_1A,df_2A,df_3A,df_joint_no2pm25_A,df_joint_wnoise_A) %>% subset(.,Year %in% c(2008:2012)) %>% dim %>% .[1]
rbind(df_1A,df_2A,df_3A,df_joint_no2pm25_A,df_joint_wnoise_A) %>% subset(.,Year %in% c(2013:2017)) %>% dim %>% .[1]
rbind(df_1B,df_2B,df_3B,df_joint_no2pm25_B,df_joint_wnoise_B) %>% subset(.,Year %in% c(1994:2007)) %>% dim %>% .[1]
rbind(df_1B,df_2B,df_3B,df_joint_no2pm25_B,df_joint_wnoise_B) %>% subset(.,Year %in% c(2008:2012)) %>% dim %>% .[1]
rbind(df_1B,df_2B,df_3B,df_joint_no2pm25_B,df_joint_wnoise_B) %>% subset(.,Year %in% c(2013:2017)) %>% dim %>% .[1]

#senstivity test
#l.df <- lapply(ls(pattern="su_"), function(x) get(x)) #the same, get df with the name "su_" into a list
l.df <- lapply(apropos("^su_"), function(x) get(x)) 
sapply(l.df,F_hypovsnonhypo) %>% `colnames<-`(apropos("^su_")) %>% `rownames<-`(c("H-D","NH-D","Total n"))


##noise, H+NH list
#misc

box_noise.hnh <- rbind(
                    df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==T),] %>% mutate(OCC = "Y") %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)==T),] %>% mutate(HNH = "H"),
                    df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==T),] %>% mutate(OCC = "Y") %>% .[which(str_detect(.$Index.Keywords,EMTREE_nh.design)==T),] %>% mutate(HNH = "NH"),
                    df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% mutate(OCC = "N") %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)==T),] %>% mutate(HNH = "H"),
                    df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% mutate(OCC = "N") %>% .[which(str_detect(.$Index.Keywords,EMTREE_nh.design)==T),] %>% mutate(HNH = "NH")
)

bo1 <- box_noise.hnh %>% .[.$OCC=="Y",] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(50)
bo2 <- box_noise.hnh %>% .[.$OCC=="N",] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(50)

bo3 <- cbind(bo1,bo2)

#word cloud
#pal = c("Grey","Green","Red")
#wordcloud2(st_uniindexkw_1CM, color = pal, backgroundColor = "white")
#50%,20% no2=156,63/pm2.5=313,125 /nosie=21,8
st_uniindexkw_1CM %>% wordcloud2(.,
           color = ifelse(.[, 2] > 21, 'Black', 
                          ifelse(.[, 2] > 8, '#808080', '#D3D3D3')),size=0.4  )


#cited count (table2)
F_table2 <- function(df){
  n <- dim(df)[1]
  a <- min(df$Year,na.rm=T)
  b <- median(df$Year,na.rm=T)
  c <- sum(df$Cited.by,na.rm=T)
  d <- mean(df$Cited.by,na.rm=T)
  e <- median(df$Cited.by,na.rm=T)
  round(c(n,a,b,c,d,e),1)
}
list(df_joint_wnoise_CM,df_joint_wnoise_A,df_joint_wnoise_B) %>% sapply(.,F_table2) %>% t
list(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% sapply(.,F_table2) %>% t
#done