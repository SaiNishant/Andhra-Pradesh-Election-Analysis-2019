jp<-readLines("jp_quora.txt")
setjp<-as.matrix(jp)

tdp<-readLines("tdp_quora.txt")
tdp<-as.matrix(tdp)

ysrcp<-readLines("ysrcp_quora.txt")
ysrcp<-as.matrix(ysrcp)

#####################
tdp_nrc<-get_nrc_sentiment(tdp)
tdp_r<-sentiment(tdp)
#####################
ysrcp_nrc<-get_nrc_sentiment(ysrcp)
ysrcp_r<-sentiment(ysrcp)
#####################
jp_nrc<-get_nrc_sentiment(jp)
jp_r<-sentiment(jp)
#####################

tdp_r$check<-"Neutral"
tdp_r_ind<-which(tdp_r$sentiment>0)
tdp_r$check[tdp_r_ind]<-"Positive"
tdp_r_ind<-which(tdp_r$sentiment<0)
tdp_r$check[tdp_r_ind]<-"Negative"

tdp_r$check<-as.factor(tdp_r$check)
tdp_sentiment<-as.data.frame(table(tdp_r$check))
colnames(tdp_sentiment)<-c("Sentiment","Percentage")
tdp_sentiment$Percentage<-c(23.63,28.40,47.96)

ggplot(data=tdp_sentiment, aes(x =Sentiment, y =Percentage))+  
  geom_bar(fill = "steelblue", stat = "identity",width = 0.6)+
  geom_text(aes(label = paste(Percentage,"%")), vjust = -0.3)  
#########################################

ysrcp_r$check<-"Neutral"
ysrcp_r_ind<-which(ysrcp_r$sentiment>0)
ysrcp_r$check[ysrcp_r_ind]<-"Positive"
ysrcp_r_ind<-which(ysrcp_r$sentiment<0)
ysrcp_r$check[ysrcp_r_ind]<-"Negative"

ysrcp_r$check<-as.factor(ysrcp_r$check)
ysrcp_sentiment<-as.data.frame(table(ysrcp_r$check))
colnames(ysrcp_sentiment)<-c("Sentiment","Percentage")

ysrcp_sentiment$Percentage<-c(25.33,20.36,54.29)

ggplot(data=ysrcp_sentiment, aes(x =Sentiment, y =Percentage))+  
  geom_bar(fill = "steelblue", stat = "identity",width = 0.6)+
  geom_text(aes(label = paste(Percentage,"%")), vjust = -0.3)  
##############################################################

jp_r$check<-"Neutral"
jp_r_ind<-which(jp_r$sentiment>0)
jp_r$check[jp_r_ind]<-"Positive"
jp_r_ind<-which(jp_r$sentiment<0)
jp_r$check[jp_r_ind]<-"Negative"

jp_r$check<-as.factor(jp_r$check)
jp_sentiment<-as.data.frame(table(jp_r$check))
colnames(jp_sentiment)<-c("Sentiment","Percentage")
jp_sentiment$Percentage<-c(16.54,42,41.54)

ggplot(data=jp_sentiment, aes(x =Sentiment, y =Percentage))+  
  geom_bar(fill = "steelblue", stat = "identity",width = 0.6)+
  geom_text(aes(label = paste(Percentage,"%")), vjust = -0.3) 
##################################################

stacked_df<-rbind(tdp_sentiment,jp_sentiment,ysrcp_sentiment)
Party<-c("TDP","TDP","TDP","JP","JP","JP","YSRCP","YSRCP","YSRCP")
Party<-as.factor(Party)
final<-cbind(Party,stacked_df)

ggplot(data=final, aes(x =Sentiment, y =Percentage,fill=Party))+  
  geom_bar(stat = "identity",width = 0.8,position = position_dodge(),color="black")+ 
  scale_fill_manual(values=c("#ff1a1a","#ffff00","#0066ff"))
#+ 
# geom_text(aes(label=paste(format(Percentage,nsmall = 2),"%")),vjust=1.6,colour="Black",size=3.5,position = position_dodge(0.85))+facet_wrap(~Party,scales="free_x")

############################################################
ggplot(data=final, aes(x = Sentiment, y = Percentage,fill=Party))+  
  geom_bar(stat = "identity",position = "fill")+ 
  scale_fill_manual(values=c("Red","Yellow","Blue"))+facet_wrap(~Sentiment,scales="free_x")

######################################################
my_tdp_four<-sum(tdp_nrc$anger+tdp_nrc$disgust+tdp_nrc$joy+tdp_nrc$trust)
my_ysrcp_four<-sum(ysrcp_nrc$anger+ysrcp_nrc$disgust+ysrcp_nrc$joy+ysrcp_nrc$trust)
my_jp_four<-sum(jp_nrc$anger+jp_nrc$disgust+jp_nrc$joy+jp_nrc$trust)

tdp_anger<-round((sum(tdp_nrc$anger)/my_tdp_four)*100,2)
tdp_disgust<-round((sum(tdp_nrc$disgust)/my_tdp_four)*100,2)
tdp_joy<-round((sum(tdp_nrc$joy)/my_tdp_four)*100,2)
tdp_trust<-round((sum(tdp_nrc$trust)/my_tdp_four)*100,2)

ysrcp_anger<-round((sum(ysrcp_nrc$anger)/my_ysrcp_four)*100,2)
ysrcp_disgust<-round((sum(ysrcp_nrc$disgust)/my_ysrcp_four)*100,2)
ysrcp_joy<-round((sum(ysrcp_nrc$joy)/my_ysrcp_four)*100,2)
ysrcp_trust<-round((sum(ysrcp_nrc$trust)/my_ysrcp_four)*100,2)

jp_anger<-round((sum(jp_nrc$anger)/my_jp_four)*100,2)
jp_disgust<-round((sum(jp_nrc$disgust)/my_jp_four)*100,2)
jp_joy<-round((sum(jp_nrc$joy)/my_jp_four)*100,2)
jp_trust<-round((sum(jp_nrc$trust)/my_jp_four)*100,2)



anger<-c(tdp_anger,ysrcp_anger,jp_anger)
disgust<-c(tdp_disgust,ysrcp_disgust,jp_disgust)
joy<-c(tdp_joy,ysrcp_joy,jp_joy)
trust<-c(tdp_trust,ysrcp_trust,jp_trust)

senti_4<-data.frame(Party = c("TDP","YSRCP","JP"),Anger=anger,Disgust=disgust,Joy=joy,Trust=trust)
library(tidyr)
library(dplyr)
senti_4<-senti_4 %>% gather(Sentiment,Percentage,Anger:Trust)
new<-c("Anger","Anger","Anger","Disgust","Disgust","Disgust","Joy","Joy","Joy","Trust","Trust","Trust")
new<-as.factor(new)
final_4<-cbind(new,senti_4)

ggplot(data=senti_4, aes(x =Sentiment, y =Percentage))+  
  geom_bar(stat = "identity",position = position_dodge(),color="black",aes(fill=Party))+
  facet_wrap(~Party,"free_x")+geom_text(aes(label=paste(format(Percentage,nsmall = 2),"%")),vjust=1.6,colour="Black",size=3.5,position = position_dodge(0.85))+scale_fill_manual(values=c("#ff1a1a","#ffff00","#0066ff"))
####################################################

