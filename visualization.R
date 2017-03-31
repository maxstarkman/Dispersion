#ALL SCENARIOS
#Particle count of all scenarios over a specific date/time
#plt1 <- ggplot()+geom_line(data = TotalCount, aes(x=particleDT_LT,y=V1,colour=as.factor(issueDT_LT))) +
 # labs(x = "Date/Time (UTC)", y = "Particle Count")
plt1 <- ggplot()+geom_line(data = TC_L, aes(x=particleDT_LT,y=ParticleCount,colour=Scenario)) +
labs(x = "Particle Date/Time (LT)", y = "Particle Count")+theme_bw()+facet_wrap(~site)+facet_wrap(~day,ncol = 1)
plt1

#Scenario metrics (max,min,mean) viewed over time
plt1a <- ggplot() + geom_line(data = TC_Num,aes(x=issueDT_LT,y=Count,colour=ScenarioMetric))+ 
  labs(x = "Particle Date/Time (LT)", y = "Particle Count")+theme_bw()+facet_wrap(~site)+facet_wrap(~Scenario)
plt1a

#plt3 <- ggplot()+geom_line(data=PuffLife,aes(x=fileDT_LT,y=V1,colour=as.factor(issueDT_LT)))

plt3 <- ggplot()+geom_line(data = PuffLife,aes(x=fileDT_LT,y=w1_sum),colour="red") + facet_wrap(~site)+facet_wrap(~day)
plt3a <- plt3+geom_line(data=PuffLife,aes(x=fileDT_LT,y=w2_sum),colour="blue")+ facet_wrap(~site)+facet_wrap(~day)
plt3b <- plt3a+geom_line(data=PuffLife,aes(x=fileDT_LT,y=w3_sum),colour="green")+ facet_wrap(~site)+facet_wrap(~day)+ labs(y = "Particle Count",x = "File Date/Time (LT)") + theme_bw()
plt3b

#plt4 <- ggplot() +geom_line(data=subset(day,issueDT_LT>="2017-01-01 06:00:00"&issueDT_LT<="2017-01-10 06:00:00"),aes(x=fileDT_LT,y=V1,colour=as.factor(issueDT_LT)))

plt4 <- ggplot() +geom_line(data=subset(nHours,issueDT_LT>="2017-01-01 06:00:00"&issueDT_LT<="2017-01-07 06:00:00"),aes(x=fileDT_LT,y=wt1),colour="red") + facet_wrap(~site)+facet_wrap(~day)
plt4a <- plt4 +geom_line(data=subset(nHours,issueDT_LT>="2017-01-01 06:00:00"&issueDT_LT<="2017-01-07 06:00:00"),aes(x=fileDT_LT,y=wt2),colour="blue")+ facet_wrap(~site)+facet_wrap(~day)
plt4b <- plt4a +geom_line(data=subset(nHours,issueDT_LT>="2017-01-01 06:00:00"&issueDT_LT<="2017-01-07 06:00:00"),aes(x=fileDT_LT,y=wt3),colour="green")+ facet_wrap(~site)+facet_wrap(~day)

plt4b

#FORM A


#FORM B
ggplot()+geom_line(data=scenarios,aes(x=sc,y=weight1),colour="red") +geom_line(data=scenarios,aes(x=sc,y=weight2),colour="blue") + 
  geom_line(data=scenarios,aes(x=sc,y=weight3),colour="green")

#FORM C
ggplot()+geom_line(data=scenarios,aes(x=sc,y=formCw1),colour="red") +geom_line(data=scenarios,aes(x=sc,y=formCw2),colour="blue") + 
  geom_line(data=scenarios,aes(x=sc,y=formCw3),colour="green")