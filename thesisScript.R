#将家庭代码替换为新老移民识别代码
mhso_w2_abc$hhcode<-1
mhsn_w2_abc$hhcode<-2
#建立新数据源
newM<-mhsn_w2_abc
oldM<-mhso_w2_abc
#合并数据
library("plyr")
chip<-rbind.fill(newM,oldM)
#处理年龄，因为保险的表格有年龄条件
chip<-chip[complete.cases(chip$a05),]
chip<-chip[!(chip$a05<=16),]
#处理幸福人群NA的问题
chip<-chip[complete.cases(chip$a55),]
#处理保险状况的问题
chip<-chip[!(chip$a21==6),]
chip<-chip[!(chip$a22==6),]
chip<-chip[!(chip$a23==6),]
chip<-chip[!(chip$a24==6),]
#处理工资收入问题
chip$c117[is.na(chip$c117)]<-0
chip<-chip[ ! chip$c117 %in% 0, ] 
#处理数值类型问题
chip$a55<-as.factor(chip$a55)
chip$c117<-as.numeric(chip$c117)
chip$a36_10<-as.factor(chip$a36_10)
chip$city<-as.factor(chip$city)
chip$hhcode<-as.factor(chip$hhcode)
#处理工作调查问题
chip<-chip[complete.cases(chip$c108),]
chip<-chip[complete.cases(chip$c112_1),]
chip<-chip[complete.cases(chip$c112_2),]
chip$c112_1<-chip$c112_1*chip$c112_2
#处理顺序问题
chip$a55<-factor(chip$a55, levels = c("4","3","2","1"))
levels(chip$a36_10)<-rev(levels(chip$a36_10))
chip$a27<-factor(chip$a27, levels = c("5","4","3","2","1"))
#回归测试
test<-polr(a55~a36_10+a15+log(c117)+c112_1+a05+a04+city+a27+a36_10,data=chip)#保险
coeftest(test)

test<-polr(a55~a36_10*a15,data=chip)#保险*健康水平
coeftest(test)

test<-polr(a55~a05,data=chip)#保险,年龄
coeftest(test)

test<-polr(a55~a36_10,data=chip)#保险，工时
coeftest(test)

test<-polr(a55~log(c117)+a36_10,data=chip)#保险，工资
coeftest(test)

test<-polr(a55~a36_10+log(c117)+a15,data=chip)#保险，工资，户口，
coeftest(test)

test<-polr(a55~log(c117)+a36_10+city+hhcode,data=chip)#保险，工资，城市，新老
coeftest(test)

test<-polr(a55~log(c117)+a36_10+hhcode+a15+a05+c112_1+city,data=chip)#保险，工资，城市，户口，年龄，新老
coeftest(test)


test<-polr(a55~log(c117)+a36_10,data=chip,subset = hhcode== "1")
coeftest(test)


test<-polr(a55~log(c117)+a36_10,data=chip,subset = city== "9")
coeftest(test)

tests<-glm(a36_10~a15+log(c117)+c112_1+a05+a04+city,family = "binomial",data=chip)
coeftest(tests)
#偏效应
met<-ocME(test)
#画图
ggplot(chip$a55,aes(x=chip$a45))+geom_bar()