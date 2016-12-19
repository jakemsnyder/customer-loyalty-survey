#Main files pulled from Salesforce
#CLS16 - Q&A https://fas.my.salesforce.com/00Ot0000000hXgn
#CLS15 Data Pull https://fas.my.salesforce.com/00Ot0000000Og8g
#CLS15 Question Mapping https://fas.my.salesforce.com/00Ot0000000Og8b

#Other files pulled from OCE google drive, gathered from the FAS program offices
#AAS Servicing Region https://drive.google.com/open?id=0B2FEzg5RjHNdN0pvM2U0V3NseEk
#CLSComments https://drive.google.com/open?id=0B2FEzg5RjHNdaVlLcDBOMmV2aGc
#zip_code_database https://drive.google.com/open?id=0B2FEzg5RjHNdT1dSN1VmUUF2LU0


setwd("/Users/jakesnyder/Documents/CLS")
library(dplyr)
library(reshape2)
library(stringr)

#####Initial load#####
#Load csv
df16 <- read.csv("CLS FY16 raw.csv",colClasses='character')
df15 <- read.csv("CLS FY15 raw.csv", colClasses='character')

#Create anonymized RespondentID
Respondent16 <- unique(df16$Contact.ID) %>% as.data.frame()
Respondent15 <- unique(df15$Contact.ID) %>% as.data.frame()
Respondent <- unique(rbind(Respondent16,Respondent15))
set.seed(520)
Respondent <- Respondent[sample(1:nrow(Respondent)),] %>% as.data.frame()
Respondent$RespondentID <- sub("^","ID",seq.int(nrow(Respondent)))
Respondent$RespondentID <- seq.int(nrow(Respondent))
Respondent$RespondentID <- str_pad(Respondent$RespondentID,5,pad='0')
Respondent$RespondentID <- sub("^","ID",Respondent$RespondentID)
rm(Respondent15,Respondent16)
colnames(Respondent) <- c('ContactID','RespondentID')

#Create df for AAS servicing region, will add later in the code
aas.region <- read.csv("AAS Servicing Region.csv",colClasses='character')
aas.region$Email <- trimws(aas.region$Email)
aas.region <- unique(filter(aas.region,Servicing.Region != ''))
df16.aas <- filter(df16,Survey..Survey.Name == 'CLS - AAS')
df16.aas <- unique(df16.aas[c(2,7)])
aas.region <- left_join(df16.aas,aas.region,by='Email')
aas.region$Servicing.Region <- replace(aas.region$Servicing.Region,grepl('^(jonathanl)|(kevin)',aas.region$Email),3)
aas.region$Servicing.Region <- replace(aas.region$Servicing.Region,grepl('^(mickey)',aas.region$Email),4)
aas.region$Servicing.Region <- replace(aas.region$Servicing.Region,grepl('^(xlp8)',aas.region$Email),8)
aas.region <- unique(aas.region)
colnames(aas.region)[3] <- 'Servicing Region'
rm(df16.aas)

#Remove unnecessary columns
df16 <- df16[c(7,6,3:5)]
colnames(df16) <- c("FeedbackID","ContactID","Question","Response","Program")

#Remove blank responses and people who were screened out of the survey
df16 <- filter(df16, Response != '')
screen <- df16 %>% group_by(FeedbackID) %>% summarise(count = n_distinct(Question)) %>% filter(count<2)
df16 <- filter(df16, !(FeedbackID %in% screen$FeedbackID))

#Removing respondents that TMVCS had concerns with
screen <- read.csv("CLSComments.csv")
colnames(screen)[3] <- 'Comment'
screen$short <- substr(screen$Comment,0,30)
screen <- screen[c(3,5)]
temp <- filter(df16,Program == 'CLS - Motor Vehicles' & Question %in% c('11 One Change','12 Additional Comments'))[c(1,4)]
temp$short <- substr(temp$Response,0,30)
screen <- left_join(screen,temp,by='short')
df16 <- filter(df16, !(FeedbackID %in% screen$FeedbackID))

####Initial cleanup, change survey to wide####
df16$Program <- as.factor(substr(df16$Program,7,99))
df16$FY <- 2016
wide <- dcast(df16, FeedbackID + Program + ContactID + FY ~ Question, value.var = "Response")

#Quirk in the data
wide$`10 Satisfaction Rating` <- replace(wide$`10 Satisfaction Rating`,wide$FeedbackID=='a0Tt0000000YuGn',8.5)

#Anonymize the survey
wide$ContactID <- as.character(wide$ContactID)
Respondent$ContactID <- as.character(Respondent$ContactID)
wide <- left_join(wide,Respondent,by='ContactID')
wide$ContactID <- NULL
wide$`11 One Change` <- NULL
wide$`12 Additional Comments` <- NULL

#Clean up core questions and add Loyalty
colnames(wide)[colnames(wide) == '01 Familiarity'] <- 'Familiarity'
colnames(wide)[colnames(wide) == '02 Recommendation'] <- 'Recommendation'
colnames(wide)[colnames(wide) == '03 Future Use'] <- 'Future Use'
colnames(wide)[colnames(wide) == '04 First Choice'] <- 'First Choice'
colnames(wide)[colnames(wide) == '05 Customer service rating'] <- 'Customer Service'
colnames(wide)[colnames(wide) == '06 Tech Rating'] <- 'Tech'
colnames(wide)[colnames(wide) == '07 Quality Rating'] <- 'Quality'
colnames(wide)[colnames(wide) == '08 Value Rating'] <- 'Value'
colnames(wide)[colnames(wide) == '09 Ease of Acquisition Rating'] <- 'Ease of Acquisition'
colnames(wide)[colnames(wide) == '10 Satisfaction Rating'] <- 'Satisfaction'
colnames(wide)[colnames(wide) == '13 Agency'] <- 'Agency'
colnames(wide)[colnames(wide) == '14 Areas'] <- 'Area'
colnames(wide)[colnames(wide) == '15 Position'] <- 'Position'
colnames(wide)[colnames(wide) == '16 Zip Code'] <- 'Zip'

wide$Recommendation <- as.numeric(wide$Recommendation)
wide$`Future Use` <- as.numeric(wide$`Future Use`)
wide$`First Choice` <- as.numeric(wide$`First Choice`)
wide$Loyalty <- rowMeans(subset(wide,select=c(Recommendation,`Future Use`,`First Choice`)),na.rm=T)

#####Agency Cleanup#####
wide$`13a Other Agency` <- tolower(wide$`13a Other Agency`)
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(judic)|(court)|(probation)|(trial)',wide$`13a Other Agency`),'Department of Justice')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(eeoc)|(equal employment)',wide$`13a Other Agency`),'Equal Employment Opportunity Commission')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(interior)',wide$`13a Other Agency`),'Department of Interior')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(epa)',wide$`13a Other Agency`),'Environmental Protection Agency')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(energy)',wide$`13a Other Agency`),'Department of Energy')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(gsa)',wide$`13a Other Agency`),'General Services Administration')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(defense logistics)|(army corps)|(coast guard)|(defense)|(dla)|(military)|(usace)',wide$`13a Other Agency`),'Department of Defense')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & wide$`13a Other Agency` %in% c('disa','dod'),
                       'Department of Defense')
wide$Agency <- replace(wide$Agency,wide$Agency=='Office of Secretary of Defense','Department of Defense')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(congress)|(legislative)|(government publish)|(house of rep)|(gao)',wide$`13a Other Agency`),'Congress')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & wide$`13a Other Agency` =='u.s.g.p.o','Congress')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(labor relations)|(broadcasting)|(consumer product)|(railroad)|(railrod)|(merit systems)|(national science foundation)|(community service)|(chemical safety)|(federal deposit)|(federal housing)|(federal mediation)|(fhfa)|(fmcs)|(national labor)|(national credit)|(national transportation safety)|(office of personnel management)|(office of government ethics)|(social security)|(special counsel)|(usaid)|(international develop)|(cpsc)'
                                                                ,wide$`13a Other Agency`),'Independent Government Agencies')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & wide$`13a Other Agency` %in% c('tva','fcc','cpsc','nlrb','cncs','ntsb','rrb','bbg','afrh','cfpb','fdic','fema','ncua','osc','state'),
                       'Independent Government Agencies')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & wide$`13a Other Agency` %in% c('dhs','homeland security investigations'),'Department of Homeland Security')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(tsa)|(customs)',wide$`13a Other Agency`),'Department of Homeland Security')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(exim)|(export-import)',wide$`13a Other Agency`),'Export-Import Bank')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(president)',wide$`13a Other Agency`),'Office of Management and Budget')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(marine)',wide$`13a Other Agency`),'Marine Corps')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(pipeline)',wide$`13a Other Agency`),'Department of Transportation')

wide$`13a Other Agency` <- NULL

####Adding servicing region for AAS####
wide <- left_join(wide,aas.region[c(2:3)],by=c('FeedbackID'='Survey.Feedback.ID'))
rm(aas.region)

#####Return to long, keeping additional demographics and adding state/region#####
data16 <- melt(wide, id.vars = c("FeedbackID","RespondentID","Program","FY","Agency","Area","Position","Zip","Loyalty")) 
colnames(data16)[colnames(data16) == 'variable'] <- 'Question'
colnames(data16)[colnames(data16) == 'value'] <- 'Response'
data16 <- filter(data16, Response != '' & Response != 'N/A')
rm(df16)

region.zip <- read.csv('zip_code_database.csv',colClasses='character')
region.zip$zip <- str_pad(region.zip$zip,5,pad='0')
data16$Zip <- str_pad(data16$Zip,5,pad='0')
data16 <- left_join(data16,region.zip, by=c('Zip'='zip'))
data16$region <- as.factor(data16$region)
colnames(data16)[colnames(data16) == 'state'] <- 'State'
colnames(data16)[colnames(data16) == 'region'] <- 'Region'

####Clean Area and ITC Questions####
data16$Area <- replace(data16$Area,grepl("^(Administrative Support)",data16$Area),'Administrative Support')

data16$Question <- as.character(data16$Question)
data16$Question <- replace(data16$Question,data16$Program %in% c('GWACs','IT Schedule 70','NITCP','NSP','USAccess') & (grepl('^(17a)',data16$Question)|data16$Question=='17 Price Rating'),'Price Compared to Open Market')
data16$Question <- replace(data16$Question,data16$Program %in% c('GWACs','IT Schedule 70','NITCP','NSP','USAccess') & (grepl('^(17b)',data16$Question)|data16$Question=='18 Value Rating'),'Value Compared to Price')
data16$Question <- replace(data16$Question,data16$Program %in% c('GWACs','IT Schedule 70','NITCP','NSP','USAccess') & (grepl('^(18a)',data16$Question)|data16$Question=='19 Program Understanding Rating'),'Understands Challenges')
data16$Question <- replace(data16$Question,data16$Program %in% c('GWACs','IT Schedule 70','NITCP','NSP','USAccess') & (grepl('^(18b)',data16$Question)|data16$Question=='20 Program Response Rating'),'Responsive to Trends')
data16$Question <- replace(data16$Question,data16$Program %in% c('GWACs','IT Schedule 70','NITCP','NSP','USAccess') & (grepl('^(18c)',data16$Question)|data16$Question=='21 Program Innovation Rating'),'Innovative')

#####Initial FY15 Load#####
#Load csv, remove unnecessary columns
#df15 <- read.csv("CLS FY15 raw.csv", colClasses='character')
Qnames <- read.csv("CLS15 Question Mapping.csv", colClasses='character')
df15 <- df15 %>% mutate(Response=ifelse(Number.Response=='',Text.Response,Number.Response))
df15 <- df15[c(1:4,7)]
df15 <- left_join(df15,Qnames,by=c("Question..Question.Name"="Question.Name","Survey..Survey.Name"="Survey..Survey.Name"))
rm(Qnames)
df15 <- df15[c(1:4,6,5)]
colnames(df15) <- c("FeedbackID","ContactID","Program","QuestionNum","Question","Response")

#Remove blank responses and clean Program
df15 <- filter(df15, Response != '' & Question != '')
df15$FY <- 2015
df15$Program <- sub('Cust Loyalty - ','',df15$Program)
df15$Program <- sub('Cust Loyalty-','',df15$Program)

#Remove screened surveys
screen <- df15 %>% filter(QuestionNum == 'S1' & Response %in% c('No','Don?t Know','Not Applicable','Neither'))
df15 <- filter(df15, !(FeedbackID %in% screen$FeedbackID))
rm(screen)

#####Question Cleanup#####
#Invalid multibyte string correction
#filter(df15,!validEnc(df15$Question)) %>% View()
df15 <- df15 %>% mutate(Question=replace(df15$Question,QuestionNum=='Q8' & !validEnc(df15$Question),'How satisfied are you with')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,QuestionNum=='S1' & !validEnc(df15$Question),'Have you purchased any products or services from Retail Operations')) %>% as.data.frame()

#Duplicate responses from the same people... Yeah I have no idea how it happened either
df15 <- df15 %>% mutate(Response = replace(df15$Response,QuestionNum == 'Q25' &
                                            FeedbackID %in% c('a0T3000000BndJK','a0T3000000BngLD'),4.5)) %>% as.data.frame()
df15 <- unique(df15)

#Cleaning up PPM
df15 <- filter(df15,!(Program=='PPM' & grepl("^(How satisfied are you with)",df15$Question) & Response=='0.00'))

#Question mapping
df15$Question <- replace(df15$Question,grepl("(federal agency)",df15$Question),'Agency')
df15$Question <- replace(df15$Question,grepl("(position)",df15$Question),'Position')
df15$Question <- replace(df15$Question,grepl("(zip code)",df15$Question),'Zip')

temp <- filter(df15,Program=='PPM' & grepl("^(How satisfied are you with)",df15$Question))
temp$Question <- replace(temp$Question,grepl("^(How satisfied are you with)",temp$Question),'Satisfaction')
temp <- temp %>% group_by(FeedbackID,ContactID,Program,FY,Question) %>% summarise(Response=mean(as.numeric(Response))) %>% as.data.frame()
temp$QuestionNum <- 'Q99'
df15 <- rbind(df15,temp)
rm(temp)

df15$Question <- replace(df15$Question,grepl("^(I am familiar with the products)",df15$Question),'Familiarity')
df15$Question <- replace(df15$Question,grepl("^(Customer [sS]ervice)",df15$Question),'Customer Service')
df15$Question <- replace(df15$Question,grepl("^(Technology and [sS]ystems)",df15$Question),'Tech')
df15$Question <- replace(df15$Question,grepl("^(Quality of products)",df15$Question),'Quality')
df15$Question <- replace(df15$Question,grepl("^(Value of products)",df15$Question),'Value')
df15$Question <- replace(df15$Question,grepl("^(Ease of acquiring products)",df15$Question),'Ease of Acquisition')
df15$Question <- replace(df15$Question,grepl("^(How satisfied are you with)",df15$Question) & !grepl("(OS3)|(MRO)|(JanSan)|(Network Services)|(Sales)|(Utilization)",df15$Question),'Satisfaction')
df15$Question <- replace(df15$Question,grepl("^(How likely are you to recommend)",df15$Question) & !grepl("(OS3)|(MRO)|(JanSan)",df15$Question),'Recommendation')
df15$Question <- replace(df15$Question,grepl("^(If you had the option, how likely would you be to use)",df15$Question) & !grepl("(OS3)|(MRO)|(JanSan)",df15$Question),'Future Use')
df15$Question <- replace(df15$Question,grepl("^(If you had the option, how likely would you be to consider)",df15$Question) & !grepl("(OS3)|(MRO)|(JanSan)",df15$Question),'First Choice')

df15$Question <- replace(df15$Question,grepl("(FAS is focused on the quality)",df15$Question),'GSA FAS is focused on the quality of the technology tools')
df15$Question <- replace(df15$Question,grepl("^(Federal Fleet Management System)",df15$Question),'Federal Fleet Management System')
df15$Question <- replace(df15$Question,grepl("^(Flexibility)",df15$Question),'Flexibility')
df15$Question <- replace(df15$Question,grepl("^([fF]ollow)",df15$Question),'Followed through on promised actions')
df15$Question <- replace(df15$Question,grepl("^(GSA Billing Systems)",df15$Question),'GSA Billing Systems (SpeedPay and VCSS)')
df15$Question <- replace(df15$Question,grepl("(has employees who are concerned)",df15$Question),'GSA FAS has employees who are concerned about my needs')
df15$Question <- replace(df15$Question,grepl("(provides innovative ways)",df15$Question),'GSA FAS provides innovative ways to help procure or acquire products or services')
df15$Question <- replace(df15$Question,grepl("(stands behind the services)",df15$Question),'GSA FAS stands behind the services that it offers')
df15$Question <- replace(df15$Question,grepl("(understands my upcoming challenges)",df15$Question),'GSA FAS understands my upcoming challenges and works with me to tailor solutions')
df15$Question <- replace(df15$Question,grepl("(responsive to new trends)",df15$Question),'GSA FAS is responsive to new trends in the market and aligns its solutions based upon demand')
df15$Question <- replace(df15$Question,grepl("^(Acquisition)",df15$Question),'Acquisition expertise')
df15$Question <- replace(df15$Question,grepl("^(Conve)",df15$Question),'Convenience')
df15$Question <- replace(df15$Question,grepl("^([dD]emonstrated knowledge)",df15$Question),'Demonstrated Knowledge')
df15$Question <- replace(df15$Question,grepl("^(Diversity of products)",df15$Question),'Diversity of products and or services')
df15$Question <- replace(df15$Question,grepl("^(Have you purchased)",df15$Question),'Have you purchased any products or services from or using us in the last 12 months?')
df15$Question <- replace(df15$Question,grepl("^(Have you visited the).*(website)",df15$Question),'Have you visited our website in the last 12 months?')
df15$Question <- replace(df15$Question,grepl("^(Helps meet sustainability)",df15$Question),'Helps meet sustainability and environmental requirements or federal mandates')
df15$Question <- replace(df15$Question,grepl("^([hH]ow important is it to take training)",df15$Question),'How important is it to take training to keep up with IT trends and acquisitions?')
df15$Question <- replace(df15$Question,grepl("^(If you could make one change)",df15$Question),'If you could make one change to improve the program, what would it be?')
df15$Question <- replace(df15$Question,grepl("^(I was able to accomplish)",df15$Question),'I was able to accomplish what I wanted to on the website')
df15$Question <- replace(df15$Question,grepl("^(If you knew about)",df15$Question),'If you knew about the training, would you be likely to sign up?')
df15$Question <- replace(df15$Question,grepl("(IT Customer Service Center)",df15$Question),'In the past 12 months, have you contacted the GSA IT Customer Service Center for an IT related issue or inquiry?')
df15$Question <- replace(df15$Question,grepl("^(In the past 12 months, have you taken)",df15$Question),'In the past 12 months, have you taken GSA FAS IT acquisition training?')
df15$Question <- replace(df15$Question,grepl("^(In the past 12 months, have you directly)",df15$Question),'In the past 12 months, have you directly interacted with a GSA rep?')
df15$Question <- replace(df15$Question,grepl("^(Knowledge about the Professional Services category)",df15$Question),'Demonstrated knowledge about the Professional Services category')
df15$Question <- replace(df15$Question,grepl("^(Knowledge)",df15$Question),'Demonstrated Knowledge')
df15$Question <- replace(df15$Question,grepl("^(Please indicate which GSA)",df15$Question),'Please indicate which GSA FAS category you primarily purchase from (select one)')
df15$Question <- replace(df15$Question,grepl("^(Please [pP]rovide any additional comments)",df15$Question),'Please provide any additional comments')
df15$Question <- replace(df15$Question,grepl("^(Please provide information)",df15$Question),'Please provide information on how we can improve')
df15$Question <- replace(df15$Question,grepl("^(Please specify the other reason)",df15$Question),'Please specify the other reason that would prevent you from asking for a price reduction')
df15$Question <- replace(df15$Question,grepl("^([pP]rovided clear answers to my questions)",df15$Question),'Provided clear answers to my questions')
df15$Question <- replace(df15$Question,grepl("^([sS]atisfactorily resolved my issue)",df15$Question),'Satisfactorily resolved my issue or inquiry in a timely manner')
df15$Question <- replace(df15$Question,grepl("^([sS]aves [tT]ime)",df15$Question),'Saves time')
df15$Question <- replace(df15$Question,grepl("^([sS]howed m)",df15$Question),'Showed me courtesy and respect')
df15$Question <- replace(df15$Question,grepl("^(The Acquisition Gateway is a useful tool)",df15$Question),'The Acquisition Gateway is a useful tool for me to gather market research and information, connect with colleagues and experts, and complete data-driven acquisition confidently and effectively')
df15$Question <- replace(df15$Question,grepl("^(The language on the website)",df15$Question),'The language on the website was clear')
df15$Question <- replace(df15$Question,grepl("^(The total price paid compared)",df15$Question),'The total price paid compared to price of the product or service on the open market')
df15$Question <- replace(df15$Question,grepl("^(The value received)",df15$Question),'The value received in comparison to the price paid')
df15$Question <- replace(df15$Question,grepl("^(The website layout made it easy to find what I was)",df15$Question),'The website layout made it easy to find what I was looking for')
df15$Question <- replace(df15$Question,grepl("^(The website provided all the information)",df15$Question),'The website provided all the information I wanted')
df15$Question <- replace(df15$Question,grepl("^(Thinking about those items you order from Non)",df15$Question),'Thinking about those items you order from Non-IT GSA Multiple Award Schedules, for what size orders do you typically ask for price reductions?')
df15$Question <- replace(df15$Question,grepl("^(Timeliness of the delivery)",df15$Question),'Timeliness of the delivery')
df15$Question <- replace(df15$Question,grepl("^([uU]nderst)",df15$Question),'Understood my needs')
df15$Question <- replace(df15$Question,grepl("^(Usefulness of the information)",df15$Question),'Usefulness of the information provided')
df15$Question <- replace(df15$Question,grepl("^(Which of the following reasons)",df15$Question),'Which of the following reasons, if any, would prevent you from asking for a price reduction?')


#####Data cleanup#####
df15$temp <- substr(df15$QuestionNum,0,1)
df15$temp2 <- gsub('.[0-9]+','',df15$QuestionNum)
df15$QuestionNum <- gsub('[^0-9]','',df15$QuestionNum)
df15$temp3 <- gsub('[^0-9]','',df15$QuestionNum)
df15$QuestionNum <- str_pad(df15$QuestionNum,2,pad='0',side='left')
df15$QuestionNum <- paste(df15$temp,df15$QuestionNum,df15$temp2,sep='')
df15$Question <- paste(df15$QuestionNum,df15$Question)

df15$Question <- replace(df15$Question,grepl("(Agency)$",df15$Question),'Agency')
df15$Question <- replace(df15$Question,grepl("(Position)$",df15$Question),'Position')
df15$Question <- replace(df15$Question,grepl("(Zip)$",df15$Question),'Zip')

df15$Question <- replace(df15$Question,grepl("(Familiarity)$",df15$Question),'Familiarity')
df15$Question <- replace(df15$Question,grepl("(Customer Service)$",df15$Question),'Customer Service')
df15$Question <- replace(df15$Question,grepl("(Tech)$",df15$Question),'Tech')
df15$Question <- replace(df15$Question,grepl("(Quality)$",df15$Question),'Quality')
df15$Question <- replace(df15$Question,grepl("(Value)$",df15$Question),'Value')
df15$Question <- replace(df15$Question,grepl("(Ease of Acquiring)$",df15$Question),'Ease of Acquiring')
df15$Question <- replace(df15$Question,grepl("(Satisfaction)$",df15$Question),'Satisfaction')
df15$Question <- replace(df15$Question,grepl("(Recommendation)$",df15$Question),'Recommendation')
df15$Question <- replace(df15$Question,grepl("(Future Use)$",df15$Question),'Future Use')
df15$Question <- replace(df15$Question,grepl("(First Choice)$",df15$Question),'First Choice')


df15 <- df15 %>% mutate(Question=replace(df15$Question,Program %in% c('Automotive Survey','Smartpay Survey') & QuestionNum=='Q44','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program %in% c('MAS Administrative and Office Supplies','PPM') & QuestionNum=='Q50','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Assisted Acquisition Services' & QuestionNum=='Q26','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Domestic Delivery Service 3' & QuestionNum=='Q21a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Fleet Survey' & QuestionNum=='Q70','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Freight Management Program' & QuestionNum=='Q26a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='GWAC' & QuestionNum=='Q67','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='IT70' & QuestionNum=='Q82a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='MAS Facilities Maintenace' & QuestionNum=='Q41a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='MAS Gen Supply' & QuestionNum=='Q53','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='MAS Integrated Workplace Acquisition' & QuestionNum=='Q41','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='MAS Management Services' & QuestionNum=='Q75','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='NITCP' & QuestionNum=='Q61','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Retail Operations' & QuestionNum=='Q28a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='SCM' & QuestionNum=='Q38','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='USAccess' & QuestionNum=='Q31','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='WITS3 Survey' & QuestionNum=='Q84','Other Agency')) %>% as.data.frame()

df15 <- df15 %>% mutate(Question=replace(df15$Question,Program %in% c('Automotive Survey','Smartpay Survey') & QuestionNum=='Q44','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program %in% c('MAS Administrative and Office Supplies','PPM') & QuestionNum=='Q50','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Assisted Acquisition Services' & QuestionNum=='Q26','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Domestic Delivery Service 3' & QuestionNum=='Q21a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Fleet Survey' & QuestionNum=='Q70','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Freight Management Program' & QuestionNum=='Q26a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='GWAC' & QuestionNum=='Q67','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='IT70' & QuestionNum=='Q82a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='MAS Facilities Maintenace' & QuestionNum=='Q41a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='MAS Gen Supply' & QuestionNum=='Q53','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='MAS Integrated Workplace Acquisition' & QuestionNum=='Q41','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='MAS Management Services' & QuestionNum=='Q75','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='NITCP' & QuestionNum=='Q61','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='Retail Operations' & QuestionNum=='Q28a','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='SCM' & QuestionNum=='Q38','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='USAccess' & QuestionNum=='Q31','Other Agency')) %>% as.data.frame()
df15 <- df15 %>% mutate(Question=replace(df15$Question,Program =='WITS3 Survey' & QuestionNum=='Q84','Other Agency')) %>% as.data.frame()

df15$temp <- NULL
df15$temp2 <- NULL
df15$temp3 <- NULL
df15$QuestionNum <- NULL

#####Convert to wide and add Loyalty#####
wide <- dcast(df15, FeedbackID + ContactID + Program + FY ~ Question, value.var = "Response")
wide$ContactID <- as.character(wide$ContactID)
wide <- left_join(wide,Respondent,by='ContactID')
wide$ContactID <- NULL
rm(Respondent)

wide$Recommendation <- as.numeric(wide$Recommendation)
wide$`Future Use` <- as.numeric(wide$`Future Use`)
wide$`First Choice` <- as.numeric(wide$`First Choice`)
wide$Loyalty <- rowMeans(subset(wide,select=c(Recommendation,`Future Use`,`First Choice`)),na.rm=T)

#####Agency Cleanup#####
wide$`Other Agency` <- tolower(wide$`Other Agency`)
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(judic)|(court)|(probation)|(trial)',wide$`Other Agency`),'Department of Justice')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(eeoc)|(equal employment)',wide$`Other Agency`),'Equal Employment Opportunity Commission')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(interior)|(doi)',wide$`Other Agency`),'Department of Interior')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(usda)',wide$`Other Agency`),'Department of Agriculture')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(epa)|(environmental protection)',wide$`Other Agency`),'Environmental Protection Agency')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(energy)',wide$`Other Agency`),'Department of Energy')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(revenue)|(treasury)',wide$`Other Agency`),'Department of Treasury')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(gsa)',wide$`Other Agency`),'General Services Administration')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(dod)|(tional geospatial)|(defense logistics)|(army corps)|(corps of eng)|(coast guard)|(defense)|(dla)|(military)|(usace)',wide$`Other Agency`),'Department of Defense')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & wide$`Other Agency` %in% c('disa','dcma','dodig','dfas','dia','mda','nga'),'Department of Defense')
wide$Agency <- replace(wide$Agency,wide$Agency=='Office of Secretary of Defense','Department of Defense')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(congress)|(legislative)|(government publish)|(house of rep)|(gao)',wide$`Other Agency`),'Congress')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & wide$`Other Agency` =='u.s.g.p.o','Congress')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(nasa)|(labor relations)|(federal communications)|(selective service)|(broadcasting)|(consumer product)|(railroad)|(railrod)|(merit systems)|(national science foundation)|(community service)|(nccc)|(chemical safety)|(federal deposit)|(federal housing)|(federal mediation)|(fhfa)|(fmcs)|(national labor)|(national credit)|(national transportation safety)|(office of personnel management)|(office of government ethics)|(social security)|(special counsel)|(usaid)|(international develop)|(cpsc)|(retirement home)|(central intelligence)'
                                                                ,wide$`Other Agency`),'Independent Government Agencies')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & wide$`Other Agency` %in% c('tva','fcc','cpsc','nlrb','cncs','ntsb','rrb','bbg','afrh','cfpb','fdic','fema','ncua','osc','state','ssa'),
                       'Independent Government Agencies')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & wide$`Other Agency` %in% c('dhs','homeland security investigations'),'Department of Homeland Security')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(tsa)|(customs)',wide$`Other Agency`),'Department of Homeland Security')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(exim)|(export-import)',wide$`Other Agency`),'Export-Import Bank')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(president)',wide$`Other Agency`),'Office of Management and Budget')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(marine)',wide$`Other Agency`),'Marine Corps')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(secretary of the army)',wide$`Other Agency`),'Department of Army')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(air force)',wide$`Other Agency`),'Department of Air Force')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(pipeline)',wide$`Other Agency`),'Department of Transportation')
wide$Agency <- replace(wide$Agency,wide$Agency=='Other' & grepl('(vha)',wide$`Other Agency`),'Department of Veterans Affairs')

#####Convert back to long, add State and Region#####
data15 <- melt(wide, id.vars = c("FeedbackID","RespondentID","Program","FY","Agency","Position","Zip","Loyalty"))
data15$Area <- NA
colnames(data15)[colnames(data15) == 'variable'] <- 'Question'
colnames(data15)[colnames(data15) == 'value'] <- 'Response'
data15 <- filter(data15, Response != '' & Response != 'N/A')
rm(df15,wide)

region.zip <- read.csv('zip_code_database.csv',colClasses='character')
region.zip$zip <- str_pad(region.zip$zip,5,pad='0')
data15$Zip <- str_pad(data15$Zip,5,pad='0')
data15 <- left_join(data15,region.zip, by=c('Zip'='zip'))
data15$region <- as.factor(data15$region)
colnames(data15)[colnames(data15) == 'state'] <- 'State'
colnames(data15)[colnames(data15) == 'region'] <- 'Region'

#Program Alignment
data15$Program <- replace(data15$Program,data15$Program=='Assisted Acquisition Services','AAS')
data15$Program <- replace(data15$Program,data15$Program=='Domestic Delivery Service 3','DDS3')
data15$Program <- replace(data15$Program,data15$Program=='GWAC','GWACs')
data15$Program <- replace(data15$Program,data15$Program=='IT70','IT Schedule 70')
data15$Program <- replace(data15$Program,data15$Program=='Smartpay Survey','SmartPay')
data15$Program <- replace(data15$Program,data15$Program=='Retail Operations','RO')
data15$Program <- replace(data15$Program,grepl("^(MAS)",data15$Program),'AO')
data15$Program <- trimws(sub('Survey','',data15$Program))

#data %>% group_by(Program) %>% summarise() %>% arrange(as.character(Program)) %>% print(n=36)

#####Combine files and write CSV#####
data <- rbind(data16,data15)
data <- data[c(1:3,5:7,8,12:13,4,9:11)]
#write.csv(data,'CLS.csv',row.names=F)
