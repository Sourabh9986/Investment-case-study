#Check point 1

#Storing companies data in "companies" data frame.
companies<-read.delim("companies.txt",sep  = "\t", stringsAsFactors = FALSE, header=TRUE)

#Storing rounds2 data in "rounds2" data frame.
rounds2 <-read.csv("rounds2.csv",stringsAsFactors = FALSE,header = TRUE)

#Loading sqldf package for using sql query where needed
library(sqldf)

#Method 1 : Finding unique companies in rounds2 dataframe
length(unique(rounds2$company_permalink,na.rm = T))

#Method 2 : Finding unique companies in rounds2 dataframe using sqldf
sqldf("select count(distinct(company_permalink)) from rounds2")

#Method 1 : Finding unique companies in compnies dataframe
length(unique(companies$permalink,na.rm = T))

#Method 2 : Finding unique companies in compnies dataframe using sqldf
sqldf("select count(distinct(permalink)) from companies")

#Below command shows there are same number of unique parmalinks in companies table as that of number of rown in that table , hence it can be used as unique key
length(unique(companies$permalink,na.rm = T))

#Below query can be used to confirm the idea that permalink can be used as unique key in companies dataframe
sqldf("select permalink,count(permalink) from companies group by permalink having count(permalink)>1")

#Loading stringr package for usage of function like str_to_lower
library(stringr)

#using %in% infix operator for checking value of a variable of one data frame present in other , sum would give 0
# Hence all companies present in rounds2 are present in companies dataframe
sum(!str_to_lower(rounds2$company_permalink) %in% str_to_lower(companies$permalink))

#As permalink in both dataframe "companies" and "rounds2" differs in case sensitivity
#Changing case of permalink in both datasets
companies$permalink<-str_to_lower(companies$permalink)
rounds2$company_permalink<-str_to_lower(rounds2$company_permalink)

#merging two dataframes "companies" and "rounds2" on permalink
master_frame<-merge(rounds2,companies,by.x="company_permalink",by.y="permalink")
#Results in a data frame with 114949 rows and 15 columns

#CheckPoint2

#Storing average amount of funding per fund type in a dataframe
average.funding.per.type<-aggregate(raised_amount_usd ~ funding_round_type,data=master_frame,mean,na.rm = T)

#average funding amount of venture type
#Method 1 : Using data frame formed using aggregate function
average.funding.per.type[(average.funding.per.type$funding_round_type=="venture"),]
#Method 2 : using sqldf
sqldf("select avg(raised_amount_usd) from master_frame where funding_round_type=\'venture\' ")

#average funding amount of angel type
#Method 1 : Using data frame formed using aggregate function
average.funding.per.type[(average.funding.per.type$funding_round_type=="angel"),]
#Method 2 : using sqldf
sqldf("select avg(raised_amount_usd) from master_frame where funding_round_type=\'angel\' ")

#average funding amount of seed type
#Method 1 : Using data frame formed using aggregate function
average.funding.per.type[(average.funding.per.type$funding_round_type=="seed"),]
#Method 2 : using sqldf
sqldf("select avg(raised_amount_usd) from master_frame where funding_round_type=\'seed\' ")

#average funding amount of private equity type
#Method 1 : Using data frame formed using aggregate function
average.funding.per.type[(average.funding.per.type$funding_round_type=="private_equity"),]
#Method 2 : using sqldf
sqldf("select avg(raised_amount_usd) from master_frame where funding_round_type=\'private_equity\' ")

#Finding best investment type with in range of 5 million and 15 million
#Method 1 : using data frame formed from aggregate function
best_type<-average.funding.per.type[(average.funding.per.type$raised_amount_usd >=5000000 & average.funding.per.type$raised_amount_usd <= 15000000),]
#Method 2 : using sqldf
sqldf("select funding_round_type,avg(raised_amount_usd) from master_frame group by funding_round_type having avg(raised_amount_usd) between 5000000 and 15000000")

#Check point 3

#Loading dplyr package for using additional functions like distinct,arrange etc
library(dplyr)

#Finding top 9 countries with highest "totoal" investment for funding type venture 
#Method 1 : Using aggregate
# post aggregation arrange is used to arrange total amount in descending order
# head is used to fetch top 9 records of descending sorted data frame
top9<-head(arrange(aggregate(raised_amount_usd ~ country_code , 
                       data=master_frame[master_frame$funding_round_type=="venture" 
                                         & master_frame$country_code!="",],sum,na.rm=T),
                   desc(raised_amount_usd)),n=9)
#Method 2 : using sqldf
top9<-sqldf("select country_code,sum(raised_amount_usd) 
            from master_frame 
            where funding_round_type=\'venture\' and country_code!=\'\' 
            group by country_code 
            order by 2 desc limit 9")

#installing "pdftools" package to read pdf file , which is provided to inndicate countries
# having "English" as offcial language
library("pdftools")

#loading entire pdf file as simple text file
text<-pdf_text("Countries_where_English_is_an_official_language.pdf")

#loading "countrycode" package to get country name like "India" from country code like "Ind"
library(countrycode)

#Three top english speaking countries amount top9 countries present in top9 data frame
#Step 1 :countrycode function would fetch country name for country code present in "top9" dataframe
# This step is required since country name is present in pdf file and not country code. 
#Step 2 :str_detect would detect if that country "name" is present in pdf file which is 
# now converted to text format and stored in "text"
#Step 3 : Since countrycode() would give a vector of country names for given vector of country codes
# ifelse function is used which operates on each of the vector element.
#Step 4: Finally head function is used to fetch only top3 countires.
head(top9[ifelse(str_detect(text,pattern = countrycode(top9$country_code,
                                                       origin = "iso3c",
                                                       destination = "country.name")),
                 TRUE,FALSE),1],n=3)


#Check point 4

#loading tidyr package for to use separate function
library(tidyr)

#Extracting primary sector from each value of category_list variable
# Step 1: Dividing category_list column in master_frame into primary_sector and 
# storing data in a new data frame called master_frame_new
# Step 2: drop and fill are added to suppress warnings since mulltiple values will come on separating
# category_list by "|".
master_frame_new<-separate(master_frame,category_list,into=c("primary_sector"),
                           remove = F,sep="\\|",extra = "drop",fill = "right")

#Loading maapping.csv into R as mapping dataframe
mapping<-read.csv("mapping.csv",stringsAsFactors = FALSE,header = TRUE)

#mapping data set is in wide format , it will be easier to merge if its in longer format
#Converting wide format mapping to long format
mapping_temp<-gather(mapping,main_sector,value,
                     Automotive...Sports:Social..Finance..Analytics..Advertising)

#Cleaning data like removing additonal rows with value as zero 
mapping_temp2<-mapping_temp[!(mapping_temp$value==0),]
rm(mapping_temp)

#Preparing data for merging by removing additional value column which now has only one value as "1"
mapping<-mapping_temp2[,-3]
rm(mapping_temp2)

#Merging master_frame_new and mapping data frames , so that each primary sector gets 
#corrsponding main sector
master_frame<-merge(master_frame_new,mapping,by.x="primary_sector",by.y="category_list",all.x=T)
rm(master_frame_new)

#Check point 5

#Finding total investments and total count per main sector for USA and top funding type i.e "venture"
d1<-merge(merge(filter(master_frame,country_code=="USA",
                           funding_round_type=="venture",
                           raised_amount_usd>=5000000,raised_amount_usd<=15000000),
                    aggregate(raised_amount_usd ~ main_sector , 
                              data=filter(master_frame,country_code=="USA",
                                          funding_round_type=="venture",
                                          raised_amount_usd>=5000000,
                                          raised_amount_usd<=15000000),
                              sum,na.rm=T),by="main_sector",suffixes = c("","_total")),
              aggregate(raised_amount_usd ~ main_sector , 
                        data=filter(master_frame,country_code=="USA",
                                    funding_round_type=="venture",
                                    raised_amount_usd>=5000000,
                                    raised_amount_usd<=15000000),
                        length),by="main_sector",suffixes = c("","_count"))

#Using sql df
d1.1<-merge(filter(master_frame,country_code=="USA",
                 funding_round_type==best_type$funding_round_type,
                 raised_amount_usd>=5000000,raised_amount_usd<=15000000),
          sqldf("select main_sector,sum(raised_amount_usd) as \'raised_amount_usd_total\',
                count(raised_amount_usd) as \'raised_amount_usd_count\' from master_frame 
                where country_code=\'USA\' and funding_round_type=
                (select funding_round_type from best_type) 
                and raised_amount_usd between 5000000 
                and 15000000 group by main_sector "),by="main_sector")

#Finding total investments and total count per main sector for GBR and top funding type i.e "venture"
d2<-merge(merge(filter(master_frame,country_code=="GBR",
                       funding_round_type=="venture",
                       raised_amount_usd>=5000000,raised_amount_usd<=15000000),
                aggregate(raised_amount_usd ~ main_sector , 
                          data=filter(master_frame,country_code=="GBR",
                                      funding_round_type=="venture",
                                      raised_amount_usd>=5000000,
                                      raised_amount_usd<=15000000),
                          sum,na.rm=T),by="main_sector",suffixes = c("","_total")),
          aggregate(raised_amount_usd ~ main_sector , 
                    data=filter(master_frame,country_code=="GBR",
                                funding_round_type=="venture",
                                raised_amount_usd>=5000000,
                                raised_amount_usd<=15000000),
                    length),by="main_sector",suffixes = c("","_count"))

#Using sql df
d2.1<-merge(filter(master_frame,country_code=="GBR",
                 funding_round_type==best_type$funding_round_type,
                 raised_amount_usd>=5000000,raised_amount_usd<=15000000),
          sqldf("select main_sector,sum(raised_amount_usd) as \'raised_amount_usd_total\',
                count(raised_amount_usd) as \'raised_amount_usd_count\' from master_frame 
                where country_code=\'GBR\' and funding_round_type=
                (select funding_round_type from best_type) 
                and raised_amount_usd between 5000000 
                and 15000000 group by main_sector "),by="main_sector")

#Finding total investments and total count per main sector for IND and top funding type i.e "venture"
d3<-merge(merge(filter(master_frame,country_code=="IND",
                       funding_round_type=="venture",
                       raised_amount_usd>=5000000,raised_amount_usd<=15000000),
                aggregate(raised_amount_usd ~ main_sector , 
                          data=filter(master_frame,country_code=="IND",
                                      funding_round_type=="venture",
                                      raised_amount_usd>=5000000,
                                      raised_amount_usd<=15000000),
                          sum,na.rm=T),by="main_sector",suffixes = c("","_total")),
          aggregate(raised_amount_usd ~ main_sector , 
                    data=filter(master_frame,country_code=="IND",
                                funding_round_type=="venture",
                                raised_amount_usd>=5000000,
                                raised_amount_usd<=15000000),
                    length),by="main_sector",suffixes = c("","_count"))

#Using sql df
d3.1<-merge(filter(master_frame,country_code=="IND",
                 funding_round_type==best_type$funding_round_type,
                 raised_amount_usd>=5000000,raised_amount_usd<=15000000),
          sqldf("select main_sector,sum(raised_amount_usd) as \'raised_amount_usd_total\',
                count(raised_amount_usd) as \'raised_amount_usd_count\' from master_frame 
                where country_code=\'IND\' and funding_round_type=
                (select funding_round_type from best_type) 
                and raised_amount_usd between 5000000 
                and 15000000 group by main_sector "),by="main_sector",na.rm=T)

#Question 1:

#Total number of investments in USA
sum(unique(d1$raised_amount_usd_count),na.rm = T)

#Total number of investments in GBR
sum(unique(d2$raised_amount_usd_count),na.rm = T)

#Total number of investments in IND
sum(unique(d3$raised_amount_usd_count),na.rm = T)


#Question 2:

#Total amount of investment in USA
sum(unique(d1$raised_amount_usd_total),na.rm = T)

#Total amount of investment in GBR
sum(unique(d2$raised_amount_usd_total),na.rm = T)

#Total amount of investment in IND
sum(unique(d3$raised_amount_usd_total),na.rm = T)


#Question 3:

#Top sector (based on count of investments) in USA
d1[which.max(d1$raised_amount_usd_count),1]
#OR
d1[which.max(d1$raised_amount_usd_count==
               sort(unique(d1$raised_amount_usd_count),
                    na.last = F,decreasing = T)[1]),1]
#OR
arrange(d1, desc(raised_amount_usd_count))[1,1]

#Top sector (based on count of investments) in GBP
d2[which.max(d2$raised_amount_usd_count),1]
#OR
d2[which.max(d2$raised_amount_usd_count==
               sort(unique(d2$raised_amount_usd_count),
                    na.last = F,decreasing = T)[1]),1]
#OR
arrange(d2, desc(raised_amount_usd_count))[1,1]

#Top sector (based on count of investments) in IND
d3[which.max(d3$raised_amount_usd_count),1]
#OR
d3[which.max(d3$raised_amount_usd_count==
               sort(unique(d3$raised_amount_usd_count),
                    na.last = F,decreasing = T)[1]),1]
#OR
arrange(d3, desc(raised_amount_usd_count))[1,1]


#Question 4:

#Second-best sector (based on count of investments) in USA
d1[which.max(d1$raised_amount_usd_count==
               sort(unique(d1$raised_amount_usd_count),
                    na.last = F,decreasing = T)[2]),1]

#Second-best sector (based on count of investments) in GBR
d2[which.max(d2$raised_amount_usd_count==
               sort(unique(d2$raised_amount_usd_count),
                    na.last = F,decreasing = T)[2]),1]

#Second-best sector (based on count of investments) in IND
d3[which.max(d3$raised_amount_usd_count==
               sort(unique(d3$raised_amount_usd_count),
                    na.last = F,decreasing = T)[2]),1]



#Question 5:

#Third-best sector (based on count of investments) in USA
d1[which.max(d1$raised_amount_usd_count==
               sort(unique(d1$raised_amount_usd_count),
                    na.last = F,decreasing = T)[3]),1]

#Third-best sector (based on count of investments) in GBR
d2[which.max(d2$raised_amount_usd_count==
               sort(unique(d2$raised_amount_usd_count),
                    na.last = F,decreasing = T)[3]),1]

#Third-best sector (based on count of investments) in IND
d3[which.max(d3$raised_amount_usd_count==
               sort(unique(d3$raised_amount_usd_count),
                    na.last = F,decreasing = T)[3]),1]


#Question 6:

#Number of investments in the top sector in USA
sum(unique(d1$raised_amount_usd_count
           [d1$main_sector==d1[which.max(d1$raised_amount_usd_count==
                sort(unique(d1$raised_amount_usd_count),
               na.last = F,decreasing = T)[1]),1]]),na.rm = T)

#Number of investments in the top sector in GBR
sum(unique(d2$raised_amount_usd_count
           [d2$main_sector==d2[which.max(d2$raised_amount_usd_count==
                       sort(unique(d2$raised_amount_usd_count),
                        na.last = F,decreasing = T)[1]),1]]),na.rm = T)

#Number of investments in the top sector in IND
sum(unique(d3$raised_amount_usd_count
           [d3$main_sector==d3[which.max(d3$raised_amount_usd_count==
                            sort(unique(d3$raised_amount_usd_count),
                             na.last = F,decreasing = T)[1]),1]]),na.rm = T)


#Question 7:

#Number of investments in the second-best sector  in USA
sum(unique(d1$raised_amount_usd_count
           [d1$main_sector==d1[which.max(d1$raised_amount_usd_count==
                    sort(unique(d1$raised_amount_usd_count),
                     na.last = F,decreasing = T)[2]),1]]),na.rm = T)

#Number of investments in the second-best sector  in GBR
sum(unique(d2$raised_amount_usd_count
           [d2$main_sector==d2[which.max(d2$raised_amount_usd_count==
                    sort(unique(d2$raised_amount_usd_count),
                      na.last = F,decreasing = T)[2]),1]]),na.rm = T)

#Number of investments in the second-best sector  in IND
sum(unique(d3$raised_amount_usd_count
           [d3$main_sector==d3[which.max(d3$raised_amount_usd_count==
                     sort(unique(d3$raised_amount_usd_count),
                          na.last = F,decreasing = T)[2]),1]]),na.rm = T)



#Question 8:

#Number of investments in the third-best sector   in USA
sum(unique(d1$raised_amount_usd_count
           [d1$main_sector==d1[which.max(d1$raised_amount_usd_count==
                  sort(unique(d1$raised_amount_usd_count),
                   na.last = F,decreasing = T)[3]),1]]),na.rm = T)

#Number of investments in the third-best sector   in GBR
sum(unique(d2$raised_amount_usd_count
           [d2$main_sector==d2[which.max(d2$raised_amount_usd_count==
                   sort(unique(d2$raised_amount_usd_count),
                       na.last = F,decreasing = T)[3]),1]]),na.rm = T)

#Number of investments in the third-best sector   in IND
sum(unique(d3$raised_amount_usd_count
           [d3$main_sector==d3[which.max(d3$raised_amount_usd_count==
                        sort(unique(d3$raised_amount_usd_count),
                           na.last = F,decreasing = T)[3]),1]]),na.rm = T)


#Question 9:

#For the top sector count-wise company that received the highest investment in USA
arrange(d1[(d1$main_sector==d1[which.max(d1$raised_amount_usd_count==
       sort(unique(d1$raised_amount_usd_count),
        na.last = F,decreasing = T)[1]),1]),],
       desc(raised_amount_usd))$company_permalink[1]

#For the top sector count-wise company that received the highest investment in GBR
arrange(d2[(d2$main_sector==d2[which.max(d2$raised_amount_usd_count==
          sort(unique(d2$raised_amount_usd_count),
           na.last = F,decreasing = T)[1]),1]),],
        desc(raised_amount_usd))$company_permalink[1]

##For the top sector count-wise company that received the highest investment in IND
arrange(d3[(d3$main_sector==d3[which.max(d3$raised_amount_usd_count==
       sort(unique(d3$raised_amount_usd_count),
        na.last = F,decreasing = T)[1]),1]),],
        desc(raised_amount_usd))$company_permalink[1]


#Question 10:

#For second best sector, company that received the highest investment in USA
arrange(d1[(d1$main_sector==d1[which.max(d1$raised_amount_usd_count==
           sort(unique(d1$raised_amount_usd_count),
            na.last = F,decreasing = T)[2]),1]),],
        desc(raised_amount_usd))$company_permalink[1]

#For second best sector, company that received the highest investment in GBR
arrange(d2[(d2$main_sector==d2[which.max(d2$raised_amount_usd_count==
         sort(unique(d2$raised_amount_usd_count),
          na.last = F,decreasing = T)[2]),1]),],
        desc(raised_amount_usd))$company_permalink[1]

#For second best sector, company that received the highest investment in IND
arrange(d3[(d3$main_sector==d3[which.max(d3$raised_amount_usd_count==
          sort(unique(d3$raised_amount_usd_count),
           na.last = F,decreasing = T)[2]),1]),],
        desc(raised_amount_usd))$company_permalink[1]
