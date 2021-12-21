#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 13 15:41:40 2021

@author: stephencranney
"""

import os
import urllib
from urllib.error import HTTPError
import datetime
from urllib.request import Request, urlopen
from datetime import datetime
import re
import os
import sys
import string
import pandas as pd
import numpy as np
import copy
import time
import timeit
#import the Beautiful soup functions to parse the data returned from the website
from bs4 import BeautifulSoup
import socket
socket.gethostbyname("")

os.getcwd()


Gender=[]
Age=[]
Cohort=[]
Name=[]

#DF = pd.DataFrame(columns = ['Gender', 'Age', 'Cohort', 'Name'])


#https://history.churchofjesuschrist.org/overlandtravel/search?first-name=&last-name=&birth-year=&death-year=&page=5
#Problem is with two different company people.  they get two names and cohorts. Do dive into each indidvieual page to get around. 


for q in range(1605, 2929):    
    
    i_str= str(q)
    dsite= "https://history.churchofjesuschrist.org/overlandtravel/search?first-name=&last-name=&birth-year=&death-year=&page="+i_str
    page = urlopen(dsite)
    soup = BeautifulSoup(page)
    
    titleZ = soup.find_all("h2", {"class": "search-result__title"})
    for i in titleZ:
        
        try:
        
            #reset
            tempname= " "
            tempgender= " "
            tempcohort= " "
            tempage= " "
        
            tempurl=str(i.contents[0]) 
            tempurl2=re.findall(r'"([^"]*)"',tempurl)[0]
            esite= "https://history.churchofjesuschrist.org"+tempurl2
            page2= urlopen(esite)
            soup2 = BeautifulSoup(page2)
            tempgenderZ = soup2.find_all("dd", {"class": "info-block__value"})
            tempgender=tempgenderZ[2].text
        
            tempage2=soup2.find(string=re.compile("Age at Departure:"))
            tempage=re.findall(r'"([^"]*)"',tempurl)[0]
            tempage=re.findall('(?<=Age at Departure:).*',tempage2)[0]
        
            tempcohort2=soup2.find(string=re.compile("\(18"))
            tempcohort=re.findall('\((.*)\)',tempcohort2)[0]
        
            tempnameZ = soup2.find_all("h1", {"class": "page-title content-page-title"})
            tempname=tempnameZ[0].text
                        
            Age.append(tempage)
            Gender.append(tempgender)
            Name.append(tempname)
            Cohort.append(tempcohort)
            
        except (urllib.error.HTTPError, TypeError):
            
            tempname= " "
            tempgender= " "
            tempcohort= " "
            tempage= " "
            
            Age.append(tempage)
            Gender.append(tempgender)
            Name.append(tempname)
            Cohort.append(tempcohort)
        
   #     print(tempname)

DF = pd.DataFrame(
    {'Gender': Gender,
     'Age': Age, 
     'Cohort': Cohort,
     'Name': Name
    })
    
DF.dtypes

#Had to cut in half because of break in scrape, modify this according to how many rounds of scraping you want to do. 
    
DF.to_csv("/Users/stephencranney/Desktop/DF_1605_2929.csv")



df1=pd.read_csv("/Users/stephencranney/Desktop/DF_1605_2929.csv")  

df2=pd.read_csv("/Users/stephencranney/Desktop/DF_1605_2.csv")    

df=df1.append(df2, ignore_index=True)

df['Gender']=df['Gender'].astype(str)
df['Age']=df['Age'].astype(str)
df['Cohort']=df['Cohort'].astype(str)
df['Name']=df['Name'].astype(str)


df.drop_duplicates(inplace=True)


df['token']= df['Gender'] + "_" + df['Age'] + "_" + df['Cohort'] + df['Name']
df.drop_duplicates(subset=['token'], keep='last', inplace=True)

test=df['Age'].value_counts()
test2=df['Cohort'].value_counts()

df['Cohort_num'] = df.Cohort.str.extract(r'(\d+[.\d]*)')
test2=df['Cohort_num'].value_counts()

df.dropna(subset = ["Cohort_num"], inplace=True)

df.replace(to_replace='1865.', value='1865', inplace=True)

df['Cohort_num'] = df['Cohort_num'].astype(int)

df=df[df['Cohort_num']>1845]

df['Age'] = df['Age'].str.strip()

df['unknownage'] = np.where(df['Age']== 'Unknown', 1, 0)
df['infant'] = np.where(df['Age']== 'Infant', 1, 0)

df['unknownage'].value_counts()

df['Age'].replace('Unknown', np.NaN, inplace=True)
df['Age'].replace('Infant', np.NaN, inplace=True)

df_num= df[df['unknownage']==0]
df_num= df_num[df_num['infant']==0]

df_num['Age_num'] = df_num['Age'].astype(int)

#Remove children
df_num_adult=df_num[df_num['Age_num']>17]

test5=pd.crosstab(df_num_adult.Cohort_num, df_num_adult.Gender)


test6=pd.crosstab(df.infant, df.Gender)
test7=pd.crosstab(df.unknownage, df.Gender)



test6.to_csv('/Users/stephencranney/Desktop/test6.csv')

#Slightly more women who came over, but more men who have unknown ages. 


































  
    



