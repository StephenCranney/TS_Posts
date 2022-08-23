#National Crime Victimization Survey, data from ICPSR
#https://www.icpsr.umich.edu/web/NACJD/series/95
#Incident and personal-level file, load 38321-0003-Data.rda and 38321-0002-Data.rda

options(tidyverse.quiet = TRUE) 
library(tidyverse) 
library(srvyr)

df = data.frame()

table(da38321.0003$MSAIND)

varlist <- c("(01) Atlanta-Sandy Springs-Roswell, GA", "(02) Austin-Round Rock, TX", "(03) Baltimore-Columbia-Towson, MD", "(04) Birmingham-Hoover, AL", "(05) Boston-Cambridge-Newton, MA-NH", 
             "(06) Buffalo-Cheektowaga-Niagara Falls, NY", "(07) Charlotte-Concord-Gastonia, NC-SC", "(08) Chicago-Naperville-Elgin, IL-IN-WI", "(09) Cincinnati, OH-KY-IN", "(10) Cleveland-Elyria, OH", 
             "(11) Columbus, OH", "(12) Dallas-Fort Worth-Arlington, TX", "(13) Denver-Aurora-Lakewood, CO", "(14) Detroit-Warren-Dearborn, MI", "(15) Hartford-West Hartford-East Hartford, CT", 
             "(16) Houston-The Woodlands-Sugar Land, TX", "(17) Indianapolis-Carmel-Anderson, IN", "(18) Jacksonville, FL", "(19) Kansas City, MO-KS", "(20) Las Vegas-Henderson-Paradise, NV",
             "(21) Los Angeles-Long Beach-Anaheim, CA", "(22) Louisville/Jefferson County, KY-IN", "(23) Memphis, TN-MS-AR", "(24) Miami-Fort Lauderdale-West Palm Beach, FL", 
             "(25) Milwaukee-Waukesha-West Allis, WI", "(26) Minneapolis-St. Paul-Bloomington, MN-WI", "(27) Nashville-Davidson-Murfreesboro-Franklin, TN", "(28) New Orleans-Metairie, LA",
             "(29) New York-Newark-Jersey City, NY-NJ-PA", "(30) Oklahoma City, OK", "(31) Orlando-Kissimmee-Sanford, FL", "(32) Philadelphia-Camden-Wilmington, PA-NJ-DE-MD", 
             "(33) Phoenix-Mesa-Scottsdale, AZ", "(34) Pittsburgh, PA", "(35) Portland-Vancouver-Hillsboro, OR-WA", "(36) Providence-Warwick, RI-MA", "(37) Raleigh, NC", 
             "(38) Richmond, VA", "(39) Riverside-San Bernardino-Ontario, CA", "(40) Rochester, NY", "(41) Sacramento-Roseville-Arden-Arcade, CA", "(42) St. Louis, MO-IL", "(43) Salt Lake City, UT", 
             "(44) San Antonio-New Braunfels, TX", "(45) San Diego-Carlsbad, CA", "(46) San Francisco-Oakland-Hayward, CA", "(47) San Jose-Sunnyvale-Santa Clara, CA", "(48) Seattle-Tacoma-Bellevue, WA",
             "(49) Tampa-St. Petersburg-Clearwater, FL", "(50) Tucson, AZ", "(51) Virginia Beach-Norfolk-Newport News, VA-NC", "(52) Washington-Arlington-Alexandria, DC-VA-MD-WV")

da38321.0003$SERIESWGT_NUM<-ifelse(da38321.0003$SERIESWGT=="(01) Not a series crime", 1, 
                                   ifelse(da38321.0003$SERIESWGT=="(06) Six", 6,   
                                          ifelse(da38321.0003$SERIESWGT=="(07) Seven", 7, 
                                                 ifelse(da38321.0003$SERIESWGT=="(08) Eight", 8,   
                                                        ifelse(da38321.0003$SERIESWGT=="(09) Nine", 9, 
                                                               ifelse(da38321.0003$SERIESWGT=="(10) Ten or more", 10, NA))))))  
table(da38321.0003$SERIESWGT_NUM, da38321.0003$SERIESWGT)

########

models <- lapply(varlist, function(x) {
  
# Step 1 - Identify records with victimization characteristics of interest

ex1_incident <- da38321.0003 %>%
  
#  select(YEARQ, IDPER, MSAIND, TOC_RECODE, V4022, WGTVIC, SERIESWGT, SERIESWGT_NUM) %>% filter(MSAIND=='(29) New York-Newark-Jersey City, NY-NJ-PA') %>%  TO REPLICATE 
  select(YEARQ, IDPER, MSAIND, TOC_RECODE, V4022, WGTVIC, SERIESWGT, SERIESWGT_NUM) %>% filter(MSAIND==x) %>%
  
  mutate(
    # Create an indicator of violent crime %>%
    
    # Different Violent to replicate example in codebook
    #CHANGE FOR REPLICATE
#    VIOLENT= TOC_RECODE %in% c('(51) Completed rape/sexual assault', '(52) Attempted rape/sexual assault', '(53) Attempted/completed robbery with injury from serious assault', 
#                               '(54) Attempted/completed robbery with injury from minor assault', '(55) Attempted/completed robbery without injury', '(56) Attempted/completed aggravated assault',
#                               '(57) Threatened assault with weapon', '(58) Simple assault completed with injury', '(59) Assault without weapon without injury',
#                               '(60) Threatened rape/sexual assault', '(61) Verbal threat of assault'), # Create an indicator for crimes that occurred outside the US 
    
    VIOLENT= TOC_RECODE %in% c('(51) Completed rape/sexual assault', '(52) Attempted rape/sexual assault', '(60) Threatened rape/sexual assault'), # Create an indicator for crimes that occurred outside the US 
    EXCLUDE_OUTUS=(V4022 == 1)
  )

###############

#Test
da38321.0003$RAPE<-ifelse((da38321.0003$TOC_RECODE=='(51) Completed rape/sexual assault') | (da38321.0003$TOC_RECODE=='(52) Attempted rape/sexual assault') | (da38321.0003$TOC_RECODE=='(60) Threatened rape/sexual assault'), 1, 0)
test2<-table(da38321.0003$RAPE, da38321.0003$MSAIND)

# Step 2 - Create a victimization summary file
ex1_victimization_summary <-
  ex1_incident %>%
  # Exclude crimes occurring outside the US and subset file to crime type of interest
filter(!EXCLUDE_OUTUS, VIOLENT) %>% group_by(YEARQ, IDPER) %>% summarise(WGTVIC2=mean(WGTVIC),
                                                                         VIOLENT=sum(VIOLENT*SERIESWGT_NUM))

# Step 3 - Merge victimization summary file onto appropriate file
ex1_person <- da38321.0002 %>%
  select(YEAR, YEARQ, IDPER, MSAIND, WGTPER, starts_with("PERREPWGT"), V3018) %>%
#  filter(MSAIND=='(43) Salt Lake City, UT')
  filter(MSAIND==x)
 
ex1_merged_file <- ex1_person %>%
  left_join(ex1_victimization_summary, by=c("YEARQ", "IDPER")) %>%
  # The incident count variable is missing for persons not included on the # victimization summary file so they are set to 0 
  mutate(VIOLENT=if_else(is.na(VIOLENT), 0, VIOLENT))

# Step 4 - Calculate the victimization adjustment factor
  ex1_analysis_file <- ex1_merged_file %>% mutate(
    # calculate the adjustment factor
    ADJINC_WT=if_else(!is.na(WGTVIC2), WGTVIC2/WGTPER, 0),
    # create an analysis variable equal to the victimization count # multiplied by the adjustment factor and 1,000 
    ANALYSISVAR=VIOLENT*ADJINC_WT*1000
  )
ex1_des <- ex1_analysis_file %>% as_survey_rep(type="JK1",
                                               repweights=starts_with("PERREPWGT"), weights=WGTPER,
                                               scale=0.96666667)
# Step 5- Calculate the victimization rate
ex1_des %>%
# for replicate change  
#  filter(between(YEAR, 2014, 2015)) %>% summarize(VIOLENT=survey_mean(ANALYSISVAR)) #CHANGE FOR REPLICATE
  filter(between(YEAR, 2005, 2015)) %>% summarize(VIOLENT=survey_mean(ANALYSISVAR))

#tempdf<-ex1_des %>%
# for replicate change filter(between(YEAR, 2014, 2015)) %>% group_by(V3018) %>% summarize(VIOLENT=survey_mean(ANALYSISVAR))
#  filter(YEAR==2015) %>% group_by(V3018) %>% summarize(VIOLENT=survey_mean(ANALYSISVAR))

})

data2 <- data.frame(x1 = numeric(),    # Create empty data frame
                    x2 = numeric(),
                    stringsAsFactors = FALSE)

for(i in 1:52) {                        # Initialize for-loop
  
  list_i <- models[[i]]      # Create some values for this row
  
  data2[i, ] <- list_i                 # Add values at the bottom of data frame
}

data2$Metro<-varlist

#Codebook, Appendix C: 

#If years between 2014-2015 and violent victimization according to definition in codebook.
#'CHANGE FOR REPLICATE' marks which parts to modify in order to replicate codebook numbers vs generating sexual assault numbers. 

#Page 218, (New York MSA) The overall violent victimization rate for both males and females is 14.0 
#victimizations per 1,000 persons with a standard error of 2.33.--Replicated. 14.040496	2.3261798

#Page 227, 1 Chicago 11.7 1.84 ## 2 Los Angeles 14.4 1.82--Replicated, 11.661543	1.8437806; 14.395164	1.8155317
  
write.csv(data2, '/Users/stephencranney/Desktop/Utah_vict.csv')



