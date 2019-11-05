    # GeoSurvey submission count per day for Supervvising Geosurvey Visual Inspector
    # W.Simbila November 2019

    # Install and load required packages
    install.packages("tidyverse", dependencies = TRUE)
    suppressPackageStartupMessages({
      require(tidyverse)
      require(rgdal)
      require(ggpl)
    })
    
    # Set working directory
    setwd("~")
    dir.create("MZQ_geosurvey", showWarnings = TRUE)
    setwd("~/MZQ_geosurvey")
    

    ## Download geosurvey data and load mozambique_l1 data into R
    Mozambique_l1 <-read.csv("mozambique_L1.csv", header = TRUE, sep = ",")
    Mozambique_l1
    head(Mozambique_l1)
    names(Mozambique_l1) # show column names of the dataset
   
    # Select specific column to sort (i.e. Submission.time, User, Submission.id, building, cropland and wcover)
    Moza_subm <- Mozambique_l1 %>% 
      select("Submission.time","User","Submission.id","building","cropland","wcover")
    head(Moza_subm)
    
    # Separate Submission.time column into Date and time
    Moza_subm1<-Moza_subm %>% 
      separate(Submission.time,into = c("Date","Time"),sep = " ")
    
    # Show column names
    names(Moza_subm1)
    Moza_subm1$Date # Extract/dispaly Date column values
    Moza_subm1$Time # Extract/display Time column values
    
    # Separate Date  into Year, Month and Day columns -------------
    Moza_subm2<- Moza_subm1%>% 
      separate(Date,into = c("Year","Month","Day"),sep = "-")
    
    # Check the dinstinct number of Day Variable
    unique(Moza_subm2$Day) 
    
    # Group dataset by Day,user,Month, and count the Total submissions per user per day
    
    count_subm<-Moza_subm2%>% 
      select(Month,Day,User) %>% 
      group_by(Month, Day, User) %>% 
      summarise(Count = n())
    
    # print the dataset
    count_subm 
   
    # Filter submission per specific Date/Day and count submissions per user 
    count_subm_day<- Moza_subm2 %>% 
      select(Month,Day,User) %>% 
      filter(str_detect(Day,pattern = "04")) %>% # on 4th day of November
      group_by(Day,Month, User) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count))
    
    count_subm_day
    
    # Convert to dataframe
    count_subm_day <- as.data.frame(count_subm_day)
    
    count_subm_day
    
