##### PREPARE WORKSPACE #####
setwd("~/Desktop/coding/CitSciAnalysis")
library(ggplot2)
library(Rmisc)

###### PREPARE DATASET ##### 
#Load all .csv data
data_all <- read.csv("~/Downloads/DATA_COMBINED_3 - DATA_COMBINED2.csv")
#remove ADMIN submissions
data_all <- subset(data_all, participant_ID!="Czimmerman")
data_all <- subset(data_all, participant_ID!="Charles Zimmerman")
#remove NON-Public submissions
data_all <- subset(data_all, public_ID=="YES")
#convert time_hour column to numeric
data_all['time_hour'] <- lapply(data_all['time_hour'], as.numeric)
#subset Transcriptions
data_transcriptions <- subset(data_all, phase_ID=="TRANSCRIPTION")
#subset FULL workflow
data_FULL_transcriptions <- subset(data_transcriptions, workflow_ID=="FULL")
#Write csv file
write.csv(data_FULL_transcriptions,'data_FULL_transcriptions.csv')
#Load FULL transcriptions as CSV data
data_FULL_transcriptions <- read.csv("~/Desktop/coding/CitSciAnalysis/data_FULL_transcriptions.csv")
#subset STATESPOTTER workflow
data_STATE <- subset(data_transcriptions, workflow_ID=="STATE")
#Write csv file
write.csv(data_STATE,'data_STATE.csv')
#Load STATE transcriptions as CSV data
data_STATE <- read.csv("~/Desktop/coding/CitSciAnalysis/data_STATE.csv")
#REMOVE TOP TRANSCRIBER
data_STATE_notop <- subset(data_STATE, participant_ID!="PVerbeeck")
data_FULL_transcriptions_notop <- subset(data_FULL_transcriptions, participant_ID!="PVerbeeck")
#Isolate numeric list of NYBG Event Days
NYBG_workshop_dates <-  data_all[(data_all$workshop_nybg=="workshop"),]
NYBG_workshop_dates <- NYBG_workshop_dates[,c("date_seq","online_event")]
NYBG_workshop_dates <- NYBG_workshop_dates[!duplicated(NYBG_workshop_dates),]
NYBG_workshop_dates
#Isolate numeric list of Online Event Days
Online_event_dates <- data_all[(data_all$online_event=="WeDigBio_2016" | 
                                data_all$online_event=="CitSciDay_2018" | 
                                data_all$online_event=="WeDigBio_2017" | 
                                data_all$online_event=="WeDigBio_2018"),]
Online_event_dates <- Online_event_dates[,c("date_seq","online_event")]
Online_event_dates <- Online_event_dates[!duplicated(Online_event_dates$date_seq),]
#Total Unique Participants
participants_unique <- 
  data_all[!duplicated(data_all[,'participant_ID']),]
participants_unique_summary <- summarySE(participants_unique, 
          measurevar="ID1",
          groupvars="portal_name")
ggplot(participants_unique_summary, aes(x=portal_name, y=N, fill=portal_name)) + 
  geom_bar(position=position_dodge(), stat="identity")

#remove duplicated values for each participant / day
participants_date_seq_unique <- 
  data_FULL_transcriptions[!duplicated(data_FULL_transcriptions[,c('date_seq','participant_ID')]),]


###### FIGURE_4 #######
#Histogram of TOTAL Daily PARTICIAPNTS (PER week)
FIGURE_4 <- ggplot(participants_date_seq_unique, aes(x=date_seq)) +
         geom_histogram(aes(fill=portal_name), bins=120) +
  geom_vline(xintercept = 9, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 372, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 737, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 150, linetype="dotted", color = "red", size=1) +
  geom_vline(xintercept = 549, linetype="dotted", color = "blue", size=1) +
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text=element_text(size=12), axis.title=element_text(size=16), panel.background=element_rect(fill="white", colour="grey50"), legend.position="bottom")+
  scale_x_continuous(limits = c(0,849), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,65), expand = c(0, 0)) +
  labs(y="Daily Participants / Week", x="Project Date Sequence (Oct. 2016 - Feb. 2019)")

###### FIGURE_8 ######
#Calculate average daily # participants by portal (Full-Transcriptions) #
FULL_participants_date_seq_unique_count <- table(participants_date_seq_unique[,c('portal_name','date_seq')])
FULL_participants_date_seq_unique_count_melted <- reshape2::melt(FULL_participants_date_seq_unique_count)
#REMOVE PRE-PROJECT Days from DIGIVOL
FULL_participants_date_seq_unique_count_melted <-
  FULL_participants_date_seq_unique_count_melted[
    !(FULL_participants_date_seq_unique_count_melted$date_seq<372 & 
        FULL_participants_date_seq_unique_count_melted$portal_name=='DIGIVOL'),]
#REMOVE NO-PROJECT days from NFN
FULL_participants_date_seq_unique_count_melted <-
  FULL_participants_date_seq_unique_count_melted[
    !(662<FULL_participants_date_seq_unique_count_melted$date_seq & 
        FULL_participants_date_seq_unique_count_melted$date_seq<736 & 
        FULL_participants_date_seq_unique_count_melted$portal_name=='NFN'),]
#Calculate average # transcribers / day / portal
FULL_daily_transcribers <- summarySE(FULL_participants_date_seq_unique_count_melted, 
          measurevar="value",
          groupvars="portal_name")
#GRAPH Average daily Community size is larger amoung Notes From Nature Portal
FIGURE_8 <- ggplot(FULL_daily_transcribers, aes(x=portal_name, y=value, fill=portal_name)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2, position=position_dodge(.9)) + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.position = "none") + 
  labs(y="Avergae Transcribers / Day", x=NULL)

####### FIGURE_2 ######
#Histogram of FULL transcriptions (full term) by transcriber
#Shows distribution of submissions throughout project highlighiting effects of event timing and high-volume participants
FIGURE_2 <- ggplot(data_FULL_transcriptions, aes(x=date_seq,fill=participant_ID)) +
  geom_histogram(aes(), color="black", bins = 121) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=16), panel.background=element_rect(fill="white", colour="grey50"), legend.position="none")+
  geom_vline(xintercept = 9, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 372, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 737, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 150, linetype="dotted", color = "red", size=1) +
  geom_vline(xintercept = 549, linetype="dotted", color = "blue", size=1) +
  scale_x_continuous(limits = c(0,849), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,2200), expand = c(0, 0)) +
  labs(y="Transcriptions / Week", x="Project Date Sequence (Oct. 2016 - Feb. 2019)")


#Analyze Transcription accumulation by PORTAL for project duration (date_seq)
data_full_transcriptions_portal_date_seq <- 
  table(data_FULL_transcriptions[,c("portal_name","date_seq")])
data_full_transcriptions_portal_date_seq_melted <- reshape2::melt(data_full_transcriptions_portal_date_seq)
#REMOVE PRE-PROJECT Days from DIGIVOL
data_full_transcriptions_portal_date_seq_nozero <-
  data_full_transcriptions_portal_date_seq_melted[
    !(data_full_transcriptions_portal_date_seq_melted$date_seq<372 & 
        data_full_transcriptions_portal_date_seq_melted$portal_name=='DIGIVOL'),]
#REMOVE NO-PROJECT days from NFN
data_full_transcriptions_portal_date_seq_nozero_nogap <-
  data_full_transcriptions_portal_date_seq_nozero[
    !(662<data_full_transcriptions_portal_date_seq_nozero$date_seq & 
        data_full_transcriptions_portal_date_seq_nozero$date_seq<736 & 
        data_full_transcriptions_portal_date_seq_nozero$portal_name=='NFN'),]
#Calculate Average transcriptions per portal per day
summary_transcriptions_portal <-
  summarySE(data_full_transcriptions_portal_date_seq_nozero_nogap, 
            measurevar="value",
            groupvars="portal_name")

###### FIGURE_6 ######
#Average daily transriptions per portal
#NFN portal is more productive per day than DIGIVOL (40 vs 30 per day)
ggplot(summary_transcriptions_portal, aes(x=portal_name, y=value, fill=portal_name)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2, position=position_dodge(.9)) + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.position = "none") + 
  labs(y="Avg. Daily Transcriptions / Portal", x=NULL)
  

#calculate transcription ACCUMULATION by portal
data_full_transcriptions_portal_date_seq_cumulative <- 
  apply(data_full_transcriptions_portal_date_seq, 1, cumsum) 
reshape2::melt(data_full_transcriptions_portal_date_seq_cumulative)
data_full_transcriptions_portal_date_seq_cumulative_melted <- 
  reshape2::melt(data_full_transcriptions_portal_date_seq_cumulative)
#Remove ZERO point for both projects
data_full_transcriptions_portal_date_seq_cumulative_melted_nonzero <-
  data_full_transcriptions_portal_date_seq_cumulative_melted[
    !(data_full_transcriptions_portal_date_seq_cumulative_melted$value==0),]

# CALCULATE PARTICIPANT ACCUMULATION #
#ANALYZE Transcription accumulation by USERNAME for project duration (date_seq)
data_FULL_transcriptions_by_user_date <- table(data_FULL_transcriptions[,c("participant_ID","date_seq")])
#Apply cumulative summation funtion to all rows in in User/Date matrix
data_FULL_transcriptions_by_user_date_cumulative <- apply(data_FULL_transcriptions_by_user_date, 1, cumsum)
#melt matrix into graphable dataframe
data_FULL_transcriptions_by_user_date_cumulative_melted <- reshape2::melt(data_FULL_transcriptions_by_user_date_cumulative)
#Remove duplicated values for each participant
data_FULL_transcriptions_by_user_date_cumulative_melted_unique <- 
  data_FULL_transcriptions_by_user_date_cumulative_melted[
    !duplicated(data_FULL_transcriptions_by_user_date_cumulative_melted[,c('value','participant_ID')]),]
#Remove ZERO point for all transcribers
data_FULL_transcriptions_by_user_date_cumulative_melted_unique_nonzero <- 
  data_FULL_transcriptions_by_user_date_cumulative_melted_unique[
    !(data_FULL_transcriptions_by_user_date_cumulative_melted_unique$value==0),]

###### FIGURE_1 #######
# Cumulative transcriptions by participant
# Demonstrates individual patterns in user participation over time
FIGURE_1 <- ggplot(data_FULL_transcriptions_by_user_date_cumulative_melted_unique_nonzero, aes(x=date_seq,y=value)) +
  geom_line(aes(color=participant_ID)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=16), panel.background=element_rect(fill="white", colour="grey50"), legend.position="none")+
  geom_vline(xintercept = 9, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 372, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 737, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 150, linetype="dotted", color = "red", size=1) +
  geom_vline(xintercept = 549, linetype="dotted", color = "blue", size=1) +
  scale_x_continuous(limits = c(0,849), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,3000), expand = c(0, 0)) +
  labs(y="Cumulative Transcriptions / Participant", x="Project Date Sequence (Oct. 2016 - Feb. 2019)")
FIGURE_1

##ANALYZE Participation Start, End, Duration, Participation days
#DETERMINE PARTICIPANT START DATE 
#sort all data by start date
data_all_date_sort <- data_FULL_transcriptions[order(data_FULL_transcriptions[,'date_text']),]
#Isolate first transcription for each participant
data_all_participant_first <- data_all_date_sort[!duplicated(data_all_date_sort$participant_ID),]
#DETERMINE PARTICIPANT END DATE
#sort all data by start date (reversed)
data_all_date_sort_reverse <- data_FULL_transcriptions[order(-data_FULL_transcriptions[,'date_text']),]
#Isolate LAST transcription for each participant
data_all_participant_last <- data_all_date_sort_reverse[!duplicated(data_all_date_sort_reverse$participant_ID),]
#DETERMINE ENGAGEMENT DURATION by Transcriber
data_all_participant_first_sortbyname <- data_all_participant_first[order(data_all_participant_first[,"participant_ID"]),]
data_all_participant_last_sortbyname <- data_all_participant_last[order(data_all_participant_last[,"participant_ID"]),]
#Combine Start & End Dates for all transcribers on single Data Frame
transcriber_start_vs_end_dates <- cbind(
  data_all_participant_first_sortbyname[c("participant_ID","date_seq")],
  data_all_participant_last_sortbyname["date_seq"])
#Rename FIRST & LAST date columns
colnames(transcriber_start_vs_end_dates) [3] <- "date_last"
colnames(transcriber_start_vs_end_dates) [2] <- "date_first"
#Calculate End-Start date => Participant duration
duration <- transcriber_start_vs_end_dates[3]-transcriber_start_vs_end_dates[2]+1
transcriber_duration <- cbind(
  transcriber_start_vs_end_dates["participant_ID"],
  duration,
  data_all_participant_first_sortbyname['portal_name'],
  data_all_participant_first_sortbyname['portal_name'])
colnames(transcriber_duration) [2] <- "participant_duration"
colnames(transcriber_duration) [4] <- "portal_duration"
#Created SCALED duration (participant_duration / portal_duration)
transcriber_duration$portal_duration <- as.character(transcriber_duration$portal_duration)
transcriber_duration$portal_duration[
  transcriber_duration$portal_duration == "NFN"] <- "825"
transcriber_duration$portal_duration[
  transcriber_duration$portal_duration == "DIGIVOL"] <- "478"
transcriber_duration$portal_duration <- as.numeric(transcriber_duration$portal_duration)
duration_scaled <- (transcriber_duration$participant_duration)/(transcriber_duration$portal_duration)*100
transcriber_duration <- cbind(transcriber_duration,duration_scaled)
#Sort transcribers by duration
transcriber_duration_sorted_high2low <- transcriber_duration[order(-transcriber_duration[,'participant_duration']),]

######## FIGURE_3 #######
#Histogram of NEW participants (full term) by Portal
#clear association with EVENTS for Notes From Nature. N
#Major influcx of DIGIVOL volunteers begining with entry to the portal
FIGURE_3 <- ggplot(data_all_participant_first, aes(x=date_seq)) +
  geom_histogram(aes(fill=portal_name), bins = 121) + #weeks in total duration
  theme(legend.title=element_blank(),legend.text=element_text(size=16), axis.text=element_text(size=12), axis.title=element_text(size=16), panel.background=element_rect(fill="white", colour="grey50"), legend.position="bottom")+
  geom_vline(xintercept = 9, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 372, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 737, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 150, linetype="dotted", color = "red", size=1) +
  geom_vline(xintercept = 549, linetype="dotted", color = "blue", size=1) +
  scale_x_continuous(limits = c(0,849), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,55), expand = c(0, 0)) +
  labs(y="New Participants / Week", x="Project Date Sequence (Oct. 2016 - Feb. 2019)")
FIGURE_3

####### FIGURE_9 ###### 
#CALCULATE AVERAGE DISCOVERY RATE vs. Portal
data_all_participant_first
#Calculate average daily NEW participants by portal (Full-Transcriptions) #
data_all_participant_first_count <- table(data_all_participant_first[,c('portal_name','date_seq')])
data_all_participant_first_melted <- reshape2::melt(data_all_participant_first_count)
#REMOVE PRE-PROJECT Days from DIGIVOL
data_all_participant_first_melted <-
  data_all_participant_first_melted[
    !(data_all_participant_first_melted$date_seq<372 & 
        data_all_participant_first_melted$portal_name=='DIGIVOL'),]
#REMOVE NO-PROJECT days from NFN
data_all_participant_first_melted <-
  data_all_participant_first_melted[
    !(662<data_all_participant_first_melted$date_seq & 
        data_all_participant_first_melted$date_seq<736 & 
        data_all_participant_first_melted$portal_name=='NFN'),]
#Calculate average # transcribers / day / portal
FULL_New_transcribers <- summarySE(data_all_participant_first_melted, 
                                     measurevar="value",
                                     groupvars="portal_name")
#GRAPH_FIGURE_9 - Average daily Community size is larger amoung Notes From Nature Portal
FIGURE_9 <- ggplot(FULL_New_transcribers, aes(x=portal_name, y=value, fill=portal_name)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2, position=position_dodge(.9)) + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.position = "none") + 
  labs(y="Avg.NEW Transcribers / Day", x=NULL)

#CALCULATE Participant STATISTICS (by portal)
##Whole term
data_FULL_transcriptions_by_user <- table(data_FULL_transcriptions[,"participant_ID"])
data_FULL_transcriptions_by_user_melted <- reshape2::melt(data_FULL_transcriptions_by_user)
colnames(data_FULL_transcriptions_by_user_melted) [1] <- "participant_ID"
#sort by name Alphebetical
data_FULL_transcriptions_by_user_sortbyname <-data_FULL_transcriptions_by_user_melted[order(data_FULL_transcriptions_by_user_melted[,"participant_ID"]),]
colnames(data_FULL_transcriptions_by_user_sortbyname) [2] <- "transcriptions_total"
#combine with portal_name
data_FULL_transcriptions_by_user_sortbyname <- cbind(
  data_FULL_transcriptions_by_user_sortbyname,
  data_all_participant_first_sortbyname['portal_name'])
#combine with Participant DURATION (raw & Scaled)
data_FULL_transcriptions_by_user_sortbyname <- cbind(
  data_FULL_transcriptions_by_user_sortbyname,
  transcriber_duration[,c('participant_duration','duration_scaled')])
##Particpant totals by day
data_FULL_transcriptions_by_user_date_melted <- reshape2::melt(data_FULL_transcriptions_by_user_date)
#Remove days with Zero transcriptions
data_FULL_transcriptions_by_user_date_nozero <- 
  data_FULL_transcriptions_by_user_date_melted[!(
    data_FULL_transcriptions_by_user_date_melted$value==0),]
#Calculate total # Participation days (by transcriber)
participation_days <- aggregate(date_seq ~ participant_ID, 
          data_FULL_transcriptions_by_user_date_nozero, 
          function(x) length(unique(x)))
colnames(participation_days) [2] <- "participation_days"
#Calculate average trancriptions per day (by transcriber)
average_daily_transcripions_by_transcriber <- aggregate(
  data_FULL_transcriptions_by_user_date_nozero$value,
  by=list(data_FULL_transcriptions_by_user_date_nozero$participant_ID),
  data=data_FULL_transcriptions_by_user_date_nozero,
  FUN=mean)
colnames(average_daily_transcripions_by_transcriber) [1:2] <- c(
  "participant_ID", "daily_transcription_average")
#calculate FREQUENCY (transcription_days / participant_duration)
participant_frequency <- participation_days$participation_days/transcriber_duration$participant_duration

#calculate avergae DAILY ATTENTION (in minutes) for each daily session
#DIGIVOL
#subset DIGIVOL
DIGIVOL_FULL_transcriptions <- subset(data_FULL_transcriptions[data_FULL_transcriptions$portal_name=="DIGIVOL",])
#select unique MINUTES & day & Transcriber
DIGIVOL_FULL_date_partcipant_minutes_unique <- 
  DIGIVOL_FULL_transcriptions[!duplicated(DIGIVOL_FULL_transcriptions[,c('date_seq','participant_ID','hour_minute')]),]
#aggregate (count) unique MINUTES per DAY & Transcriber
DIGIVOL_FULL_participant_daily_minutes <- table(DIGIVOL_FULL_date_partcipant_minutes_unique[,c('participant_ID','date_seq')])
DIGIVOL_FULL_participant_daily_minutes_melted <- reshape2::melt(DIGIVOL_FULL_participant_daily_minutes)
#Remove days with Zero MINUTES
DIGIVOL_FULL_participant_daily_minutes_nozero <- 
  DIGIVOL_FULL_participant_daily_minutes_melted[!(
    DIGIVOL_FULL_participant_daily_minutes_melted$value==0),]
#add date as factor & rename to "date_seq_factor"
DIGIVOL_FULL_participant_daily_minutes_nozero <- cbind(
  DIGIVOL_FULL_participant_daily_minutes_nozero,
  factor(DIGIVOL_FULL_participant_daily_minutes_nozero$date_seq))
colnames(DIGIVOL_FULL_participant_daily_minutes_nozero) [4] <- "date_seq_factor"

#Calculate Average Minutes per day (ATTENTION) for DIGIVOL transcriber 
DIGIVOL_attention <- summarySE(DIGIVOL_FULL_participant_daily_minutes_nozero, 
          measurevar="value")
#NFN
#subset NFN
NFN_FULL_transcriptions <- subset(data_FULL_transcriptions[data_FULL_transcriptions$portal_name=="NFN",])
#select unique MINUTES & day & Transcriber
NFN_FULL_date_partcipant_minutes_unique <- 
  NFN_FULL_transcriptions[!duplicated(NFN_FULL_transcriptions[,c('date_seq','participant_ID','hour_minute')]),]
#aggregate (count) unique MINUTES per DAY & Transcriber
NFN_FULL_participant_daily_minutes <- table(NFN_FULL_date_partcipant_minutes_unique[,c('participant_ID','date_seq')])
NFN_FULL_participant_daily_minutes_melted <- reshape2::melt(NFN_FULL_participant_daily_minutes)
#Remove days with Zero MINUTES
NFN_FULL_participant_daily_minutes_nozero <- 
  NFN_FULL_participant_daily_minutes_melted[!(
    NFN_FULL_participant_daily_minutes_melted$value==0),]
#add date as factor & rename to "date_seq_factor"
NFN_FULL_participant_daily_minutes_nozero <- cbind(
  NFN_FULL_participant_daily_minutes_nozero,
  factor(NFN_FULL_participant_daily_minutes_nozero$date_seq))
colnames(NFN_FULL_participant_daily_minutes_nozero) [4] <- "date_seq_factor"
#Calculate Average Minutes per day for NFN transcriber 
NFN_attention <- summarySE(NFN_FULL_participant_daily_minutes_nozero, 
          measurevar="value")
#COMBINE NFN & DIGIVOL STATISTICS
attention_analysis <- rbind(DIGIVOL_attention, NFN_attention, STATESPOTTER_attention)
row.names(attention_analysis) [1:3] <- c("DIGIVOL", "NFN", "STATESPOTTER")
attention_analysis [1:3,1] <- c("DIGIVOL", "NFN", "STATESPOTTER")
colnames(attention_analysis) [1] <- "workflow"
attention_analysis_FULL <- attention_analysis[1:2,]

####### ATTENTION ANALYSIS - COMPARE NFN / DIGIVOL
#DIGIVOL & NFN users did not display significant differences in Daily attention
ggplot(attention_analysis_FULL, aes(x=workflow, y=value, fill=workflow)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

#ORGANIZE TRANSCRIBER STATISTICS SUMMARY SHEET #
transcriber_data <- cbind(
  data_FULL_transcriptions_by_user_sortbyname,
  transcriber_start_vs_end_dates[2:3],
  average_daily_transcripions_by_transcriber["daily_transcription_average"],
  participation_days["participation_days"],
  transcriber_duration["portal_duration"],
  participant_frequency)
#Sort by Total Transcriptions
transcriber_data_sorted <- transcriber_data[order(-transcriber_data[,'transcriptions_total']),]
#Impose FACTOR ORDER to list of transcriber names (based on Transcriptions total)
transcriber_data_sorted$participant_ID <- factor(
  transcriber_data_sorted$participant_ID, 
  levels = transcriber_data_sorted$participant_ID[order(
    transcriber_data_sorted$transcriptions_total)])

###### FIGURE_5 #######
#BARGRAPH - USER PARTICIPATION TOTAL (BY PORTAL) 
#Demonstrates relative influence of power participants amoung NFN & DIGIVOL
#NFN has more imbalence amoung users (some very high, many very low)
#Even low-quanity users contribute significatly to overall project balence
#DIGIVOL has greater number of power users, and many fewer rapid users
ggplot(transcriber_data_sorted, aes(x=portal_name,y=transcriptions_total, fill=portal_name)) +
  geom_bar(width = 0.9, stat = "identity", color="black")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.position = "none") + 
  labs(y="Total Transcriptions (Portal | Participant))", x=NULL)

#PRODUCTIVITY - AVERAGE TOTAL TRANSCRIPTIONS by Individual vs. PORTAL
#summarize data for Barchart
summary_transcriptions_total_portal <-
  summarySE(transcriber_data_sorted, 
            measurevar="transcriptions_total",
            groupvars="portal_name")
#GRAPH Productivity difference between portals
#On average, DIGIVOL users submitted a higher total number of transcriptions, but the difference was not significant (95% CI error bars overlap)
ggplot(summary_transcriptions_total_portal, aes(x=portal_name, y=transcriptions_total, fill=portal_name)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=transcriptions_total-ci, ymax=transcriptions_total+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + theme(legend.position = "none")


#PRODUCTIVITY (Daily) AVERAGE DAILY TRANSCRIPTIONS by individual (vs. portal)
#summarize data for Barchart
summary_daily_transcription_average_portal <-
  summarySE(transcriber_data_sorted, 
            measurevar="daily_transcription_average",
            groupvars="portal_name")
#DIGIVOL & Notes from nature users display same average daily transcriptions
ggplot(summary_daily_transcription_average_portal, aes(x=portal_name, y=daily_transcription_average, fill=portal_name)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=daily_transcription_average-se, ymax=daily_transcription_average+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + theme(legend.position = "none")

##### FIGURE_7 ###### Avg.Participation DURATION (scaled) by PORTAL ######
#summarize data for Barchart
summary_duration_scaled_portal <-
  summarySE(transcriber_data_sorted, 
            measurevar="duration_scaled",
            groupvars="portal_name")
#GRAPH
#DIGIVOL users displayed significantly higher scaled DURATION on average
FIGURE_7 <- ggplot(summary_duration_scaled_portal, aes(x=portal_name, y=duration_scaled, fill=portal_name)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=duration_scaled-ci, ymax=duration_scaled+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.position = "none") + 
  labs(y="Avg. Engagement Duration (% of Portal Duration)", x=NULL)


#FREQUENCY - Avg. participant FREQUNECY (participation days / engagement duration) vs. Portal #
#Digivol participants have slightly Higher Frequency (but not significant)
#summarize data for Barchart
transcriber_data_frequnecy_not1 <- subset(
  transcriber_data_sorted,transcriber_data_sorted$participant_frequency !=1)
summary_frequency_portal <-
  summarySE(transcriber_data_frequnecy_not1, 
            measurevar="participant_frequency",
            groupvars="portal_name")
#GRAPH
FULL_transciber_frequency <- ggplot(summary_frequency_portal, aes(x=portal_name, y=participant_frequency, fill=portal_name)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=participant_frequency-ci, ymax=participant_frequency+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + theme(legend.position = "none")

####################### STATE SPOTTER ANALYSIS #################################

#Calculate # Submissions by transcriber -- Whole term
data_STATE_by_user <- table(data_STATE[,"participant_ID"])
data_STATE_by_user_melted <- reshape2::melt(data_STATE_by_user)
colnames(data_STATE_by_user_melted) [1] <- "participant_ID"
#sort by username
data_STATE_by_user_sortbyname <-data_STATE_by_user_melted[order(data_STATE_by_user_melted[,"participant_ID"]),]
colnames(data_STATE_by_user_sortbyname) [2] <- "transcriptions_total"
#sort by Transcriptions total
data_STATE_by_user_sortbytotal <-data_STATE_by_user_sortbyname[order(-
  data_STATE_by_user_sortbyname[,"transcriptions_total"]),]

###### FIGURE_10 #####
#GRAPH STATE SPOTTER submissions by Transcriber 
FIGURE_10 <- ggplot(data_STATE_by_user_sortbytotal, aes(x=" ", y=transcriptions_total)) +
  geom_bar(width = 1, stat = "identity", fill="grey", color="black")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.position = "none") + 
  labs(y="Total Classifications (by Participant))", x="US State Spotter")

# ATTENTION (Statespotter) Avg. # minutes per transcription day #
#select unique MINUTES & day & Transcriber
data_STATE_date_partcipant_minutes_unique <- 
  data_STATE[!duplicated(data_STATE[,c('date_seq','participant_ID','hour_minute')]),]
#aggregate (count) unique MINUTES per DAY & Transcriber
STATE_participant_daily_minutes <- table(data_STATE_date_partcipant_minutes_unique[,c('participant_ID','date_seq')])
STATE_participant_daily_minutes_melted <- reshape2::melt(STATE_participant_daily_minutes)
#Remove days with Zero MINUTES
STATE_participant_daily_minutes_nozero <- 
  STATE_participant_daily_minutes_melted[!(
    STATE_participant_daily_minutes_melted$value==0),]
#add date as factor & rename to "date_seq_factor"
STATE_participant_daily_minutes_nozero <- cbind(STATE_participant_daily_minutes_nozero, factor(STATE_participant_daily_minutes_nozero$date_seq))
colnames(STATE_participant_daily_minutes_nozero) [4] <- "date_seq_factor"

##CALCULATE MINUTES (ATTENTION) 
#select unique MINUTES & day & Transcriber
data_STATE_date_partcipant_minutes_unique <- 
  data_STATE[!duplicated(data_STATE[,c('date_seq','participant_ID','hour_minute')]),]
#aggregate (count) unique MINUTES per DAY & Transcriber
STATE_participant_daily_minutes <- table(data_STATE_date_partcipant_minutes_unique[,c('participant_ID','date_seq')])
STATE_participant_daily_minutes_melted <- reshape2::melt(STATE_participant_daily_minutes)
#Remove days with Zero MINUTES
STATE_participant_daily_minutes_nozero <- 
  STATE_participant_daily_minutes_melted[!(
    STATE_participant_daily_minutes_melted$value==0),]
#add date as factor & rename to "date_seq_factor"
STATE_participant_daily_minutes_nozero <- cbind(STATE_participant_daily_minutes_nozero, factor(STATE_participant_daily_minutes_nozero$date_seq))
colnames(STATE_participant_daily_minutes_nozero) [4] <- "date_seq_factor"
#ATTENTION - Avg. MINUTES per DAY for state spotter expeditions (includes top transcriber)
STATESPOTTER_attention <- summarySE(STATE_participant_daily_minutes_nozero, 
          measurevar="value")

#CALCULATE ATTENTION (MINUTES) (NO TOP)
#select unique hours & day & Transcriber
data_STATE_date_partcipant_minutes_notop_unique <- 
  data_STATE_notop[!duplicated(data_STATE_notop[,c('date_seq','participant_ID','hour_minute')]),]
#aggregate (count) unique HOURS per DAY & Transcriber
STATE_participant_daily_minutes_notop <- table(data_STATE_date_partcipant_minutes_notop_unique[,c('participant_ID','date_seq')])
STATE_participant_daily_minutes_notop_melted <- reshape2::melt(STATE_participant_daily_minutes_notop)
#Remove days with Zero MINUTES
STATE_participant_daily_minutes_nozero_notop <- 
  STATE_participant_daily_minutes_notop_melted[!(
    STATE_participant_daily_minutes_notop_melted$value==0),]
#add date as factor & rename to "date_seq_factor"
STATE_participant_daily_minutes_nozero_notop <- cbind(STATE_participant_daily_minutes_nozero_notop, factor(STATE_participant_daily_minutes_nozero_notop$date_seq))
colnames(STATE_participant_daily_minutes_nozero_notop) [4] <- "date_seq_factor"

#Calculate ATTENTION (NO TOP) average daily minutes / transcriber
summarySE(STATE_participant_daily_minutes_nozero_notop, 
          measurevar="value")

#BAR graph of MINUTES per day for project duration (by transcriber)
ggplot(STATE_participant_daily_minutes_nozero, aes(x=date_seq,y=value,fill=participant_ID)) +
  geom_bar(stat="identity", color="black") + 
  theme(legend.position = "none")

#HISTOGRAM: total transcriptions by DAY (Project duration)
#ALL
ggplot(data_STATE, aes(x=date_seq, fill=participant_ID)) +
  geom_histogram(color="black", bins = 50) + 
  theme(legend.position = "none")
#NO TOP
ggplot(data_STATE_notop, aes(x=date_seq, fill=participant_ID)) +
  geom_histogram(color="black", bins = 50) + 
  theme(legend.position = "none")

#Calculate # TOTAL PARTICIPANTS per DAY 
#remove duplicated values for each participant / day
STATE_participants_date_seq_unique <- 
  data_STATE[!duplicated(data_STATE[,c('date_seq','participant_ID')]),]

#####HISTOGRAM: total participants by DAY (Project duration)
#ALL
ggplot(STATE_participants_date_seq_unique, aes(x=date_seq, fill=participant_ID)) +
  geom_histogram(color="black", bins = 50) + 
  theme(legend.position = "none")
#NO TOP
ggplot(data_STATE_notop, aes(x=date_seq, fill=participant_ID)) +
  geom_histogram(color="black", bins = 50) + 
  theme(legend.position = "none")

##### FIGURE_9 ###### 
#LAYERED-histogram of STATE transcriptions (Expedition_seq) by participant #####
ggplot(data_STATE, aes(x=date_seq_expedition_phase)) +
  geom_histogram(aes(fill=participant_ID), color="black", bins=10) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=16), panel.background=element_rect(fill="white", colour="grey50"), legend.position="none")+ 
  labs(y="Total Classifications (by Participant)", x="Expedition Days")
  
#VARIANT - NO TOP Transcriber
ggplot(data_STATE_notop, aes(x=date_seq_expedition_phase)) +
  geom_histogram(aes(fill=participant_ID), color="black", bins=10) + 
  theme(legend.position = "none")

#Graph of differences in expedtion longevity bewteen STATESPOTTER & FULL-transcription expedtiions
ggplot(data_all, aes(x=date_seq_expedition_phase)) +
  geom_histogram(aes(fill=workflow_ID), bins=200) + 
  theme(legend.title=element_blank(),legend.position="bottom", axis.text=element_text(size=12),axis.title=element_text(size=16), panel.background=element_rect(fill="white", colour="grey50"))+ 
  labs(y="Total Classifications", x="Expedition Days") +
  scale_x_continuous(limits = c(0,200), expand = c(0, 0))
