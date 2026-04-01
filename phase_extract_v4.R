# author: madelyn miles
# date: 260326
# purpose: rip sleepStat from phase package for my own uses >:D
# version history...
#   v1: calculates and exports daily sleep stats for each channel
#   v2: also exports sleep traces for each day and removes dead flies
#   v3: sorts stats by condition; makes a matrix of tst for easy copy/paste
#   v4: bugs squashed; added outputs for sleep architecture stats
# how to use...
#   1. pass DAM files through DAMFileScan software (available on Trikinetics
#      site). script defaults to 1-minute bins, but this can be changed below.
#      export with settings '+Ct' and 'Sum into bin', and do not make channel
#      files. dates can be set to anything (actual data collection is set in
#      this script) but a tighter window can reduce the time/computational cost
#      of loading the files into R during this script.
#   2. create an experiment folder on your hard drive. this folder should have a
#      copy of this script (so you can save the settings for each analysis run),
#      the DAMFileScan outputs, and 2 .csv configuration files detailing 1) the
#      arrangement of conditions across all monitors and 2) groups to compare
#      when calculating sleep suppression/rebound. see example files for how
#      these .csv files should be formatted.
#   3. set settings in the 'inputs and configs' code block in this script; each
#      is explained with comments.
#   4. run the whole script! outputs autosave to the experiment folder.
#

library(phase)
library(utils)

##########################
### inputs and configs ###
##########################

# folder where all scanned monitor files are stored (and where data will output)
experiment_folder = '/Users/maddym/Desktop/rothenfluh lab/experiments/analysis/phase_collate demo/'

# names of each scanned monitor file in the data folder
filenames = c('251216CtM004.txt','251216CtM005.txt','251216CtM006.txt','251216CtM007.txt')

# dates for each experimental day, typically excluding days flies were flipped
baseline_dates = c('06 Dec 25','07 Dec 25')
exposure_dates = c('09 Dec 25','10 Dec 25')
recovery_dates = c('12 Dec 25','13 Dec 25')
# must format dates as DD Mon YY or else PhaseR gets grumpy :)

# .csv files configuring the conditions and rebound calculations...
channel_labels = '4.4_channelconfig.csv' # column = monitor and row = channel
rebound_calcs = '4.4_reboundconfig.csv' # each row is a comparison; column 1 = control, column 2 = experimental
# do NOT use any backslashes inside these config files
# need condition labels in 'channel_labels' to match those in 'rebound_calcs'

# what data do you want to save to the experiment folder?
save_raw_data = T # unannotated data file
exclude_dead_flies = T # when generating data and traces, replaces values for dead flies with NA

save_grouped_data = T # sorts stats by group
save_easy_tst = T # easy tst for copy/paste into prism

clear_workspace_and_console = T # clear workspace and console after script runs

# rarely need to change these
death_criterion = 720 # flies "sleeping" longer than this amount on the last night are excluded from file outputs
sleep_def = 5 # c(5,30) # define a sleep bout by minimum number of minutes or as a range of minutes
zt0 = '06:00' # lights-on time
bin.size = 1 # our DAM data is binned every minute
trace.bin.size = 30 # width of each bin for sleep traces
t.cycle = 24 # number of hours for a single day (or other relevant cycle)
photoperiod = 12 # number of hours of light per day, starting at ZT0
n.days = 1 # number of days after each of the inputted dates to analyze in a batch

#############################################
### trim, analyze, and export sleep stats ###
#############################################
#
# this section reads each monitor file, calculates sleep stats for each day of
# data collection, and lumps everything into one big matrix for export/use.
# 
# if 'save_raw_data' is True, it exports this as a .csv to the data folder.
#

# combine all dates into one list for the upcoming for loop
all_dates = c(baseline_dates,exposure_dates,recovery_dates)

# cycle through each monitor file
for (m in 1:length(filenames)) {
    # read current monitor file
    data = read.delim(paste(experiment_folder, filenames[m],sep = ''), header = FALSE, sep = "\t")
    
    # cycle through each date of data collection
    for (d in 1:length(all_dates)) {
        # trim the data for this monitor for the current day
        data_trimmed = trimData(data = data, start.date = all_dates[d], start.time = zt0, n.days = n.days, bin = bin.size, t.cycle = t.cycle)
        
        # calculate sleep stats for this monitor for the current day
        sleep_stat = sleepStat(data = data_trimmed, sleep.def = sleep_def, t.cycle = t.cycle, photoperiod = 12)
        # tack on a date column for easier reading later
        sleep_stat = cbind(data.frame(Date=all_dates[d]),sleep_stat)
        
        # calculate sleep trace for this monitor for the current day
        sleep_data = sleepData(data_trimmed, sleep.def = sleep_def, bin = trace.bin.size, t.cycle = t.cycle)
        # note: if you want to average across flies or days, would need to use add profilesSleep function here, but sleepData works for no averaging
        
        # on the first rep, make fresh dataframes to start appending stuff to
        if (d == 1) {
            small_stats_dataframe = sleep_stat
            small_trace_dataframe = sleep_data
        } else {
            small_stats_dataframe = rbind(small_stats_dataframe,sleep_stat)
            small_trace_dataframe = rbind(small_trace_dataframe,sleep_data)
        }
    }
    
    if (m == 1) {
        big_stats_df = small_stats_dataframe
        big_trace_df = small_trace_dataframe
    } else {
        big_stats_df = cbind(big_stats_df,small_stats_dataframe)
        big_trace_df = cbind(big_trace_df,small_trace_dataframe)
    }
}

# exclude dead flies by replacing stats with N/A
if (exclude_dead_flies) {
    startcheck = (length(all_dates)-1)*32 # start looking at the last day of data
    
    for (m in 1:length(filenames)) {
        for (f in 1:32) {
            # if the fly sleeps more than the death criterion, cut it out of stats and traces
            if (big_stats_df[startcheck+f,12*m] >= death_criterion) {
                # print(c(m,f)) # prints monitor-channel of each dead fly, for testing code
                
                # delete entries from stats dataframe
                for (d in 0:(length(all_dates)-1)) {
                    big_stats_df[(d*32)+f,(12*(m-1))+(2:12)] = rep(NA, times=11)
                }
                # delete entries from trace dataframe
                big_trace_df[(1:(48*length(all_dates))),(f+1)+(m-1)*33] = rep(NA, times=48*length(all_dates))
            }
        }   
    }
}


# save the data
if (save_raw_data) {
    write.csv(big_stats_df, file = paste(experiment_folder,'sleep_stats_raw.csv', sep=''), row.names = F)
    write.csv(big_trace_df, file = paste(experiment_folder,'sleep_traces_raw.csv', sep=''), row.names = F)
}



###########################
### sort stats by group ###
###########################
#
# this section reads the stats file and sorts data into groups defined in the
# 'channel_labels.csv' file, in which each monitor is a separate column and
# each channel in that monitor is a separate row.
#
# if 'save_grouped_data' is True, it exports each condition as a separate .csv,
# with each statistic at each time point put into sequential columns for easy
# copy/pasting (e.g., total day sleep: ... > baseline 2 > exposure 1 >...).
#

# import channel config file
channel_setup = read.csv(paste(experiment_folder, channel_labels, sep = ''), header = FALSE)

# get a list of condition names from config file
group_names = unique(unlist(channel_setup))
num_monitors = ncol(channel_setup) # stats file has 13 columns per monitor
num_groups = length(group_names) # how many groups are there?

grouped_stats = vector(mode='list',length=num_groups)

# for each group, identify channel/monitor with that group and extract data
for (g in 1:num_groups) { # loop groups
    for (m in 1:num_monitors) { # loop monitors
        # looping through dates is messier and less efficient but makes it easier to keep things in order of day
        for (d in 1:length(all_dates)) {
            for (c in 1:32) { # loop channels
                if (channel_setup[c,m] == group_names[g]) { # index channel_setup to check if this entry is in this group
                    # if the list entry for this condition is empty, first make a dataframe
                    if (is.null(grouped_stats[[g]])) {
                        grouped_stats[[g]] = big_stats_df[((d-1)*32)+c,(((m-1)*12)+1:12)]
                    } else { # otherwise just rbind to existing dataframe
                        grouped_stats[[g]] = rbind(grouped_stats[[g]],big_stats_df[((d-1)*32)+c,(((m-1)*12)+1:12)])
                    }
                }
            }
        }
    }
    grouped_stats[[g]] = cbind(data.frame(Condition=rep(group_names[g],dim(grouped_stats[[g]])[1])),grouped_stats[[g]])
    # export a .csv for each condition
    #if (save_grouped_data) {
    #    write.csv(grouped_stats[[g]], file = paste(experiment_folder,'sorted_stats_raw','.csv', sep=''), row.names = F)
    #}
}

if (save_grouped_data) {
    write.csv(do.call("rbind", grouped_stats), file = paste(experiment_folder,'sleep_stats_raw_grouped.csv', sep=''), row.names = F)
}

# extract and format tst
tst_splitby_date = list()
day_bout_freq = list()
night_bout_freq = list()
day_bout_dur = list()
night_bout_dur = list()

for (g in 1:length(grouped_stats)){
    # extract day and night tst for each condition
    day_splitby_date = as.data.frame(split(grouped_stats[[g]]$Day.Total,grouped_stats[[g]]$Date))
    night_splitby_date = as.data.frame(split(grouped_stats[[g]]$Night.Total,grouped_stats[[g]]$Date))
    
    # sum day and night to calculate tst
    tst_splitby_date[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),(day_splitby_date + night_splitby_date))
    
    # formatting stuff...
    colnames(tst_splitby_date[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
    
    # also extract sleep bout frequency and duration 
    day_bout_dur[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                              as.data.frame(split(grouped_stats[[g]]$Day.BoutDuration.Mean,grouped_stats[[g]]$Date)))
    colnames(day_bout_dur[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
    
    night_bout_dur[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                                as.data.frame(split(grouped_stats[[g]]$Night.BoutDuration.Mean,grouped_stats[[g]]$Date)))
    colnames(night_bout_dur[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
    
    day_bout_freq[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                               as.data.frame(split(grouped_stats[[g]]$Day.BoutNumber,grouped_stats[[g]]$Date)))
    colnames(day_bout_freq[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
    
    night_bout_freq[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                                 as.data.frame(split(grouped_stats[[g]]$Night.BoutNumber,grouped_stats[[g]]$Date)))
    colnames(night_bout_freq[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
}

easy_tst = do.call("rbind", tst_splitby_date)
day_bout_freq = do.call("rbind", day_bout_freq)
night_bout_freq = do.call("rbind", night_bout_freq)
day_bout_dur = do.call("rbind", day_bout_dur)
night_bout_dur = do.call("rbind", night_bout_dur)

# export a single .csv with all TSTs arranged neatly for copy/paste into Prism
if (save_easy_tst) {
    write.csv(easy_tst, file = paste(experiment_folder,'tst_all.csv',sep=''), row.names = F)
    write.csv(day_bout_freq, file = paste(experiment_folder,'day_bout_freq.csv',sep=''), row.names = F)
    write.csv(night_bout_freq, file = paste(experiment_folder,'night_bout_freq.csv',sep=''), row.names = F)
    write.csv(day_bout_dur, file = paste(experiment_folder,'day_bout_dur.csv',sep=''), row.names = F)
    write.csv(night_bout_dur, file = paste(experiment_folder,'night_bout_dur.csv',sep=''), row.names = F)
}



#########################
### calculate rebound ###
#########################
#
# this section reads a sorted tst file and calculates rebound between groups
# defined in the 'rebound_calculations.csv' file, which is a set of pairs of
# group names (first name is the age-matched control, second name is
# experimental group for which relative rebound is calculated. rebound is
# calculated for each non-baseline day of the experiment.
# 

# import file describing which rebound calculations to run
rebound_setup = read.csv(paste(experiment_folder, rebound_calcs, sep = ''), header = FALSE)

# empty list to be filled with dataframes
rebound_stats = vector(mode='list',length=length(rebound_setup[,1]))

# identify final baseline day to use as baseline for calculations
baseline_index = which(all_dates == tail(baseline_dates,1))[[1]]

# runs each calculation sequentially
for (c in 1:length(rebound_setup[,1])) {
    # find average tst of control group (given in first column) for each day starting on last baseline day
    ctlmeans = colMeans(easy_tst[easy_tst$Condition==rebound_setup[c,1],1+(baseline_index:length(all_dates))], na.rm = T)
    
    # calculate average change in control group sleep time from last day of baseline to later dates
    ctldiffs = rep(0,length(ctlmeans)-1)
    for (d in 1:(length(ctlmeans)-1)) {
        ctldiffs[d] = ctlmeans[d+1] - ctlmeans[1]
    }
    
    # calculate fly-wise change in experimental group sleep time from last day of baseline to later dates
    expvalues = easy_tst[easy_tst$Condition==rebound_setup[c,2],1+(baseline_index:length(all_dates))]
    expdiffs = data.frame(matrix(data=0,nrow=length(expvalues[,1]), ncol=length(all_dates)-baseline_index))
    for (d in 1:(length(expvalues[1,])-1)) {
        expdiffs[,d] = expvalues[,d+1] - expvalues[,1]
    }
    
    # subtract ctl sleep diff from exp sleep diff to get rebound
    rebound_stats[[c]] = cbind(
        data.frame(Calculation=rep(paste(rebound_setup[c,2], 'vs.', rebound_setup[c,1]),length(expvalues[,1]))),
        sweep(expdiffs, 2, ctldiffs))
    colnames(rebound_stats[[c]]) = c('Calculation',all_dates[(baseline_index+1):length(all_dates)])
}

if (save_easy_tst) {
    write.csv(do.call("rbind", rebound_stats), file = paste(experiment_folder,'rebound.csv',sep=''), row.names = F)
}

if (clear_workspace_and_console) {
    rm(list=ls())
    cat('\014')
}
