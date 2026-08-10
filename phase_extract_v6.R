# author: madelyn miles
# date: 260810
# purpose: rip sleepStat from phase package for my own uses >:D
# version history...
#   v1: calculates and exports daily sleep stats for each channel
#   v2: also exports sleep traces for each day and removes dead flies
#   v3: sorts stats by condition; makes a matrix of tst for easy copy/paste
#   v4: bugs squashed; added outputs for sleep architecture stats
#   v5: squashed bug where tst sort got messed up for experiments spanning more
#         than one month; added labels to sleep trace output
#   v6: new functionality: p(wake), p(doze), waking activity calculations, and
#         death criterion based on inactivity instead of sleep
#       QoL improvements: restored functionality to 'save_X_data' parameters,
#         saves outputs to dedicated folders created within experiment folder
# how to use...
#   1. pass DAM files through DAMFileScan software (available on Trikinetics
#      site). script defaults to 1-minute bins, but this can be changed below.
#      export with settings '+Ct' and 'Sum into bin', and do not make channel
#      files. start date MUST be at least the day before data collection window
#      begins, though actual data collection window is set in this script (e.g.,
#      if flies were loaded into incubators on Mar 26 and data collection is set
#      to begin on Mar 27 @ ZT0 (6am), then scan from Mar 26 @ 11:59 at latest).
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
experiment_folder = '/Users/maddym/Desktop/rothenfluh lab/analysis scripts/phaseR-main/test_v6/'

# names of each scanned monitor file in the data folder
filenames = c('260731CtM076.txt','260731CtM077.txt','260731CtM078.txt','260731CtM079.txt')

# dates for each experimental day, typically excluding days flies were flipped
baseline_dates = c('22 Jul 26','23 Jul 26','24 Jul 26')
exposure_dates = c('26 Jul 26','27 Jul 26')
recovery_dates = c('29 Jul 26','30 Jul 26')
# must format dates as DD Mon YY or else PhaseR gets grumpy :)

# .csv files configuring the conditions and rebound calculations...
channel_labels = '6.3f_channelconfig.csv' # column = monitor and row = channel
rebound_calcs = '6.3f_reboundconfig.csv' # each row is a comparison; column 1 = control, column 2 = experimental
# do NOT use any backslashes inside these config files
# need condition labels in 'channel_labels' to match those in 'rebound_calcs'

# analysis parameters
death_criterion = 15 # beam breaks; flies with fewer than this many counts in the last 12h are likely dead so excluded from all but raw outputs
sleep_def = 5 # c(5,30) # minutes; define a sleep bout by minimum number of minutes or as a range of minutes
zt0 = '06:00' # 'HH:MM'; lights-on time, according to the data collection computer
trace.bin.size = 30 # minutes; width of each bin for sleep traces...code might break if it's not a round dividend of 60 minutes

# output parameters
exclude_dead_flies = T # boolean: replaces values for dead flies (activity < criterion) with NA
clear_workspace_and_console = T # boolean: clear workspace and console after script runs
save_raw_data = T # unannotated data
save_sorted_data = T # tst, tstrace, and sleep bout data sorted by group
save_pwakepdoze = T # conditional sleep/wake probabilities
save_rebound = T # calculate and rebound (set to F to avoid error message when not computing rebound)

# rarely change these...make sure you have good reason
bin.size = 1 # minutes; DAMFileScan data should be in 1 minute bins
t.cycle = 24 # hours; number of hours for a single day (or other relevant cycle)
photoperiod = 12 # hours; number of hours of light per day, starting at ZT0
n.days = 1 # days; number of days after each of the inputted dates to analyze in a batch


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
        data_trimmed = trimData(data = data, start.date = all_dates[d], 
                                start.time = zt0, n.days = n.days, 
                                bin = bin.size, t.cycle = t.cycle)
        
        # calculate sleep stats for this monitor for the current day
        sleep_stat = sleepStat(data = data_trimmed, sleep.def = sleep_def, 
                               t.cycle = t.cycle, photoperiod = 12)
        # tack on a date column for easier reading later
        sleep_stat = cbind(data.frame(Date=all_dates[d]),sleep_stat)
        
        # calculate sleep trace for this monitor for the current day
        sleep_data = sleepData(data_trimmed, sleep.def = sleep_def, 
                               bin = trace.bin.size, t.cycle = t.cycle)
        # note: if you want to average across flies or days, would need to use add profilesSleep function here, but sleepData works for no averaging
        
        # on the first rep, make fresh dataframes to start appending data to
        if (d == 1) {
            small_stats_dataframe = sleep_stat
            small_trace_dataframe = sleep_data
            small_activity_dataframe = data_trimmed[,11:42]
        } else {
            small_stats_dataframe = rbind(small_stats_dataframe,sleep_stat)
            small_trace_dataframe = rbind(small_trace_dataframe,sleep_data)
            small_activity_dataframe = rbind(small_activity_dataframe,data_trimmed[,11:42])
        }
    }
    
    # on the first rep, make fresh dataframes to start appending data to
    if (m == 1) {
        big_stats_df = small_stats_dataframe
        big_trace_df = small_trace_dataframe
        activity_df = small_activity_dataframe
    } else {
        big_stats_df = cbind(big_stats_df,small_stats_dataframe)
        big_trace_df = cbind(big_trace_df,small_trace_dataframe)
        activity_df = cbind(activity_df,small_activity_dataframe)
    }
}

# save data to experiment folder
if (save_raw_data) {
    raw_pathname = paste(experiment_folder,'outputs_raw/',sep='')
    dir.create(raw_pathname)
    write.csv(big_stats_df, file = paste(raw_pathname,'sleep_stats_raw.csv', sep=''), row.names = F)
    write.csv(big_trace_df, file = paste(raw_pathname,'tstrace_raw.csv', sep=''), row.names = F)
    write.csv(activity_df, file = paste(raw_pathname,'activity_by_minute_raw.csv', sep=''), row.names = F)
}



#######################################
### process and sort stats by group ###
#######################################
#
# this section reads the stats file and sorts data into groups defined in the
# 'channel_labels.csv' file, in which each monitor is a separate column and
# each channel in that monitor is a separate row.
#
# if 'save_sorted_data' is True, it exports grouped data as a separate .csv,
# with each statistic at each time point put into sequential columns for easy
# copy/pasting (e.g., total day sleep: ... > baseline 2 > exposure 1 >...).
#
# currently, this package sorts tst, tstrace, and sleep bout stats by group
#

# exclude dead flies by replacing stats and activity with NA
if (exclude_dead_flies) {
    # startcheck = (length(all_dates)-1)*32 # start looking at the last day of data
    
    for (m in 1:length(filenames)) {
        for (f in 1:32) {
            # if the fly is too inactive in the last 12 hours, cut it out of stats and traces
            if (sum(activity_df[(length(all_dates)*(24*60 / bin.size)):(length(all_dates)*(24*60 / bin.size) - (12*60 / bin.size)),((m-1)*32)+f]) < death_criterion) {
            # if (big_stats_df[startcheck+f,12*m] >= death_criterion) { # sleep-based death criterion
                # print(c(m,f)) # prints monitor-channel of each dead fly, for testing code

                # delete entries from stats dataframe
                for (d in 0:(length(all_dates)-1)) {
                    big_stats_df[(d*32)+f,(12*(m-1))+(2:12)] = rep(NA, times=11)
                }
                # delete entries from trace dataframe
                big_trace_df[(1:(24*(60/trace.bin.size)*length(all_dates))),(f+1)+(m-1)*33] = rep(NA, times=(24*(60/trace.bin.size)*length(all_dates)))

                # delete entries from activity dataframe
                activity_df[(1:(24*(60/bin.size)*length(all_dates))),(f)+(m-1)*32] = rep(NA, times=(24*(60/bin.size)*length(all_dates)))
            }
        }   
    }
}

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
                    # calculate waking activity for this entry
                    d_tst = big_stats_df[((d-1)*32)+c,(((m-1)*12)+7)]
                    n_tst = big_stats_df[((d-1)*32)+c,(((m-1)*12)+12)]
                    
                    d_act = sum(activity_df[(((d-1)*1440)+1:720),((m-1)*32)+c],na.rm = TRUE)
                    n_act = sum(activity_df[(((d-1)*1440)+721:1440),((m-1)*32)+c],na.rm = TRUE)
                    
                    d_waking_act = d_act / (720 - d_tst)
                    n_waking_act = n_act / (720 - n_tst)
                    
                    # output: if the list entry for this condition is empty, first make a dataframe...
                    if (is.null(grouped_stats[[g]])) {
                        grouped_stats[[g]] = cbind(big_stats_df[((d-1)*32)+c,(((m-1)*12)+1:12)],
                                                   data.frame(Day.WakingActivity=d_waking_act),
                                                   data.frame(Night.WakingActivity=n_waking_act),
                                                   data.frame(Experiment.Day=d))
                    } else { # otherwise just rbind to existing dataframe
                        grouped_stats[[g]] = rbind(grouped_stats[[g]],cbind(big_stats_df[((d-1)*32)+c,(((m-1)*12)+1:12)],
                                                                            data.frame(Day.WakingActivity=d_waking_act),
                                                                            data.frame(Night.WakingActivity=n_waking_act),
                                                                            data.frame(Experiment.Day=d)))
                    }
                }
            }
        }
    }
    grouped_stats[[g]] = cbind(data.frame(Condition=rep(group_names[g],dim(grouped_stats[[g]])[1])),grouped_stats[[g]])
}

# extract and format tst
tst_splitby_date = list()
day_bout_freq = list()
night_bout_freq = list()
day_bout_dur = list()
night_bout_dur = list()
day_waking_activity = list()
night_waking_activity = list()

for (g in 1:length(grouped_stats)){
    # extract day and night tst for each condition
    day_splitby_date = as.data.frame(split(grouped_stats[[g]]$Day.Total,grouped_stats[[g]]$Experiment.Day))
    night_splitby_date = as.data.frame(split(grouped_stats[[g]]$Night.Total,grouped_stats[[g]]$Experiment.Day))
    
    # sum day and night to calculate tst
    tst_splitby_date[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),(day_splitby_date + night_splitby_date))
    
    # formatting stuff...
    colnames(tst_splitby_date[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
    
    # also extract sleep bout frequency and duration 
    day_bout_dur[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                              as.data.frame(split(grouped_stats[[g]]$Day.BoutDuration.Mean,grouped_stats[[g]]$Experiment.Day)))
    colnames(day_bout_dur[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
    
    night_bout_dur[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                                as.data.frame(split(grouped_stats[[g]]$Night.BoutDuration.Mean,grouped_stats[[g]]$Experiment.Day)))
    colnames(night_bout_dur[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
    
    day_bout_freq[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                               as.data.frame(split(grouped_stats[[g]]$Day.BoutNumber,grouped_stats[[g]]$Experiment.Day)))
    colnames(day_bout_freq[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
    
    night_bout_freq[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                                 as.data.frame(split(grouped_stats[[g]]$Night.BoutNumber,grouped_stats[[g]]$Experiment.Day)))
    colnames(night_bout_freq[[g]]) = c('Condition',all_dates) # label each row with the condition that it is

    day_waking_activity[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                                 as.data.frame(split(grouped_stats[[g]]$Day.WakingActivity,grouped_stats[[g]]$Experiment.Day)))
    colnames(day_waking_activity[[g]]) = c('Condition',all_dates) # label each row with the condition that it is

    night_waking_activity[[g]] = cbind(data.frame(Condition=rep(group_names[g],length(day_splitby_date[,1]))),
                                 as.data.frame(split(grouped_stats[[g]]$Night.WakingActivity,grouped_stats[[g]]$Experiment.Day)))
    colnames(night_waking_activity[[g]]) = c('Condition',all_dates) # label each row with the condition that it is
}

easy_tst = do.call("rbind", tst_splitby_date)
day_bout_freq = do.call("rbind", day_bout_freq)
night_bout_freq = do.call("rbind", night_bout_freq)
day_bout_dur = do.call("rbind", day_bout_dur)
night_bout_dur = do.call("rbind", night_bout_dur)
day_waking_activity = do.call("rbind", day_waking_activity)
night_waking_activity = do.call("rbind", night_waking_activity)

# label each channel in sleep traces file with group name, and remove the ZT
# columns between monitors for easier copy/paste

chopped_trace_df = big_trace_df

# chop all ZT-containing columns except the first
if (length(filenames) > 1) {
  for (c in sort(seq(33,33*num_monitors-1,33),TRUE)+1) {
    chopped_trace_df = chopped_trace_df[-c]
  }
}

colnames(chopped_trace_df) = c('ZT',unlist(channel_setup,use.names=FALSE))


# export a single .csv with all TSTs arranged neatly for copy/paste into Prism
if (save_sorted_data) {
    sorted_pathname = paste(experiment_folder,'outputs_sorted/',sep='')
    dir.create(sorted_pathname)
    
    write.csv(do.call("rbind", grouped_stats), file = paste(sorted_pathname,'sleep_stats_grouped.csv', sep=''), row.names = F)
    
    write.csv(easy_tst, file = paste(sorted_pathname,'tst.csv',sep=''), row.names = F)
    write.csv(day_bout_freq, file = paste(sorted_pathname,'bout_freq_day.csv',sep=''), row.names = F)
    write.csv(night_bout_freq, file = paste(sorted_pathname,'bout_freq_night.csv',sep=''), row.names = F)
    write.csv(day_bout_dur, file = paste(sorted_pathname,'bout_dur_day.csv',sep=''), row.names = F)
    write.csv(night_bout_dur, file = paste(sorted_pathname,'bout_dur_night.csv',sep=''), row.names = F)
    write.csv(day_waking_activity, file = paste(sorted_pathname,'waking_activity_day.csv',sep=''), row.names = F)
    write.csv(night_waking_activity, file = paste(sorted_pathname,'waking_activity_night.csv',sep=''), row.names = F)
    
    write.csv(chopped_trace_df, file = paste(sorted_pathname,'tstrace_labeled.csv',sep=''), row.names = F)
}



#####################################
### calculate p(wake) and p(doze) ###
#####################################
#
# conditional sleep-wake state transition probabilities are calculated using the
# method described in Wiggin...Griffith, 2020 (doi.org/10.1073/pnas.1917573117).
#

# make empty matrices to bin pdoze
pdoze_binned = matrix(,(dim(activity_df)[1]/trace.bin.size),dim(activity_df)[2])

# loop by fly
for (f in 1:(dim(activity_df)[2])) {
    # skip over the dead ones
    if (is.na(activity_df[1,f])) {
        pdoze_binned[,f] = NA
    } else {
        # otherwise loop by bin to calculate conditional probabilities
        for (i in 1:((dim(activity_df)[1]/trace.bin.size)-1)) {
            start_index = (i-1)*trace.bin.size+1
            
            # set state variables
            state_transitions = 0
            active_to_inactive_transitions = 0
            active_now = 0
            
            # start scanning through activity_df, monitoring total transitions and wake-to-sleep transitions
            for (bin in start_index:(start_index+trace.bin.size)) {
                if (active_now == 1) {
                    state_transitions = state_transitions + 1
                    if (activity_df[bin,f] == 0){
                        active_to_inactive_transitions = active_to_inactive_transitions + 1
                    }
                }
                if (activity_df[bin,f] > 0) {
                    active_now = 1
                }
                else {
                    active_now = 0
                }
            }
            
            # calculate pdoze
            if (state_transitions > 0) {
                pdoze_binned[i,f] = active_to_inactive_transitions / state_transitions
            }
            else {
                pdoze_binned[i,f] = NA
            }
        }
    }
}

# make empty matrices to bin pwake
pwake_binned = matrix(,(dim(activity_df)[1]/trace.bin.size),dim(activity_df)[2])

# loop by fly
for (f in 1:(dim(activity_df)[2])) {
    # skip over the dead ones
    if (is.na(activity_df[1,f])) {
        pdoze_binned[,f] = NA
    } else {
        # then loop by bin 
        for (i in 1:((dim(activity_df)[1]/trace.bin.size)-1)) {
            start_index = (i-1)*trace.bin.size+1
            
            # set state variables
            state_transitions = 0
            inactive_to_active_transitions = 0
            active_now = 1 # this one line is the whole reason I had to duplicate this block of code for pwake...
                           # there's definitely a way to consolidate it, but the starting assumption slightly
                           # changes the math, so for now I'm leaving it like this, even if it's
            
            # start scanning through activity_df, monitoring total transitions and wake-to-sleep transitions
            for (bin in start_index:(start_index+trace.bin.size)) {
                if (active_now == 0) {
                    state_transitions = state_transitions + 1
                    if (activity_df[bin,f] > 0){
                        inactive_to_active_transitions = inactive_to_active_transitions + 1
                    }
                }
                if (activity_df[bin,f] > 0) {
                    active_now = 1
                }
                else {
                    active_now = 0
                }
            }
            
            # calculate pdoze
            if (state_transitions > 0) {
                pwake_binned[i,f] = inactive_to_active_transitions / state_transitions
            }
            else {
                pwake_binned[i,f] = NA
            }
        }
    }
}

# then label these columns with the condition they're in, and add a ZT column for good measure
colnames(pdoze_binned) = unlist(channel_setup,use.names=FALSE)
colnames(pwake_binned) = unlist(channel_setup,use.names=FALSE)

pdoze_binned = cbind(chopped_trace_df[1],pdoze_binned)
pwake_binned = cbind(chopped_trace_df[1],pwake_binned)

if (save_pwakepdoze) {
    pwkdz_pathname = paste(experiment_folder,'outputs_pwake_pdoze/',sep='')
    dir.create(pwkdz_pathname)
    
    write.csv(pdoze_binned, file = paste(pwkdz_pathname,'pdoze.csv',sep=''), row.names = F)
    write.csv(pwake_binned, file = paste(pwkdz_pathname,'pwake.csv',sep=''), row.names = F)
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

if (save_rebound) {
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
    
    # write to output folder
    rebound_pathname = paste(experiment_folder,'outputs_rebound/',sep='')
    dir.create(rebound_pathname)
    write.csv(do.call("rbind", rebound_stats), file = paste(rebound_pathname,'rebound.csv',sep=''), row.names = F)
}

if (clear_workspace_and_console) {
    rm(list=ls())
    cat('\014')
}
