# rebound
uses functions from Phase (developed by Shafer lab) to calculate sleep stats and rebound for experiments using the Drosophila Activity Monitor (DAM)

# setup
## prep files
create a folder on your hard drive to serve as the experiment folder. put a copy of channelconfig.csv, reboundconfig.csv, and phase_extract.R into this folder.

## prep DAM data
use DAMFileScan (from Trikinetics website) to scan DAM data. sum the activity counts into each bin, and do not create channel files. move the scanned files to the experiment folder.

## edit channelconfig.csv
this file lists all conditions in the experiment. each column corresponds to a different monitor file in alphanumeric order by the filename of the scanned DAM file (e.g., for M024 through M026, column 1 is M024, column 2 is M025, and column 3 is M026). each row is the DAM channel in that monitor.

you can use whatever condition names you want, but do not use '/' in the names.

## edit reboundconfig.csv
this file lists rebound calculations to be performed. each comparison to be made is one row (e.g., if you want to compare sleep change in METH- and AMPH- exposed flies to control flies, you'd use two rows: one for METH, and one for AMPH). enter the control condition into the first column and the experimental condition in the second column (making sure these condition names match exactly the ones provided in channelconfig.csv).

the script will output the change in sleep time for each fly in the experimental group, relative to the average change in sleep time in age-matched control flies (see Abhilash, Evans, & Shafer 2026 Fig. 4 for a graphic).

## set up script
add the filepath, filenames, and experiment dates, as listed in the annotations in the script.

## run the script
run it! if you don't have any rebound comparisons to make, you can still run the script to get sleep stats and traces sorted by group, but it might throw up an error.
