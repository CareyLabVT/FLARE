#!/bin/bash

LOGFILE=/data/SCCData/noaa-logs/git.log
TIMESTAMP=$(date +"%D %T")

echo -e "\n\n-------------- $TIMESTAMP --------------" 2>&1 | tee -a $LOGFILE

echo -e "\n\nPull the R Script from GitHub:\n" 2>&1 | tee -a $LOGFILE

cd /data/SCCData/noaa-source #https://github.com/CareyLabVT/SCCData/tree/noaa-source
git pull 2>&1 | tee -a $LOGFILE
echo -e "\n" 2>&1 | tee -a $LOGFILE
for i in *.R; do Rscript $i 2>&1 | tee -a $LOGFILE; done

echo -e "\n\nGitHub Server:\n" 2>&1 | tee -a $LOGFILE

cd /data/SCCData/NOAA #https://github.com/CareyLabVT/SCCData/tree/noaa-data
#git pull &>> ~/data/SCCData/noaa-logs/git.log
git add *.*
git commit -m "$TIMESTAMP: Git Backup" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

cd /data/SCCData/NOAA/FCRE #https://github.com/CareyLabVT/noaa_gefs_forecasts/tree/fcre
#git pull &>> ~/data/SCCData/noaa-logs/git.log
git add .
git commit -m "$TIMESTAMP: Git Backup" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

cd /data/SCCData/NOAA/SUGG #https://github.com/CareyLabVT/noaa_gefs_forecasts/tree/sugg
#git pull &>> ~/data/SCCData/noaa-logs/git.log
git add .
git commit -m "$TIMESTAMP: Git Backup" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

cd /data/SCCData/NOAA/SUNP #https://github.com/CareyLabVT/noaa_gefs_forecasts/tree/sunp
#git pull &>> ~/data/SCCData/noaa-logs/git.log
git add .
git commit -m "$TIMESTAMP: Git Backup" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

echo -e "\n\nPush the Logs to GitHub:\n" 2>&1 | tee -a $LOGFILE

cd /data/SCCData/noaa-logs #https://github.com/CareyLabVT/SCCData/tree/noaa-logs
git add .
git commit -m "$TIMESTAMP: Logs" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE
git add .
git commit -m "$TIMESTAMP: Logs" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE
