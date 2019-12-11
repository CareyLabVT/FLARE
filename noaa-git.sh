GFILE=/data/SCCData/noaa-logs/git.log
TIMESTAMP=$(date +"%D %T")

echo -e "\n\n-------------- $TIMESTAMP --------------" 2>&1 | tee -a $LOGFILE

echo -e "\n\nPull the R Script from GitHub:\n" 2>&1 | tee -a $LOGFILE

cd /data/SCCData/noaa-source
git pull 2>&1 | tee -a $LOGFILE
echo -e "\n" 2>&1 | tee -a $LOGFILE
for i in *.R; do Rscript $i 2>&1 | tee -a $LOGFILE; done

echo -e "\n\nGitHub Server:\n" 2>&1 | tee -a $LOGFILE

cd /data/SCCData/NOAA
#git pull &>> ~/data/SCCData/noaa-logs/git.log
git add *.*
git commit -m "$TIMESTAMP: Git Backup" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

cd /data/SCCData/NOAA/FCRE
#git pull &>> ~/data/SCCData/noaa-logs/git.log
git add .
git commit -m "$TIMESTAMP: Git Backup" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

cd /data/SCCData/NOAA/SUGG
#git pull &>> ~/data/SCCData/noaa-logs/git.log
git add .
git commit -m "$TIMESTAMP: Git Backup" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

cd /data/SCCData/NOAA/SUNP
#git pull &>> ~/data/SCCData/noaa-logs/git.log
git add .
git commit -m "$TIMESTAMP: Git Backup" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

echo -e "\n\nPush the Logs to GitHub:\n" 2>&1 | tee -a $LOGFILE

cd /data/SCCData/noaa-logs
git add .
git commit -m "$TIMESTAMP: Logs" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE
git add .
git commit -m "$TIMESTAMP: Logs" 2>&1 | tee -a $LOGFILE
/home/scc/applications/git-retry.sh push -f 2>&1 | tee -a $LOGFILE

