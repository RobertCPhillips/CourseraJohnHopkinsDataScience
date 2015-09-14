Cleaning Data Project
---------------------
The script run_analysis.R is the main script.  It downloads the zip file from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" into the working directory as a file named "harDataSet.zip"

Once the file is unzipped, the conatined text files are processed per the comments in the script.  This script uses the _dplyr_ package.

The output file is named data.summary.txt.