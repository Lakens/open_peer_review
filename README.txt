This zipfile contains 6 R scripts, 5 txt files and 2 csv files.

The csv files are the datasets for PeerJ and TRS.
Article_ID_OB and Article_ID_OS contain al the article ID's used for crating the TRS dataset.
OB_pdf_list and OS_pdf_list conatin the ID's of articles that did not have a corrupted pdf file (mostly due to the review history not being published).

The PeerJ code and RoyalSociety code are the codes used for downloading, cleaning and adding labels to the reviews and turns them into workable txt files.

#Royal Society code
First the pdf's need to be downloaded using Article_ID_OB and Article_ID_OS.
The downloading script for TRS is in comments, because the pdf's only have to be downloaded once. 
The script has to be run twice, once for Open Biology and once for Open Science (the different lines of codes for Open Science are comments).
After downloading the files the corrupted code can be used to create a txt file (OB_pdf_list and OS_pdf_list) with only the ID's of the non corrupted pdf's.
Then the rest of RoyalSociety coda can ben run (put download into back into comments) resulting in the txt files for TRS.

#Corrupted
Need to be run after the pdf's are downloaded to seperate the working pdf's from the corrupted pdf's
Needs to be run once for OB and once for OS.
Creates OB_pdf_list and OS_pdf_list 

#PeerJ code
The PeerJ code can be run without any difficulties resulting in the txt files for PeerJ


The text mining code for The Royal Society and PeerJ are used to extract data from the txt files and create a csv file with the data

#TM_RoyalSociety
The text mining code for the royal society needs to be run twice, once using the OB_pdf_list and once using OS_pdf_list.
The code creates a dataframe from the data contained in the txt files. This data needs to be stored into 2 df's (OB_df and OS_df) which then can be combined into
one dataframe (total_df). These df's can be found at the last lines of the codes. The total_df is then turned into a csv file (TRS_Dataset.csv)

#TM_PeerJ
The PeerJ code can be run without any difficulties resulting in the csv file for PeerJ. (PeerJ_Dataset.csv)

#Report_code
The report code uses the two csv files to analyze the data and create the graphs and tables used in the report.