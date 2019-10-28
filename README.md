# Analyis of Open Peer Review at PeerJ and Royal Society.
 
This is a repository to analyze open peer reviews at PeerJ and the Royal Society Open Science and Royal Society Open Biology. It accompanies the manuscript "Researchers Decision to Sign Reviews is Related to Their Recommendation"

It contains scripts to download all reviews and convert them in text files that are textmined and analyzed. 

## For PeerJ articles:

PeerJ has one main section, and more recently created specific subsections, of which only computer science has enough submissions to take along (225 at the time of analysis). 

1) Run peerj_download_reviews_create_text.R. This script will cycle through the websites https://peerj.com/articles/1/reviews/ to https://peerj.com/articles/7930/reviews/ and download the html on each page (the number 7930 is hardcoded - update if you want to download newer articles as well). It will store each html page as a text file. In this text file text labels are added (so the txt files are not just the html text). The script adds:
- label_version
- label_recommendation
- label_br (basic reporting)
- label_author (author of the review)
- label_ed (experimental design)
- label_votf (validity of the findings)
- label_cfta (comments for the authors)

These labels are used to find information  in the text mining phase.

For PeerJ Computer Science, repeat the step above by running peerj_cs_download_reviews_create_text.R.

2) run textmining_peerj.R. This script will go through all the review files in the peerj_reviews_txt folder (change the number 7930 is updating this script in the future) and extract the information for the final dataframe: 
- df_link: Link to the reviews
- df_section: section of peerJ the article is published in
- df_days: Days between submission and date of the current version
- df_version: Version of the manuscript (revision number). Note this is coded from most recent to oldest. Hence, 1 is often the decision letter from the editor (and hence, there are no reviews for version 1), while there will be reviews for higher (older) version numbers. 
- df_recommendation: Recommendation (for PeerJ by the editor)
- df_word_count: Word count for all different sections the reviewer fills in.
- df_masked: Is the reviewer masked or known
- df_reviewer_name: If known, what is the name of the reviewer (if not known, Reviewer and number)

This script saves the generated dataframe (where each row contains the data for one review for a version of the manuscript) as a rds file: peerj_data.rds. 

For PeerJ Computer Science, repeat the step above by running textmining_peerj.R which stores data in peerj_cs_data.rds.

## For Royal Society Open Science articles:

1) Run 'download_RSOS_pdf.R'. This will download the RSOS reviews using the scopus_export_rsos.csv, which contains a Scopus exported list of all articles in RSOS.

For 7 papers, RSOS ISSN number articles actually have a rsob DOI in scopus, and these are indeed Open Biology. These are not downloaded (the script has a web addresss with rsos hard-coded so will return errors). For 6 articles the incorrect doi is included in Scopus, e.g., for http://dx.doi.org/10.1098/rsos.190279. These articles are also not downloaded. 

2) We now have a folder with downloaded pdf files. However, some are corrupt and can not be opened, because the reviews are not open, but the download script still stored a PDF. Run 'check_corrupt_royal_society_open science.R' which tries to read each PDF. When succesful, it stored the ID in a list. This list is written as OS_pdf_list.txt, which thus contains all ID's of articles that have open peer review. 

3) Run royal_society_open_science_create_txt.R. The script reads in all pdf files in the OS_pdf_list.txt and transforms them to text files, which are also stored. In this text file text labels are added (so the txt files are not just the html text). The script adds:
- label_version
- label_recommendation
- label_br (basic reporting)
- label_author (author of the review)
- label_ed (experimental design)
- label_votf (validity of the findings)
- label_cfta (comments for the authors)

These labels are used to find information in the text mining phase.

2) run textmining_open_science.R. This script will go through all the review files in the open_science folder in the royal_society_txt_files folder and extracts the information for the final dataframe: 
- df_link: Link to the reviews
- df_section: section of peerJ the article is published in
- df_days: Days between submission and date of the current version
- df_version: Version of the manuscript (revision number)
- df_recommendation: Recommendation (for PeerJ by the editor)
- df_word_count: Word count for all different sections the reviewer fills in.
- df_masked: Is the reviewer masked or known
- df_reviewer_name: If known, what is the name of the reviewer (if not known, empty)
- df_reviewer_number: The reviewer number assigned by Royal Society - this can be linked to their individual recommendation in the datafile.

This script saved the generated dataframe (where each row contains the data for one review for a version of the manuscript) as a rds file: royal_society_data_os.rds.  

## For Royal Society Open Biology articles:

The procedure is basically identical to the Open Science journals. The script contains one tiny difference (recommendations are found searching for "Recommendation?"" not Recommendation"). The data file generated is royal_society_data_ob.rds.