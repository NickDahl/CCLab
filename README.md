# CCLab
Yale Carbon Containment Lab GitHub Repository

## File management ###########################################
All project files and working code must be kept on GitHub.
Never email files. Keep them on GitHub and provide a location, e.g.
~\CCLab\AMMWork\RPackage\Log File.txt
If you are making changes to the content of an important file,
Save a copy of this file in ~\CCLab\Drafts\
Change the name of the copy to include the date, e.g., Log File 2019-03-29.txt
All .zip or other compressed files should be downloaded to ~\Source\Zip\ and then the contents extracted to the appropriate subfolder in ~\Source\.

Other
The subject line of all project-related emails should start with sub-project.
Always reply to confirm appointments.
Write dates in the following format: YYYY-MM-DD

Documentation
Document everything. Replication is the foundation of research, and anyone should be able to replicate your work based upon your comments. 
If you left the project today, someone should be able to walk in tomorrow and pick up where you left off based on your documentation.
As-received files.
Every data file we receive from a data partner, whether downloaded from a website, attached to an email, or handed to us on a thumb drive, 
must have a log file that describes how we obtained it.
At the top of each log file, include the following details:
Data source
Description of file
Your name
Date started
Date last edited
Name of received file
Within each log file, describe the mode of receipt (email, URL, etc.), date, any instructions for how to obtain the dataset (where to click on the website, name of data series, dropdown menu options, etc.), and whether you had to do anything further to the file, e.g., unzip it.
If there is a second (or third …) file, repeat the previous step in a new section.
If there is any metadata for the data, either download the file and put the file name in the log file, or give the URL for the metadata page.
Save the log file as a .txt file.

Example log file:

Data source:	WRI CAIT
Description:	Greenhouse gas emissions data
Name(s)	:	Dahl, N.
Date started:	2019-03-29
Last edited:	2019-04-08
File received:	CAIT_Country_GHG_Emissions_-_csv_10022017.zip

##### 2019-03-29 #####
Received via web: http://www.wri.org/resources/data-sets/cait-historical-emissions-data-countries-us-states-unfccc
Date downloaded:	2017-10-08
Special instructions:
	In the upper left corner, click on the red “Download as table” button. A pop-up window will then ask for your email address, which can be left blank. Click on “next.” It will then ask for which format you want. Select “CSV.” Click on download. You will receive a zipped file with four files in it:

CAIT Country CO2 Emissions.csv
	* This has several variables related to CO2 emissions, broken down by sub-type.
CAIT Country GHG Emissions.csv
	* This has several variables related to different greenhouse gases, and different versions of those gases.
CAIT Country Socio-Economic Data.csv
	* This has population and two versions of GDP
License.txt
	* Contains metadata.

Saved this zip file in ~\CCLab\Source\Zip\

## Code #######################################################
At the top of each script, include the following details:
Project title
Description of what the code does
Your name
Date started
Date last edited
Input files
Output files
setwd is never allowed in the code.
If the script has several discrete tasks, divide it up into sections and make verbose comments about what is happening in each task.
Throughout the script, make note of what is happening, especially if it is novel or complicated.
Throughout the script, make notes of places where you are uncertain or have questions.

Missingness
-9999	the as-received dataset has cells with missing values
-8888	the country is not in the as-received dataset
-7777	the missing values are missing because they are not material
-6666	the normalizing variable is missing
-5555	the as-received dataset does not cover the years
-4444	the data values are not reliable due to country size
