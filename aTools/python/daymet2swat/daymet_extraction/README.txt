#Author: Ranjeet Devarakonda. (2012). Daymet: Single Pixel Data Extraction Tool. Available: http://daymet.ornl.gov/singlepixel.html.
#Oak Ridge National Laboratory
#Java 1.6

Instructions for multiple location extraction:

1.    Unzip the attached daymet.zip onto your desktop
2.    Edit latlon.txt with desired location (lat lon) information
3.    Open command line terminal from your unzipped daymet directory
4.    From the command line run the following: java -Xms512m -Xmx1024m -Dhttps.protocols=TLSv1.1,TLSv1.2 -jar daymet_multiple_extraction.jar latlon.txt OR Simply run daymet_multiple_extraction.sh
5.    Above program creates separate CSV files for each lat lot. All variables, all years.

	
