#Author: Ranjeet Devarakonda. (2012). Daymet: Single Pixel Data Extraction Tool. Available: http://daymet.ornl.gov/singlepixel.html.
#Oak Ridge National Laboratory
#Java 1.6

java -Xms512m -Xmx1024m -Dhttps.protocols=TLSv1.1,TLSv1.2 -jar daymet_multiple_extraction.jar latlon.txt
