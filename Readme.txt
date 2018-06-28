This is a short documentation describing how to use a series of R codes to create an EML document for ecological data archive. 

The R code reads the metabase in excel format, uses EML package in R, and generates an EML file in xml format. Because the R functions navigate among different files based on their relative locations, the name and location of the folder should not be changed (unless you also want to make changes on the file paths in the R functions). 

Several preparation steps to make sure the code could run smoothly:

1. Install all the library needed (listed in the batch_xml.R file)

2. The IntellectualRights.docs document has the CC-BY license. If you prefer any other licence, please replace all the content in this document. 

3. In the boilerplat.xml document, search for "person_name" (in the access childnote), and replace it with your registered name in EDI. In addition, fill in all the contact info for the organization and publisher. These contents are for the organization, not a personal contact. Generally, you only need to fill in this information once and never need to change. If you need to frequently change the organization contact and publisher, you might want to put it into the R functions to automate this info in your EML. 

4. Project.99 and project.1001 are examples of showing you how you can fill in the metadata excel file. For adding a new dataset, you generally only need to fill in the first 9 sheets, from left to right. The 6 sheets at the end are for vocabulary controls. Thus, if you have question about what you can put into some of the columns, refer to these 6 sheets.  

5. In any of the project folder (your dataset folder), we have to have Abstract.** (dataset ID), Method.**, the dataset(s) you want to archive, and a R code called project.**

6. Run the R code in the project folder, from top to bottom, you should be able to generate a good EML document. Note, change the datsetid, and be cleared about how many entities in your dataset package. One-table data package can be refered to an example in project.1001 and any data package had more than one data tables (or any non-tabular file) can be refered to example in project.999.

7. It is always nice to open your EML in Oxygen to double check if things look right before uploading to EDI. 

To create an EML for a new dataset, follow steps below: 

a. assign a project ID for a in-coming data, and create a folder called "project.**". 

b. In the "project.**" folder, paste in all the data tables (or other data related files); create an abstract and method document; paste in "project.**.R" file. 

c. Open the Metadata.xlsx file and fill in all the meta data. 

d. run project.**.R file. Then you should have a EML file called "XML_**.xml" in your project folder.  