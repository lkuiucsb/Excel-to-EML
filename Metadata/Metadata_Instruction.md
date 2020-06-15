# Read me for "Metadata.xlsx"
   
## Date created (YYYYMMDD) | Date Modified (YYYYMMDD)

20191008 | 202006015

## Summary

This is an instruction describing how to use the Excel metadata workbook (".xlsx") for filling out metadata information. "Metadata.xlsx" is the main metadata storage for dataset description and information essential to creating an EML document. "File descriptions" will detail each Excel Sheet from the first (left) to the last (right) in order. Get ready, this is long!

Ecological Metadata Language (EML) standards and specification can be seen: https://knb.ecoinformatics.org/external//emlparser/docs/index.html 

## File descriptions

### DataSet

The first step to starting your archive. Enter data packet information in this sheet. 

"datasetid": has to be numeric and is the numeric value indicating each individual "project" or data packet.

"alternatedid": is the id used in the "PackageID" at the beginning of the EML document. If you use EDI (Environmental Data Initiative), this id is generated by the EDI system: https://portal.edirepository.org/nis/reservations.jsp (Require login). Depending on your data archive node, the format can be very different. 

"edinum": indicate revision, starting with revision number 1 and increase this number as you revise the data package over time. Useful for updating datasets.

"title": should describe the data packet itself and not the same as the publication it is associated with. EDI suggests at least five words to make it descriptive enough for users.

"Temporal_begindate/enddate": The start and end dates of your datasets. 

"pubdate": intended or actual data packet publication date (YYYY-MM-DD).

"bp_setting": this is the boilerplate setting, see more details in the "boilerplate" tab. 

"project_PI": this is the name id from the listPeople tab. 


### DataSetEntities

"datasetid": dataset ID assigned in "DataSet".

"entity_position": numerated index for each data entity to be archived. Start at "1". This is for some data package that includes more than one datasheets. 

"entitytype": type of document to be archived. R function currently only deal with two types: "dataTable" or "otherEntity"

"entityname": should describe the individual data table itself. Similar to the data packet title, at least five words to make it descriptive for users.

"entitydescription": detailed description of the data table. Usually include sample size and general information not explained elsewhere.

"filetype": specific file type for the dataset. Refer to the "FileTypeList" for possible file types and notes.

"urlpath": url location for the dataset if they have another copy stored somewhere else.

"filename": full file names for the dataset(s) to be archived.

### DataSetAttributes

"attributeName": individual column names for each dataset uploaded. Suggest to copy and paste names directly from the data tables to avoid errors. (e.g. lat)

"attributeDefinition": detailed description of the column. Include any information that will not be explained including datums used, exceptions, etc. be specific!

"class": type of data for the attribute. Chose from the drop-down list. Typically text field would be "character", number fields would be "numeric", date/time fields would be "Date", and any categorical data that you want to describe in more details in the "DataSetAttributeCodeDefinition" will be "categorical" class. 

"formatString": defines the formatting for the "Date" class, e.g. "YYYY-MM-DD"

"unit": require for "numeric" class and it is measurement units from the "listUnitDictionary" list. Refer to "listUnitDictionary" for options.

"precision": describes the precision (how many decimals) for the "numericDomain" attribute. (34.6667 precision is 0.0001)

"numberType": describes the type of number used in this field. Typically use "real" or "integer".

"missingValueCode": describes the code (e.g. "NA", ".", "-9999", "NULL") that indicates missing values for numerical column. DO NOT USE BLANK AS MISSING VALUE FOR YOUR DATASET! Any missing value code is okay to use as long as it is defined here. Remember the main purpose is to facilitate the use of data and missing value codes can be translated by users when defined.

"missingValueCodeExplanation": explain use of "missingValueCode". Suggest using "Value not recorded or not available"

### EMLAttributeCodeDefinitions

"datasetid": dataset ID assigned in "Dataset".

"entity_position": entity position assigned in "DataSetAttributes"

"attributeName": attributes that were defined as "categorical" in "DataSetAttributes" for each of the dataset entities. Remember this is the column name. This will be repeated for each factor.

"code": the actual code or names used for each categorical values in the column. Must be identical to the datasets!  A good time to check your data management and quality control!

"definition": specific definitions that describe each "code".

### DataSetKeywords

"datasetid": dataset ID assigned in "Dataset".

"keyword": selected keywords linked to this data packet. One of the few places with quite a bit of freedom. But try and use some more common and standard keywords to make data more visible.

"keyword_thesaurus": describes the source for the keywords. Largely depends on data publication requirements from agencies or funding sources. EDI has additional guides (https://environmentaldatainitiative.org/five-phases-of-data-publishing/phase-3/controlled-vocabularies/).

### DataSetPersonnel

"datasetid": dataset ID assigned in "Dataset".

"authorshiprole": This is the role of the author for the data packet. You could have many different roles for the personnel. Make sure to have at least one "creator" and one "contact" for each dataset. 

"peopleid": This is the peopleid listed in the "listPeople" table. 

### DataSetSites

"datasetid": dataset ID assigned in "Dataset".

"geographicdescription": describes the geographic location where the data was collected and/or generated.

"northboudingcoordinate": coordinate for the north-most latitude of the location in decimal degrees.

"southboundingcoordinate": coordinate for the south-most latitude of the location in decimal degrees.

"eastboundingcoordinate": coordinate for the east-most longitude of the location in decimal degrees.

"westboundingcoordinate": coordinate for the west-most longitude of the location in decimal degrees.

*four coordinates could draw a rectangular area, line or single point depending on the data*

### ListPeople
"peopleid": id for the people you list here. 

"givenname": first name or given name

"givenname2": middle name or initials. Leave blank if none.

"surname": last name or family name

"organization": organization of the author

"address1": first line of organization address

"address2": second line of organization address

"city": city of the organization

"state": state of the organization

"country": country of the organization

"zipcode": zipcode of the organization address

"phone": author phone number. Leave blank if none.

"email": author email address

"orcid": author Open Researcher and Contributor ID (ORCID). Leave blank if none. 

### Boilerplate
"bp_setting": Boilerplate setting that refer to the scope and userid

"scope": this is "scope" in namespace of the EML document. If you use EDI system, this will be EDI. 

"userid": This is for the "principal" access part of the EML. It is the account name for EDI user. 
 
### ListFileType- Reference

Reference sheet containing the information for the various file types that can be archived. Typical ".csv" output from "R" would either be "csv_E" or "csv_F". The main difference between the two is that "csv_E" are ".csv" files that have quoted strings ("") around some characters. Check the dataset using "WordPad", "Rstudio" or any other text editor.

### ListUnitDictionary - Reference

A huge list of available measurement units to use for numeric attributes used or described. Do a quick "CTRL+F" search for the units you want before creating a new unit. Refer to the available units if you need to create a new unit. Remember to enter "TRUE" under the "custom" column for a customized unit. If you are using a customized unit, the "R" script will produce some "warnings()" when executed to let you know that it noticed some units that were not part of the default EML unit list.

## Additional notes

You have reached the end of this long instruction document! By now, you should realize that there this a very comprehensive manner for archiving and publishing environmental and ecological data. If there are any general or specific questions, make use of the resources on the EDI website (https://environmentaldatainitiative.org/search-this-site/). 