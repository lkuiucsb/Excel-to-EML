# Read me for EDI_data_packets
   
## Date created (YYYYMMDD) | Date Modified (YYYYMMDD)

20191008 | 20200603

## Summary

This is short documentation describing how to use R programming language to create an EML document from an Excel metadata workbook (".xlsx") for the ecological data archive. The "R" script reads the metadata Excel workbook, and generates an EML file in ".xml" format. 

This EDI data packet generation script requires these packages installed with the latest version: 1) EMLassemblyline, 2) XML 3) readxl and 4) dplyr. 

Ecological Metadata Language (EML) standards and specification can be seen: https://knb.ecoinformatics.org/external//emlparser/docs/index.html 

## List of files included in the EML generation process

   1.EML_generation
   
    All functions stored in EML_funs
    
   2.Metadata - Metadata Excel workbook, and its instruction
    
   3.project.99 and project.100 as examples

## File descriptions

### EML_generation

**EML_generation** stores the main "R" functions and template information for the creation of the ".xml" file. The functions in the "R" script and codes navigate among different files based on their relative locations, the name and location of the folder should not be changed (unless you also want to make changes on the file paths for all "R" functions used). 

### Metadata

For adding a new dataset in the Excel workbook, you need to fill in the first 7 sheets, from left to right. The 4 sheets at the end are lists of peoples (may need to update once a while if you have new personnel) and for vocabulary controls. Refer to the "Metadata_Readme.md" for a detailed guide on each sheet of the "Metadata.xlsx".

### project folder

For a new dataset, create its won project folder called "project.XXX" ("XXXX" = dataset ID). Here, the project.99 is a vegetation dislodgment data package as an example.

In the project folder, we have to have "abstract.docx", "methods.docx", the dataset(s) you want to archive, and an "R" script called "project.XXXX.R" ("XXXX" = dataset ID). 

Make sure the "abstract.docx" document is the abstract for the data instead of the abstract for article publications. If the dataset has a protocol, include a URL within the methods document. 

As of today, 06/15/2020, the abstract and methods document can't handle bold and hyperlink. Make sure to remove those before generating the EML. Otherwise your EML will be invalid. 

## Run R code
Once you have all the metadata information filled out and all the relevant files placed in the project folder, run the "project.XXXX.R" in the project folder, from top to bottom to generate your EML document. The script will generate error messages if any part of the validation fails while "R" would also provide additional "warnings()" and additional messages should it arise when executing the functions.

## Additional information

The R code and functions here are built on top of the **EMLassemblyline**. So if you update the EMLassemblyline package with the major revision, the code could break. See the GitHub repo: https://github.com/EDIorg/EMLassemblyline.

The R codes currently accommodate the CC-By license, if you want to use other data publication license, you want to modify the R function "generate_EML_Assemblyline" around L123 to choose other licenses or read in a special license that in a Word document. 
  
If you manage more than 50 datasets, another similar tool I want to recommend is the **Core-metabase**. This is the tool developed using PostgresSQL which has great benefits from additional controls and tables (vocabulary controls or extra tables for long-term data management purposes). The core-metabase means to manage a large number of datasets. The installation code for the LTER-core-metabase is here: https://github.com/lter/LTER-core-metabase. An R package has developed parallel to the LTER-core-metabase for generating EML: https://github.com/BLE-LTER/MetaEgress.