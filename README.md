# InsectsArctic

This is the Git reposity for taking DNA barcoding data from BOLD and generating various diagrams, maps, charts, etc.

## Data sources

### Data from the Kitikmeot region of Nunavut

The data included in this repository are from specimens that I and others at Polar Knowledge Canada, the University of Guelph, ARCBIO, and other participants collected in the communities of Cambridge Bay, Kugluktuk, Gjoa Haven, and Kugaaruk.

It is attached to this repository as a 7Zip archive titled "data.7z". Simply extract to a folder called "data/"

Since the sequencing data has been downloaded as FASTA format files, the use of the R library "phylotools" to read the FASTA format, selecting the data required and exporting as TSV was used.

Note: I chose to download the specimen data and sequencing data separately and used R to match the BIN numbers to the Sample ID's.

Since I can vouch for the collection and overall accuracy of this data set, it's the primary data set used. The other data sets are to be used for comparison only.

### Public BOLD data

You can also download data from BOLD via the public data portal by selecting terms like "Nunavut" or other provinces/territories as a search term and filtering from there within R. Here is the public data portal:

https://boldsystems.org/index.php/Public_BINSearch?searchtype=records

For the provinces/territories, you simply need to download the data for each province/territory as TSV files. I would suggest downloading in the combined specimen and sequencing TSV format. This data is time consuming as the downloads may fail and will take some time to download.

Once the individual province/territory Barcode data is downloaded, use the repository here to stitch it all together into a single TSV file: https://github.com/hominidae/ProcessBOLDPublicData

Alternatively, here is Google Drive link to the data:
https://drive.google.com/file/d/17Hsf81Qq95XPkpZpbaJrfSDK0hQ5D13x/view?usp=share_link

(Warning: File is 2.13GB)

### Global Biodiversity Information Facility data

In addition, human observation data from GBIF is used as well. You can perform a search an occurance search on GBIF here:
https://www.gbif.org/occurrence/search

Select combined csv file as the download, you can elect to go with human observations or research grade observations.

Alternatively, here is a Google Drive link to the data:
https://drive.google.com/file/d/1unJboimp8S8fS0mzVaAiSPFR448ru_j7/view?usp=share_link

(Warning: File is 628MB)

## Process DNA sequencing data
Run 001_LoadSequencingData.R to process the available sequencing data from "data.7z" and save as a tsv file.

This is necessary because the BOLD specimen data alone does not contain the unique BIN numbers for each processed specimen when downloading through the BOLD Data Console.
