# InsectsArctic

This is the Git reposity for taking DNA barcoding data from BOLD and generating various diagrams, maps, charts, etc.

## Data sources

### Data from the Kitikmeot region of Nunavut

The data included in this repository are from specimens that I and others at Polar Knowledge Canada, the University of Guelph, ARCBIO, and other participants collected in the communities of Cambridge Bay, Kugluktuk, Gjoa Haven, and Kugaaruk.

It is attached to this repository as a 7Zip archive titled "Data.7z". However, since all of these data have relied on public funds the data is generally considered to be open source. You can also download these data from BOLD directly and it will be part of any search there too.

Since the sequencing data has been downloaded as FASTA format files, the use of the R library "phylotools" to read the FASTA format, selecting the data required and exporting as TSV was used.

Note: I chose to download the specimen data and sequencing data separately and used R to match the BIN numbers to the Sample ID's.

Since I can vouch for the collection and overall accuracy of this data set, it's the primary data set used. The other data sets are to be used for comparison only.

### Public BOLD data

You can also download data from BOLD via the public data portal by selecting terms like "Nunavut" or other provinces/territories as a search term and filtering from there within R. Here is the public data portal:

https://boldsystems.org/index.php/Public_BINSearch?searchtype=records

You simply need to download as a TSV file. I would suggest downloading in the combined specimen and sequencing TSV format for this part.

### Global Biodiversity Information Facility data

In addition, human observation data from GBIF is used as well. You can perform a search an occurance search on GBIF here:
https://www.gbif.org/occurrence/search

## Process DNA sequencing data
Run 001_LoadSequencingData.R to process the available sequencing data from "Data.7z" and save as a tsv file.

This is necessary because the BOLD specimen data alone does not contain the unique BIN numbers for each processed specimen when downloading through the BOLD Data Console.

The active columns are "seq.data","seq.text","process.id","taxon","sample.id","bin.uri"

However, we're only really interested in the "bin.uri" and "sample.id"

## Load collection data, taxonomy, and sequencing data

Run 002_LoadBOLDData.R to change "sample.id" in the sequencing data to "Sample ID" and perform an inner join with the other datasets.

Then script then saves that all as "working_dataset.tsv" which is the combined sequencing data and other data in one file.

## Process BOLD data

003_ProcessBOLDData.R is not done yet, but we'll leave it as is for now. Essentially, it will take the BOLD Public data from just Cambridge Bay and Kugluktuk and generate various graphs and figures.

Before that happens though, I need to perform a unit test on the ggVennDiagram code to confirm it's actually doing what I intended it to.

## Process the combined Data.7z data

004_OwnDataOnly.R processes the data from Data.7z

So far the code doesn't do much. Will update as I go. But essentially, I'll be creating several community composition diagrams linking communities by shared their shared BIN's.

The intent is to answer questions like "What species are present in only one location?" and "What species are shared between communities in the Kitikmeot?" are intended to be answered.

The previous 003_ProcessBOLDData.R script is intended to form a comparison against both the NWT and the rest of Nunavut. However that data will need to be sterilized of various error types.

I am starting with Springtails, also known as Collembola. As per this paper: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0230827

From that paper, I will be looking at differentiating the community composition of various species of Springtails: Poduromorpha, Symphypleona, Entomobryomorpha between the communities of Cambridge Bay, Kugluktuk, Gjoa Haven, and Kugaaruk.

After Springtails, I will perform the same rudimentary comparative analysis for several other insect species in the collected sample sets.

## Mapping
Along with the charts/figures/diagrams generated in the two previous scripts, I will also utilize GIS mapping features within R to generate maps like these.

https://imgur.com/gallery/p87EEdo

Long term, I'd like this to be a website that is interactive but for now generating static image pointmaps will have to suffice.
