# InsectsArctic

This is the Git reposity for taking DNA barcoding data from BOLD and generating various diagrams, maps, charts, etc.

## Data

First, you will need to download the data. I've used 7Zip to compress the data I and others at Polar Knowledge Canada, the University of Guelph, and ARCBIO collected in the communities of Cambridge Bay, Kugluktuk, Gjoa Haven, and Kugaaruk.

It is attached to this repository as a 7Zip archive titled "Data.7z". However, since all of these data have relied on public funds the data is generally considered to be open source.

As a result, you can also download this data from BOLD via the public data portal by selecting "Nunavut" as a search term and filtering from there.

https://boldsystems.org/index.php/Public_BINSearch?searchtype=records

However, I suggest that you download the archive here instead because the public data available from BOLD is not sanitized. The introduction of errors as a result of data entry issues, poor field notes, errors in GPS coordinates, etc does result in problems during processing the data.

Note: I chose to download the metadata and sequencing data separately and using R to match BIN numbers to RecordID's.

Since the sequencing data has been download as FASTA format files, the use of the R library "phylotools" to read the FASTA format, selecting the data required and exporting as TSV was used.

## Process DNA sequencing data
Run 001_LoadSequencingData.R to process the sequencing data and save them as tsv files.

The active columns are "seq.data","seq.text","process.id","taxon","sample.id","bin.uri"
However, we're only really interested in the bin.uri and sample.id

## Load collection data, taxonomy, and sequencing data
Run 002_LoadBOLDData.R to change "sample.id" in the sequencing data to "Sample ID" and perform an inner join with the other datasets.

Then script then saves that all as "working_dataset.tsv" which is the combined sequencing data and other data in one file.

## Process BOLD data
003_ProcessBOLDData.R is not done yet, but we'll leave it as is for now. Essentially, it will take the data and generate various graphs and figures.

Currently, it's not functional unless you download the BOLD public data for Nunavut. I haven't uploaded the script for turning BOLD public data into something useful yet. But it will occur later once I'm done working with the BOLD data I've helped contribute to. That's the Data.7z file, which contains sanitized data that is expected to be high quality, and free of many common errors.

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
