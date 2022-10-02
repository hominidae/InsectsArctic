# InsectsArctic

First, you will need to download the data. I've used 7Zip to compress the data I and others at Polar Knowledge Canada, the University of Guelph, and ARCBIO collected in the communities of Cambridge Bay, Kugluktuk, Gjoa Haven, and Kugaaruk.

It is attached to this repository as a 7Zip archive titled "Data.7z". However, since all of these data have relied on public funds the data is generally considered to be open source.

As a result, you can also download this data from BOLD via the public data portal by selecting "Nunavut" as a search term and filtering from there.

https://boldsystems.org/index.php/Public_BINSearch?searchtype=records

However, I suggest that you download the archive here instead because the public data available from BOLD is not sanitized. The introduction of errors as a result of data entry issues, poor field notes, errors in GPS coordinates, etc does result in problems during processing the data.

Note: I chose to download the metadata and sequencing data separately and using R to match BIN numbers to RecordID's.

Since the sequencing data has been download as FASTA format files, the use of the R library "phylotools" to read the FASTA format, selecting the data required and exporting as TSV was used.
