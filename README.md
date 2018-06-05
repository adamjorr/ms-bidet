# ms-bidet
Mass Spec Biomarker Discovery with Exact Tests

## Summary
Usage: `Rscript ms-bidet.R [directory]`

The `directory` argument is optional; if it is given, it will look for input data in all subdirectories of that directory. The default is to use the current working directory.

## Data Structure
Create a directory for each category of sample.
There should be one directory called negative for negative samples and one directory called positive for positive samples.
Each sample should be its own csv file within the appropriate directory for its category.
One of the columns in the csv is expected to have a header labeled 'm/z'.

## Control Data
You can include data that appears in the output but is not included in the significance test by creating a group called 'control' (case sensitive). Any samples in this folder will be included in the output but will be ignored when doing significance testing.

## Output
An output file will be created in the current working directory called `full_out.csv`. `full_out.csv` includes the detected m/z, a p-value, a q-value (p-value corrected for multiple comparisons) and a column for each sample with a boolean showing whether the m/z was detected in that sample. If there is a category called `negative`, an output called `short_out.csv` will also be created that only includes rows from `full_out.csv` where all negative samples do not include the m/z.
