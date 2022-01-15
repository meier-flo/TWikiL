# TWikiL
Repository containing all additional material for the Twitter Wikipedia Link (TWikiL) dataset. TWikiL contains all Wikipedia links referenced in Tweets that were posted between 2006 and January 2021. This links were enriched with their Wikidata identifiers. 
[The dataset can be downloaded from Zenodo](https://doi.org/10.5281/zenodo.5845374).

**twikil_explore.R** can be used to recreate all descriptive statistics in the paper including the visualisations.

**hydrate_example.R** shows an example on how to work with the SQLite database, by filtering out Tweets that link to Donald Trumps Wikipedia article. These Tweet objects are further collected by using the rtweet package. 
