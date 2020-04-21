SAPFLUXNET data paper
================
## Journal requirements and formats

A data paper is something we're not very used to. Here are some guidelines for manuscript preparation.

The target journal is [Earth System Science Data](https://www.earth-syst-sci-data.net/).See general instructions to 
prepare the mannuscript [here](https://www.earth-system-science-data.net/for_authors/submit_your_manuscript.html). See also
some recommendations [here](https://www.earth-syst-sci-data.net/10/2275/2018/).

Here are the instructions for [manuscript structure](https://www.earth-system-science-data.net/for_authors/manuscript_preparation.html).
However, I find that this structure is only orientative and it differs among papers. I looked for some examples of papers that could be used as inspiration.

[An open-source database for the synthesis of soil radiocarbon data: International Soil Radiocarbon Database (ISRaD) version 1.0](https://www.earth-syst-sci-data.net/12/61/2020/)

[A spatially explicit database of wind disturbances in European forests over the period 2000–2018](https://www.earth-syst-sci-data.net/12/257/2020/)

[The global long-term microwave Vegetation Optical Depth Climate Archive (VODCA)](https://www.earth-syst-sci-data.net/12/177/2020/)

[Global Carbon Budget 2019](https://www.earth-syst-sci-data.net/11/1783/2019/)

[The PROFOUND database for evaluating vegetation models and simulating climate impacts on forests](https://www.earth-syst-sci-data-discuss.net/essd-2019-220/)

This one is from Nature Scientific data, but it's relevant, thematically:
[The FLUXCOM ensemble of global land-atmosphere energy fluxes](https://www.nature.com/articles/s41597-019-0076-8)

## Reproducing figures, tables and supplement.

To run the code in this repository, one needs the following structure
within the project folder:

  - data: contains the sapfluxnet data version(s), e.g. 0.1.4.
  - maps: contains the necessary base maps
      - maps/Crowther\_Nature\_Files\_Revision\_01\_WGS84\_GeoTiff:
        contains global tree density map from Crowther in Nature

* Scripts to run once:
 -  `read_metadata.R`: reads sfn metadata, re-run only if sfn data changes
 (e.g. new version), writes cache.
 - `maps_base.R`: creates base maps for figures. Stores objects in `maps_base.RData`.
 - `datasets_length.R`: calculations for datasets periods and duration. Stores objects in `dataset_duration_data.RData`.
 
* Scripts to generate results (figures and tables)
 - `figures_draft.Rmd`: it calls other R scripts. Output: `figures_draft.docx`
 
* Scripts to generate the supplement (figures and tables)
 - `supporting.Rmd`: output: - `supporting.docx`


## Text

* I'm using Google docs with Zotero connector. See especially the 'Collaboration' section here:
https://www.zotero.org/support/google_docs

* Consider this for final editing:
https://www.zotero.org/blog/move-zotero-citations-between-google-docs-word-and-libreoffice/

* Link to edit the document

https://docs.google.com/document/d/1uzsOiBBLLPNdMS_f4p3h8uQOMtGJa6z2fiZgeE_kHNk/edit



## Copernicus template

- I downloaded a markdown template from copernicus (not implemented yet), see https://www.earth-system-science-data.net/for_authors/manuscript_preparation.html
