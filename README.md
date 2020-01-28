SAPFLUXNET data paper results
================

## Requirements

To run the code in this repository, one needs the following structure
within the project folder:

  - data: contains the sapfluxnet data version(s), e.g. 0.1.3.
  - maps: contains the necessary base maps
      - maps/Crowther\_Nature\_Files\_Revision\_01\_WGS84\_GeoTiff:
        contains global tree density map from Crowther in Nature

## Structure

* Text is being written in google docs.

https://docs.google.com/document/d/1uzsOiBBLLPNdMS_f4p3h8uQOMtGJa6z2fiZgeE_kHNk/edit

* Scripts to run once:
 -  `read_metadata.R`: reads sfn metadata, re-run only if sfn data changes
 (e.g. new version), writes cache.
 - `maps_base.R`: creates base maps for figures. Stores objects in `maps_base.RData`.
 - `datasets_length.R`: calculations for datasets periods and duration. Stores objects in `dataset_duration_data.RData`.
 
* Scripts to generate results (figures and tables)
 - `figures_draft.Rmd`: it calls other R scripts in
 
* Scripts to generate the supplement (figures and tables)
 - `supporting.Rmd`:
The supplement has some wide tables that need to be rotated to landscape 
in libreoffice (insert->manual break->page style->landscape). Some tables can be shortened
applying 'minimal row height' in menu Table.

## Output

- `figures_draft.docx`
- `supporting.docx`

## Copernicus template

- I downloaded a markdown template from copernicus (not implemented yet), see https://www.earth-system-science-data.net/for_authors/manuscript_preparation.html
