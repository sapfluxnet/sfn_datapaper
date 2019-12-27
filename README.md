A repository for results of the Sfn data paper
================

## Requirements

To run the code in this repository, one needscthe following structure
within the project folder:

  - data: contains the sapfluxnet data version(s), e.g. 0.1.3.
  - maps: contains the necessary base maps
      - maps/Crowther\_Nature\_Files\_Revision\_01\_WGS84\_GeoTiff:
        contains global tree density map from Crowther in Nature

## Structure

### Scripts
-  metadata_wrangling.R: reads from cache, performs summaries and aggregations
-  maps_sites.R: maps 
-  genus_species.R: species and genera 

### Non-functional and other scripts
 - read_metadata.R: reads sfn metadata, re-run only if sfn data changes
 (e.g. new version), writes cache
 - sfndatapaper_main.R: not functional, an attempt to create main script
 - maps_base.R:  creates base maps for figures

## Rendering

### Current version

Documents
- tables

## Other versions

Tried a markdown template from copernicus, see https://www.earth-system-science-data.net/for_authors/manuscript_preparation.html
