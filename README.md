SAPFLUXNET data paper
================

### Data and project folder structure

To run the code in this repository, one needs the following structure
within the project folder:

  - data: contains the sapfluxnet data version(s), e.g. 0.1.5.Download from [here](https://zenodo.org/record/3971689)
  - maps: contains the necessary base maps
      - maps/Crowther\_Nature\_Files\_Revision\_01\_WGS84\_GeoTiff:
        contains global tree density map from Crowther in Nature. Download from:
        [here](https://elischolar.library.yale.edu/cgi/viewcontent.cgi?filename=1&article=1000&context=yale_fes_data&type=additional)

### Scripts to run once:
 -  `read_metadata.R`: reads sfn metadata, re-run only if sfn data changes
 (e.g. new version), writes cache.
 - `maps_base.R`: creates base maps for figures. Stores objects in `maps_base.RData`.
 - `datasets_length.R`: calculations for datasets periods and duration. Stores objects in `dataset_duration_data.RData`.

### Reproducing results
 
* Scripts to generate results (figures and tables)
 - `figures_draft.Rmd`: it calls other R scripts. Output: `figures_draft.docx`
 
* Scripts to generate the supplement (figures and tables)
 - `supporting.Rmd`: output: - `supporting.docx`

* Script to generate Appendix A:
 - `appendix.Rmd`: output: - `appendix.docx`
 
* Scripts to generate uncertainty estimation (Appendix B)
- `sfn_uncertainty.R`
