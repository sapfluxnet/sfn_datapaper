rmarkdown::draft(file = "sfn_datapaper_ms.Rmd",
                 
                 template = "copernicus_article",
                 
                 package = "rticles", edit = FALSE) 
rmarkdown::render(input = "sfn_datapaper_ms/sfn_datapaper_ms.Rmd") 
