# https://bioconductor.org/packages/release/bioc/vignettes/BiocFHIR/inst/doc/B_handling.html
pacman::p_load(
  "BiocManager",
  "here",
  "readr",
  "jsonlite"
  )

BiocManager::install("BiocFHIR")
library("BiocFHIR")


# LOAD A SINGLE BUNDLE ------------------------------------
tfile = here("fhir", "Abe604_Greenfelder433_0e3a711b-a318-537e-b339-046cc71c68e5.json")
patient = jsonlite::fromJSON(tfile)
names(patient$entry$resource)

View(patient$entry)
  
# PROCESS A LIST OF FILES --------------------------------
# https://bioconductor.org/packages/release/bioc/vignettes/BiocFHIR/inst/doc/C_tables.html

# read a list of json files from a directory
files <- list.files(path = "fhir",
           recursive = TRUE,
           pattern = "\\.json$",
           full.names = TRUE)

# need to remove the active property from patient records as it is not in synthea files apparently
schemas = FHIR_retention_schemas()
# need to remove the active property from patient records as it is not in synthea files apparently
schemas$Patient <- schemas$Patient[-11]
# need to remove the multipleBirthControl property from patient records as it is not in synthea files apparently
schemas$Patient <- schemas$Patient[-9]

bundles <- sapply(files, process_fhir_bundle, schemas=schemas)
bundles[[1]]$Condition


# ANALZYE RESOURCES ACROSS BUNDLES ------------------------------

# Get available processing functions:
# ls("package:BiocFHIR") |> grep(x=_, "process_[A-Z]", value=TRUE)

# Check if the respective bundles have the Observation Resource
# TODO: Change x$Observation (on both lines!) to another bundle type as needed
# TODO: Change the processing function from process_Condition to the Resource you need

hascond = sapply(bundles, function(x)length(x$Condition)>0)
conds = do.call(rbind, lapply(bundles[hascond], function(x)process_Condition(x$Condition)))
View(conds$code.coding.code)
