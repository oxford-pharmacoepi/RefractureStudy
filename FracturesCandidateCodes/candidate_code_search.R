# set up renv
renv::activate()
renv::restore()

# packages
library(DBI)
library(arrow)
library(dbplyr)
library(dplyr)
library(openxlsx)
library(here)
library(CodelistGenerator) 
library(tibble)
library(tictoc)
library(CDMConnector)
library(RPostgres)

#########################
#server
#########################

# database name
databaseName <- "cdm_aurum_202106"

# connect to database
# database connection details
server     <- Sys.getenv("DB_SERVER_cdm_aurum_202106")
server_dbi <- Sys.getenv("DB_SERVER_cdm_aurum_202106_dbi")
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT")
host       <- Sys.getenv("DB_HOST")

db <- DBI::dbConnect(RPostgres::Postgres(), dbname = server_dbi, port = port, host = host, user = user,
                     password = password)

vocabularyDatabaseSchema <- "public"

# specify dbms
dbmsName <- "postgresql"

# searching parameter set up -------

# search strategy for conditions/procedures
standardConceptConditions <-c("Standard", "Classification")
searchViaSynonymsConditions <- TRUE
searchInSynonymsConditions <- TRUE
searchNonStandardConditions <- TRUE
includeDescendantsConditions <- TRUE
includeAncestorConditions <- FALSE

#######################
# CONDITIONS & PROCEDURES-----
#######################
hip_fracture_condition_codes<-getCandidateCodes(keywords= "hip fracture" ,
                                                    domains=c("Condition", "Procedure"),
                                                    standardConcept = standardConceptConditions,
                                                    searchInSynonyms = searchInSynonymsConditions,
                                                    searchViaSynonyms = searchViaSynonymsConditions,
                                                    searchNonStandard = searchNonStandardConditions,
                                                    includeDescendants = includeDescendantsConditions,
                                                    includeAncestor = includeAncestorConditions,
                                                    db = db,
                                                    verbose = TRUE) 

vert_fracture_condition_codes<-getCandidateCodes(keywords= "vertebra fracture" ,
                                                domains=c("Condition", "Procedure"),
                                                standardConcept = standardConceptConditions,
                                                searchInSynonyms = searchInSynonymsConditions,
                                                searchViaSynonyms = searchViaSynonymsConditions,
                                                searchNonStandard = searchNonStandardConditions,
                                                includeDescendants = includeDescendantsConditions,
                                                includeAncestor = includeAncestorConditions,
                                                db = db,
                                                verbose = TRUE) 

femur_fracture_condition_codes<-getCandidateCodes(keywords= "femur fracture" ,
                                                 domains=c("Condition", "Procedure"),
                                                 standardConcept = standardConceptConditions,
                                                 searchInSynonyms = searchInSynonymsConditions,
                                                 searchViaSynonyms = searchViaSynonymsConditions,
                                                 searchNonStandard = searchNonStandardConditions,
                                                 includeDescendants = includeDescendantsConditions,
                                                 includeAncestor = includeAncestorConditions,
                                                 db = db,
                                                 verbose = TRUE) 

humerus_fracture_condition_codes<-getCandidateCodes(keywords= "humerus fracture" ,
                                                  domains=c("Condition", "Procedure"),
                                                  standardConcept = standardConceptConditions,
                                                  searchInSynonyms = searchInSynonymsConditions,
                                                  searchViaSynonyms = searchViaSynonymsConditions,
                                                  searchNonStandard = searchNonStandardConditions,
                                                  includeDescendants = includeDescendantsConditions,
                                                  includeAncestor = includeAncestorConditions,
                                                  db = db,
                                                  verbose = TRUE) 

shoulder_fracture_condition_codes<-getCandidateCodes(keywords= "shoulder fracture" ,
                                                  domains=c("Condition", "Procedure"),
                                                  standardConcept = standardConceptConditions,
                                                  searchInSynonyms = searchInSynonymsConditions,
                                                  searchViaSynonyms = searchViaSynonymsConditions,
                                                  searchNonStandard = searchNonStandardConditions,
                                                  includeDescendants = includeDescendantsConditions,
                                                  includeAncestor = includeAncestorConditions,
                                                  db = db,
                                                  verbose = TRUE) 

upper_arm_fracture_condition_codes<-getCandidateCodes(keywords= "upper arm fracture" ,
                                                     domains=c("Condition", "Procedure"),
                                                     standardConcept = standardConceptConditions,
                                                     searchInSynonyms = searchInSynonymsConditions,
                                                     searchViaSynonyms = searchViaSynonymsConditions,
                                                     searchNonStandard = searchNonStandardConditions,
                                                     includeDescendants = includeDescendantsConditions,
                                                     includeAncestor = includeAncestorConditions,
                                                     db = db,
                                                     verbose = TRUE)

forearm_fracture_condition_codes<-getCandidateCodes(keywords= "forearm fracture" ,
                                                      domains=c("Condition", "Procedure"),
                                                      standardConcept = standardConceptConditions,
                                                      searchInSynonyms = searchInSynonymsConditions,
                                                      searchViaSynonyms = searchViaSynonymsConditions,
                                                      searchNonStandard = searchNonStandardConditions,
                                                      includeDescendants = includeDescendantsConditions,
                                                      includeAncestor = includeAncestorConditions,
                                                      db = db,
                                                      verbose = TRUE)

wrist_fracture_condition_codes<-getCandidateCodes(keywords= "wrist fracture" ,
                                                    domains=c("Condition", "Procedure"),
                                                    standardConcept = standardConceptConditions,
                                                    searchInSynonyms = searchInSynonymsConditions,
                                                    searchViaSynonyms = searchViaSynonymsConditions,
                                                    searchNonStandard = searchNonStandardConditions,
                                                    includeDescendants = includeDescendantsConditions,
                                                    includeAncestor = includeAncestorConditions,
                                                    db = db,
                                                    verbose = TRUE)

pelvic_fracture_condition_codes<-getCandidateCodes(keywords= "pelvic fracture" ,
                                                  domains=c("Condition", "Procedure"),
                                                  standardConcept = standardConceptConditions,
                                                  searchInSynonyms = searchInSynonymsConditions,
                                                  searchViaSynonyms = searchViaSynonymsConditions,
                                                  searchNonStandard = searchNonStandardConditions,
                                                  includeDescendants = includeDescendantsConditions,
                                                  includeAncestor = includeAncestorConditions,
                                                  db = db,
                                                  verbose = TRUE)

rib_fracture_condition_codes<-getCandidateCodes(keywords= "rib fracture" ,
                                                   domains=c("Condition", "Procedure"),
                                                   standardConcept = standardConceptConditions,
                                                   searchInSynonyms = searchInSynonymsConditions,
                                                   searchViaSynonyms = searchViaSynonymsConditions,
                                                   searchNonStandard = searchNonStandardConditions,
                                                   includeDescendants = includeDescendantsConditions,
                                                   includeAncestor = includeAncestorConditions,
                                                   db = db,
                                                   verbose = TRUE)

tibia_fracture_condition_codes<-getCandidateCodes(keywords= "tibia fracture" ,
                                                domains=c("Condition", "Procedure"),
                                                standardConcept = standardConceptConditions,
                                                searchInSynonyms = searchInSynonymsConditions,
                                                searchViaSynonyms = searchViaSynonymsConditions,
                                                searchNonStandard = searchNonStandardConditions,
                                                includeDescendants = includeDescendantsConditions,
                                                includeAncestor = includeAncestorConditions,
                                                db = db,
                                                verbose = TRUE)

foot_fracture_condition_codes<-getCandidateCodes(keywords= "foot fracture" ,
                                                  domains=c("Condition", "Procedure"),
                                                  standardConcept = standardConceptConditions,
                                                  searchInSynonyms = searchInSynonymsConditions,
                                                  searchViaSynonyms = searchViaSynonymsConditions,
                                                  searchNonStandard = searchNonStandardConditions,
                                                  includeDescendants = includeDescendantsConditions,
                                                  includeAncestor = includeAncestorConditions,
                                                  db = db,
                                                  verbose = TRUE)

ankle_fracture_condition_codes<-getCandidateCodes(keywords= "ankle fracture" ,
                                                 domains=c("Condition", "Procedure"),
                                                 standardConcept = standardConceptConditions,
                                                 searchInSynonyms = searchInSynonymsConditions,
                                                 searchViaSynonyms = searchViaSynonymsConditions,
                                                 searchNonStandard = searchNonStandardConditions,
                                                 includeDescendants = includeDescendantsConditions,
                                                 includeAncestor = includeAncestorConditions,
                                                 db = db,
                                                 verbose = TRUE)

elbow_fracture_condition_codes<-getCandidateCodes(keywords= "elbow fracture" ,
                                                  domains=c("Condition", "Procedure"),
                                                  standardConcept = standardConceptConditions,
                                                  searchInSynonyms = searchInSynonymsConditions,
                                                  searchViaSynonyms = searchViaSynonymsConditions,
                                                  searchNonStandard = searchNonStandardConditions,
                                                  includeDescendants = includeDescendantsConditions,
                                                  includeAncestor = includeAncestorConditions,
                                                  db = db,
                                                  verbose = TRUE)

knee_fracture_condition_codes<-getCandidateCodes(keywords= "knee fracture" ,
                                                  domains=c("Condition", "Procedure"),
                                                  standardConcept = standardConceptConditions,
                                                  searchInSynonyms = searchInSynonymsConditions,
                                                  searchViaSynonyms = searchViaSynonymsConditions,
                                                  searchNonStandard = searchNonStandardConditions,
                                                  includeDescendants = includeDescendantsConditions,
                                                  includeAncestor = includeAncestorConditions,
                                                  db = db,
                                                  verbose = TRUE)

fibula_fracture_condition_codes<-getCandidateCodes(keywords= "fibula fracture" ,
                                                 domains=c("Condition", "Procedure"),
                                                 standardConcept = standardConceptConditions,
                                                 searchInSynonyms = searchInSynonymsConditions,
                                                 searchViaSynonyms = searchViaSynonymsConditions,
                                                 searchNonStandard = searchNonStandardConditions,
                                                 includeDescendants = includeDescendantsConditions,
                                                 includeAncestor = includeAncestorConditions,
                                                 db = db,
                                                 verbose = TRUE)

fracture_procedures <- getCandidateCodes(keywords= "fracture" ,
                                         domains=c("Procedure"),
                                         standardConcept = standardConceptConditions,
                                         searchInSynonyms = searchInSynonymsConditions,
                                         searchViaSynonyms = searchViaSynonymsConditions,
                                         searchNonStandard = searchNonStandardConditions,
                                         includeDescendants = includeDescendantsConditions,
                                         includeAncestor = includeAncestorConditions,
                                         db = db,
                                         verbose = TRUE)

# save searches to excel (will write each object into a separate tab in a excel document)
candidateCodelists <- list("Hip_fracture" = hip_fracture_condition_codes, 
                           "vertebra_fracture" = vert_fracture_condition_codes,
                           "femur_fracture" = femur_fracture_condition_codes,
                           "humerus_fracture" = humerus_fracture_condition_codes,
                           "shoulder_fracture" = shoulder_fracture_condition_codes,
                           "upper_arm_fracture" = upper_arm_fracture_condition_codes, 
                           "forearm_fracture" = forearm_fracture_condition_codes,
                           "wrist_fracture" = wrist_fracture_condition_codes,
                           "pelvic_fracture" = pelvic_fracture_condition_codes,
                           "rib_fracture" = rib_fracture_condition_codes,
                           "tibia_fracture" = tibia_fracture_condition_codes,
                           "foot_fracture" = foot_fracture_condition_codes,
                           "ankle_fracture" = ankle_fracture_condition_codes,
                           "elbow_fracture" = elbow_fracture_condition_codes,
                           "knee_fracture" = knee_fracture_condition_codes,
                           "fibula_fracture" = fibula_fracture_condition_codes,
                           "procedure_fracture" = fracture_procedures)
write.xlsx(candidateCodelists, file = here::here("candidate_codes.xlsx"))

