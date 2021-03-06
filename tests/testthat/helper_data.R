mammals_data <- read.csv(system.file("testdata/mammals.csv", package="bdclean"), fileEncoding="latin1")
mammals_flagged <- read.csv(system.file("testdata/mammals-flag.csv", package="bdclean"), fileEncoding="latin1")

question1 <- BdQuestion(
    question = "Do you worry about taxonomical aspect of the data?",
    possible.responses = c("Yes", "No"),
    question.type = "Router",
    router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
    question.id = "taxonMain",
    ui.type = "single-checkbox"
)

question2 <- BdQuestion(
    question = "What is the lowest taxonomic level you require in your data?",
    possible.responses = c(
        "Subspecies",
        "Species",
        "Genus",
        "Family",
        "Order",
        "Class"
    ),
    question.type = "Child",
    quality.checks = c("taxo_level"),
    question.id = "taxonLevel",
    ui.type = "select"
)

question2$set_response("SPECIES")

responses <- readRDS(system.file("testdata/questionnaire-response.rds", package="bdclean")) # Loaded as 'responses'

input_data <- mammals_data
flagged_data <- mammals_flagged
cleaned_data <- mammals_data
