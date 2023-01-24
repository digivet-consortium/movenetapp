parallelProcessingInput <- function(){
  tagList(
    h3("Parallel processing"),
    fluidRow(
      column(4,
             numericInput("threads",
                          label = "Number of threads",
                          min = 1,
                          value = 4))
  ))}
