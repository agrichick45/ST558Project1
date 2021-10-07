ST558Project
================
Mandy Liesch
10/2/2021

-   [Required Packages](#required-packages)
-   [Function Creation](#function-creation)
    -   [Type Fuctions](#type-fuctions)
        -   [allPoke(): Returns ALL
            Pokemon](#allpoke-returns-all-pokemon)
        -   [types(): User Defined Type](#types-user-defined-type)
        -   [typeFrame(): Returning Several
            Columns](#typeframe-returning-several-columns)
        -   [listPrep(): Creating the Complete
            Dataframe](#listprep-creating-the-complete-dataframe)
        -   [cleanFrame(): Processing the Type Data into
            Columns](#cleanframe-processing-the-type-data-into-columns)
    -   [Generation Functions](#generation-functions)
        -   [allGen(): Returns All Generations of
            Pokemon](#allgen-returns-all-generations-of-pokemon)
        -   [genOut() Function](#genout-function)
        -   [genFrame(): Returning a Single Row of Generations
            Column](#genframe-returning-a-single-row-of-generations-column)
    -   [Merging Functions](#merging-functions)
-   [Data Analysis](#data-analysis)
    -   [Preparation](#preparation)
        -   [Run the Functions](#run-the-functions)

\#`{r, include=FALSE} #knitr::opts_chunk$set(echo = TRUE) #`

# Required Packages

``` r
#load the required packages
require("httr")
require("jsonlite")
require("tidyverse")
require("RCurl")
require("ggplot2")
library(usethis)
library(knitr)
library(rmarkdown)
use_git_config(user.name="Mandy Liesch", user.email="amliesch@ncsu.edu")
```

``` r
#code to create the rendering
rmarkdown::render("ST558Projecct1.Rmd", 
                  output_format = "github_document", 
                  output_options = list(toc=TRUE, toc_depth = 3, html_preview=FALSE), 
                  output_file = "README.md")
```

# Function Creation

## Type Fuctions

### allPoke(): Returns ALL Pokemon

There are 18 different types of Pokemon. These 18 types apply to both
Pokémon and their moves. It is possible for Pokemon to have two
different types, but they usually have a primary type. This initial
function was created to help the user figure out which type they would
like to specify. There are several functions necessary to create the
output by type.

``` r
#Return all the pokemon names and their informative urls into a dataframe. 
allPoke<-function(){
  #Connect to the Pokemon endpoint, using a limit to make sure all are returned.
  allPokeURL<-"https://pokeapi.co/api/v2/pokemon?limit=1500"
  #Utilize the RCurl to get all of the json data from the endpoint
  pokeData <- RCurl::getURL(allPokeURL)
  #Parse the JSon data into a list with the content variables
  pokeDataDF<-jsonlite::fromJSON(pokeData)
  #pull all of the important results into the dataframe (the pokemon name and URL)
  output <- pokeDataDF$results
  #Use the purr package to pull all of the JSON Data for each pokemon URL into a list.
  pokeData1<-purrr::map(output$url, jsonlite::fromJSON)
  #Return this list when the function is called. 
  return(pokeData1)
}
```

### types(): User Defined Type

This function utilizes the Type endpoint to return the information of a
type that the user specifies. It has many different layers, and allows
the user to input a number from 1-20, that corresponds to a pokemon
type, OR a type character string, like ‘fire’. If ‘all’ is specified, a
list is returned with all pokemon, from the allPoke() function, listed
previously. If an incorrect string or number is put in, a list is
returned of appropriate options in Pokemon type.

``` r
types <- function(type){
  ###
  # This functions returns a data.frame with the numeric key and the name of the types      # associated with pokemon. It can also return the data for a single type if a type ID     # number or name is passed.
  ###
  
  # Get the overall type from the type endpoint, allowing a user specified type.
  baseURL<-"https://pokeapi.co/api/v2/type/"
  # pull the information for all types from the endpoint
  typeData <- RCurl::getURL(baseURL)
  # convert this into a dataframe
  typeDataDF<-jsonlite::fromJSON(typeData)
  # Select and create the type data.frame from the results.
  output <-  typeDataDF$results
  #Create a table to return the type and number in case bad input is specified.
  outputType <- data.frame(type=1:20, output)
  
  #First If Function
  # If type does not equal "all", check if it is a valid pokemon type or numeric value.
  if (type != "all"){
    
    # If type is in the numeric type column, subset output for just that row.
    if (type %in% outputType$type){
      #set the function to query
      pokeType <- type
      #Create the new link to read the user specified API value
      pokeTypeSpec <-GET(paste0(baseURL,pokeType))
      #Run the API through the data, return the JSON
      pokeTypeparsed <- pokeTypeSpec$content %>% rawToChar() %>% fromJSON()
      #Pull the desired data from the pokemon subset of list.
      typeParseRes<-pokeTypeparsed$pokemon
      #Use the purr package to return the json data
      pokePrimary<-purrr::map(typeParseRes$pokemon$url, jsonlite::fromJSON)
      #Return the dataframe.
      return(pokePrimary)
    }
    # If type is in the name column, subset output for just that table, see above for             # documentation.
    if (type %in% outputType$name){
      pokeType <- type
      pokeTypeSpec <-GET(paste0(baseURL,pokeType))
      pokeTypeparsed <- pokeTypeSpec$content %>% rawToChar() %>% fromJSON()
      typeParseRes<-pokeTypeparsed$pokemon
      pokePrimary<-purrr::map(typeParseRes$pokemon$url, jsonlite::fromJSON)
      return(pokePrimary)
    }
    # Otherwise, if type does not equal all, throw an informative error, return the options       # table.
    else {
    message <- paste("ERROR: The numeric Index of Pokemon Type, or Pokemon Type is not                          clear.",
                    "Select from the menu above, or type('all') to find information of all                      of the Pokemon Type you're looking for.")
    print(outputType[1:2])
    stop(message)
    }
  }
  #If the user specified the all function, return the output of the allPoke() function.
  else {
  pokePrimary<-allPoke() 
  return(pokePrimary)
  }
}
```

### typeFrame(): Returning Several Columns

The list function dataset is very complex. This nested function turns
the user specified type list into the dataframe that we desire. The
index value is a numerical input that is used to return the function.
Due to speed concerns, I removed this nesting function capability, for
processing, requiring data frames to be run with the user specified
query. The top lines of code change to:

``` r
typeFrame<-function(type, index){
  frame<-types(type)  
```

However, for speed reason, this is the function used. The index is a
numerical pokemon value. It outputs a single row with 12 columns.

``` r
typeFrame<-function(frame, index){
  ###
  # This functions takes the specified type defined by a dataframe in the types function,   # and creates one row of a dataframe by pokemon index number. 
  ###
 
  #for simplification, the indexing process is changed to a shorter command
  agg<-frame[[index]]
  #Pull the data for the pokemon ID. 
  pokeid<-agg$id
  #Pull the data for the pokemon Name.
  pokeName<-agg$name
  #Pull Pokemon Height Data
  pokeHeight<-agg$height
  #Pull Pokemon Weight Data
  pokeWeight<-agg$weight
  #Pull data from Type 1. 
  pokeType1<-agg$types$type$name[1]
  #Pull data from Type 2.
  pokeType2<-agg$types$type$name[2]
  #The stats column for pokemon stats is vertical, and needs to be transposed
  #so it fits onto one line. It is the first column of the $stats data.
  stats<-t(agg[["stats"]][1])
  #Create a dataframe
  pokeFinal<-as.data.frame(cbind(pokeid, pokeName, pokeHeight, pokeWeight, pokeType1,        pokeType2, stats))
  #return the dataframe.
  return(pokeFinal)
}
```

### listPrep(): Creating the Complete Dataframe

This function is a wrapper function, that takes the single row output of
the typeFrame() function, and repeats it for the entire length of the
data frame generated from the type function.

``` r
listPrep<-function(typePrep){
  ###
  # This list prep function is a wrapper function that takes in the frame specified 
  # by the user specified type functions, and populates the single row from the typeFrame   # and passes it through the lapply function to get a data frame.
  ###
  
  #To determine the number of repititions, we need to determine the length of the types     frame. 
  listLen<-length(typePrep)
  #Create the index to wrap through the lapply function.
  numIndex<-1:listLen
  #Run the typeFrame function on the user specified pokemon type frame the number of times   specified in the index.
  finalTypeFrame<-lapply(X = numIndex, FUN = typeFrame, frame=typePrep)
  #Format the lists into a data frame, and return the output.
  finalFrame<-do.call(rbind.data.frame, finalTypeFrame)
  return(finalFrame)
}
```

### cleanFrame(): Processing the Type Data into Columns

The list prep function creates a data frame, but it is all character
data. To utilize this data frame, the column names need to be changed,
formatted, and the extras are to be deleted. This function is a wrapper
to the listPrep() wrapper function, and generates a fully clean data
frame of the user specified type.

``` r
cleanFrame<-function(typePrep){
  ### The purpose of this function is to take the list run through lapply, and clean and
  #   correct the column formatting so the data can be manipulated with dplyr.
  ###
  lapplyFrame<-listPrep(typePrep)
  #rename the columns to their correct name
  lapplyFrame$hp<-lapplyFrame$`1`
  #change the column type to numeric
  lapplyFrame$hp<-as.numeric(lapplyFrame$hp)
  #remove the old column value.
  lapplyFrame$`1`<-NULL
  #repeat this for all columns that it needs to happen to. 
  lapplyFrame$attack<-lapplyFrame$`2`
  lapplyFrame$attack<-as.numeric(lapplyFrame$attack)
  lapplyFrame$`2`<-NULL
  lapplyFrame$defense<-lapplyFrame$`3`
  lapplyFrame$defense<-as.numeric(lapplyFrame$defense)
  lapplyFrame$`3`<-NULL
  lapplyFrame$specialattack<-lapplyFrame$`4`
  lapplyFrame$specialattack<-as.numeric(lapplyFrame$specialattack)
  lapplyFrame$`4`<-NULL
  lapplyFrame$specialdefense<-lapplyFrame$`5`
  lapplyFrame$specialdefense<-as.numeric(lapplyFrame$specialdefense)
  lapplyFrame$`5`<-NULL
  lapplyFrame$speed<-lapplyFrame$`6`
  lapplyFrame$speed<-as.numeric(lapplyFrame$speed)
  lapplyFrame$`6`<-NULL
  lapplyFrame$pokeHeight<-as.numeric(lapplyFrame$pokeHeight)
  lapplyFrame$pokeWeight<-as.numeric(lapplyFrame$pokeWeight)
  lapplyFrame$pokeid<-as.numeric(lapplyFrame$pokeid)
  return(lapplyFrame)
}  
```

## Generation Functions

### allGen(): Returns All Generations of Pokemon

Like the allPoke() function above, this generation function queries the
Pokemon Species Endpoint API to get all of the information available.
This is not a function that is user queryable, and is part of the
generation function.

``` r
allGen<-function(){
  #Connect to the Pokemon Species endpoint, using a limit to make sure all are returned.
  allGenURL<-"https://pokeapi.co/api/v2/pokemon-species?limit=1000/"
  #Utilize the RCurl to get all of the json data from the endpoint
  genData <- RCurl::getURL(allGenURL)
  #Parse the JSon data into a list with the content variables
  genDataDF<-jsonlite::fromJSON(genData)
  #pull all of the important results into the dataframe (the pokemon name and URL)
  output <- genDataDF$results
  #Use the purr package to pull all of the JSON Data for each pokemon URL into a list.
  genData1<-purrr::map(output$url, jsonlite::fromJSON)
  #Return this list when the function is called.
  return(genData1)
}
```

### genOut() Function

This function returns the user specified generational frame, including
wrapping in the all functions. It outputs a dataframe of lists that will
be run through future functions. Any user specified variables other than
the ‘all’ generations query the Generation endpoint.

``` r
genOut <- function(generation){
  ###
  # This functions returns a data.frame with the numeric key and the name of the types      #associated with pokemon. It can also return the data for a single type if a type ID      #number or name is passed.
  ###
  
  # Get the overall generation from the type endpoint, allowing a user specified            # generation.
  baseURL<-"https://pokeapi.co/api/v2/generation/"
  #pull all of the generation URLs, and put them into a json.
  genData <- RCurl::getURL(baseURL)
  genDataDF<-jsonlite::fromJSON(genData)
  
  # Select and create the type data.frame from the results.
  output <-  genDataDF$results
  #create the data frame needed for specifying errors.
  outputGen <- data.frame(gen=1:8, output)
  
  # If generation does not equal "all", check if it is a valid pokemon type or numeric       #value.
  if (generation != "all"){
    
    # If generation is in the number column, subset output for just that row.
    if (generation %in% outputGen$name){
      #use the user specified value to generate the new API section.
      pokeGen <- generation
      #put the url into the API
      pokeGenSpec <-GET(paste0(baseURL,pokeGen))
      #parse through the data to get the URLs from the API query.
      pokeGenparsed <- pokeGenSpec$content %>% rawToChar() %>% fromJSON()
      #Pull data from the pokemon species subset of the generations.
      genParseRes<-pokeGenparsed$pokemon_species
      #Get the individual pokemon URLs
      genPrimary<-purrr::map(genParseRes$url, jsonlite::fromJSON)
      #Return the list.
      return(genPrimary)
    }
    # If generation is in the generation list column, subset output for just that row.
    #Repeats from above
    if (generation %in% outputGen$gen){
      pokeGen <- generation
      pokeGenSpec <-GET(paste0(baseURL,pokeGen))
      pokeGenparsed <- pokeGenSpec$content %>% rawToChar() %>% fromJSON()
      genParseRes<-pokeGenparsed$pokemon_species
      genPrimary<-purrr::map(genParseRes$url, jsonlite::fromJSON)
      return(genPrimary)
    }
    # Otherwise, throw an informative error to get the appropriate values that occur. .
    else {
    message <- paste("ERROR: The number or index of generation chosen is not a valid                           selection.",
                     "Select from the menu above, of try generation('all') to find the                         Pokemon and Generation you're looking for.")
               print(outputGen[1:2])
               stop(message)
    }
  }
  #If all is specified, then run the allGen() function that we defined earlier.
  else {
  genPrimary<-allGen() 
  return(genPrimary)
 } 
}
```

### genFrame(): Returning a Single Row of Generations Column

Like the type API, because of the volume of the output potential, the
object specified as the genOut() passes through this function as the
‘frame’ argument, and any number put into the index function returns an
individual pokemon’s row. This returns a single row

``` r
genFrame<-function(frame, index){
  gen<-frame[[index]]
  pokeid<-gen$id
  pokeName<-gen$name
  pokeGen<-gen$generation$name
  pokeGrowth<-gen$growth_rate$name
  pokeHappy<-gen$base_happiness
  pokeCapture<-gen$capture_rate
  pokeGenFinal<-as.data.frame(cbind(pokeid, pokeName, pokeGen, pokeGrowth, pokeHappy,       pokeCapture))
  return(pokeGenFinal)
}
```

``` r
genListPrep<-function(genPrep){
  ###
  # This list prep function is a wrapper function that takes in the generation frame        # specified by the user specified type functions, and populates the single row from the   # genFrame and passes it through the lapply function to get a data frame.
  ###
  
  #To determine the number of repetitions, we need to determine the length of the types     frame. 
  listLen<-length(genPrep)
  #Create the index to wrap through the lapply function.
  numIndex<-1:listLen
  #Run the typeFrame function on the user specified pokemon generation frame the number of   # times specified in the index.
  finalGenFrame<-lapply(X = numIndex, FUN = genFrame, frame=genPrep)
  #Format the lists into a data frame, and return the output.
  finalGenFrame<-do.call(rbind.data.frame, finalGenFrame)
  return(finalGenFrame)
}
```

``` r
cleanGen<-function(genPrep){
  ### The purpose of this function is to take the list run through lapply, and clean and
  #   correct the column formatting so the data can be manipulated with dplyr.
  ###
  lapplyGen<-genListPrep(genPrep)
  lapplyGen$pokeid<-as.numeric(lapplyGen$pokeid)
  lapplyGen$pokeHappy<-as.numeric(lapplyGen$pokeHappy)
  lapplyGen$pokeCapture<-as.numeric(lapplyGen$pokeCapture)
  return(lapplyGen)
}
```

## Merging Functions

We have looked at the two main functions that are put together. In order
to do final anlysis (in my case), we want to have the generations data
set merged together with the

``` r
mergedFinal<-function(typePrep, genPrep){
  typeMerge<-cleanFrame(typePrep)
  genMerge<-cleanGen(genPrep)
  finalMerge<-merge(typeMerge, genMerge, by=c('pokeid', 'pokeName'), all.y=TRUE)
  return(finalMerge)
}
```

# Data Analysis

## Preparation

### Run the Functions

``` r
#Running the types function for the user determined function (want 'all'). Specify any of the types that the user wishes is possible (there are 20, for reference).
typeAllFrame<-types('all')
#Run the genOut function to determine the user defined generation data. Specify any geneartion of Pokemon (there are 8, for reference.)
genAllFrame<-genOut('all')
#Run the mergedFinal function to putt together the pokemon id and name, and 14 other desired variables. 
mergedFF<-mergedFinal(typeAllFrame, genAllFrame)
```

``` r
tables<-as_tibble(mergedFF)

tables<-tables[!is.na(tables$pokeType1),]

bargraph<-tables %>%
    mutate(SUM = rowSums(.[7:12])) %>%
    group_by(pokeType1) %>%
    summarise_at(vars(SUM), list(Average = mean))

plot1 <- ggplot(bargraph)
  

gen1Type<- tables %>%
  count(pokeType1) %>%
  mutate(Percentage=n/sum(n)*100)

#knitr::kable(
  #gen1Type,
  #caption=paste("Pokemon Primary Type by Count and Percentage"), digits=2)

gen2Type<- tables %>%
  count(pokeType2) %>%
  mutate(Percentage=n/sum(n)*100)

#knitr::kable(
  #gen2Type,
  #caption=paste("Pokemon Primary Type by Count and Percentage"), digits=2)
  
knitr::kable(list(gen1Type, gen2Type))


hist2 <- tables %>%
  group_by(pokeType2) %>%
  summarise(Freq=n())
```