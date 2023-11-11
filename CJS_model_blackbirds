#Install packages if have not been used before
#Load packages
library(RMark)
library(dplyr)
library(tidyr)
library(magrittr)

#Sorting the Data Matrices (Ensure Original Code File with Datamatrix,Datamatrixa
#etc has been ran as a prerequisite)
capture_data <- data.frame()
# Extract the capture counts for the first row of Datamatrix
capture_counts <- Datamatrix[1, ]
# Initialize a unique ID counter
unique_id_counter <- 1

# Loop through each row of the Datamatrix (ensure Datamatrix has been run from the given data code titled unfiltereddata.r)
for (x in 1:20) {
  # Extract the capture counts for the current row
  capture_counts <- Datamatrix[x, ]
  
  # Initialize a binary vector for each bird
  bird_binary <- rep(0, 20)
  
  # Loop through each occasion and generate IDs for captured individuals
  for (occasion in 1:20) {
    num_captures <- capture_counts[occasion]
    if (num_captures > 0) {
      # Generate unique IDs for captured individuals
      captured_ids <- unique_id_counter:(unique_id_counter + num_captures - 1)
      unique_id_counter <- unique_id_counter + num_captures
      
      # Update the binary vector for the bird at position x
      bird_binary[x] <- 1  # Add a "1" when an individual is captured
      bird_binary[occasion] <- 1  # Add a "1" for the corresponding occasion
      
      # Determine the occasion based on the first numerical value in the binary string
      first_occasion <- which(bird_binary == 1)[1]
      
      # Create a data frame for captured individuals
      captured_data <- data.frame(
        ID = captured_ids,
        Occasion = first_occasion,  # Set the occasion based on the first "1" in binary string
        ch = paste(bird_binary, collapse = ""),  # Set the binary string
        Marks = NA  # Replace NA with actual mark data if available
      )
      # Append the captured individuals' data to the capture_data data frame
      capture_data <- rbind(capture_data, captured_data)
      
      # Reset the binary vector for the next iteration
      bird_binary[x] <- 0
      bird_binary[occasion] <- 0
    }
  }
  
  # Update the binary vector for the bird at position x when it is recaptured
  bird_binary[x] <- 1
}

capture_data <- subset(capture_data, !is.na(ID))



#---------------------------------------------------------


# Create an empty data frame to store the transformed data
capture_dataj <- data.frame()
# Extract the capture counts for the first row of Datamatrix
capture_countsj <- Datamatrixj[1, ]
# Initialize a unique ID counter
unique_id_counter <- 1

# Loop through each row of the Datamatrix (ensure Datamatrix has been run from the given data code titled unfiltereddata.r)
for (x in 1:20) {
  # Extract the capture counts for the current row
  capture_countsj <- Datamatrixj[x, ]
  
  # Initialize a binary vector for each bird
  bird_binaryj <- rep(0, 20)
  
  # Loop through each occasion and generate IDs for captured individuals
  for (occasion in 1:20) {
    num_capturesj <- capture_countsj[occasion]
    if (num_capturesj > 0) {
      # Generate unique IDs for captured individuals
      captured_idsj <- unique_id_counter:(unique_id_counter + num_capturesj - 1)
      unique_id_counter <- unique_id_counter + num_capturesj
      
      # Update the binary vector for the bird at position x
      bird_binaryj[x] <- 1  # Add a "1" when an individual is captured
      bird_binaryj[occasion] <- 1  # Add a "1" for the corresponding occasion
      
      # Determine the occasion based on the first numerical value in the binary string
      first_occasion <- which(bird_binaryj == 1)[1]
      
      # Create a data frame for captured individuals
      captured_dataj <- data.frame(
        ID = captured_idsj,
        Occasion = first_occasion,  # Set the occasion based on the first "1" in binary string
        ch = paste(bird_binaryj, collapse = ""),  # Set the binary string
        Marks = NA  # Replace NA with actual mark data if available
      )
      # Append the captured individuals' data to the capture_data data frame
      capture_dataj <- rbind(capture_dataj, captured_dataj)
      
      # Reset the binary vector for the next iteration
      bird_binaryj[occasion] <- 0
    }
  }
  
  # Update the binary vector for the bird at position x when it is recaptured
  bird_binaryj[x] <- 1
}

capture_dataj <- subset(capture_dataj, !is.na(ID))



#---------------------------------------------------------

#Sorting the Data into Binary Strings for Adult Blackbirds

# Create an empty data frame to store the transformed data
capture_dataa <- data.frame()
# Extract the capture counts for the first row of Datamatrix
capture_countsa <- Datamatrixa[1, ]
# Initialize a unique ID counter
unique_id_counter <- 1

# Loop through each row of the Datamatrix (ensure Datamatrix has been run from the given data code titled unfiltereddata.r)
for (x in 1:20) {
  # Extract the capture counts for the current row
  capture_countsa <- Datamatrixa[x, ]
  
  # Initialize a binary vector for each bird
  bird_binarya <- rep(0, 20)
  
  # Loop through each occasion and generate IDs for captured individuals
  for (occasion in 1:20) {
    num_capturesa <- capture_countsa[occasion]
    if (num_capturesa > 0) {
      # Generate unique IDs for captured individuals
      captured_idsa <- unique_id_counter:(unique_id_counter + num_capturesa - 1)
      unique_id_counter <- unique_id_counter + num_capturesa
      
      # Update the binary vector for the bird at position x
      bird_binarya[x] <- 1  # Add a "1" when an individual is captured
      bird_binarya[occasion] <- 1  # Add a "1" for the corresponding occasion
      
      # Determine the occasion based on the first numerical value in the binary string
      first_occasion <- which(bird_binarya == 1)[1]
      
      # Create a data frame for captured individuals
      captured_dataa <- data.frame(
        ID = captured_idsa,
        Occasion = first_occasion,  # Set the occasion based on the first "1" in binary string
        ch = paste(bird_binarya, collapse = ""),  # Set the binary string
        Marks = NA  # Replace NA with actual mark data if available
      )
      # Append the captured individuals' data to the capture_data data frame
      capture_dataa <- rbind(capture_dataa, captured_dataa)
      
      # Reset the binary vector for the next iteration
      bird_binarya[occasion] <- 0
    }
  }
  
  # Update the binary vector for the bird at position x when it is recaptured
  bird_binarya[x] <- 1
}

capture_dataa <- subset(capture_dataa, !is.na(ID))



#---------------------------------------------------------



#Modelling the data using Cormack Jolly-Seber (CJS) with the RMark library
#Pulli
cjs_model_P <- mark(capture_data, model = "CJS")
#Juvienille
cjs_model_J <- mark(capture_dataj, model = "CJS")
#Adult
cjs_model_A <- mark(capture_dataa, model = "CJS")




#---------------------------------------------------------



#Do not touch data manipulation
chocc <- capture_data  [-c(2,4)]
binary_strings <- chocc[, -1]
process_cjs_form <- function(binary_strings) {
  result <- lapply(binary_strings, function(string) {
    if (sum(strsplit(string, "")[[1]] == "1") == 1) {
      return(1)
    } else {
      return(0)
    }
  })
  return(result)
}

firstyear <- process_cjs_form(binary_strings)
firstyear <- as.numeric(unlist(firstyear))
print(firstyear)

capture_data_firstyear <- cbind(first, firstyear)


calculate_years_lived <- function(binary_strings) {
  result <- sapply(binary_strings, function(string) {
    # Split the binary string into a vector of characters
    string_vector <- strsplit(string, "")[[1]]
    
    # Find the indices of "1" in the string
    one_indices <- which(string_vector == "1")
    
    if (length(one_indices) < 2) {
      return(1)  # Return 0 for birds that don't have both "1"s
    } else {
      # Calculate the gap between the first and second "1" and convert it to years
      years_lived <- (one_indices[2] - one_indices[1])
      return(years_lived)  # Return the calculated age
    }
  })
  return(result)
}


#---------------------------------------------------------
#Creating rain data for time period
#Ensure weather_df has been ran
# Create a new data frame 'annual_avg_rain'
# Remove rows 253 to 325
weather_df <- weather_df[-c(240:325), ]
rain <- weather_df[-c(1,2,3,4,5,7)]
annual_avg_rain <- data.frame()
rain <- as_tibble(rain)

# Calculate the annual average precipitation
annual_avg_rain <- rain %>%
  mutate(Year = rep(1:ceiling(n()/12), each = 12)[1:n()]) %>%
  group_by(Year) %>%
  summarize(Annual_Avg_Rain = mean(Rain, na.rm = TRUE))

# Print the resulting tibble
print(annual_avg_rain)
colnames(annual_avg_rain) <- c("Year","rain")

chocc <- capture_data[-c(2,4)]
pullibb <- chocc
pullibb$age <- calculate_years_lived(capture_data$ch)
# Merge data frames based on age
pullibb <- merge(pullibb, annual_avg_rain, by.x = "age", by.y = "Year", all.x = TRUE)


#---------------------------------------------------------


chocc <- capture_data[-c(2,4)]
pullibb <- chocc
pullibb$age <- calculate_years_lived(capture_data$ch)


# Define age group thresholds
age_group_thresholds <- c(-1, 2, 4,14)  # Define your age group boundaries

# Create a new column for age groups
Mixture <- pullibb$age_group <- cut(pullibb$age, breaks = age_group_thresholds, labels = c("Pulli", "Adult", "Old"))


# Assuming your dataframe is called "df"
colnames(pullibb)[colnames(pullibb) == "age_group"] <- "age_group"
columnadd <- capture_data[,2]
pullibb <- pullibb[-c(3)]
pullibbage<−choccage <- choccyears_lived
pullibb <- cbind(pullibb, capture_data$Occasion)
colnames(pullibb)[colnames(pullibb) == "capture_data$Occasion"] <- "occasion"


#---------------------------------------------------------Modelling w covariates



bbp.pr <- process.data(pullibb, model = "CJS",
                       age.var = 1,
                       initial.ages = c(0, 1, 2),
                       groups = ("age_group"))

# Create default design data  
# For Phi, values are 0 or older; for p the values are 1 or older
# This provides an individual covariate for each parameter type
# that changes as the animal ages


bbp.ddl = make.design.data(bbp.pr,
                           parameters = list(Phi = list(age.bins = c(0, 2, 4, 20)),
                                             p = list(age.bins = c( 2, 4,20))),
                           right = FALSE)


run.bbp = function(){
  #  Define range of models for Phi
  #
  Phi.dot = list(formula =  ~ 1)
 # Phi.occ = list(formula =  ~ occasion)
  Phi.age = list(formula = ~ age)
  #Phi.prec = list(formula = ~rain )
 # Phi.age.occ = list(formula = ~ age + occasion)
  #Phi.age.prec = list(formula = ~ age + rain)
  #Phi.occ.prec = list(formula = ~ occasion + rain )
 # Phi.occ.age.prec = list(formula = ~ occasion + age + rain )
  #  Define range of models for p
  p.dot = list(formula =  ~ 1)
  p.time = list(formula =  ~ -1 + time)
  #p.prec = list(formula = ~ rain) #Not sure if precip should go on p or Phi
  
  
  # Create models for all combinations of phi & p
  bbp.model.list = create.model.list("CJS")
  
  bbp.results = mark.wrapper(bbp.model.list,
                             data = bbp.pr, ddl = bbp.ddl) #,output = FALSE) #Remove first hash to remove all matrix output
  
  # Return model table and list of models
  
  return(bbp.results)
}

bbp.res <- run.bbp()



#---------------------------------------------------------

