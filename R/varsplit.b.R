
# This file is a generated template, your changes will not be overwritten

varsplitClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "varsplitClass",
    inherit = varsplitBase,
    private = list(
        .run = function() {
            # Get the variable to split
            var_to_split <- self$data[[self$options$dep]]
            
            if (is.null(var_to_split) || length(var_to_split) == 0) {
                self$results$text$setContent("No data available in the selected column.")
                return()
            }
            
            # Get the separator based on the option
            separator <- if (self$options$separator == "semicolon") ";" else ","
            
            # Convert variable to character if it's not already
            var_to_split <- as.character(var_to_split)
            
            # Remove all whitespace from values
            var_to_split <- gsub("\\s+", "", var_to_split)
            
            # Split all values and get unique elements
            all_elements <- unique(unlist(strsplit(var_to_split, separator)))
            all_elements <- all_elements[!is.na(all_elements) & all_elements != ""]
            
            # Sort elements alphabetically for better readability
            all_elements <- sort(all_elements)
            
            # Create the result message
            result_message <- paste0(
                "Analysis of column '", self$options$dep, "':\n\n",
                "Separator used: ", separator, "\n",
                "Number of unique values found: ", length(all_elements), "\n\n",
                "List of unique values:\n",
                paste("- ", all_elements, collapse = "\n")
            )

            # Create keys, titles, and descriptions for each unique value
            keys <- character(length(all_elements))
            titles <- character(length(all_elements))
            descriptions <- character(length(all_elements))
            measureTypes <- rep("nominal", length(all_elements))

            for (i in seq_along(all_elements)) {
                keys[i] <- as.character(i)
                titles[i] <- paste0(self$options$dep, "_", all_elements[i])
                descriptions[i] <- paste0("Binary indicator for value '", all_elements[i], "' in column '", self$options$dep, "'")
            }

            # Set the factor scores output
            self$results$factorScoresOV$set(
                keys=keys,
                titles=titles,
                descriptions=descriptions,
                measureTypes=measureTypes
            )

            # Create binary vectors for each unique value
            for (i in seq_along(all_elements)) {
                # Create a binary vector indicating presence/absence of the current value
                binary_vector <- sapply(var_to_split, function(x) {
                    split_values <- unlist(strsplit(x, separator))
                    ifelse(all_elements[i] %in% split_values, "yes", "no")
                })
                
                # Set the values for this component
                self$results$factorScoresOV$setValues(index=i, binary_vector)
            }

            # Set the results text
            self$results$text$setContent(result_message)
        }
    )
)
