# df <- data.frame(
#   COUNTRY = c("Afghanistan", "Afghanistan", "Afghanistan"),
#   # AllColRow = c("AnyColRow", "AnyColRow", "AnyColRow"),
#   # AltiClass = c("Alti1_0", "Alti1_0", "Alti2_300"),
#   # SlpClass = c("Slp1_0", "Slp1_0", "Slp1_0"),
#   # SoilClass = c("Soil1_Sandy", "Soil2_Medium", "Soil1_Sandy"),
#   CrpLnd = c(56.73, 54.85, 11.36),
#   CrpLnd_H = c(0.29, 3.78, 0.06),
#   CrpLnd_L = c(33.70, 36.84, 4.68),
#   CrpLnd_I = c(22.73, 14.23, 6.62),
#   CrpLnd_S = c(NA, NA, NA),
#   OthAgri = c(90.49, 104.25, 61.81),
#   Grass = c(69.88, 27.39, 73.39),
#   Forest = c(1.82, 3.73, 0),
#   WetLnd = c(588.39, 52.79, 446.40),
#   OthNatLnd = c(807.42, 240.20, 623.20)
#  )
# ID.col.number = 1
# gms.type = "TABLE"
# gms.name = "LANDCOVERALL_INIT"
# var.name = "LC_TYPES_EPIC"
# gms.text = "Initial land cover (1000 ha)"
# write.gms(df,gms.type, gms.name, var.name, gms.text, ID.col.number=1, col_style = "float", gms_file = "test.gms")

write.gms <- function(df,gms.type="", gms.name="", var.name="", gms.text="" ,col_style="float", col_width=15, ID.col.number=1,decimals=2, gms_file="./output.gms"){


  get_longest_string <- function(df) {
    longest <- max(nchar(as.character(unlist(df))), na.rm = TRUE)
    return(longest)
  }


  if(col_style=="float"){
    col_width <- max(nchar(colnames(df[(ID.col.number+1):ncol(df)])),get_longest_string(df[(ID.col.number+1):ncol(df)]))+1
  }

# Open file connection
file_conn <- file(gms_file, "w")

if(ID.col.number!=1){
  maxlength <- max(nchar(apply(df[,1:ID.col.number],1,paste, collapse = "")))+ID.col.number-1
} else {
  maxlength <- max(nchar(df[,1]))
}

# Function to format each value with fixed width
format_value <- function(x, width = col_width) {
  sprintf(paste0("%", width, paste0(".",decimals,"f")), as.numeric(x))  # Right-align numbers with 2 decimal places
}


# Function to calculate and return spaces
pad_spaces <- function(input_string, total_length) {
  current_length <- nchar(input_string)  # Get current string length
  if (current_length >= total_length) {
    return("")  # No spaces needed if already at or over the limit
  }
  spaces_needed <- total_length - current_length
  return(paste(rep(" ", spaces_needed), collapse = ""))
}

# Write the header
header <- paste0(gms.type," ",gms.name,"(",paste(colnames(df)[1:ID.col.number],collapse = ","),",",var.name,") ", gms.text,"\n", collapse = "")
writeLines(header, file_conn)

# Write column headers
land_types <- colnames(df)[(ID.col.number+1):ncol(df)]
column_header <- paste("",pad_spaces("", total_length = maxlength), collapse = "")
for(jjj in land_types){
  column_header <- paste0(column_header, paste(pad_spaces(jjj, total_length = col_width), jjj, collapse = ""), collapse = "")
}


writeLines(column_header, file_conn)

# Loop through dataframe and write data with fixed-width spacing
for (i in 1:nrow(df)) {
  rows <- df[i, ]

  # Format row identifier: Country.AllColRow.Altitude.Slope.Soil
  row_id <- paste(rows[1:ID.col.number], collapse = ".")
  row_id_formatted <-paste(row_id,pad_spaces(row_id, total_length = maxlength), collapse = "")  # Ensure fixed width for row ID
  # row_id_formatted <- paste(row_id_formatted,pad_spaces(row_id_formatted, total_length = maxlength), collapse = "")

  # Format numeric values with fixed width
  land_values <- paste(sapply(rows[(ID.col.number+1):ncol(df)], format_value), collapse = " ")
  land_values <- gsub("NA","  ",land_values)


  # Write line to file
  writeLines(paste(row_id_formatted, land_values), file_conn)
}
 writeLines(paste(";"), file_conn)
# Close file connection
close(file_conn)
}

