
library(readxl)
library(dplyr)

# --------------------------------
# External credentials file
# --------------------------------

credentials_file <- file.path(
  getwd(),
  "secure_data",
  "malaria_dashboard_users.xlsx"
)

# --------------------------------
# Check file availability
# --------------------------------

if (!file.exists(credentials_file)) {
  stop(
    "Credentials file not found: ",
    credentials_file
  )
}

# --------------------------------
# Read credentials
# --------------------------------

credentials <- read_excel(
  credentials_file,
  sheet = "users"
) |>
  as.data.frame()

# --------------------------------
# Standardize column types
# --------------------------------

credentials <- credentials |>
  mutate(
    user = as.character(user),
    password = as.character(password),
    fullname = as.character(fullname),
    role = as.character(role),
    county = as.character(county),
    facility = as.character(facility)
  )

# --------------------------------
# Validate credentials
# --------------------------------

required_columns <- c(
  "user",
  "password",
  "fullname",
  "role",
  "county",
  "facility"
)

if (!all(required_columns %in% names(credentials))) {
  stop("Credentials file has missing columns.")
}

if (anyDuplicated(credentials$user)) {
  stop("Duplicate usernames found.")
}

if (any(is.na(credentials$user)) ||
    any(is.na(credentials$password))) {
  stop("Missing usernames or passwords found.")
}

message("Credentials loaded successfully.")
