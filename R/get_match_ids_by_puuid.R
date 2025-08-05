#' Fetch TFT Match IDs by PUUID
#'
#' Retrieves a list of Teamfight Tactics (TFT) match IDs for a given player's
#' PUUID, optionally filtered by time and starting index.
#'
#' @param puuid A character string representing the player's PUUID (Player Universally Unique ID).
#' @param routing_region A character string representing the player's region (americas, asia, europe).
#' @param api_key A character string containing your valid Riot API key.
#' @param start An integer for the starting index of results for pagination (max 999). Default is 0. Optional.
#' @param startTime An integer (epoch milliseconds) for the start of the time range. Optional.
#' @param endTime An integer (epoch milliseconds) for the end of the time range. Optional.
#' @param count An integer for the number of match IDs to return (max 200). Default is 20. Optional.
#'
#' @details
#' This function interacts with the `https://{routing_region}.api.riotgames.com/tft/match/v1/matches/by-puuid/{puuid}/ids`
#' endpoint.
#'
#' The `puuid` (Player Universally Unique ID) is a unique identifier for a player
#' across all Riot Games titles and regions. It can be obtained from other APIs
#' like `get_account_by_riot_id()` or from match data.
#'
#' The `routing_region` parameter (e.g., "americas", "asia", "europe") determines
#' which API cluster the request is sent to. For Account-V1 endpoints, the account
#' data is globally accessible. This means you can query any PUUID through any
#' of these regional routing values, and you will receive the same result.
#' It is recommended to use the regional routing value closest to your service's
#' physical location to minimize latency.
#'
#' **Optional Parameters:**
#' * `start`: The starting index for pagination (default: 0, max: 999). Riot only stores 1,000 most recent match IDs.
#' * `count`: The number of match IDs to return (default: 20, max: 200). Riot only allows 200 match IDs per API call.
#' * `startTime`: Epoch milliseconds for the start of the time range (inclusive).
#' * `endTime`: Epoch milliseconds for the end of the time range (inclusive).
#'
#' @return A character vector of TFT match IDs.
#' @examples
#' \dontrun{
#' # Replace with your actual API key and a valid PUUID
#' api_key <- "YOUR_API_KEY"
#' example_puuid <- "YOUR_EXAMPLE_PUUID_HERE"
#'
#' # Get the 20 most recent match IDs for a PUUID in the Americas region
#' recent_match_ids <- get_match_ids_by_puuid(
#'   puuid = example_puuid,
#'   regional_routing_value = "americas",
#'   api_key = api_key
#' )
#' print(head(recent_match_ids))
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom checkmate assert_character assert_choice assert_integerish
#' @importFrom httr2 request req_url_path_append req_headers_redacted req_url_query req_error req_perform resp_status resp_body_json
#'
get_match_ids_by_puuid <- function(
    puuid,
    routing_region,
    api_key,
    start = 0,
    count = 20,
    startTime = NULL,
    endTime = NULL
) {
  # Input validation
  assert_character(puuid, len = 1, any.missing = FALSE)
  assert_character(routing_region, len = 1, any.missing = FALSE)
  assert_choice(tolower(routing_region), choices = c("americas", "asia", "europe"))
  assert_character(api_key, len = 1, any.missing = FALSE)

  # Optional parameter validation
  assert_integerish(startTime, len = 1, null.ok = TRUE)
  assert_integerish(endTime, len = 1, null.ok = TRUE)
  assert_integerish(start, len = 1, any.missing = FALSE, lower = 0L, upper = 999L)
  assert_integerish(count, len = 1, any.missing = FALSE, lower = 1L, upper = 200L)

  # Base URL for TFT Match-V1 endpoint
  base_url <- paste0(
    "https://", tolower(routing_region),
    ".api.riotgames.com/tft/match/v1/matches/by-puuid"
  )

  # Construct the request
  request_obj <- request(base_url) %>%
    req_url_path_append(puuid, "ids") %>%
    req_headers_redacted("X-Riot-Token" = api_key)

  # Add optional query parameters if they are not NULL
  query_params <- list(
    start = start,
    startTime = startTime,
    endTime = endTime,
    count = count
  )
  # Filter out NULL parameters before adding to URL query
  query_params <- query_params[!sapply(query_params, is.null)]

  if (length(query_params) > 0) {
    request_obj <- request_obj %>% req_url_query(!!!query_params)
  }

  # Perform the request and handle errors
  response <- request_obj %>%
    req_error(is_error = function(resp) FALSE) %>% # Suppress httr2 errors for manual handling
    req_perform()

  # Check the status code
  status_code <- resp_status(response)

  if (status_code == 200) {
    # Success: Return the JSON body
    resp_body_json(response, simplifyVector = TRUE)
  } else {
    # Error handling for non-200 status codes

    # Try to extract error message from JSON body
    error_details <- tryCatch({
      resp_body_json(response, simplifyVector = TRUE)
    }, error = function(e) {
      # If the body isn't JSON (e.g., HTML error page), return a standard message
      paste("Could not parse error response body. Status:", status_code)
    })

    # Extract the Riot API specific status message if available
    if (is.list(error_details) && !is.null(error_details$status$message)) {
      error_message <- error_details$status$message
    } else {
      error_message <- "Unknown API error."
    }

    stop(paste0(
      "Riot API request failed (Status ", status_code, "): ", error_message
      ), call. = FALSE)
  }
}
