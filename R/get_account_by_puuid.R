#' Fetch Riot Account Data by PUUID
#'
#' Retrieves a player's Riot Account information (PUUID, gameName, and tagLine)
#' using the Riot Account-V1 endpoint, specifically by providing their PUUID.
#'
#' @param puuid A character string representing the player's PUUID (Player Universally Unique ID).
#' @param routing_region A character string representing the player's region (americas, asia, europe)
#' @param api_key A character string containing your valid Riot API key.
#'
#' @details
#' This function interacts with the `https://{routing_region}.api.riotgames.com/riot/account/v1/accounts/by-puuid/{puuid}`
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
#' @return A list containing the player's PUUID, gameName, and tagLine.
#' @examples
#' \dontrun{
#' # Replace with your actual API key and a valid PUUID
#' api_key <- "YOUR_API_KEY"
#' example_puuid <- "YOUR_EXAMPLE_PUUID_HERE"
#' account_info_by_puuid <- get_account_by_riot_id(puuid = example_puuid, routing_region = "americas", api_key = api_key)
#' print(account_info_by_puuid$gameName)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom checkmate assert_character assert_choice
#' @importFrom httr2 request req_url_path_append req_headers_redacted req_error req_perform resp_status resp_body_json
#'
get_account_by_puuid <- function(puuid, routing_region, api_key) {

  # Input validation
  assert_character(puuid, len = 1, any.missing = FALSE)
  assert_character(routing_region, len = 1, any.missing = FALSE)
  assert_choice(tolower(routing_region), choices = c("americas", "asia", "europe"))
  assert_character(api_key, len = 1, any.missing = FALSE)

  # Base URL for the account-v1 endpoint by PUUID
  base_url <- paste0("https://", tolower(routing_region), ".api.riotgames.com/riot/account/v1/accounts/by-puuid")

  # Construct the request
  response <- request(base_url) %>%
    req_url_path_append(puuid) %>%
    req_headers_redacted("X-Riot-Token" = api_key) %>%
    # Use req_error() to prevent httr2 from stopping on 4xx/5xx
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()

  # Check the status code
  status_code <- resp_status(response)

  if (status_code == 200) {
    # Success: Return the JSON body as a simplified R object (list/data frame)
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

    # Stop the function and provide a descriptive error message
    stop(paste0("Riot API request failed (Status ", status_code, "): ", error_message),
         call. = FALSE)
  }
}
