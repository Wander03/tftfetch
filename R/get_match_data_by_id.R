#' Fetch TFT Match Data by Match ID
#'
#' Retrieves a single, full Teamfight Tactics (TFT) match's detailed data
#' using its unique match ID. This function returns a list of normalized
#' data frames by default, or the raw JSON list if requested.
#'
#' @details
#' This function interacts with the `https://{routing_region}.api.riotgames.com/tft/match/v1/matches/{matchId}`
#' endpoint.
#'
#' The `matchId` is a unique identifier for a specific TFT match. This ID is
#' typically obtained by first calling the `get_match_ids_by_puuid()` function.
#'
#' The `routing_region` parameter (e.g., "americas", "asia", "europe") determines
#' which API cluster the request is sent to. For Account-V1 endpoints, the account
#' data is globally accessible. This means you can query any PUUID through any
#' of these regional routing values, and you will receive the same result.
#' It is recommended to use the regional routing value closest to your service's
#' physical location to minimize latency.
#'
#' @param matchId A character string representing the unique identifier of the TFT match.
#' @param routing_region A character string representing the player's region (americas, asia, europe).
#' @param api_key A character string containing your valid Riot API key.
#' @param raw A logical value. If `TRUE`, the function returns the raw, nested JSON list.
#'   If `FALSE` (the default), it returns a list of normalized data frames.
#'
#' @return
#' If `raw = FALSE`: A named list of four data frames (`matches`, `participants`, `traits`, `units`).
#' If `raw = TRUE`: A nested list containing the full data for the specified match.
#' @examples
#' \dontrun{
#' # Replace with your actual API key and a valid match ID
#' api_key <- "YOUR_API_KEY"
#' example_matchId <- "YOUR_MATCH_ID" # Obtained from get_match_ids_by_puuid()
#'
#' # Get the full match data as normalized data frames
#' match_data_clean <- get_match_data_by_id(
#'   matchId = example_matchId,
#'   routing_region = "americas",
#'   api_key = api_key
#' )
#'
#' # View the separate data frames
#' print(match_data_clean$matches)
#' print(match_data_clean$participants)
#'
#' # Get the raw JSON response
#' match_data_raw <- get_match_data_by_id(
#'   matchId = example_matchId,
#'   routing_region = "americas",
#'   api_key = api_key,
#'   raw = TRUE
#' )
#' print(names(match_data_raw))
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom checkmate assert_character assert_choice assert_logical
#' @importFrom httr2 request req_url_path_append req_headers_redacted req_error req_perform resp_status resp_body_json
#' @importFrom purrr map map_chr list_rbind
#' @importFrom rlang `!!!`
#'
get_match_data_by_id <- function(
    matchId,
    routing_region,
    api_key,
    raw = FALSE
) {
  # Input validation
  assert_character(matchId, len = 1, any.missing = FALSE)
  assert_character(routing_region, len = 1, any.missing = FALSE)
  assert_choice(tolower(routing_region), choices = c("americas", "asia", "europe"))
  assert_character(api_key, len = 1, any.missing = FALSE)
  assert_logical(raw, len = 1, any.missing = FALSE)

  # Base URL for TFT Match-V1 endpoint
  base_url <- paste0(
    "https://", tolower(routing_region),
    ".api.riotgames.com/tft/match/v1/matches"
  )

  # Construct the request
  request_obj <- request(base_url) %>%
    req_url_path_append(matchId) %>%
    req_headers_redacted("X-Riot-Token" = api_key)

  # Perform the request and handle errors
  response <- request_obj %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()

  # Check the status code
  status_code <- resp_status(response)

  if (status_code != 200) {
    error_details <- tryCatch({
      resp_body_json(response, simplifyVector = TRUE)
    }, error = function(e) {
      paste("Could not parse error response body. Status:", status_code)
    })

    if (is.list(error_details) && !is.null(error_details$status$message)) {
      error_message <- error_details$status$message
    } else {
      error_message <- "Unknown API error."
    }

    stop(paste0(
      "Riot API request failed (Status ", status_code, "): ", error_message,
    ), call. = FALSE)
  }

  # Get the raw JSON response (as a nested list)
  raw_data <- resp_body_json(response, simplifyVector = FALSE)

  # Return the raw data if requested
  if (raw) {
    return(raw_data)
  }
  # --- Data Normalization (if raw = FALSE) ---

  # 1. Create a data frame for match-level metadata
  match_metadata <- raw_data$metadata
  match_info <- raw_data$info

  matches_df <- data.frame(
    match_id = match_metadata$match_id,
    data_version = match_metadata$data_version,
    game_id = match_info$gameId,
    game_datetime = match_info$game_datetime,
    game_length = match_info$game_length,
    game_version = match_info$game_version,
    tft_set_number = match_info$tft_set_number,
    tft_set_core_name = match_info$tft_set_core_name,
    tft_game_type = match_info$tft_game_type,
    queue_id = match_info$queue_id
  )

  # 2. Create a data frame for participants
  participants_list <- raw_data$info$participants
  participants_df_list <- purrr::map(participants_list, function(p) {
    # Exclude nested lists
    p_flat <- p[!(names(p) %in% c("companion", "traits", "units", "missions"))]
    p_df <- as.data.frame(p_flat)
    p_df$match_id <- match_metadata$match_id
    return(p_df)
  })
  participants_df <- purrr::list_rbind(participants_df_list)

  # 3. Create a data frame for traits
  traits_df_list <- purrr::map(participants_list, function(p) {
    p_puuid <- p$puuid
    if (length(p$traits) > 0) {
      traits_df_p <- purrr::map(p$traits, function(t) as.data.frame(t))
      traits_df_p <- purrr::list_rbind(traits_df_p)

      traits_df_p$match_id <- match_metadata$match_id
      traits_df_p$puuid <- p_puuid
      return(traits_df_p)
    }
    return(NULL) # Return NULL for participants with no traits
  })
  traits_df <- purrr::list_rbind(traits_df_list)

  # 4. Create a data frame for units
  units_df_list <- purrr::map(participants_list, function(p) {
    p_puuid <- p$puuid
    if (length(p$units) > 0) {
      units_df_p <- purrr::map(p$units, function(u) {
        # Flatten the itemNames list into a single string *before* creating the data frame
        u$itemNames <- paste(u$itemNames, collapse = ",")
        as.data.frame(u)
      })
      units_df_p <- purrr::list_rbind(units_df_p)

      units_df_p$match_id <- match_metadata$match_id
      units_df_p$puuid <- p_puuid
      return(units_df_p)
    }
    return(NULL) # Return NULL for participants with no units
  })
  units_df <- purrr::list_rbind(units_df_list)

  # Return a named list of the normalized data frames
  return(list(
    matches = matches_df,
    participants = participants_df,
    traits = traits_df,
    units = units_df
  ))
}
