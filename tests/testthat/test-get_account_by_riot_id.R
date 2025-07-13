httptest2::with_mock_dir("fixtures/account_data", {

  # --- TEST 1: Valid Inputs ---
  test_that("get_account_by_riot_id returns expected data for valid Riot ID", {

    result <- get_account_by_riot_id(game_name = "Wander", tag_line = "HENRO", region = "americas", api_key = "MOCKED_API_KEY")

    expect_type(result, "list")
    expect_true("puuid" %in% names(result))
    expect_true("gameName" %in% names(result))
    expect_equal(result$gameName, "Wander")
  })

  # --- TEST 2: Invalid Inputs / API Errors ---
  test_that("get_account_by_riot_id handles invalid inputs gracefully", {

    # Test 1: Input validation
    expect_error(get_account_by_riot_id(game_name = 123, tag_line = "HENRO", region = "americas", api_key = "MOCKED_API_KEY"))
    expect_error(get_account_by_riot_id(game_name = "123", tag_line = "123", region = "atlantic ocean", api_key = "MOCKED_API_KEY"))

    # Test 2: API 404/429 error handling
    expect_error(
      get_account_by_riot_id(game_name = "NonExistentName", tag_line = "XYZ", region = "americas", api_key = "MOCKED_API_KEY"),
      regexp = "Riot API request failed"
    )
  })

})
