httptest2::with_mock_dir("fixtures/summoner_by_puuid", {

  # --- TEST 1: Valid Inputs ---
  test_that("get_summoner_by_puuid returns expected data for valid Riot ID", {

    result <- get_summoner_by_puuid(puuid = "qXLBEI63Edhj_pF8IEzDU4F-R7qKE_CqMiuZ4MTSEEXzezC-NjPk6O7UezMSeomll5icuPvimp_5RA",
                                   region = "na1",
                                   api_key = "MOCKED_API_KEY")

    expect_type(result, "list")
    expect_true("puuid" %in% names(result))
    expect_true("profileIconId" %in% names(result))
    expect_true("revisionDate" %in% names(result))
    expect_true("summonerLevel" %in% names(result))
    expect_equal(result$profileIconId, 4270)
  })

  # --- TEST 2: Invalid Inputs / API Errors ---
  test_that("get_summoner_by_puuid handles invalid inputs gracefully", {

    # Test 1: Input validation
    expect_error(get_summoner_by_puuid(puuid = 123, region = "na1", api_key = "MOCKED_API_KEY"))
    expect_error(get_summoner_by_puuid(puuid = "123",
                                      region = "atlantic ocean",
                                      api_key = "MOCKED_API_KEY")
    )

    # Test 2: API 404/429 error handling
    expect_error(
      get_summoner_by_puuid(puuid = "NonExistentPuuid", region = "na1", api_key = "MOCKED_API_KEY"),
      regexp = "Riot API request failed"
    )
  })

})
