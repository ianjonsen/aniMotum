context("test ho_bouts and haulout splitting")

test_that("ho_bouts merges records into bouts", {
  h <- data.frame(ref = c("a", "a", "a", "b"),
                  s_date = c("2024-01-01T10:00:00Z", "2024-01-01T14:00:00Z",
                             "2024-01-05T02:00:00Z", "2024-01-02T00:00:00Z"),
                  e_date = c("2024-01-01T12:00:00Z", "2024-01-01T20:00:00Z",
                             "2024-01-05T02:20:00Z", "2024-01-02T03:00:00Z"))
  b <- ho_bouts(h)
  expect_equal(nrow(b), 3)
  expect_equal(b$n_records[b$id == "a"], c(2L, 1L))
  expect_equal(b$dur[b$id == "a"][1], 10)
  expect_equal(nrow(ho_bouts(h, min_dur = 1)), 2)
  ## a shorter gap splits the first bout
  expect_equal(nrow(ho_bouts(h, gap = 1)), 4)
  expect_error(ho_bouts(h, ref = "id"), "not found")
})

test_that("impute_tracks splits tracks at haulout bouts", {
  skip_on_cran()
  f <- fit_ssm(ellie, model = "crw", time.step = 24, control = ssm_control(verbose = 0))
  p <- grab(f, "predicted", as_sf = FALSE)
  ## a bout covering three grid times of the first track
  b <- data.frame(id = p$id[1], start = p$date[10] - 3600, end = p$date[12] + 3600)
  set.seed(1)
  imp <- suppressMessages(impute_tracks(f, reps = 2, haulout = b))
  i0 <- imp[imp$rep == 0 & imp$id == p$id[1], ]
  expect_false(any(i0$date %in% p$date[10:12]))
  expect_equal(length(unique(i0$segment)), 2)
  ## every imputation has the same grid times and segments
  i1 <- imp[imp$rep == 1 & imp$id == p$id[1], ]
  expect_equal(i0$date, i1$date)
  expect_equal(i0$segment, i1$segment)
  ## a bout between grid times drops none, but still splits the track
  b2 <- data.frame(id = p$id[1], start = p$date[20] + 3600, end = p$date[20] + 7200)
  imp2 <- suppressMessages(impute_tracks(f, reps = 1, haulout = b2))
  j0 <- imp2[imp2$rep == 0 & imp2$id == p$id[1], ]
  expect_equal(nrow(j0), sum(p$id == p$id[1]))
  expect_equal(length(unique(j0$segment)), 2)
})
