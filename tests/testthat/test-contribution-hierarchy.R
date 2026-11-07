test_that("a complete non-overlapping direct-child partition is accepted", {
        hierarchy <- data.frame(
                coicop18_code = c("P", "A", "B", "A1"),
                parent_code = c(NA, "P", "P", "A")
        )
        result <- validate_direct_child_partition(hierarchy, "P", c("B", "A"))

        expect_true(result$valid)
        expect_equal(result$components, c("A", "B"))
})

test_that("incomplete and invalid hierarchies are rejected", {
        hierarchy <- data.frame(
                coicop18_code = c("P", "A", "B"),
                parent_code = c(NA, "P", "P")
        )
        incomplete <- validate_direct_child_partition(hierarchy, "P", "A")
        leaf <- validate_direct_child_partition(hierarchy, "A")
        duplicate <- rbind(hierarchy, hierarchy[hierarchy$coicop18_code == "A", ])
        duplicated_result <- validate_direct_child_partition(duplicate, "P")

        expect_false(incomplete$valid)
        expect_match(incomplete$message, "missing B")
        expect_false(leaf$valid)
        expect_match(leaf$message, "no direct children")
        expect_false(duplicated_result$valid)
        expect_match(duplicated_result$message, "duplicate")
})

test_that("overlapping direct children are rejected", {
        hierarchy <- data.frame(
                coicop18_code = c("P", "A", "A1"),
                parent_code = c(NA, "P", "P")
        )
        result <- validate_direct_child_partition(hierarchy, "P")

        expect_false(result$valid)
        expect_match(result$message, "overlap")
})
