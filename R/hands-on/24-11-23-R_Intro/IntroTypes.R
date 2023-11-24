bmc.data <- data.frame(
               fname = c("Alice", "Bob", "Carol", "David"),
               gender = as.factor(c("Female", "Male", "Female", "Male")),
               disorder = c("autism", "anxiety", "autism", "depression"),
               age = c(20, 45, 15, 12),
               biomarker1 = c(5.70, 4.96, 1.37, 10.44),
               clinicalstage = c("1b", "1a", "1a", "2"),
               random = seq(1:4),
               stringsAsFactors = FALSE
            )

bmc.data[bmc.data$gender == "Male" | bmc.data$age > 15, ]
