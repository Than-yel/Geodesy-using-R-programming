# Function to calculate r based on elliptical equation
calculate_r <- function(a, b, e, psi) {
  # Convert psi from degrees to radians if needed
  psi_rad <- ifelse(psi > 2*pi, psi * pi/180, psi)
  
  # Calculate r using the first formula: r = a(1-e^2)^(1/2) / (1-e^2*cos^2(psi))
  r1 <- (a * (1-e^2)^(1/2)) / (1-e^2 * cos(psi_rad)^2)
  
  # Calculate r using the second formula: r = b / sqrt(1-e^2*cos^2(psi))
  r2 <- b / sqrt(1-e^2 * cos(psi_rad)^2)
  
  # Return both calculations for comparison
  return(list(r_formula1 = r1, r_formula2 = r2))
}

# Get user input
cat("This program calculates r based on the elliptical equation.\n")
a <- as.numeric(readline(prompt = "Enter value for a: "))
b <- as.numeric(readline(prompt = "Enter value for b: "))
e <- as.numeric(readline(prompt = "Enter value for e (eccentricity): "))
psi <- as.numeric(readline(prompt = "Enter value for psi (angle in degrees): "))

# Calculate r
result <- calculate_r(a, b, e, psi)

# Display results
cat("\nResults:\n")
cat("r (using formula 1) =", result$r_formula1, "\n")
cat("r (using formula 2) =", result$r_formula2, "\n")

# Check if formulas give approximately the same result
if(abs(result$r_formula1 - result$r_formula2) < 1e-10) {
  cat("The two formulas yield the same result (within numerical precision).\n")
} else {
  cat("Warning: The two formulas yield different results. Check if a and b are consistent with e.\n")
  cat("Note: For an ellipse, b = a * sqrt(1-e^2)\n")
}