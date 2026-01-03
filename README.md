# MP-shocks
Identifying monetary policy shocks using Beige Book sentiments

Project Overview
----------------
This empirical project analyzes the effects of monetary policy shocks on key macroeconomic variables using both:
- High-frequency financial market surprises (following Jarociński & Karadi, 2020), and
- Text-based sentiment indicators extracted from the Beige Book (following Aruoba & Drechsel, 2024).

A Principal Component Analysis (PCA) is applied to 14 standardized sentiment indicators to capture the Fed's internal information. 
This component is then used to "clean" traditional monetary policy shocks, isolating their exogenous component.

Project Structure
-----------------
The project is organized as follows:
- Part 1: Data loading and descriptive analysis
- Part 2: Identification using high-frequency monetary policy shocks (Jarociński & Karadi)
- Part 3: Identification using text-based sentiment shocks (Aruoba & Drechsel)
- Part 4: PCA on Beige Book sentiments and regression to isolate exogenous shocks
- Part 5: VAR estimation and impulse response analysis using "cleaned" shocks
- Robustness: Comparison of different shocks a

Required Files
--------------
- jk2020-us_variables.csv
- jk2020_us_shocks.csv
- Beigebook sentiments.csv
- Auroba shocks.csv

Required R Packages
-------------------
zoo, vars, stargazer, factoextra, corrplot, lubridate

Output
------
The analysis produces:
- Time series plots of macro variables and sentiment indicators
- Impulse Response Functions (IRFs) for:
  - Traditional MP shocks
  - Sentiment-based MP shocks
  - Cleaned/orthogonalized MP shocks
- PCA scree plot and sentiment principal component
- Comparison of shock residuals and estimated regression

Suggested Improvements
----------------------
- Extend the PCA to more sentiment categories (dimension reduction)
- Estimate nonlinear VARs or local projection models
- Implement Bayesian VARs for more robust inference
- Incorporate post-2008 data to test for structural shifts

Key References
----------
- Jarociński, M., & Karadi, P. (2020). Deconstructing monetary policy surprises: The role of information shocks. AEJ: Macroeconomics, 12(2), 1–43.
- Aruoba, S. B., & Drechsel, T. (2024). Monetary policy and the Fed’s internal communication.
- Loughran, T., & McDonald, B. (2011). When is a liability not a liability? Journal of Finance, 66(1), 35–65.
- Gilchrist, S., & Zakrajšek, E. (2012). Credit spreads and business cycle fluctuations. AER, 102(4), 1692–1720.
