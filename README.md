# petrol-station-price-dynamics in Germany

## App Visibility as a Trigger for Price Competition

This project investigates whether petrol stations adjust their prices in response to their ranking visibility on consumer price comparison apps.

## Project Objective
With real-time price comparison apps widely used by consumers, petrol stations operate under high market transparency. This study explores whether stations lower their prices when they notice their ranking drops out of the top X positions in their local area.

Real-time price data from app and website: Clever-Tanken.

A localized region was picked for hypothesis testing: Rosenheim.

## Hypothesis
- Petrol stations are more likely to initiate price cuts when their app-based ranking falls X threshold among nearby competitors.

## Methodology
- Collected public petrol price data and station rankings via app-based visibility metrics.
- Conducted spatial analysis using **ArcGIS** to define localized competitive areas.
- Applied a **Lasso logistic regression model in R** to estimate the likelihood of a petrol station reducing prices after a ranking drop.
- Focused on capturing the predictive power of visibility-triggered price cuts while controlling for multicollinearity.

## Key Findings
- The model showed a weak but present correlation between ranking drops and subsequent price adjustments.
- App visibility appears to influence price competition locally, but with limited explanatory power compared to other market factors.
- Certain branks are more likely to engage, potentially due to brank-wide positioning.

## Deliverables
- R script (data preprocessing, Lasso logistic regression implementation)
- ArcGIS workflow for spatial competitor mapping (only extracted data in repository)
- Final project report summarizing hypothesis, methodology, and insights

