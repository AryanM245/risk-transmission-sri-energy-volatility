<img width="1387" height="241" alt="image" src="https://github.com/user-attachments/assets/7aa4abf0-cfa9-4516-aaf1-3de610c3febe" /># risk-transmission-sri-energy-volatility
DCC, ADCC, and BEKK-GARCH analysis of risk transmission across ESG, alternative energy, and commodity volatility indices.
# Uncovering Risk Transmission between Socially Responsible Investments, 
Alternative Energy Investments, and the Implied Volatility of Major Commodities

This project examines volatility spillovers and dynamic correlations between
socially responsible investment (SRI) indices, alternative energy investments,
and implied volatility indices of major commodities.

## Methodology
- DCC-GARCH
- ADCC-GARCH
- BEKK-GARCH

## Data
Daily time-series data including:
- Implied volatility indices (OVX, GVZ, VXSLV)
- Alternative energy indices
- ESG / SRI equity indices (FTSE4Good series)

## Data Availability
The dataset used in this study was obtained from the Bloomberg Terminal
through institutional (university) access.

Due to Bloomberg’s data licensing restrictions, the raw data cannot be
shared publicly. Researchers with Bloomberg access can replicate the
analysis by extracting the same series and placing the data file in the
`/data` directory.

⚠️ Data should be placed in the `/data` directory.

## References

1. Olofsson, P., Råholm, A., Uddin, G. S., Troster, V., & Kang, S. H. (2020).
   *Ethical and unethical investments under extreme market conditions*.
   International Review of Financial Analysis.

2. Shahid, M. N., Azmi, W., Ali, M., Islam, M. U., & Rizvi, S. A. R. (2021).
   *Uncovering risk transmission between socially responsible investments,
   alternative energy investments and the implied volatility of major commodities*.
   Energy Economics.

3. Engle, R. F. (2002).
   *Dynamic Conditional Correlation: A Simple Class of Multivariate GARCH Models*.
   Journal of Business & Economic Statistics.


## Software
R programming language with:
```r
rugarch
rmgarch
quantmod
PerformanceAnalytics
ggplot2
lubridate
