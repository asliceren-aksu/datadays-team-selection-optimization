# Preliminary round case summary

Team C+++ audited the employee case data, calculated a value score for each candidate, and formulated a binary integer optimization problem. The objective was to maximize total case-defined value subject to salary, seniority, junior share, average technical score, and high-risk share constraints.

The report records a 15-person team consisting of seven juniors and eight seniors at a total salary of 49,971 against the 50,000 budget. Its objective score was 309,976.9. The best feasible alternative evaluated in the one-swap search scored 308,305.1, a difference of 1,671.8.

Lowering the budget to 40,000 reduced team size and lowered the objective score by roughly 15% in the reported scenario. Increasing the technical-score weight did not change the selected team in the tested scenario. These results depend on the input data, cleaning choices, objective weights, and constraints. The one-swap search measures sensitivity to a single substitution.
