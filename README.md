# Team Selection Optimization — Data Days Datathon

**Preliminary round case · Team C+++ · March 2026**

We built an R workflow to select a team that maximizes the case's value score under budget, skills, seniority, and retention-risk constraints. The work combines data cleaning, binary integer optimization, and scenario analysis.

[Annotated R script](team_optimization.R) · [Case report summary](CASE_SUMMARY.md)

## Approach

1. Audit missing values, duplicates, inconsistent categories, and out-of-range scores.
2. Calculate the case-defined value score and use `lpSolve` to choose team members with binary decision variables.
3. Compare a lower budget, a higher technical-score weight, and feasible alternatives formed by swapping one team member.

The base case required a salary total of at most 50,000, at least one senior, at least 30% juniors, an average technical score of at least 80, and at most 40% high-risk candidates.

## Reported result

| Metric | Base case | Best feasible one-swap alternative |
|---|---:|---:|
| Team size | 15 | 15 |
| Total salary | 49,971 | 49,833 |
| Objective value | 309,976.9 | 308,305.1 |
| Average technical score | 80.40 | 80.33 |

## Run locally

Install the R packages `data.table` and `lpSolve`. Place your authorized copy of the competition input beside the script as `datadays.csv` and run `Rscript team_optimization.R` from the repository directory. The script writes a local `datadays_clean.csv`. The competition input is required to reproduce the reported numbers.

## Team C+++

Begüm Ece Kaya · İlbey Bebek · Aslı Ceren Aksu
