# Income Question

This folder contains the code for the income question part of the Socio-Economic section of the survey.

## Description

The income question consists of two parts:
1. A multiple-choice question with different income brackets.
2. A follow-up slider question to provide a more accurate income range.

### Files

- `income_question.html` - Contains the HTML structure for the income question.
- `income_styles.css` - Styles specific to the income question.
- `income_script.js` - JavaScript to handle the logic and interactions for the income question.

## Implementation

### Multiple-Choice Question

The multiple-choice question allows respondents to select an income bracket:
- Below $10,000
- $10,000 - $14,999
- ...
- More than $100,000

### Slider Question

Based on the selection from the multiple-choice question, a slider is presented:
- If a respondent selects $10,000 - $14,999, the slider range is set accordingly.
- The slider allows for more precise income input within the selected range.

### Scoring

Each income bracket is assigned a score from 1 to 10:
- 1 = Below 10,000
- 2 = $10,000 - 14,999
- ...
- 10 = More than 100,000

## Usage
<img width="1551" alt="Income" src="https://github.com/benjamin199-6/Social-Identity-and-Sustainability-Crowdfunding/assets/72379630/2fc3a7c9-41e2-44a6-8da9-3a77a47e6800">
