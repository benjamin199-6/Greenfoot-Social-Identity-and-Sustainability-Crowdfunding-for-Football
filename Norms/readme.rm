# Empirical Norm Task

This folder contains the code for the Empirical Norm section of the survey, adapted from "Measuring the Tightness and Looseness of Social Norms" by Eugen Dimant.

## Description

The Empirical Norm task asks participants to allocate points to indicate their guesses about the most common answer among a group of participants. This task uses a distribution builder to allow participants to distribute 10 points among various options. The task also includes a follow-up slider to guess the average value based on the distribution.

### Files

- `empirical_norm.html` - Contains the HTML structure for the Empirical Norm task.
- `empirical_norm_styles.css` - Styles specific to the Empirical Norm task.
- `empirical_norm_script.js` - JavaScript to handle the logic and interactions for the Empirical Norm task.

## Instructions for Use

### HTML Structure

### `empirical_norm.html`

This file includes the structure and layout of the Empirical Norm task. Key components include:

- Instructions for participants.
- A distribution builder for allocating points.
- A slider for participants to guess the average value based on their distribution.

### `empirical_norm.html` Content
```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Empirical Norm Study</title>
    <link rel="stylesheet" href="empirical_norm_styles.css">
</head>
<body>

<div class="container">
    <h2>Empirical Norm Study for ${q://QID2/ChoiceGroup/SelectedChoices}</h2>
    <div style="text-align:justify;">
        We asked other participants <u>what they believe</u> that the <strong>other 9 group members invested</strong> in the <b>project</b>. What do you believe was the most common answer?
    </div>
    <div style="text-align:justify;">&nbsp;</div>
    <div style="text-align:justify;">
        <span style="font-size:16px;">
            You can allocate up to 10 points to an option by clicking on the plus and minus buttons below to indicate your guess. 
            <strong>The more likely you think one option is, the more points you would allocate to it.</strong> 
            <strong>Your guesses need to add up to 10</strong>.
        </span>
    </div>
    <br>
    The 9 other group members invested...
    <div id="targetdiv">&nbsp;</div>
    &nbsp;
    <div id="BallsAllocated">You have allocated 0 out of 10 points.</div>
    <div id="bounds" style="margin-top: 20px;">&nbsp;</div>
    <div id="averageGuessContainer" style="margin-top: 20px;">
        <label for="averageGuess">According to your guess, your value is between and . What do you think is the average? </label> 
        <input id="averageGuess" name="averageGuess" step="0.01" style="width: 100%;" type="range">
        <div id="averageError" style="color:red; display:none;">Please enter a value within the calculated range.</div>
    </div>
</div>

<script src="empirical_norm_script.js"></script>

</body>
</html>
