# Empirical Norm Task

This folder contains the code for the Empirical Norm section of the survey, adapted from "Measuring the Tightness and Looseness of Social Norms" by Eugen Dimant. Please refer to his description and cite accordingly: https://osf.io/xkc5w/

## Description

The Empirical Norm task asks participants to allocate points to indicate their guesses about the most common answer among a group of participants. This task uses a distribution builder to allow participants to distribute 10 points among various options. The task also includes a follow-up slider to guess the average value based on the distribution.

### Files

- `empirical_norm.html` - Contains the HTML structure for the Empirical Norm task.
- `empirical_norm_styles.css` - Styles specific to the Empirical Norm task, taken from Dimant (2022).
- `empirical_norm_script.js` - JavaScript to handle the logic and interactions for the Empirical Norm task, adapted from Dimant (2022).

## Instructions for Use

### Step 1: Modify Look & Feel in Qualtrics

1. **Go to Look & Feel:**
   - Open your survey in Qualtrics.
   - Click on `Look & Feel` in the survey menu.

2. **Include External Libraries in Header:**
   - Go to the `General` tab.
   - In the `Header` section, copy and paste the following code:
     ```html
     <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/QuentinAndre/DistributionBuilder@master/lib/distributionbuilder.css">
     <script src="https://cdn.jsdelivr.net/gh/QuentinAndre/DistributionBuilder@master/lib/distributionbuilder.min.js"></script>
     ```

3. **Add Custom CSS:**
   - Go to the `Style` tab.
   - In the `Custom CSS` section, copy and paste the following CSS:
     ```css
     #slider-container {
         text-align: center;
     }

     .Skin .QuestionOuter {
         display: flex;
         flex-direction: column;
         align-items: center;
     }

     .Skin .q-slider {
         width: 80% !important;
         margin: 0 auto;
     }

     #CustomButton, #CustomButton2, #CustomButton3, #CustomButton4 {
         border: none;
         color: #fff;
         font-size: 16px;
         padding: 8px 20px;
         border-radius: 4px;
         cursor: pointer;
         margin: 0;
         text-align: center;
         text-decoration: none;
         -webkit-appearance: none;
         transition: background .3s;
         background-color: #008dc0;
     }

     div.modal {
         display: none;
         position: fixed;
         z-index: 10;
         padding-top: 100px;
         left: 0;
         top: 0;
         width: 100%;
         height: 100%;
         overflow: auto;
         background-color: rgba(0,0,0,0.4);
     }

     div.modal-content {
         background-color: #fefefe;
         margin: auto;
         border: 1px solid #888;
         border-radius: 5px;
         max-width: 750px;
         padding: 20px;
     }

     div.modal-text2 {
         width: 90%;
         padding: 20px;
         text-align: justify;
         margin: auto;
         font-size: 17px;
         border: 1px solid #000000;
         border-radius: 5px;
     }

     table.centerbox {
         margin: auto;
     }

     .center {
         display: block;
         margin-left: auto;
         margin-right: auto;
         width: 50%;
     }

     .center2 {
         display: block;
         margin-left: auto;
         margin-right: auto;
         width: 100%;
     }
     ```

### Step 2: Setup HTML Structure

#### `empirical_norm.html`

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
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/QuentinAndre/DistributionBuilder@master/lib/distributionbuilder.css">
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

<script src="https://cdn.jsdelivr.net/gh/QuentinAndre/DistributionBuilder@master/lib/distributionbuilder.min.js"></script>
<script src="empirical_norm_script.js"></script>

</body>
</html>
