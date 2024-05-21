# Investment Game

This folder contains the code for the Investment Game section of the survey.

## Description

The Investment Game simulates decision-making scenarios where participants allocate funds into different investment options. The game uses a slider to determine how much of the 60 ECU the participant wants to invest. The returns are calculated based on whether the investment threshold is reached or not.

### Files

- `investment_game.html` - Contains the HTML structure for the investment game.
- `styles.css` - Styles specific to the investment game (Note: Currently, main styles are embedded in the HTML file for simplicity). This file has to be included in the LOOK and FEEL section under style external CSS ... 
- `investment_script.js` - JavaScript to handle the logic and interactions for the investment game.

## HTML Structure

### investment_game.html

This file includes the structure and layout of the investment game. Key components include:

- A slider for participants to choose their investment amount.
- Real-time updates of investment, kept amount, and potential returns.
- Descriptions and details of the investment scenario.

## JavaScript Logic

### investment_script.js

This script updates the investment summary based on the slider value and logs the details for debugging. It also sets embedded data in Qualtrics to store the participant's investment.

### Key Functions:

- **updateSummary**: Updates the displayed investment amounts and calculates returns based on the slider input.
- **Event Listener**: Listens for changes in the slider input to update the summary in real-time.

## Embedded Data Fields

To properly store and use the investment data in Qualtrics, you need to set up the following embedded data fields in your survey flow:

- **Actual_Investment_1**: Stores the amount of ECU the participant decides to invest.

### How to Set Up Embedded Data Fields in Qualtrics

1. **Navigate to Survey Flow:**
   - Open your survey in Qualtrics.
   - Click on the `Survey Flow` option in the survey menu.

2. **Add Embedded Data:**
   - Click on `+ Add a New Element Here` and select `Embedded Data`.
   - Enter `Actual_Investment_1` as the field name.
   - Leave the value blank as it will be set dynamically through the JavaScript in your survey.

3. **Save Flow:**
   - Click `Save Flow` to ensure your changes are applied.

## Usage

1. Ensure the embedded data fields are set up in your Qualtrics survey as described above.
2. Open `investment_game.html` in a web browser to view the investment game.
3. Ensure that `investment_script.js` is correctly linked.


## License

Please cite
