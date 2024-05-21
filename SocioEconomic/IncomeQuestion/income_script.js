Qualtrics.SurveyEngine.addOnload(function() {
    // Fetch the score from the previous question
    var score = parseInt("${q://QID66/SelectedChoicesRecode}"); // use the pipe function in qualtrcis to reference the previous question (income bracket selected). Dont forget to set scores in the look and feel menu. to  
    var minIncome, maxIncome;

    // Determine the income range based on the score (You have to assign scores to the questions, 1 is below 10k, .... 10 is above 100k 
    switch(score) {
        case 1: 
            minIncome = 0;
            maxIncome = 10000;
            break;
        case 2:
            minIncome = 10000;
            maxIncome = 14999;
            break;
        case 3:
            minIncome = 15000;
            maxIncome = 24999;
            break;
        case 4:
            minIncome = 25000;
            maxIncome = 34999;
            break;
        case 5:
            minIncome = 35000;
            maxIncome = 44999;
            break;
        case 6:
            minIncome = 45000;
            maxIncome = 54999;
            break;
        case 7:
            minIncome = 55000;
            maxIncome = 64999;
            break;
        case 8:
            minIncome = 65000;
            maxIncome = 74999;
            break;
        case 9:
            minIncome = 75000;
            maxIncome = 84999;
            break;
        case 10:
            minIncome = 85000;
            maxIncome = 99999;
            break;
        case 11:
            minIncome = 100000;
            maxIncome = null; // No upper bound for the highest income bracket
            break;
    }

    // Select DOM elements for manipulation
    var sliderContainer = jQuery("#sliderContainer");
    var inputContainer = jQuery("#inputContainer");
    var exactIncomeSlider = jQuery("#exactIncomeSlider");
    var exactIncomeInput = jQuery("#exactIncomeInput");
    var sliderValue = jQuery("#sliderValue");
    var incomeError = jQuery("#incomeError");

    if (maxIncome !== null) {
        // If there is an upper bound, show the slider
        sliderContainer.show();
        exactIncomeSlider.attr("min", minIncome); // Set minimum value for the slider
        exactIncomeSlider.attr("max", maxIncome); // Set maximum value for the slider
        exactIncomeSlider.val((minIncome + maxIncome) / 2); // Set slider to midpoint initially
        sliderValue.text(((minIncome + maxIncome) / 2).toFixed(2) + " Pound");

        // Update displayed value and save data when slider is used
        exactIncomeSlider.on("input", function() {
            var exactIncome = parseFloat(this.value);
            sliderValue.text(exactIncome.toFixed(2) + " Pound");
            Qualtrics.SurveyEngine.setEmbeddedData('ExactIncome', exactIncome.toFixed(2));
        });

        // Initial save of midpoint value
        Qualtrics.SurveyEngine.setEmbeddedData('ExactIncome', ((minIncome + maxIncome) / 2).toFixed(2));
    } else {
        // If there is no upper bound, show the input field
        inputContainer.show();
        exactIncomeInput.attr("min", minIncome); // Set minimum value hint for the input
        exactIncomeInput.attr("placeholder", "Enter a value greater than " + minIncome);

        // Validate input and save data when input field is used
        exactIncomeInput.on("input", function() {
            var exactIncome = parseFloat(this.value);
            if (exactIncome >= minIncome) {
                incomeError.hide(); // Hide error if input is valid
                Qualtrics.SurveyEngine.setEmbeddedData('ExactIncome', exactIncome.toFixed(2));
                $j("#NextButton").attr("disabled", false); // Enable next button
            } else {
                incomeError.show(); // Show error if input is invalid
                $j("#NextButton").attr("disabled", true); // Disable next button
            }
        });

        // Initial validation if the input already has a value
        exactIncomeInput.trigger("input");
    }
});
