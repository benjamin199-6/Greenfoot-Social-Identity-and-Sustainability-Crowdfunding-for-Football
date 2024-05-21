function updateSummary() {
    const investmentSlider = document.getElementById('investmentSlider');
    const investment = parseInt(investmentSlider.value);
    const successfulReturn = investment * 2; // 200% return if the project reaches the threshold
    const unsuccessfulReturn = investment; // 100% return if the project does not reach the threshold
    const keptAmount = 60 - investment; // Amount not invested
    const amountKept = keptAmount * 1.5; // 150% of the kept amount

    // Update the displayed values
    document.getElementById('investmentAmount').innerText = investment;
    document.getElementById('keptAmount').innerText = keptAmount;
    document.getElementById('successfulReturn').innerText = successfulReturn;
    document.getElementById('unsuccessfulReturn').innerText = unsuccessfulReturn;
    document.getElementById('amountKept').innerText = amountKept.toFixed(2);

    // Debug information to verify the values
    console.log('Investment:', investment);
    console.log('Kept Amount:', keptAmount);
    console.log('Successful Return:', successfulReturn);
    console.log('Unsuccessful Return:', unsuccessfulReturn);
    console.log('Amount Kept:', amountKept);

    // Save the investment value to Qualtrics Embedded Data
    try {
        Qualtrics.SurveyEngine.setEmbeddedData('Actual_Investment_1', investment);
        console.log('Embedded Data Actual_Investment_1 set to:', investment);
    } catch (error) {
        console.error('Error setting embedded data:', error);
    }
}

document.addEventListener('DOMContentLoaded', (event) => {
    const investmentSlider = document.getElementById('investmentSlider');
    investmentSlider.addEventListener('input', updateSummary);

    // Initialize the summary with the default slider value
    updateSummary();
});
