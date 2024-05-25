Qualtrics.SurveyEngine.addOnload(function() {});

Qualtrics.SurveyEngine.addOnReady(function() {
    $j("#NextButton").attr("disabled", true);

    document.getElementById("targetDiv1").innerHTML = "";
    if (localStorage.getItem('results1') != null) {
        var results1 = localStorage.getItem('results1');
    } else {
        var results1 = [];
    }

    var balls1 = 9;
    $j('#ballsAllocated1').text("You have allocated " + 0 + " out of 9 points.");

    var distBuilder1 = new DistributionBuilder({
        nRows: 9,   // how many points 
        nBuckets: 5, // how many buckets do we show 
        minVal: 0, // what is the min value of points 
        maxVal: 9, // what is max value 
        nBalls: balls1,
        onChange: function() {
            var ballsAllocated1 = balls1 - this.getRemainingBalls();
            $j('#ballsAllocated1').text("You have allocated " + ballsAllocated1 + " out of 9 points.");

            if (this.isComplete()) {
                results1 = distBuilder1.getDistribution().toString();
                console.log(results1);
                Qualtrics.SurveyEngine.setEmbeddedData('Belief1', results1);
                localStorage.setItem('results1', results1);

                // Calculate lower and upper bounds in pence
                var distribution1 = distBuilder1.getDistribution();
                var lowerBound1 = 0;
                var upperBound1 = 0;

                var lowerRanges1 = [0, 1, 100, 200, 300]; // These are used to calculate the mean guess.... depending on the buckets need to be changed
                var upperRanges1 = [0, 99, 199, 299, 300]; 

                for (var i = 0; i < distribution1.length; i++) {
                    lowerBound1 += distribution1[i] * lowerRanges1[i];
                    upperBound1 += distribution1[i] * upperRanges1[i];
                }

                // Display the calculated bounds in pounds
                lowerBound1 = lowerBound1;
                upperBound1 = upperBound1;
                $j('#lowerBound1').text(lowerBound1);
                $j('#upperBound1').text(upperBound1);

                // Show the average guess container
                $j('#averageGuessContainer1').show();

                // Update slider attributes to reflect the calculated range
                $j('#averageGuess1').attr('min', lowerBound1);
                $j('#averageGuess1').attr('max', upperBound1);
                $j('#averageGuess1').val((lowerBound1 + upperBound1) / 2); // Initialize to midpoint (Slider is place in the middle of the uppper and lower bound) 
                $j('#averageGuessValue1').text(((lowerBound1 + upperBound1) / 2) + " p"); // Show initial value

                // Update slider value display and save to embedded data (THIS is used to allow participant to also indicate the average of the total contribution using pre calculated values (upper lower bound) based on their allocation of points! 
                $j('#averageGuess1').on('input', function() {
                    var averageGuess1 = parseFloat($j(this).val());
                    $j('#averageGuessValue1').text(averageGuess1 + " p");
                    Qualtrics.SurveyEngine.setEmbeddedData('AverageGuess1', averageGuess1); // Save average guess

                    if (averageGuess1 >= lowerBound1 && averageGuess1 <= upperBound1) {
                        $j("#averageError1").hide();
                        $j("#NextButton").attr("disabled", false);
                    } else {
                        $j("#averageError1").show();
                        $j("#NextButton").attr("disabled", true);
                    }
                });

                // Initial save of midpoint value
                Qualtrics.SurveyEngine.setEmbeddedData('AverageGuess1', ((lowerBound1 + upperBound1) / 2));

                $j("#NextButton").attr("disabled", false);
            } else {
                $j("#NextButton").attr("disabled", true);
            }
        }
    });

    distBuilder1.render("targetDiv1", "labels-grid-buttons");
    distBuilder1.labelize({
        labels: ["0", "1-99", "100-199", "200-299", "300"], // Initialize to midpoint here you have to define the Value above the buckets 
        suffix: ' pence',
    });
    if (localStorage.getItem('results1') != null) {
        var dist1 = results1.split`,`.map(x => +x);
        distBuilder1.setDistribution(dist1);
    }
});

Qualtrics.SurveyEngine.addOnUnload(function() {
    /*Place your JavaScript here to run when the page is unloaded*/
});
