Qualtrics.SurveyEngine.addOnload(function() { });

Qualtrics.SurveyEngine.addOnReady(function() {
    $j("#NextButton").attr("disabled", true);

    if (localStorage.getItem('results') != null) {
        var results = localStorage.getItem('results');
    } else {
        var results = [];
    }

    var n_balls = 10;
    $j('#BallsAllocated').text("You have allocated " + 0 + " out of 10 points.");

    var distbuilder = new DistributionBuilder({
        nRows: 10,
        nBuckets: 8,
        minVal: 0,
        maxVal: 10,
        nBalls: n_balls,
        onChange: function() {
            var ballsallocated = n_balls - this.getRemainingBalls();
            $j('#BallsAllocated').text("You have allocated " + ballsallocated + " out of 10 points.");

            if (this.isComplete()) {
                results = distbuilder.getDistribution().toString();
                console.log(results);
                Qualtrics.SurveyEngine.setEmbeddedData('EmpiricalBelief1', results);
                localStorage.setItem('results', results);

                // Calculate lower and upper bounds
                var distribution = distbuilder.getDistribution();
                var lowerBound = 0;
                var upperBound = 0;

                var lowerRanges = [0, 1, 10, 20, 30, 40, 50, 60];
                var upperRanges = [0, 9, 19, 29, 39, 49, 59, 60];

                for (var i = 0; i < distribution.length; i++) {
                    lowerBound += distribution[i] * lowerRanges[i];
                    upperBound += distribution[i] * upperRanges[i];
                }

                // Display the calculated bounds
                $j('#lowerBound').text(lowerBound);
                $j('#upperBound').text(upperBound);

                // Show the average guess container
                $j('#averageGuessContainer').show();

                // Update slider attributes to reflect the calculated range
                $j('#averageGuess').attr('min', lowerBound);
                $j('#averageGuess').attr('max', upperBound);
                $j('#averageGuess').val((lowerBound + upperBound) / 2); // Initialize to midpoint
                $j('#averageGuessValue').text(((lowerBound + upperBound) / 2).toFixed(2) + " ECU"); // Show initial value

                // Update slider value display and save to embedded data
                $j('#averageGuess').on('input', function() {
                    var averageGuess = parseFloat($j(this).val());
                    $j('#averageGuessValue').text(averageGuess.toFixed(2) + " ECU");
                    Qualtrics.SurveyEngine.setEmbeddedData('averageGuess', averageGuess.toFixed(2)); // Save average guess

                    if (averageGuess >= lowerBound && averageGuess <= upperBound) {
                        $j("#averageError").hide();
                        $j("#NextButton").attr("disabled", false);
                    } else {
                        $j("#averageError").show();
                        $j("#NextButton").attr("disabled", true);
                    }
                });

                // Initial save of midpoint value
                Qualtrics.SurveyEngine.setEmbeddedData('averageGuess', ((lowerBound + upperBound) / 2).toFixed(2));

                $j("#NextButton").attr("disabled", false);
            } else {
                $j("#NextButton").attr("disabled", true);
            }
        }
    });

    distbuilder.render("targetdiv", "labels-grid-buttons");
    distbuilder.labelize({
        labels: ["0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60"],
        suffix: ' token',
    });
    if (localStorage.getItem('results') != null) {
        var dist = results.split`,`.map(x => +x);
        distbuilder.setDistribution(dist);
    }
});

Qualtrics.SurveyEngine.addOnUnload(function() { });
