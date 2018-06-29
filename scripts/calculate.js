var avg = 0.0;
var std = 1.0;
var score = 1.0;
var z = (score-avg)/std;
var p = "%";


function update_input() {
    if ($( "#average" ).val()) {
        avg = parseFloat($( "#average" ).val());
    }else{
        avg = 0.0;
    }
    if ($( "#std" ).val()) {
        std = parseFloat($( "#std" ).val());
    }else{
        std = 1.0;
    }
    if ($( "#score" ).val()) {
        score = parseFloat($( "#score" ).val());
    }else{
        score = 1.0;
    }
    z = (score-avg)/std;
    $("#score_val").html(score);
    $("#z_score").html(z.toFixed(2));
    $("#p_val").html(p);
}
$(function() {
    $("#score_val").val(score);
    $("#z_score").html(z);
    $("#p_val").html(p);
    $("#average").keyup(function(event) {
        update_input();
    });
    $("#average").val(avg);
    $("#std").keyup(function(event) {
        update_input();
    });
    $("#std").val(std);
    $("#score").keyup(function(event) {
        update_input();
    });
    $("#score").val(score);
});