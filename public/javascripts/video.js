// http://code.google.com/apis/visualization/documentation/reference.html#drawToolbar
// http://code.google.com/apis/chart/docs/chart_wizard.html
// http://code.google.com/apis/youtube/js_api_reference.html#Events

// var bucket = 10;
// var buckets = null;
// var chart = null;
// var dataTable = null;

// google.load("visualization", "1", {packages:["imagechart"]});
// google.setOnLoadCallback(onLoadCallback);

$(document).ready(function() {
    $('a.session-event').live('click', function() {
      var jumpTo = parseFloat($(this).parent().find('.time').text());
      $("#player")[0].seekTo(jumpTo - 2, true);

      return false;
    });

    $("button", "#feedback").button();
    $("button", "#feedback").click(function() {

      var activity = $(this).attr("action");
      var session = $(this).attr("session");
      var video = $(this).attr("video");

      var time = $("#player")[0].getCurrentTime();

      if (parseFloat(time) > 0) {

        // log to database
        $.post("/events", { time: time, activity: activity, session: session, video: video });

        // log to screen
         $('#log').prepend($("<li><strong class='time'>"+ time + "</strong> : <a href='' class='session-event'>" + activity + "</a></li>"));
     }

    });
});
