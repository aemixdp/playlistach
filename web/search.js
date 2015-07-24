$(document).ready(function () {
    $("#search").click(function () {
        var searchResultsElem = $("#search-results");
        var query = $("#search-query").val();
        $.get("api/search?query=" + query, function (data) {
            console.log("data: ", data);
            data.forEach(function (entry) {
                var titleElem = $("<div>");
                var audioElem = $("<audio controls type=\"audio/mpeg\">");
                titleElem.text(entry.artist + " - " + entry.title);
                audioElem.attr("src", "api/audio?url=" + encodeURIComponent(entry.url));
                searchResultsElem.append(titleElem);
                searchResultsElem.append(audioElem);
            });
        });
    });
});