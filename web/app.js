var RESULTS_PER_PAGE = 6;

var app = angular.module("playlistach", []);

app.controller("MainCtrl", function ($scope) {
    var testEntry = {
        "id": "123918",
        "title": "Burial - Archangel",
        "origin": "SC"
    };

    $scope.searchQuery = "";
    $scope.pageCount = 1;
    $scope.currentPage = 0;
    $scope.currentPlayingTrack = testEntry["id"];
    $scope.results = [[testEntry]];

    $scope.search = function (query) {
        $.get("api/search?query=" + encodeURIComponent(query), function (data) {
            var newPageCount = 1;
            var newResults = [];
            var page = [];
            data.forEach(function (entry) {
                page.push(entry);
                if (page.length == RESULTS_PER_PAGE) {
                    newResults.push(page);
                    newPageCount += 1;
                    page = [];
                }
            });
            $scope.results = newResults;
            $scope.pageCount = newPageCount;
        });
    };

    $scope.play = function (track) {
        alert("play");
    };

    $scope.pause = function (track) {
        alert("pause");
    };
});