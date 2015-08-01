var RESULTS_PER_PAGE = 6;

function Indexed (index, data) {
    this.index = index;
    this.data = data;
};

function Paginator (pageSize) {
    this.pageSize = pageSize;
    this.pageCount = 1;
    this.pages = [[]];
};

Paginator.Index = function (page, offset) {
    this.page = page;
    this.offset = offset;
};

Paginator.prototype = {
    add: function (entry) {
        var page = this.pages[this.pageCount - 1];
        var offset = page.length;
        if (offset < this.pageSize) {
            var index = new Paginator.Index(this.pageCount - 1, offset);
            page.push(new Indexed(index, entry));
        } else {
            var index = new Paginator.Index(this.pageCount, 0);
            this.pages.push([new Indexed(index, entry)]);
            this.pageCount += 1;
        }
    },
    removeAt: function (index) {
        var p = index.page;
        var page = this.pages[p];
        page.splice(index.offset, 1);
        for (var i = index.offset; i < page.length; i++) {
            page[i].index.offset -= 1;
        }
        if (p < this.pageCount - 1) {
            var entry = this.pages[p + 1][0];
            this.removeAt(entry.index);
            entry.index.page = p;
            entry.index.offset = this.pageSize;
            page.push(entry);
        }
    }
};

function AudioStreamPlayer () {
    this.reset();
};

AudioStreamPlayer.prototype = {
    reset: function () {
        if (this.current) {
            this.audioStreams[this.current].pause();
        }
        this.current = null;
        this.audioStreams = new Object(null);
    },
    play: function (url) {
        var audio = this.audioStreams[url];
        if (!audio) {
            audio = new Audio(url);
            this.audioStreams[url] = audio;
        }
        audio.play();
    },
    pause: function (url) {
        var audio = this.audioStreams[url];
        if (audio) {
            audio.pause();
        }
    }
};

var playlistach = angular.module("playlistach", []);

playlistach.directive("ngPlaylist", function () {
    return {
        templateUrl: "playlist.html",
        restrict: "A",
        scope: {
            tracks: "=",
            mode: "@",
            dual: "=",
            player: "="
        },
        link: function ($scope, $elem, $attr) {
            $scope.page = 0;
            $scope.move = function (entry) {
                $scope.tracks.removeAt(entry.index);
                $scope.dual.add(entry.data);
            };
        }
    }
});

playlistach.controller("MainCtrl", function ($scope) {
    $scope.searchQuery = "";
    $scope.searchResultsPaginator = new Paginator(RESULTS_PER_PAGE);
    $scope.userPlaylistPaginator = new Paginator(RESULTS_PER_PAGE);

    $scope.searchResultsPaginator.add({
        "title": "Burial - Archangel",
        "origin": "SC",
        "streamUrl": "1231"
    }); // test entry

    $scope.search = function (query) {
        $.get("api/search?query=" + encodeURIComponent(query), function (data) {
            var newSearchResultsPaginator = new Paginator();
            data.forEach(function (entry) {
                newSearchResultsPaginator.add(entry);
            });
        });
    };
});