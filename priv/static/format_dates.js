(function() {
    var times = document.getElementsByClassName('format-time');
    for(var i = 0; i < times.length; i++) {
        var T = times[i];
        T.innerHTML = (new Date(T.dateTime)).toLocaleString();
    }
})();

(function() {
    var dates = document.getElementsByClassName('format-date');
    for(var i = 0; i < dates.length; i++) {
        var T = new Date(dates[i].dateTime);
        var D = new Date(T.getTime() + T.getTimezoneOffset() * 60000);
        dates[i].innerHTML = D.toLocaleDateString();
    }
})();

