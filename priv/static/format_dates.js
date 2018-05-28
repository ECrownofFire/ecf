(function() {
    var times = document.getElementsByClassName('format-time');
    for(var i = 0; i < times.length; i++) {
        T = times[i];
        T.innerHTML = (new Date(T.dateTime)).toLocaleString();
    }
})();

