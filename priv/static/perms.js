function checkPermission(type, id, mode, callback) {
    var xhr = new XMLHttpRequest();
    var url = constructPermURL(type, id) + "?perm=" + mode;
    xhr.open('GET', url);
    xhr.responseType = "json";
    xhr.onload = function() {
        callback(xhr.response);
    };
    xhr.send();
}

//examples for perm: (TODO: move this into docs later)
//{class: ["user", 1], mode: "edit_forum", set: "allow"}
//{class: ["group", 2], mode: "create_thread", set: "allow"}
//{class: "others", mode: "create_post", set: "deny"}
function editPermission(type, id, perm, callback) {
    var xhr = new XMLHttpRequest();
    var url = constructPermURL(type, id);
    var req = JSON.stringify(perm);
    xhr.open('PATCH', url);
    xhr.responseType = "json";
    xhr.onready = function() {
        console.log(xhr.status);
    };
    xhr.send(req);
}

//same as above without set
function deletePermission(type, id, perm, callback) {
    var xhr = new XMLHttpRequest();
    var url = constructPermURL(type, id);
    var req = JSON.stringify(perm);
    xhr.open('DELETE', url);
    xhr.responseType = "json";
    xhr.onready = function() {
        callback(xhr.status);
    };
    xhr.send(req);
}

function constructPermURL(type, id) {
    var url = BASE_URL + "/" + type
    if (type !== "global") {
        return url + "/" + id + "/perms";
    } else {
        return url + "/perms";
    }
}

