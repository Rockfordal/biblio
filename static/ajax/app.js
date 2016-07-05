var username = "user";
var password = "password";

var b2a = btoa(username + ":" + password);

function beforeSend(xhr) {
    xhr.setRequestHeader("Authorization", "Basic " + b2a);
}

var js = function() {
    var XMLReq = new XMLHttpRequest();
    var url = "http://localhost:3001/users"
    XMLReq.open("GET", url, false, username, password);
    XMLReq.withCredentials = true;
    XMLReq.setRequestHeader("Authorization", "Basic " + b2a);
    XMLReq.send(null);
}

var ajaks = function() {
    $.ajax({
        url: "http://localhost:3000/users",
        // data: {username: "user",
        //        password: "password"},         // Query String Parameters
        // headers: {
        //     contentType : 'application/x-www-form-urlencoded'
        //     // Authorization: library.basicHeader('mlm', 'asdf1234')
        //     Authorization: b2a
        // },
        // crossDomain: true,
        // type : 'GET',
        // contentType : 'application/json',
        // contentType : 'application/x-www-form-urlencoded',
        // dataType: 'json',
        // dataType: 'jsonp',
        // xhrFields: {
        //     withCredentials: true
        // },
        beforeSend: beforeSend,
        success: function(data) {
            console.log("success",data);
        },
        error: function(data) {
            console.log("error", data);
        }
    })
}

ajaks();
// console.log(btoa("user:password"));
