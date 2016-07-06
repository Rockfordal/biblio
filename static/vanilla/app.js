function sleep(time) {
    return new Promise((resolve) => setTimeout(resolve, time));
}

function onSuccess(res){
    console.log("success", res);
}

function onError(err){
    console.log("error", err);
}

var user = "user";
var password = "password";
var headerAuthorization = "Basic " + btoa(user + ":" + password);

    console.log('getHello:');
    getHello("Anders", onSuccess, onError);

sleep(100).then(function() {
    console.log('getBookById:');
    getBookById(1, onSuccess, onError);
    getBookById(999, onSuccess, onError);
});

sleep(300).then(function() {
    console.log('getBook:');
    // getBook = function(searchField, searchStr, offset, limit, onSuccess, onError);
    getBook("title", "", 0, 10, onSuccess, onError);
});

sleep(500).then(function() {
    console.log('getBooks:');
    // getBooks = function(onSuccess, onError)
    getBooks(onSuccess, onError);
});


function successJson(res){
    console.log("success JSON");
}

sleep(700).then(function() {
    console.log('getUsers:');
    // getUsers = function(headerAuthorization, onSuccess, onError)
    getUsers(headerAuthorization, onSuccess, onError);
});

