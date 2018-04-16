'use strict';

// import css from 'main.css';


// require('ace-css/css/ace.css');
// require('font-awesome/css/font-awesome.css');

require('purecss/build/pure-min.css');
// Require index.html so it gets copied to dist
require('./main.css');
require('./index.html');

var notificationSound = require('../static/slow-spring-board.ogg');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode);

app.ports.playSound.subscribe(function(soundFile) {
    var audio = new Audio(notificationSound);
    audio.play();
});

app.ports.getCurrentUrl.subscribe(function() {
    app.ports.currentUrl.send(window.location.href);
});

app.ports.title.subscribe(function(title) {
    document.title = title;
});

app.ports.getCookies.subscribe(function() {
    app.ports.cookies.send(document.cookie);
});