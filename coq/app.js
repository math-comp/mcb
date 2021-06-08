var express = require('express');
var app = express();

//setting middleware
app.use(express.static(__dirname)); //Serves resources from current folder


var server = app.listen(8010);
