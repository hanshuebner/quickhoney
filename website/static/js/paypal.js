
// jquery log plugin
(function(a){a.extend({log:function(){if(arguments.length>0){var b=(arguments.length>1)?Array.prototype.join.call(arguments," "):arguments[0];try{console.log(b);return true}catch(c){try{opera.postError(b);return true}catch(c){}}alert(b);return false}}})})(jQuery);

// JSON shortcut
var json = JSON.stringify;

var BKNRObject = function (options) {
    var defaults = {
	ensureAdmin: false,
	showCMS: true
    };

    for (var i in defaults) {
	if (options[i] == undefined) {
	    options[i] = defaults[i];
	}
    }
    
    return {
	isLoggedIn: false,
	username: "anonymous",
	options: options,

	init : function() {
	    var self = this;
	    $.getJSON("/json-login", function (data) { self.onJSONLogin(data); });
	},

	/* JSON callbacks */
	onJSONLogin : function (data) {
	    var self = this;
	    
	    self.isLoggedIn = data.admin;
	    self.username = data.login;

	    if (self.isLoggedIn) {
		$("#body").append('<div></div>');
		$("#body").load("/index #cms", function () {
				   $("#login_status").css({visibility: "visible"});
			       });
	    }
	    
	    if (self.options.ensureAdmin) {
		if (!self.isLoggedIn) {
		    window.location.replace("/admin");
		}
	    }
	}
    };
};

var PaypalObject = function (node) {
    return {
	node: node,

	init : function() {
	}
    };
};

var BKNR;
var Paypal;
    
    
// load up paypal interface
$(document).ready(
    function () {
	BKNR = new BKNRObject({ ensureAdmin: true });
	BKNR.init();
	Paypal = new PaypalObject();
	Paypal.init();
    });