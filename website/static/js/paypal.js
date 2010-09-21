
// jquery log plugin
(function(a){a.extend({log:function(){if(arguments.length>0){var b=(arguments.length>1)?Array.prototype.join.call(arguments," "):arguments[0];try{console.log(b);return true}catch(c){try{opera.postError(b);return true}catch(c){}}alert(b);return false}}})})(jQuery);

// JSON shortcut
var json = JSON.stringify;

var BKNRObject = function (options) {
    options = options || {};
    
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

	showCMS: function() {
	    $("#cms").css({top: "106px",
			   left: "730px",
			   width: "400px"});
	},

	logout: function () {
	    var self = this;
	    $.getJSON("/json-logout");
	    self.isLoggedIn = false;
	    $("#login_status").hide();
	},

	/* JSON callbacks */
	onJSONLogin : function (data) {
	    var self = this;
	    
	    self.isLoggedIn = data.admin;
	    self.username = data.login;

	    if (self.isLoggedIn) {
		self.showCMS();
		$("#username").html(self.username);
		$("#login_status").css({visibility: "visible"});
	    }
	    
	    if (self.options.ensureAdmin) {
		if (!self.isLoggedIn) {
		    $.log("not logged in " + json(data));
		    //		    window.location.replace("/admin");
		    return;
		}
	    } 

	    $("#body").trigger("BKNR_initialized");
	}
	
    };
};

function dateToUniversalTime(date) {
    return date.getTime() / 1000 + 2208985200;
}

var fooTx;

var PaypalObject = function (options) {
    options = options || {};

    var defaults = {
	resultsPerPage: 20
    };

    for (var i in defaults) {
	if (options[i] == undefined) {
	    options[i] = defaults[i];
	}
    }

    var obj = {
	dateFrom: null,
	dateTo: null,
	options: options,
	
	init : function() {
	    var self = this;
	    self.showPaypalForm();
	},

	doForm: function () {
	    var self = this;
	    var fromDate = $("#datepicker_from").datepicker("getDate");
	    var toDate = $("#datepicker_to").datepicker("getDate");

	    $("#paypal_info").html(H2(null,
				     "Transactions from " + fromDate.toDateString()
				      + " until " + toDate.toDateString()));
	    
	    var queryString = "from=" + (dateToUniversalTime(fromDate))
		+ "&until=" + (dateToUniversalTime(toDate));
	    $("#paypalstatus :checked").map(function () {
						queryString += "&status=" + $(this).attr("name");
					    });
	    $.getJSON("/json-paypal-admin?" + queryString,
		      function (data) {
			   self.onJSONPaypal(data);
		      });
	},

	onJSONPaypal: function (data) {
	    var resultList = $("#paypal_results");
	    var paypalInfo = $("#paypal_info");

	    var totalAmount = 0;
	    var totalFees = 0;
	    var txCount = 0;
	    
	    resultList.empty();
	    
	    for (var i in data.paypalTransactions) {
		var tx = data.paypalTransactions[i];
		var elt = LI(null, DIV({"class": "paypal_tx"},
				       DIV({"class": "tx_info"},
					   IMG({"src": "/image/" + tx.image.name + "/thumbnail,,60,60"}),
					   DIV({"class": "tx_top"},
					       "Bought for ",
					       SPAN({"class": "tx_price"},
						    
						    tx["paypal-result"].amt, " ",
						    tx["paypal-result"].currencycode),
					       " on ",
					       SPAN({"class": "tx_date"},
						    tx["paypal-result"].ordertime),
					       " by ",
					       SPAN({"class": "tx_email"}, tx["paypal-info"].email),
					       BR(),
					       "Valid until ",
					       SPAN({"class": "tx_date"},
						    tx.valid_until)
					      )),
				       DIV({"class": "tx_more_info"},
					   SPAN({"class": "tx_token"}, tx.token),
					   SPAN({"class": "tx_email"}, tx["paypal-info"].email)
					   )));
		elt = resultList.append(elt);
		elt.find(".tx_mode_info").toggle();
		fooTx = tx;
	    }
	    var res = resultList.evtpaginate({perPage: 5});
	},

	onEvtPaginateInitialized : function(e, startnum, totalnum) {
	    $.log("paginate init");
	    $.log("start " + startnum + " total num " + totalnum);
	    $("#paypal_cur").html(startnum);
	    $("#paypal_total").html(totalnum);
	},

	onEvtPaginateFinished: function (e, num, isFirst, isLast) {
	    $.log("paginate finished");
	    $.log(e);
	    $("#paypal_cur").html(num);
	},

	showPaypalForm: function() {
	    var self = this;
	    // XXX set month range automatically
	    $("#datepicker_from").datepicker(
		{ onSelect: function (dateText, inst) { self.doForm(); },
		defaultDate: -14 });
	    $("#datepicker_to").datepicker(
		{ onSelect: function (dateText, inst) { self.doForm(); }});
	    $("#paypalstatus :checkbox").change(function (obj) {
						    $("#paypalstatus :checkbox").map(
							function () {
							    if (this != obj.target) {
								$(this).attr("checked", false);
							    }});
						self.doForm(); });
	    $('#paypalstatus input[name="successful"]').attr("checked", true);
	    var resultList = $("#paypal_results");
	    resultList.bind("initialized.evtpaginate", function(e, startnum, totalnum) {
					 self.onEvtPaginateInitialized(e, startnum, totalnum);
				     });
	    resultList.bind("finished.evtpaginate", function(e, num, isFirst, isLast) {
					 self.onEvtPaginateFinished(e, num, isFirst, isLast);
				     });

	    $("#paypal_prev").click(function() { resultList.trigger("prev.evtpaginate"); return false; });
	    $("#paypal_next").click(function() { resultList.trigger("next.evtpaginate"); return false; });
	    self.doForm();

	    $("#paypalform").css({visibility: "visible"});
	    
	}
    };

    $("#body").bind("BKNR_initialized", function () { obj.init(); });

    return obj;
};

var BKNR;
var Paypal;
    
    
// load up paypal interface
$(document).ready(
    function () {
	BKNR = new BKNRObject({ ensureAdmin: true });
	Paypal = new PaypalObject();

	BKNR.init();
    });