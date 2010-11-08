
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
		    window.location.replace("/admin");
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

	reactivate: function (token, sendEmail) {
	    var self = this;
	    var url = "/json-paypal-admin?action=reactivate&token=" + token;
	    if (sendEmail) {
		url += "&sendEmail=true";
	    }
	    $.getJSON(url, function (data) {
			  self.doForm();
		      });
	},

	onJSONPaypal: function (data) {
	    var resultList = $("#paypal_results");
	    var paypalInfo = $("#paypal_info");

	    var stats = {
		totalAmount: 0,
		totalFees: 0,
		txCount: 0
	    };
	    
	    resultList.empty();
	    
	    for (var i in data.paypalTransactions) {
		var tx = data.paypalTransactions[i];

		var templateData = {
		    image_url: "/image/" + tx.image.name + "/thumbnail,,60,60",
		    price: tx.paypal_result.amt + " " + tx.paypal_result.currencycode,
		    date: tx.paypal_result.ordertime,
		    email: tx.paypal_info.email,
		    valid_until: tx.valid_until,
		    token: tx.token
		};

		document.tx = tx;

		stats.txCount++;
		stats.totalAmount += parseFloat(tx.paypal_result.amt);
		stats.totalFees += parseFloat(tx.paypal_result.feeamt);
		
		var template =
		    '<li>'+
		    '<div class="paypal_tx">' +
		    '<img src="{{image_url}}"/>' +
		    '<div class="tx_top">' +
		    ' Bought for <span class="tx_price">{{price}}</span>' +
		    ' on <span class="tx_date">{{date}}</span> by <span class="tx_email">{{email}}</span><br/>' +
		    ' Valid until <span class="tx_date">{{valid_until}}</span>' +
		    (tx.status == "successful" ? 
		    ' (<a href="#" onclick=\'Paypal.reactivate("{{token}}")\'>Reactivate</a>) ' +
		    ' (<a href="#" onclick=\'Paypal.reactivate("{{token}}", true)\'>Reactivate + Email client</a>) ' :
		    '') +
		    '</div></div></li>';
		    
		var elt = resultList.append(Mustache.to_html(template, templateData));
		elt.find(".tx_mode_info").toggle();
		fooTx = tx;
	    }

	    stats.totalAmount = Number(stats.totalAmount).toFixed(2);
	    stats.totalFees = Number(stats.totalFees).toFixed(2);
	    var res = resultList.evtpaginate({perPage: 10});

	    var infoTemplate = "{{txCount}} transactions, total: {{totalAmount}} USD, fees: {{totalFees}} USD";
	    $("#paypal_info").append(Mustache.to_html(infoTemplate, stats));
	},

	onEvtPaginateInitialized : function(e, startnum, totalnum) {
	    $.log("paginate init");
	    $.log("start " + startnum + " total num " + totalnum);
	    $("#paypal_cur").html(startnum);
	    $("#paypal_total").html(totalnum);
	},

	onEvtPaginateFinished: function (e, num, isFirst, isLast) {
	    $.log("paginate finished " + isFirst + " " + isLast);
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
		{ onSelect: function (dateText, inst) { self.doForm(); },
		defaultDate: +1 });
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