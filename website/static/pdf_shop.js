function init_shop() {
    $("upload_price_select").innerHTML = make_price_selector();
}

var pdf_prices = [12, 15];

function make_price_selector() {
    var price_options = '<select name="price-select" id="pdf_price_select" size="1">';
    for (var i = 0; i < pdf_prices.length; i++) {
	price_options += '<option value="' + pdf_prices[i] + '">' + pdf_prices[i] + '</option>';
    }
    price_options += '</select>';

    return price_options;
}

function shop_set_price_selector(price) {
    log("shop set price " + price);
    var selector = $("pdf_price_select");
    if (selector) {
	for (var i in selector.options) {
	    if (selector.options[i].value == price) {
		selector.selectedIndex = i;
		break;
	    }
	}
    }
}

function shop_show_pricetags(page, subpath) {
    $("pricetag-a").href = window.location.hash;
    
    if ((subpath != undefined) && (subpath != "")) {
	$("pricetag-big").style.visibility = "hidden";
	$("pricetag-medium").style.visibility = "hidden";
	return;
    }

    switch (page) {
	case "home":
	$("pricetag-big").style.visibility = "visible";
	$("pricetag-medium").style.visibility = "hidden";
	break;

	case "pixel":
	case "vector":
	$("pricetag-big").style.visibility = "hidden";
	$("pricetag-medium").style.visibility = "visible";
	break;

	default:
	$("pricetag-big").style.visibility = "hidden";
	$("pricetag-medium").style.visibility = "hidden";
    }
}

function shop_show_form_for_image(current_image) {
    log("shop show form");
    $("shop_upload_form_element").action = "/upload-shop/" + current_image.id;
    if (current_image.shop_file != undefined) {
	$("upload_pdf_field").style.visibility = "hidden";
	$("upload_pdf_button").style.visibility = "hidden";
	$("edit_pdf_button").style.visibility = "visible";
	$("delete_pdf_button").style.visibility = "visible";
	shop_set_price_selector(current_image.shop_price);
    } else {
	$("upload_pdf_field").style.visibility = "visible";
	$("upload_pdf_button").style.visibility = "visible";
	$("edit_pdf_button").style.visibility = "hidden";
	$("delete_pdf_button").style.visibility = "hidden";
    }
}

function shop_hide_form() {
    $("upload_pdf_field").style.visibility = "hidden";
    $("upload_pdf_button").style.visibility = "hidden";
    $("edit_pdf_button").style.visibility = "hidden";
    $("delete_pdf_button").style.visibility = "hidden";    
}


function init_shop_overlay(image) {
    submit_json('/json-paypal?price=' + image.shop_price + '&image=' + image.id,
		null, // no form
		partial(make_shop_overlay, image)
	       );
}

function show_eula() {
    var eula_window = window.open('/static/eula.html', 'eula_window', "width=700,height=400,status=no,toolbar=no,menubar=no,scrollbars=yes");
    return eula_window;
}

function make_shop_overlay(image, json) {
    var paypalLink = json.queryResult.paypalLink;
    log("link " + paypalLink);
    make_overlay('buy-file', 'Buy Art as Vector PDF File', 426,
                 FORM({ action: '#', onsubmit: 'return false' },
		      DIV({'align': 'center'},
			   IMG({'src': '/image/' + image.id + '/thumbnail,,160,160'})),
                      SPACER("Download Artwork ", ARTWORK_NAME(image.name), " for one-time private use only.  ",
                             "Please read our ",
                             A({ onclick: "show_eula()"},
			       "User Agreement"),
                             " and tick the box below to indicate that you agree to be bound to it.",
                             BR(), BR(),
                             INPUT({ type: 'checkbox', name: 'agree-to-license'}),
                             " I have read and understood the 'User Agreement' and agree to be bound to the terms set forth in it",
                             BR(), BR(),
                             PRICE(image.shop_price + "$"), 
                             BR()
//                             buy_product_button(buy_file),
			    )));
}

INFOTITLE = partial(SPAN, { 'class': 'notice' });

function make_pdf_info_overlay() {
    make_overlay('pdf-info', 'Download Art as Vector PDF',
		 280,
		 P({'class': 'pdf-info'},
		   "Selected Artwork available for download as vector pdf file! Look for the pricetag!"));
}

