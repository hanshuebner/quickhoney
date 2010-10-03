function init_shop() {
    $("upload_price_select").innerHTML = make_price_selector();
}

var pdf_prices = [12, 15];

function shop () {
    directory('shop', 'shop');
}

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
    $("shop_upload_form_element").action = "/upload-shop/" + current_image.id;
    if (current_image.shop_file != undefined) {
	$("pdf_field").style.visibility = "visible";
	$("pdf_download_link").href = "/pdf-admin/" + current_image.shop_file;
	$("upload_pdf_field").style.visibility = "hidden";
	$("upload_pdf_button").style.visibility = "hidden";
	$("edit_pdf_button").style.visibility = "visible";
	$("delete_pdf_button").style.visibility = "visible";
	$("pdf-generate-warning").style.visibility = "hidden";
	$("pdf-generate-upload").style.visibility = "hidden";
	$("pdf-file-upload").style.visibility = "hidden";
	shop_set_price_selector(current_image.shop_price);
	$("pdf-activate").checked = current_image.shop_active;
    } else {
	$("pdf_field").style.visibility = "hidden";
	$("upload_pdf_field").style.visibility = "visible";

	if (current_image.category == "pixel") {
	    if ((current_image.width * current_image.height) >= (400 * 400)) {
		$("pdf-generate-warning").style.visibility = "visible";
	    } else {
		$("pdf-generate-warning").style.visibility = "hidden";
	    }
	    $("pdf-generate-upload").style.visibility = "visible";
	    $("pdf-file-upload").style.visibility = "hidden";
	} else {
	    $("pdf-generate-upload").style.visibility = "hidden";
	    $("pdf-file-upload").style.visibility = "visible";
	}
	
	$("upload_pdf_button").style.visibility = "visible";
	$("edit_pdf_button").style.visibility = "hidden";
	$("delete_pdf_button").style.visibility = "hidden";
	$("pdf-activate").checked = false;
    }
}

function shop_hide_form() {
    $("upload_pdf_field").style.visibility = "hidden";
    $("upload_pdf_button").style.visibility = "hidden";
    $("edit_pdf_button").style.visibility = "hidden";
    $("delete_pdf_button").style.visibility = "hidden";    
}

function wait_for_pdf_generation_cb(options, json_data) {
    log("pdf generation " + JSON.stringify(json_data.image) + json_data.image.shop_price);
    if (json_data.image.shop_price === undefined) {
	if (options.count-- <= 0) {
	    options.callback(options, {error: "PDF generation stalled"});
	    return true;
	}
	return false;
    } else {
	options.callback(options, json_data);
	return true;
    }
}

function wait_for_pdf_generation(image_name, callback) {
    var options = {
	image_name: image_name,
	count: 30,
	callback: callback
    };
    poll_json("/json-image-info/" + image_name, partial(wait_for_pdf_generation_cb, options));
}

function after_pdf_generation_upload(options, json) {
    hide_cue();
    log("after pdf generation " + JSON.stringify(json));
    if (json.error) {
	$("information").innerHTML = "Error while converting image to PDF!";
    } else {
	$("information").innerHTML = "Image successfully converted to PDF!";
	window.opener.current_image.shop_price = json.image.shop_price;
	window.opener.current_image.shop_file = json.image.shop_file;
	window.opener.current_image.shop_active = json.image.shop_active;
	window.opener.after_image_edit();
    }
    $("ok").style.visibility = "visible";
}

function wait_for_pdf_generation_upload() {
    $("ok").style.visibility = "hidden";
    show_cue();
    wait_for_pdf_generation(window.opener.current_image.name, after_pdf_generation_upload);
}

function show_eula() {
    var eula_window = window.open('/static/eula.html', 'eula_window',
				  "width=700,height=400,status=no,toolbar=no,menubar=no,scrollbars=yes");
    return eula_window;
}

function format_file_size(size) {
    return Math.floor(size / 1024) + "kb";
}

function handle_eula_agreed() {
    var eula_checkbox = $("eula-checkbox");
    if (eula_checkbox.checked) {
    }
}

function check_eula_agreed() {
    var eula_checkbox = $("eula-checkbox");
    if (!eula_checkbox.checked) {
	alert("Please agree to the EULA!");
	return false;
    } else {
	return true;
    }
}

var pricetag_animation;

function pulsate_pricetag(id) {
    var pricetag = $('pricetag-small') || $('pricetag-micro-' + id);
    pricetag_animation = pulsate(pricetag, {'pulses': 250, duration: 100, afterFinish: pulsate_pricetag});
}

function pulsate_pricetag_stop(id) {
    pricetag_animation.cancel();
    var pricetag = $('pricetag-small') || $('pricetag-micro-' + id);
    appear(pricetag);    
}

function wait_for_paypal_link_cb(options, json) {
    if (json.status == "error") {
	options.callback({error: "Paypal link generation failed"});
	return true;
    } else if ((json.status == "ongoing") && (json.paypalLink != undefined) && (json.buttonLink != undefined)) {
	options.callback(json);
	return true;
    } else {
	if (options.count-- <= 0) {
	    options.callback({error: "Paypal link generation stalled"});
	    return true;
	}
	return false;
    }
}

function wait_for_paypal_link(image, json) {
    if (json.id != undefined) {
	var options = {
	    count: 30,
	    image: image,
	    callback: partial(make_shop_overlay, image)
	};
	poll_json("/json-paypal-checkout?id=" + json.id, partial(wait_for_paypal_link_cb, options));
    } else {
	make_shop_overlay(image, {error: "Could not create paypal transaction"});
    }
}

function init_shop_overlay(image) {
    pulsate_pricetag(image.id);
    submit_json('/json-paypal-checkout?image=' + image.id +
		'&color=' + pages[current_directory].link_color,
		null, // no form
		partial(wait_for_paypal_link, image));
}

function make_shop_overlay(image, json) {
    if (json.error) {
	// error while doing paypal stuff
	var overlay = make_overlay_content($('overlay'),
					   {
					       id: 'buy-file',
					       title: 'Buy Art as Vector PDF File',
					       width: 426,
					       cssClass: 'paypal-overlay',
					       fade: true,
					       waitForImages: true,
					       onShow:     partial(pulsate_pricetag_stop, image.id)
					   },
					   FORM({ action: '#', onsubmit: 'return false' },
						SPACER(
						    IMG({'src': '/image/' + image.id + '/thumbnail,,160,160'}), BR(), BR(),
						    "FILE#: " + image.id, BR(),
						    "File Type: Vector PDF", BR(),
						    "File Size: " + format_file_size(image.shop_size), BR(),
						    "Price: " + image.shop_price + "$" , BR(), BR(),

						    "Something went wrong while creating your personal item. Please try to buy the image again. We apologize for the inconvenience.")));
	return;
    }

    var paypalLink = json.paypalLink;
    var buttonLink = json.buttonLink;
    log("link " + paypalLink);
    var overlay = make_overlay_content($('overlay'),
			 {
			     id: 'buy-file',
			     title: 'Buy Art as Vector PDF File',
			     width: 426,
			     cssClass: 'paypal-overlay',
			     fade: true,
			     waitForImages: true,
			     onShow:     partial(pulsate_pricetag_stop, image.id)
			 },
                 FORM({ action: '#', onsubmit: 'return false' },
                      SPACER(
			  IMG({'src': '/image/' + image.id + '/thumbnail,,160,160'}), BR(), BR(),
			  "FILE#: " + image.id, BR(),
			  "File Type: Vector PDF", BR(),
			  "File Size: " + format_file_size(image.shop_size), BR(),
			  "Price: " + image.shop_price + "$" , BR(), BR(),
			  
			  "Download Artwork ", BOLD(image.name), " for one-time private use only.  ",
                             "Please read our ",
                             A({ onclick: "show_eula()", id: 'eula-link'},
			       "User Agreement"),
                             " and tick the box below to indicate that you agree to be bound to it.",
                             BR(), BR(),
                             INPUT({ id: 'eula-checkbox',
				     type: 'checkbox',
				     onclick: 'handle_eula_agreed()',
				     name: 'agree-to-license'}),
                             " I have read and understood the 'User Agreement' and agree to be bound to the terms set forth in it.",
                          BR(), BR(),
			  "Vector pdfs are resolution-independent, and can be scaled to any size.", BR(), BR(),
			  
			  
			  DIV({},
			      A({'onclick': 'return check_eula_agreed()',
				 'target': '_parent',
				'href': paypalLink },
				IMG({'src': buttonLink}))),
			  BR()
		      )));
}

INFOTITLE = partial(SPAN, { 'class': 'notice' });

function make_pdf_info_overlay() {
    make_overlay_content($('overlay'),
			 { id: 'pdf-info',
			   title: 'Download Art as Vector PDF',
			   left: '64px',
			   'cssClass': 'pdf-info-overlay',
			   width: 320 },
		 P({'class': 'pdf-info'},
		   "Selected Artwork available for download as vector pdf file! ", BR(), BR(),
		   "Vector pdfs are resolution-independent, and can be scaled to any size." , BR(), BR(),
		   ITALIC("Look for the pricetag!")));
}

function show_paypal_page(subpath) {
    log("paypal - " + subpath);

    submit_json("/json-paypal-txn/" + subpath, null, show_paypal_txn);
}

function make_paypal_overlay(json) {
    log("make-overlay");
    var overlay2 = $("overlay-pdf");
    if (json.valid) {
	var image = json.image;
	var pdfLink = partial(A, {'href': "/pdf-client/" + json.token, 'target': '_blank'});
	var cssClass = (json.image.category == "pixel" ? "pixel-download download-overlay" :
			"vector-download download-overlay");
	var buttonColor = (json.image.category == "pixel" ? "ff00ff" : "00ccff");
	make_overlay_content(overlay2, { id: 'buy-file',
					 title: 'Download your Vector PDF File!',
					 cssClass: cssClass,
					 color: buttonColor,
					 invertColor: true,
					 width: 426,
					 fade: true },
                     SPACER(
			 pdfLink(IMG({'src': '/image/' + image.id + '/thumbnail,,160,160'})), BR(), BR(),
			 "FILE#: ", image.id, BR(),
			 "File Type: Vector PDF", BR(),
			 "File Size: " + format_file_size(image.shop_size), BR(),
			 "Price: " + image.shop_price + "$", BR(),
			 "Purchased on " + json.bought_on, BR(),
			 "Download valid until " + json.valid_until, BR(),
			 BR(),
			 
			 pdfLink("Download"), " Artwork ", pdfLink(image.name), " for one-time private use only.  ",
			 BR()
		     ));
    } else if (json.expired && (json.status == "successful")) {
	var image = json.image;
	make_overlay_content(overlay2,
			     { id: 'buy-file',
			       title: 'Your Vector PDF File has expired!',
			       cssClass: cssClass,
			       width: 426,
			       color: buttonColor,
			       invertColor: true,
			       fade: true
			       },
                     SPACER(
			 IMG({'src': '/image/' + image.id + '/thumbnail,,160,160'}), BR(), BR(),
			 "FILE#: " + image.id, BR(),
			 "File Type: Vector PDF", BR(),
			 "File Size: " + format_file_size(image.shop_size), BR(),
			 "Price: " + image.shop_price + "$", BR(),
			 "Purchased on " + json.bought_on, BR(),
			 "Download valid until " + json.valid_until, BR(),
			 BR(),
			 
			 "Sadly, your PDF download has expired!",
			 BR()
		     ));
    } else if (json.status == "cancelled") {
	/* do nothing */
    } else {
	var image = json.image;
	make_overlay_content(overlay2,
			     {id: 'buy-file',
			      title: 'Wrong Paypal Transaction!',
			      cssClass: cssClass,
			      width: 426,
			      color: buttonColor,
			      invertColor: true,
			      fade:true },
                     SPACER(
			 IMG({'src': '/image/' + image.id + '/thumbnail,,160,160'}), BR(), BR(),
			 "FILE#: " + image.id, BR(),
			 "File Type: Vector PDF", BR(),
			 "File Size: " + format_file_size(image.shop_size), BR(),
			 "Price: " + image.shop_price + "$", BR(),
			 BR(),
			 
			 "Something went wrong with your Paypal transaction! ",
			 "No money has been debited from your Paypal account. ",BR(),
			 "Please try to buy the item again.",
			 BR()
		     ));
    }
    wait_for_images(hide_cue);
}
function show_paypal_txn(json) {
    log("txn  " + JSON.stringify(json));
    if (json.image != undefined) {
	directory(json.image.category, json.image.subcategory, json.image.name);
    }
    make_paypal_overlay(json);
}

