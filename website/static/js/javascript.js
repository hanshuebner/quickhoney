// Copyright 2005-2008 Hans Huebner, hans.huebner@gmail.com
// All rights reserved.

/* safari global variable - used to trigger some compatibility hacks */

var safari = false;

/* configuration */

var max_news_items = 50;        /* maximum number of news items to display */

/* directory definitions */

var home_buttons = ['pixel', 'vector', 'pen', 'news']; /* 'shop' replaced by 'pen' */

var subcategories = {
    pixel: ['birdview', 'headon', 'spot', 'icons', 'animation', 'smallworld'],
    vector: ['icons', 'portraits', 'celebrities', 'blackwhite', 'editorial', 'microspots', 'nudes'],
    pen: ['honeypen', 'portraits', 'nudes', 'stuff']
};

/* rich text editor toolbar configuration */

var editorToolbarConfig = {
    collapse: false,
    draggable: false,
    buttons: [
        { group: 'textstyle', label: 'Font Style',
          buttons: [
              { type: 'push', label: 'Bold CTRL + SHIFT + B', value: 'bold' },
              { type: 'push', label: 'Italic CTRL + SHIFT + I', value: 'italic' },
              { type: 'push', label: 'Underline CTRL + SHIFT + U', value: 'underline' },
              { type: 'separator' },
              { type: 'color', label: 'Font Color', value: 'forecolor', disabled: true },
              { type: 'color', label: 'Background Color', value: 'backcolor', disabled: true }
          ]
        },
        { type: 'separator' },
        { group: 'indentlist', label: 'Lists',
          buttons: [
              { type: 'push', label: 'Create an Unordered List', value: 'insertunorderedlist' },
              { type: 'push', label: 'Create an Ordered List', value: 'insertorderedlist' }
          ]
        },
        { type: 'separator' },
        { group: 'insertitem', label: 'Insert Item',
          buttons: [
              { type: 'push', label: 'HTML Link CTRL + SHIFT + L', value: 'createlink', disabled: true }
          ]
        }
    ]
};

/* logged_in - will be set when the user has CMS access */

var logged_in;
var application_initialized = false;
var news_editor;

/* current colors */

/* login status */

function login_status(json_result) {

    log('login_status, admin: ' + json_result.admin + ' login: ' + json_result.login);

    logged_in = json_result.admin;

    if (logged_in) {
        replaceChildNodes("username", json_result.login);
        $("login_status").style.visibility = 'visible';
        news_editor = new YAHOO.widget.SimpleEditor('news_editor', { toolbar: editorToolbarConfig });
        news_editor.render();

	// clear query cache on login
	db_cache = { };
        show_cms_window();
    }
}

/* CMS functionality */

function show_cms_window(name) {
    log('show_cms_window ' + name);
    
    $("cms").style.top = "106px";
    $("cms").style.left = "730px";
    $("cms").style.width = "400px";

    var elements = $("cms").childNodes;

    if (logged_in) {
        
	for (var i = 0; i < elements.length; i++) {
	    if (elements[i].id) {
		elements[i].style.visibility = (elements[i].id == name) ? "visible" : "hidden";
	    }
	}
        
	$("login_status").style.visibility = 'visible';
	
	if (name == "edit_form") {
	    shop_show_form_for_image(current_image);
	} else {
	    shop_hide_form();
	}
    } else {
	      for (var i = 0; i < elements.length; i++) {
	          if (elements[i].id) {
		            elements[i].style.visibility = "hidden";
	          }
	      }
    }

    log('show_cms_window ' + name + ' done');
}

function send_login() {
    return false;
}

function send_logout() {
    logged_in = false;
    show_cms_window();
    loadJSONDoc("/json-logout")
        .addCallbacks(function () {}, alert);
    // clear query cache on logout
    db_cache = { };
    do_query();
}

/* image editing */

function all_inputs(parent) {
    var inputs = [];
    map(function (tag) {
            inputs = inputs.concat(getElementsByTagAndClassName(tag, null, parent));
        }, ['textarea', 'select', 'input']);
    return inputs;
}

function submit_json(url, form, handler) {
    var names = [];
    var values = [];

    if (form) {
        map(function (input) {
                names.push(input.name);
                if (input.type != 'checkbox' || input.checked) {
                    values.push(input.value);
                }
            }, all_inputs(form));
    }

    log("values " + queryString(names, values));
    log("url " + url);

    MochiKit.Async.doXHR(url, { mimeType: 'text/plain',
                                headers: [['Accept', 'application/json']],
                                method: 'POST',
                                headers: {"Content-Type":"application/x-www-form-urlencoded"},
                                sendContent: queryString(names, values) })
        .addCallbacks(function (req) { handler(MochiKit.Base.evalJSON(req.responseText)); },
                      alert);
}

function do_edit() {
    current_image.client = $("edit_client").value;
    current_image.spider_keywords = $("edit_keywords").value;
    current_image.description = $("edit_description").value;
    current_image.keywords.explicit = $("edit_explicit").checked ? true : false;

    show_cms_window('saving_edits_form'); // hide edit window until server replies

    submit_json('/json-edit-image/' + current_image.id + '?action=edit', 'edit_form_element', image_edited);

    return false;
}

function poll_json_cb(json_url, callback, interval, json_data) {
    if (!callback(json_data)) {
	setTimeout(partial(poll_json, json_url, callback), interval);
    }
}

// poll a JSON source until callback returns true
function poll_json(json_url, callback, interval) {
    if (interval == undefined) {
	interval = 1000;
    }
    submit_json(json_url, null, partial(poll_json_cb, json_url, callback, interval));
}

function after_image_edit() {
    show_cms_window("image_edited_form");
    display_current_image();
    setTimeout("show_cms_window('edit_form')", 2000);
}

function image_edited(json_data) {
    if (json_data.result == 'error') {
        alert(json_data.message);
    }
    after_image_edit();
}

function news_edited(json_data) {
    if (json_data.result == 'error') {
        alert(json_data.message);
    }
    show_cms_window("image_edited_form");
    setTimeout("show_cms_window('edit_news')", 2000);
}

function do_delete() {

    if (confirm('delete this image?')) {
        show_cms_window('saving_edits_form'); // hide edit window until server replies

        submit_json('/json-edit-image/' + current_image.id + '?action=delete', null, image_deleted);
    }

    return false;
}

function image_deleted() {

    show_cms_window("image_deleted_form");
    current_image = null;
    $("image_detail").innerHTML = '';
    setTimeout("if (!current_image) { show_cms_window('upload_form'); }", 2000);
    do_query();
}

/* clients list */

var client_names = [];

function make_clients_selector(textentry_field) {
    var client_options
	= '<select name="client-select" size="1" onchange="$('
	+ "  '" + textentry_field + "'" + ').value = options[selectedIndex].value;"><option value="" />';

    for (var i = 0; i < client_names.length; i++) {
	client_options += '<option value="' + client_names[i] + '">' + client_names[i] + '</option>';
    }
    client_options += '</select>';

    return client_options;
}

function set_clients(json_data) {
    client_names = json_data.clients;
    var rendered_clients = [];
    for (var i = 0; i < client_names.length; i++) {
	      var client_name = client_names[i];
	      if (client_name.search(/,/)) {
	          rendered_clients[i] = client_name.replace(/^(.*)(\s\S+,\s*.*)$/, "<b>$1</b>$2");
	      } else {
	          rendered_clients[i] = "<b>" + client_name + "</b>";
	      }
    }
    $("client_names").innerHTML = rendered_clients.join("; ");
    $("upload_client_select").innerHTML = make_clients_selector('upload_client');
    $("upload_animation_client_select").innerHTML = make_clients_selector('upload_client');
    $("edit_client_select").innerHTML = make_clients_selector('edit_client');
}

/* news */

var current_news_item;
var month_names = [ 'January', 'February', 'March', 'April', 'May', 'June',
                    'July', 'August', 'September', 'October', 'November', 'December' ];

function intersection(a, b)
{
    var result = [];
    map(function (value) {
            if (findValue(b, value) != -1) {
                result.push(value);
            }
        }, a);
    return result;
}

function getScrollXY() {
  if (typeof (window.pageYOffset) == 'number') {
      // Netscape compliant
      return { x: window.pageXOffset, y: window.pageYOffset };
  } else if (document.body && ( document.body.scrollLeft || document.body.scrollTop)) {
      // DOM compliant
      return { x: document.body.scrollLeft, y: document.body.scrollTop };
  } else if (document.documentElement && (document.documentElement.scrollLeft || document.documentElement.scrollTop)) {
      // IE standards compliant mode
      return { x: document.documentElement.scrollLeft, y: document.documentElement.scrollTop };
  } else {
      return { x: 0, y: 0 };
  }
}

function permalink(path) {
    return A(
        { href: '/' + path,
          onclick: function () { document.location.href = '#' + path; return false; } },
        'permalink');
}

function make_upload_item(item)
{
    var color = pages[item.category] ? pages[item.category].link_color : '000000';
    var path = item.category + '/' + item.subcategory + '/' + encodeURI(item.name);
    return DIV({ 'class': 'newsentry autonews news_' + item.category },
               A({ href: '/#' + path, onclick: function () { jump_to(path); } },
                 IMG({ src: "/image/" + encodeURI(item.name) + '/cutout-button,,' + color + ',98,98,0,' + item.category,
                             width: 98, height: 98 })),
               DIV(null,
                   H1(null, item.name),
                   item.date, ' by ', item.owner, ' | ',
                   permalink(path),
                   BR(),
                   item.description));
}

function make_news_item(item)
{
    var delete_button = '';

    if (logged_in) {
        delete_button = BUTTON({ 'class': 'delete-button' }, "Delete");
        delete_button.onclick = function () {
            if (confirm('Delete this news article?')) {
                loadJSONDoc('/json-edit-image/' + item.id + '?action=delete')
                    .addCallbacks(reload_news, alert);
            }
            return false;
        };
    }
    var text_div = DIV({ 'class': 'item-text' });
    text_div.innerHTML = item.text;
    return DIV({ 'class': 'newsentry' },
               IMG({ src: "/image/" + encodeURI(item.name) + '/news-article-cutout' }),
               DIV(null,
                   H1(null, item.title),
                   item.date, ' by ', item.owner, ' | ',
                   permalink('news/' + item.name),
                   BR(),
                   text_div,
                   delete_button));
}

function load_news(single_entry, data)
{
    try {
        if (data.items.length > max_news_items) {
            data.items.length = max_news_items;
        }
        replaceChildNodes('newsentries',
                          map(function (item) {
                                  return [ ((item.type == 'upload') ? make_upload_item : make_news_item)(item),
                                           DIV({ 'class': 'news_sep' }) ];
                              }, data.items));
        $('archive-navigation').style.visibility = 'inherit';

        wait_for_images(function () { hide_cue(); $('newsentries').style.visibility = 'inherit'; });

        current_news_item = null;
        if (logged_in) {
            current_news_item = data.items[0];
            if (single_entry) {
                $('news_title').value = current_news_item.title;
                news_editor.setEditorHTML(current_news_item.text);
                $('news_editor').value = current_news_item.text;
                $('edit_news_action').value = 'edit';
                $('edit_image_upload_row').style.visibility = 'hidden';
            } else {
                $('edit_news_form').reset();
                news_editor.setEditorHTML("");
                $('edit_news_action').value = 'create';
                $('edit_image_upload_row').style.visibility = 'inherit';
            }
        }

        display_cms_window();
    }
    catch (e) {
        log('error displaying news: ' + e);
    }
}

function reload_news()
{
    $('edit_news_form').reset();
    news('');
}

function show_news_archive_navigation(year, month) {

    map(function (element) {
            ((element.year == year && (month || element.month)) ? addElementClass : removeElementClass)
                (element, 'active');
        }, $('archive-navigation').childNodes);

    for (i = 1; i <= 12; i++) {
        ((month == i) ? addElementClass : removeElementClass)('archive-navigation', 'm' + i);
    }
}

function news(subpath) {

    if (!subpath.match(/^[0-9\/]*$/)) {

        show_cue();
        $('newsentries').style.visibility = 'hidden';
        loadJSONDoc('/json-news/' + subpath)
            .addCallbacks(partial(load_news, true), alert);

        $('edit_news_form').reset();
        replaceChildNodes('edit_news_title', 'Edit News Item');

    } else {

        var args = subpath.split('/');
        var year = args[0];
        var month = args[1];

        log('news year ' + year + ' month ' + month);

        show_news_archive_navigation(year, month);

        if (month || !year) {
            show_cue();
            $('newsentries').style.visibility = 'hidden';
            loadJSONDoc('/json-news/quickhoney' + (month ? ('?month=' + subpath) : ''))
                .addCallbacks(partial(load_news, false), alert);
        }

        $('edit_news_form').reset();
        replaceChildNodes('edit_news_title', 'Create News Item');
    }
}

function initialize_news_archive(data)
{
    try {
        if (!data.months) {
            alert('no archive data found');
        }
        var currentYear;
        var activeYear = document.location.href.replace(/.*news\/(\d+).*/, "$1");
        replaceChildNodes('archive-navigation',
                          map(function (entry) {
                                  var year = entry[0];
                                  var month = entry[1];
                                  var result = [];
                                  if (year != currentYear) {
                                      currentYear = year;
                                      var link = A({ href: '#news/' + year,
                                                     'class': 'year' + (year == activeYear ? ' active' : '') },
                                                   year, BR());
                                      link.year = year;
                                      result.push(link);
                                  }
                                  var link = A({ href: '#news/' + year + '/' + month,
                                                 'class': 'month ' + 'm' + month + (year == activeYear ? ' active' : '')},
                                               month_names[month - 1], BR());
                                  link.month = month;
                                  link.year = year;
                                  result.push(link);
                                  return result;
                              }, data.months));
    }
    catch (e) {
        log('error while processing archive data: ' + e);
    }
}

/* image database */

var current_directory;
var current_subdirectory;
var current_image;

/* current query result */

var query_result = [];
var query_result_pages = [];
var query_position = 0;
var reload_query = false;

function display_query_result() {

    // make sure that the right elements are displayed
    if (document.show_picture) {
	log('directly jump to ' + document.show_picture);
	for (var i = 0; i < query_result.length; i++) {
	    if (query_result[i].name == document.show_picture) {
		display_image(i);
                document.show_picture = null;
		break;
	    }
	}
        if (document.show_picture) {
            /* not found */
            document.show_picture = null;
            document.location.href = '#' + current_directory + '/' + current_subdirectory;
        }
    } else {
        if (document.show_page) {
            goto_results_page(document.show_page);
            document.show_page = null;
        }
	display_thumbnail_page();
    }

    display_path();
}

function show_cue() {
    $("cue").style.visibility = 'visible';
    $('footer').style.visibility = 'hidden';
}

function hide_cue() {
    $("cue").style.visibility = 'hidden';
    if (application_initialized) {
        $('footer').style.visibility = 'inherit';
    }
}

var db_cache = {};

function process_query_result(key, json_result) {
    log('process query result, key ' + key);

    db_cache[key] = json_result;

    query_result = [];
    query_result_pages = json_result.queryResult;
    var result_position = 0;
    for (var i = 0; i < query_result_pages.length; i++) {
	var page = query_result_pages[i];
	for (var j = 0; j < page.length; j++) {
	    var row = page[j];
	    for (var k = 2; k < row.length; k++) {
		push(query_result, row[k]);
		row[k].position = result_position++;
	    }
	}
    }

    log('got ', query_result.length.toString(), ' images');
    display_query_result();
}

function query_imagedb(directory, subdirectory, force) {
    log('query_imagedb - keywords: ', directory, " ", subdirectory);

    var key = directory + ((subdirectory == "browseall") ? "" : ("/" + subdirectory)) + ((subdirectory == "smallworld") ? "?layout=smallworld" : "");

    query_result = [];
    query_position = 0;
    current_directory = directory;
    current_subdirectory = subdirectory;

    if (db_cache[key] && !force) {

        process_query_result(key, db_cache[key]);

    } else {
        show_cue();
	      loadJSONDoc("/json-image-query/" + key)
            .addCallbacks(partial(process_query_result, key), alert);
    }
}

function do_query()
{
    query_imagedb(current_directory, current_subdirectory, true);
}

/* pages handling */

function Page(link_color, action) {
    this.link_color = link_color;
    this.action = action;
}

var pages = {
    home: new Page('953cfd', function() { current_directory = 'home'; show_home_buttons(); }),
    pixel: new Page('ff00ff',
                    partial(directory, 'pixel')),
    vector: new Page('00ccff',
                     partial(directory, 'vector')),
    pen: new Page('ff0000',
                     partial(directory, 'pen')),
    news: new Page('30be01',
                   news),
    paypal: new Page('ff00ff', show_paypal_page),
    /* no shopping cart for now
    shop: new Page('0054ff',
                   shop),
    cart: new Page('0054ff',
                   show_shopping_cart),
     */
    contact: new Page('ffa200')
};

function display_cms_window() {
    if (logged_in) {
	if (current_directory == "home") {
            show_cms_window();
        } else if (current_directory == "news") {
            show_cms_window("edit_news");
	} else if (current_directory && current_subdirectory) {
	    if (current_image) {
		show_cms_window('edit_form');
	    } else {
		replaceChildNodes("upload_category", current_directory, " / ", current_subdirectory);
		$("upload_form_element").setAttribute("action", "/upload-image/" + current_directory + "/" + current_subdirectory);
		if (current_directory == 'pixel' && current_subdirectory == 'animation') {
		    show_cms_window('upload_animation_form');
		} else {
		    show_cms_window('upload_form');
		}
	    }
	} else if (current_directory == 'pixel') {
	    show_cms_window("pixel_button_upload_form");
	} else {
	    show_cms_window();
	}
    }
}

function show_page(pagename, subpath) {
    document.title = "QuickHoney " + pagename + (subpath ? "/" + subpath : '');

    if (!pages[pagename]) {
        document.location.href = '/';
        return false;
    }

    var page = pages[pagename];

    shop_show_pricetags(pagename, subpath);

    log('show_page ' + pagename + ' subpath ' + subpath + ' current_directory ' + current_directory);

    $('loading').style.display = 'none';
    $('path-and-version').style.visibility = 'visible';

    /* workaround for IE, which does not display the overlapping menu items expectedly */
    map(function (keyword) {
            $('m_' + keyword).style.zIndex = (keyword == pagename) ? 101 : 100;
        }, ['pixel', 'vector', 'pen', 'news', 'contact']); /* , 'shop' disabled */

    // Activate the menu by coloring the menu choices correctly
    $('menu').className = pagename;

    if (document.body.currentPage) {
        removeElementClass(document.body, document.body.currentPage);
    }
    removeElementClass(document.body, "image_browser");
    document.body.currentPage = pagename;

    replaceChildNodes("thumbnails", " ");
    overlay_remove();

    // Update globals
    current_directory = pagename;
    current_subdirectory = null;
    current_image = null;

    // Update path display
    display_path();

    if (page.action) {
        page.action(subpath);
    }

    display_cms_window();

    addElementClass(document.body, pagename);

    return false;
}

function internal_link(path, text) {
    return A({ href: '#' + encodeURI(path) }, text);
}

/* path display */

function display_path() {
    var path = [];

    if (current_directory) {
	if (current_subdirectory) {
	    path.push(internal_link(current_directory, current_directory));
	} else {
	    path.push(current_directory);
	}
    }
    if (current_subdirectory) {
        path.push(' / ');
	if (current_image) {
	    path.push(internal_link(current_directory + "/" + current_subdirectory, current_subdirectory));
	} else {
	    path.push(current_subdirectory);
	}
    }
    if (current_image) {
        path.push(' / ');
	path.push(current_image.name.toLowerCase());
    }

    replaceChildNodes("path", path);
}

/* directory - first level category */

var button_images = [];
var last_category_buttons = {};

function random_button_image(category, subcategory, width, height, cut_category) {

    /* We want the last used button for a category used on the home
     * page to also appear on that categories page next.  For that, we
     * save the previously displayed home image button here.  Not pretty.
     */

    var key = category + '/' + subcategory;
    var image_ids = button_images[key];
    if (image_ids && image_ids.length) {
        var type = image_ids[0];
        var ids = image_ids.slice(1);
        var image_id = ids[Math.floor(Math.random() * ids.length)];
        if (last_category_buttons[category] && findValue(ids, last_category_buttons[category]) != -1) {
            image_id = last_category_buttons[category];
            last_category_buttons[category] = null;
        }
        if (category == 'home') {
            last_category_buttons[subcategory] = image_id;
        }
        if (type == 'buttons') {
            return '/image/' + image_id;
        } else {
            return '/image/' + image_id + '/cutout-button,' + subcategory + ',ffffff,' + width + ',' + height + ',8,' + cut_category;
        }
    } else {
        log('no button image for ' + key + ' found');
    }
}

/* the buttons for the directories are shared, so we need to update
 * them every time we change the directory that is being displayed.
 */

function show_directory_buttons(category) {

    show_cue();

    var i = 0;
    function make_subcategory_button(subcategory, index, height) {
        // The index defines the reveal order
        return A({ href: '#' + category + '/' + subcategory,
                   'class': 'button',
                   id: 'button' + (index || i)
                 },
                 IMG({ 'class': 'button-image',
                       id: 'hidden-button' + i++,
                       src: random_button_image(category, subcategory, 208, height || 208, category),
                       style: 'visibility: hidden' }));
    }

    var buttons;

    var subs = subcategories[category];
    if (category == 'vector') {
        buttons = [ DIV(null,
                        make_subcategory_button(subs[0], 0, 428),
                        make_subcategory_button(subs[1], 1, 428),
                        make_subcategory_button(subs[2], 2)),
                    DIV(null,
                        make_subcategory_button(subs[3], 5)),
                    DIV(null,
                        make_subcategory_button(subs[4], 8),
                        make_subcategory_button(subs[5], 7),
                        make_subcategory_button(subs[6], 6)) ];
        rows = 3;
    } else if (category == 'pen') {
        buttons = [ DIV(null,
                        make_subcategory_button(subs[0], 0, 428),
                        make_subcategory_button(subs[1], 1, 428),
                        make_subcategory_button(subs[2], 2)),
                    DIV(null,
                        make_subcategory_button(subs[3], 5)) ];
        rows = 2;
    } else {
        buttons = [ DIV(null, map(make_subcategory_button, subs.slice(0, 3))) ];
        if (subs.length > 3) {
            buttons.push(DIV(null, map(make_subcategory_button, subs.slice(3, 6))));
        }
        rows = Math.ceil(subs.length / 3);
    }
    setStyle($('directory'), { height: rows * 224 + 'px' });
    replaceChildNodes('directory', buttons);

    wait_for_images(function () { reveal_buttons_nicely(map(partial(operator['add'], 'hidden-button'), seq(0, subs.length))); });
}

function show_home_buttons() {

    show_cue();

    function make_category_button(category) {
        return A({ href: '#' + category, 'class': 'home_button'},
                 IMG({ id: 'home_' + category,
                       'class': 'button-image',
                       width: 318, height: 318,
                       src: random_button_image('home', category, 318, 318, category),
                       style: 'visibility: hidden' }));
    }

    var buttons = [ DIV(null, map(make_category_button, home_buttons.slice(0, 2))),
                    DIV(null, map(make_category_button, home_buttons.slice(2, 4))) ];

    replaceChildNodes('home', buttons);

    wait_for_images(function () { reveal_buttons_nicely(map(partial(operator['add'], 'home_'), home_buttons)); });
}

function stop_revealing_buttons() {
    window.button_reveal_timer && window.button_reveal_timer.cancel();
}

function reveal_buttons_nicely(images, n) {
    if (n == null) {
        stop_revealing_buttons();
        n = 0;
    }
    $(images[n]).style.visibility = 'inherit';
    n++;
    if (n != images.length) {
        window.button_reveal_timer = callLater(0.1, partial(reveal_buttons_nicely, images, n));
    }
}

function load_button_images(callback) {

    loadJSONDoc('/json-buttons'
                + '/home/pixel,vector,news,pen' /* had shop */
                + '/pixel/' + subcategories['pixel'].join(',')
                + '/vector/' + subcategories['vector'].join(',')
                + '/pen/' + subcategories['pen'].join(','))
        .addCallbacks(function(json_result) {
                button_images = json_result.buttons;
                callback && callback();
            }, alert);
}

function directory(directory_name, subpath, image_name) {

    log('directory: ' + directory_name + ' subpath: ' + subpath + ' current_directory: ' + current_directory);

    replaceChildNodes("thumbnails", " ");

    current_directory = directory_name;
    current_subdirectory = null;
    current_image = null;
    $("image_detail").innerHTML = '';

    if (subpath) {
        var components = subpath.split("/");
        if ((components.length > 1) && components[1].match(/^\d+$/)) {
            document.show_page = components[1];
        } else {
            document.show_picture = components[1];
        }
	if (image_name != undefined) {
	    document.show_picture = image_name;	    
	}
        subdirectory(components[0]);
    } else {
        show_directory_buttons(directory_name);
        $('image_browser').className = 'directory page';
    }

    display_cms_window();

    addElementClass(document.body, 'image_browser');

    return false;
}

/* subdirectory - second level category with thumbnail pages */

function subdirectory_button_click(event, dir) {
    subdirectory(dir);
}

function subdirectory(subdirectory, page) {

    log('subdirectory: ', subdirectory, ' page: ', page);

    $('explicit').checked = (subdirectory == 'nudes');

    current_image = null;
    replaceChildNodes("thumbnails", " ");

    query_imagedb(current_directory, subdirectory);

    display_cms_window();

    return false;
}

function goto_results_page(page) {
    query_position = 0;
    for (var page_index = 0; page_index < (page - 1); page_index++) {
	for (var row_index = 0; row_index < query_result_pages[page_index].length; row_index++) {
	    query_position += query_result_pages[page_index][row_index].length - 2;
	}
    }

    display_thumbnail_page();
}

function position_to_page_number(position) {
    var count = 0;
    for (var page_index = 0; page_index < query_result_pages.length; page_index++) {
	for (var row_index = 0; row_index < query_result_pages[page_index].length; row_index++) {
	    for (var image_index = 2; image_index < query_result_pages[page_index][row_index].length; image_index++) {
		if (count++ == position) {
		    return page_index + 1;
		}
	    }
	}
    }
    return 1;
}

function pages_navbar_start_index() {
    return Math.max(0, query_position - 2);
}

function pages_navbar_end_index() {
    if (pages_navbar_start_index() > 0) {
	return Math.min(query_result.length - 1, query_position + 2);
    } else {
	return Math.min(query_result.length, 4);
    }
}

function make_pages_navbar() {
    var result_links = [];
    var current_page = position_to_page_number(query_position);

    if (query_result_pages.length > 0) {
	if (current_page > 1) {
	    push(result_links,
                 internal_link(current_directory + '/' + current_subdirectory + '/' + (current_page - 1),
                               "<<"));
	} else {
	    push(result_links, "<<");
	}
        push(result_links, " ");
	var last_page = position_to_page_number(query_result.length - 1);
	if (current_page < last_page) {
	    push(result_links,
                 internal_link(current_directory + '/' + current_subdirectory + '/' + (current_page + 1).toString(),
                               ">>"));
	} else {
	    push(result_links, ">>");
	}
	replaceChildNodes("page_navbar", result_links);

	result_links = [];

	var page_count = query_result_pages.length;
	for (var page_number = 1; page_number < (page_count + 1); page_number++) {
            if (page_number > 1) {
                push(result_links, " | ");
            }
	    if (page_count > 5) {
		if ((page_number > 2)
		    && ((current_page - 1) > page_number)) {
		    push(result_links, "... | ");
		    page_number = current_page - 1;
		} else if ((page_number < (page_count - 2))
			   && ((current_page + 1) < page_number)) {
		    push(result_links, "... | ");
		    page_number = page_count - 2;
		}
	    }
	    if (page_number == current_page) {
		push(result_links, page_number.toString());
	    } else {
		push(result_links,
                     internal_link(current_directory + '/' + current_subdirectory + '/' + page_number.toString(),
                                   page_number));
	    }
	}

	replaceChildNodes("result_page_count", "pages ", result_links);

    } else {
	replaceChildNodes("page_navbar", "no images in this category");
	replaceChildNodes("result_page_count");
    }
}

function reveal_thumbnails() {
    var waiting = 0;
    map(function (image) {
            if (!image.complete) {
                waiting++;
            } else {
                image.style.visibility = 'inherit';
            }
        }, getElementsByTagAndClassName('img', null, 'thumbnails'));

    if (waiting == 0) {
        hide_cue();
        window.reveal_thumbnails_timer = null;
    } else {
        window.reveal_thumbnails_timer = callLater(.2, reveal_thumbnails);
    }
}

function display_thumbnail_page() {
    var current_page_index = position_to_page_number(query_position) - 1;

    $('image_browser').className = 'results page';
    make_pages_navbar();
    overlay_remove();

    var page = query_result_pages[current_page_index];
    var thumbnail_nodes = [];

    for (var row_index = 0; row_index < page.length; row_index++) {
	var row = page[row_index];
	var cell_width = row[0];
	var cell_height = row[1];
	for (var image_index = 2; image_index < row.length; image_index++) {
	    var image = row[image_index];
            var imageElement = IMG({ 'class': 'inherited_image',
                                     width: cell_width,
                                     height: cell_height});
            imageElement.src = '/image/' + encodeURI(image.name) + '/cell,ffffff,' + cell_width + ',' + cell_height + ',8';
	    var priceTag = null;
	    if ((image.shop_file != undefined) && (image.shop_price != undefined) && (image.shop_active)) {
		var border_width = 8;
		var img_ratio = Math.max(image.width / (cell_width - (2 * border_width)),
					 image.height / (cell_height - (2 * border_width)));
		var tb_height = Math.min(image.height, Math.round(image.height / img_ratio));
		var y_offset = Math.round((cell_height - tb_height) / 2);
		var tb_width = Math.min(image.width, Math.round(image.width / img_ratio));
		var x_offset = Math.round((cell_width - tb_width) / 2);

		/* XXX micro-pricetags */
		var right_offset = 5 + x_offset;
		var top_offset = -cell_height  + 13 + y_offset + 3;
		var bottom_offset = 7;
		/*
		log("img_ratio " + img_ratio);
		log("border_width " + border_width);
		log("cell_height " + cell_height + " tb_height " + tb_height + " y_offset " + y_offset);
		log("cell_width " + cell_width + " tb_width " + tb_width + " y_offset " + x_offset);
		log("right " + right_offset + " top " + top_offset);
		 */
		
		priceTag = A({ href: window.location.hash},
			     IMG({'class': 'image_pricetag_micro',
				  'src': "/image/pricetag-micro",
				  'id': 'pricetag-micro-' + image.id,
				  'onclick': "init_shop_overlay(query_result_pages[" +
				  current_page_index +"][" + row_index + "][" + image_index + "])",
				  'style': "bottom: " + bottom_offset + "px; right: " + right_offset + "px"}));
	    }

	    var imageLink = A({ href: '#' + current_directory + '/' + current_subdirectory + '/' + encodeURI(image.name) },

                              imageElement
			     );
	    var imageSpan = SPAN({'style': "position:relative"}, imageLink, priceTag);
	    thumbnail_nodes.push(imageSpan);
        }
        thumbnail_nodes.push(BR());
    }

    replaceChildNodes("thumbnails", thumbnail_nodes);

    window.reveal_thumbnails_timer && window.reveal_thumbnails_timer.cancel();
    show_cue();
    reveal_thumbnails();
}

/* image browser - displaying one image */

function make_images_navbar() {
    var result_links = [];

    if (query_position > 0) {
	push(result_links,
             internal_link(current_directory + '/' + current_subdirectory + '/' + query_result[query_position - 1].name,
                           "<<"));
    } else {
	push(result_links, "<<");
    }
    push(result_links, " ");
    if (query_position < (query_result.length - 1)) {
	push(result_links,
             internal_link(current_directory + '/' + current_subdirectory + '/' + query_result[query_position + 1].name,
                           ">>"));
    } else {
	push(result_links, ">>");
    }
    $('back_to_results_link').href = '#' + current_directory + '/' + current_subdirectory + '/' + position_to_page_number(query_position);

    replaceChildNodes("image_navbar", result_links);
    replaceChildNodes("result_image_count", "result " + (query_position + 1) + " of " + query_result.length);
}

function display_image(index) {

    $('image_browser').className = 'browse page';

    index = index - 0; // ensure integer
    query_position = index;
    current_image = query_result[index];

    log('display_image index ' + index);

    display_current_image();
}

function display_current_image() {
    overlay_remove();
    display_path();
    make_images_navbar();
    make_image_action_buttons();

    var ratio = 1 / Math.max(current_image.width / 648, current_image.height / 648);
    var imageproc_ops = "";

    if (ratio > 1 && current_directory == 'pixel') {
	ratio = Math.floor(ratio);
    }

    var display_width = Math.round(current_image.width * ratio);
    var display_height = Math.round(current_image.height * ratio);

    if (ratio >= 2) {
	imageproc_ops = "/double," + ratio;
    }

    current_image.ratio = ratio;

    var percent = Math.round(100 * ratio);

    log("calculated scaling: ratio " + (Math.round(ratio * 100) / 100) + " percent " + percent + " width " + display_width + " height " + display_height);

    var may_enlarge = (current_directory == 'pixel' && (current_image.ratio < 1));

    var image_info = [];

    var link = current_directory + '/' + current_subdirectory + '/' + encodeURI(query_result[query_position].name);

    replaceChildNodes("metadata");
    if (current_image.description) {
        appendChildNodes("metadata", current_image.description, BR(), BR());
    }
    appendChildNodes("metadata", "Image name: ", current_image.name, " ", permalink(link), BR());
    if (current_image.client != "") {
	appendChildNodes("metadata", "Client: ", current_image.client, BR());
    }

    appendChildNodes("metadata", current_image.width + "x" + current_image.height
                     + "px | " + percent + "%");

    if (may_enlarge) {
	replaceChildNodes("full_click", A({ href: '#' + link, onclick: enlarge },
                                          "full size"));
    } else {
	replaceChildNodes("full_click");
    }

    if (current_directory == 'pixel' && current_subdirectory == 'animation') {
        var content;
        var animation_type = current_image.animation_type;
        if (animation_type == "application/x-director" && !detectDirector()) {
            content = "<p>To display this content, the <a href='http://www.adobe.com/shockwave/download/' target='_new'>Adobe Shockwave plugin</a> is required.</p>";
        } else if (animation_type == "application/x-shockwave-flash" && !detectFlash()) {
            content = "<p>To display this content, the <a href='http://www.adobe.com/products/flashplayer/' target='_new'>Adobe Flash Player</a> is required.</p>";
        } else if (animation_type == "video/quicktime") {
            if (!detectQuickTime()) {
                content = "<p>To display this content, the <a href='http://www.apple.com/quicktime/download/' target='_new'>Apple Quicktime plugin</a> is required.</p>";
            } else {
                content = "<div style='height: 81px'> </div>"
                    + _QTGenerate("QT_GenerateOBJECTText_XHTML", true, ['/animation/' + current_image.id, '648', '486', '',
                                                                        'scale', 'aspect',
                                                                        'showlogo', 'false',
                                                                        'bgcolor', 'WHITE']);
            }
        } else {
            content
                = '<object width="648" height="648" classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,0,0">'
                + ' <param name="movie" value="/animation/' + current_image.id + '" />'
                + ' <embed src="/animation/' + current_image.id + '" width="648" height="648">'
                + ' </embed>'
                + '</object>';
        }

        $("image_detail").innerHTML = content;

    } else {
        show_cue();

	var top_padding = Math.round(648 - display_height) / 2;
	var left_padding = Math.round(648 - display_width) / 2;
        var img = IMG({ width: display_width,
                        height: display_height,
                        style: 'visibility: hidden',
                        src: '/image/' + encodeURI(current_image.name) + imageproc_ops });

	var priceTag = null;
	if ((current_image.shop_file != undefined) && (current_image.shop_price != undefined)) {
	    priceTag = A({href: window.location.hash
			 },
			 IMG({'class': 'image_pricetag',
			      'src': "/image/pricetag-small-" + current_image.shop_price,
			      'id': "pricetag-small",
			      'style': "right: " + (648 - display_width) / 2 + "px;",
			      'onclick': "init_shop_overlay(current_image)"}));
	}
	
	var divNode = DIV({ style: 'position: relative; margin-top: ' + top_padding + 'px; margin-left: ' + left_padding + 'px' },
                              may_enlarge ? A({ onclick: 'enlarge()', href: '#' }, img) : img,
			 priceTag);
        replaceChildNodes('image_detail', divNode);
        wait_for_images(function () { img.style.visibility = 'inherit'; });
    }

    if (logged_in) {
	$("edit_client").value = current_image.client;
        $("edit_keywords").value = current_image.spider_keywords || "";
        $("edit_description").value = current_image.description || "";

        map(function(keyword) {
                $('edit_' + keyword).checked = current_image.keywords[keyword] ? true : false;
            }, ['explicit']);
	show_cms_window("edit_form");
    }
}

function hey_send()
{
    var d = doXHR("/digg-image/" + current_image.id,
                  { method: 'POST',
                    headers: {"Content-Type":"application/x-www-form-urlencoded"},
                    sendContent: queryString({ from: $('hey_from').value, to: $('hey_to').value, text: $('hey_text').value }) })
        .addCallback(overlay_remove);
    make_overlay('send-comment', 'Sending your comment', 300);
    return false;
}

function enlarge() {
    var detail_window = open("/static/image-full.html#" + encodeURI(current_image.name),
			     "_new",
			     "width=" + current_image.width + ",height=" + current_image.height + ",status=no,toolbar=no,menubar=no");

    detail_window.focus();
}

function upload_news (target) {
    var edit_mode = $('edit_news_action').value == 'edit';

    if (!edit_mode && $('news_file').value == '') {
        alert('Please upload an image!');
        return false;
    }
    if ($('news_title').value == '') {
        alert('Please enter a title for the news item!');
        $('news_title').focus();
        return false;
    }
    news_editor.saveHTML();     // update editor's value from html

    if (edit_mode) {
        show_cms_window('saving_edits_form'); // hide edit window until server replies
        submit_json('/json-edit-news/' + current_news_item.id + '?action=edit', 'edit_news_form', news_edited);
        return false;
    } else {
        do_upload(target);
        return true;
    }
}
/* open the upload window */

function do_upload(target) {
    log('do_upload - target is ', target);

    var upload_window = window.open('', target, "width=500,height=300,status=no,toolbar=no,menubar=no,resizable=no,scrollbars=no");
    upload_window.focus();

    return true;
}

function do_tweet(form) {

    form.action = "/tweet-image/" + window.location.hash.replace(/.*\//, '');
    log('action: ', form.action);

    var tweet_window = window.open('', form.target, "width=500,height=300,status=no,toolbar=no,menubar=no,resizable=no,scrollbars=no");
    tweet_window.focus();

    return true;
}

/* open the button upload window */

function do_button_upload(target) {
    log('do_button_upload - target is ', target);

    var upload_window = window.open('', target, "width=500,height=300,status=no,toolbar=no,menubar=no,resizable=no,scrollbars=no");
    upload_window.focus();

    return true;
}

/* open the upload window for home images */

function do_quickhoney(target) {

    return false;
}

/* main initialization routine */

function safari_compatibility_hack() {
    // for safari, we change the style sheet so that images are not hidden during load
    log('changing stylesheet for safari');
    for (var i = 0; i < document.styleSheets.length; i++) {
	      var rules = document.styleSheets[i][document.all ? 'rules' : 'cssRules'];
	      for (var j = 0; j < rules.length; j++) {
	          var rule = rules[j];
	          var ruleText = rule.selectorText.toLowerCase();
	          if (ruleText.indexOf('img.inherited_image') != -1) {
		            rule.style['visibility'] = 'visible';
	          }
	      }
    }
}

function init_application() {
    log('init_application');
    if (navigator.userAgent.indexOf("Safari") != -1) {
	      safari = true;
	      safari_compatibility_hack();
    }

    function log_error(url, error) {
        log('fetching ' + url + ' failed: ' + error);
    }

    function load_json(url, handler) {
        loadJSONDoc(url).addCallbacks(handler, partial(log_error, url));
    }

    load_json("/json-login", login_status);
    load_json("/json-clients", set_clients);
    load_json('/json-news-archive/quickhoney', initialize_news_archive);

    init_shop();

    var path = 'home';
    if (document.location.pathname != '/') {
        path = document.location.pathname.substr(1);
    } else if (document.location.href.match(/#./)) {
        path = document.location.href.substr(document.location.href.indexOf("#") + 1);
    }
    document.location.href = "/#" + path;

    poll_path();

    application_initialized = true;
    log('init_application done');
}

function button_images_loaded() {
    log('button_images_loaded');
    wait_for_images(init_application);
}

function init() {
    log('init');
    show_cue();

    load_button_images(button_images_loaded);
}

function jump_to(path) {
    document.current_path = path;

    log('jump_to - path is ', path);

    path = decodeURI(path.replace(/[?#].*/, ""));
    var components = path.split("/");
    show_page(components[0], path.replace(/.*?($|\/(.*))/, "$2"));
}

function poll_path() {
    var url_path = (document.location.href + "#").split("#")[1];
    if (url_path && (url_path != document.current_path)) {
        if ((typeof pageTracker != "undefined") && pageTracker) {
            try {
                pageTracker._trackPageview(document.location.href.replace("#", "/").replace(/^[^\/]+:\/\/[^\/]+/, ""));
            }
            catch (e) {
                log("tracking failed: " + e);
            }
        }
        jump_to(url_path);
    }

    setTimeout("poll_path()", 250);
}

/* old cms support */

function check(button, checkboxname, b) {
  checkboxes = button.form[checkboxname];
  for (i = 0; i < checkboxes.length; i++) {
    checkboxes[i].checked = b;
  }
}

function overlay_remove()
{
    replaceChildNodes('overlay');
    $('overlay').style.visibility = 'hidden';
    return false;
}

/* make_overlay() opens an overlay window with the given title.
 * Additional arguments passed are assumed to be DOM child nodes and
 * are added to the overlay window.
 */

function fade_out_page(to, callback) {
    to = to || 0.3;
    var duration = 0.7;
    fade('menu', {to: to,
		  duration: duration});
    fade('path-and-version', {to: to,
			     duration: duration});
    fade('image_browser', {to: to,
			  duration: duration});
    fade('footer', {to: to,
		    duration: duration});
    setTimeout(callback, duration * 1000);
}

function make_overlay_content(overlay, options) {
    log("options " + JSON.stringify(options));
    var id = options.id;
    var cssClass = options.cssClass || '';
    var title = options.title;
    var width = options.width;
    var closeID = 'close' + id;
    var buttonColor = options.color || pages[current_directory].link_color;
    var invertColor = options.invertColor || false;
    var colorString = (!options.invertColor ? "000000," + buttonColor :
		      "ffffff," + buttonColor + ",000000,ffffff");

    overlay.style.visibility = 'hidden';
    overlay.style.top = options.top || '144px';
    overlay.style.left = options.left || '36px';
    overlay.className = cssClass + ' ' + current_directory + " overlay";

    var inner = DIV({ 'class': 'inner' },
                    H1(null, title),
                    IMG({ src: '/image/overlay-close/color,' + colorString,
                          id: closeID, 'class': 'close',
			  width: 13, height: 13}),
                    BR());
    replaceChildNodes(overlay,
                      DIV({ 'class': 'ydsf' },
                          inner));
    overlay.style.width = width + 'px';
//    $(closeID).style.left = (width - 23) + 'px';
    $(closeID).style.right = '13px';
    $(closeID).onclick = function () {
	overlay.style.visibility = 'hidden';
	replaceChildNodes(overlay);
	if (options.fade) {
	    fade_out_page(1.0);
	}
	if (options.callback != undefined) {
	    options.callback();
	}
    };
    var elements = [];
    for (var i = 2; i < arguments.length; i++) {
        elements.push(arguments[i]);
    }
    appendChildNodes(inner, DIV({id: id}, elements));

    /* wait for fade, fade speed XXX */
    var showOverlay = function () {
	function show() {
	    if (options.onShow) {
		options.onShow();
	    }
	    overlay.style.visibility = 'inherit';
	}
	show();
    };


    var fadeOut = function () {
	if (options.fade) {
	    fade_out_page(0.3, showOverlay);
	} else {
	    showOverlay();
	}
    };
    
    if (options.waitForImages) {
	wait_for_images(fadeOut);
    } else {
	fadeOut();
    }

    return overlay;
}

function make_overlay(id, title, width) {
    log('make_overlay ' + id);
    var overlay = $('overlay');
    var args = [];
    for (var i = 3; i < arguments.length; i++) {
	args[i-3] = arguments[i];	
    }
    partial(make_overlay_content, overlay, { id: id, title: title, width: width}).apply(this, args);
}

function make_post_mail_form() {
    var subject = 'Send image ' + current_image.name + ' by email';
    make_overlay('hey', 'Comment', 320,
                 FORM({ method: 'POST', action: '#' },
                      'your email',
                      BR(),
                      INPUT({ type: 'text', id: 'hey_from', value: '' }),
                      BR(),
                      'receipient email',
                      BR(),
                      INPUT({ type: 'text', id: 'hey_to', value: '' }),
                      BR(),
                      BR(),
                      'comment',
                      BR(),
                      TEXTAREA({ name: 'text', id: 'hey_text', rows: 4, columns: 40 }, ''),
                      BR(),
                      INPUT({ type: 'submit', id: 'hey_send', value: 'Send' })));
    $('hey_send').onclick = hey_send;
}

function make_comment_form() {
    var subject = 'Comment on ' + current_image.name;
    make_overlay('hey', 'Comment', 320,
                 FORM({ method: 'POST', action: '#' },
                      'email',
                      BR(),
                      INPUT({ type: 'text', id: 'hey_from', value: '' }),
                      INPUT({ type: 'hidden', id: 'hey_to', value: '' }),
                      BR(),
                      BR(),
                      'comment',
                      BR(),
                      TEXTAREA({ name: 'text', id: 'hey_text', rows: 4, columns: 40 }, ''),
                      BR(),
                      INPUT({ type: 'submit', id: 'hey_send', value: 'Send' })));
    $('hey_send').onclick = hey_send;
}

function compute_title() {
    var title = "QuickHoney";
    if (current_directory) {
        title += ': ' + current_directory;
        if (current_subdirectory) {
            title += '/' + current_subdirectory;
            if (current_image) {
                title += ': ' + current_image.name;
            }
        }
    }
    return title;
}

var social_bookmark_sites = [
                        { icon: 'blinkbits.gif', width: 16, height: 16,
                          name: 'Blinkbits',
                          url: 'http://www.blinkbits.com'},
                        { icon: 'blinklists.gif', width: 18, height: 18,
                          name: 'Blinklist',
                          url: 'http://www.blinklist.com',
                          action: 'http://www.blinklist.com/index.php?Action=Blink/addblink.php&Description=&Url=%URL%&Title=%TITLE%',
                          window: 'location=yes,links=no,scrollbars=no,toolbar=no,width=550,height=380'},
                        { icon: 'blogdigger.gif', width: 16, height: 16,
                          name: 'Blogdigger',
                          url: 'http://www.blogdigger.com'},
                        { icon: 'blogmarks.gif', width: 16, height: 16,
                          name: 'Blogmarks',
                          url: 'http://blogmarks.net'/* ,
                                                        action: 'http://blogmarks.net/my/new.php?mini=1&title=%TITLE%&url=%URL%' */},
                        { icon: 'buzznet.gif', width: 16, height: 16,
                          name: 'Buzznet',
                          url: 'http://www.buzznet.com'},
                        { icon: 'citeulike.gif', width: 16, height: 16,
                          name: 'Citeulike',
                          url: 'http://www.citeulike.org'},
                        { icon: 'comments.gif', width: 16, height: 16,
                          name: 'Co.mments',
                          url: 'http://co.mments.com'},
                        { icon: 'connotea.gif', width: 16, height: 16,
                          name: 'Connotea',
                          url: 'http://www.connotea.org'},
                        { icon: 'delicious.png', width: 16, height: 16,
                          name: 'Delicious',
                          url: 'http://del.icio.us',
                          action: 'http://delicious.com/save?url=%URL%&title=%TITLE%&v=5&noui=1&jump=doclose',
                          window: 'location=yes,links=no,scrollbars=no,toolbar=no,width=550,height=550'
                        },
                        { icon: 'digg.png', width: 16, height: 14,
                          name: 'Digg',
                          url: 'http://www.digg.com',
                          action: 'http://digg.com/submit?phase=2&url=%IMAGEURL%&title=%TITLE%&media=image&topic=design&thumbnails=0'},
                        { icon: 'fark.gif', width: 15, height: 18,
                          name: 'Fark',
                          url: 'http://www.fark.com'},
                        { icon: 'feedmelinks.gif', width: 20, height: 19,
                          name: 'Feedmelinks',
                          url: 'http://feedmelinks.com'},
                        { icon: 'feedster-icon.gif', width: 16, height: 16,
                          name: 'Feedster',
                          url: 'http://feedster.com'},
                        { icon: 'findory.gif', width: 16, height: 16,
                          name: 'Findory',
                          url: 'http://findory.com'},
                        { icon: 'flickr.gif', width: 16, height: 16,
                          name: 'Flickr',
                          url: 'http://www.flickr.com'},
                        { icon: 'furl.png', width: 16, height: 17,
                          name: 'Furl',
                          url: 'http://www.furl.net',
                          action: 'http://www.furl.net/storeIt.jsp?u=%URL%&t=%TITLE%',
                          window: 'location=yes,links=no,scrollbars=no,toolbar=no,width=570,height=700'},
                        { icon: 'google.gif', width: 16, height: 16,
                          name: 'Google',
                          url: 'http://www.google.com',
                          action: 'http://www.google.com/bookmarks/mark?op=add&bkmk=%URL%&title=%TITLE%' },
                        { icon: 'icerocket.gif', width: 16, height: 16,
                          name: 'Ice Rocket',
                          url: 'www.icerocket.com'},
                        { icon: 'linkagogo.gif', width: 16, height: 16,
                          name: 'Linkagogo',
                          url: 'http://www.linkagogo.com'},
                        { icon: 'magnolia.gif', width: 16, height: 16,
                          name: 'Magnolia',
                          url: 'http://ma.gnolia.com'},
                        { icon: 'mister-wong.gif', width: 16, height: 16,
                          name: 'Mister Wong',
                          action: 'http://www.mister-wong.de/index.php?action=addurl&bm_url=%URL%&bm_description=%TITLE%' },
                        { icon: 'netscape.gif', width: 16, height: 15,
                          name: 'Netscape',
                          url: 'http://www.netscape.com'},
                        { icon: 'newsvine.gif', width: 16, height: 16,
                          name: 'Newsvine',
                          url: 'http://www.newsvine.com'},
                        { icon: 'nowpublic.gif', width: 15, height: 11,
                          name: 'NowPublic',
                          url: 'http://www.nowpublic.com'},
                        { icon: 'onlywire.gif', width: 20, height: 16,
                          name: 'OnlyWire',
                          url: 'http://www.onlywire.com'},
                        { icon: 'paypal.gif', width: 16, height: 16,
                          name: 'PayPal',
                          url: 'http://www.paypal.com'},
                        { icon: 'reddit.gif', width: 18, height: 18,
                          name: 'Reddit',
                          url: 'http://reddit.com',
                          action: 'http://reddit.com/submit?url=%URL%&title=%TITLE%',
                          window: 'location=yes,links=no,scrollbars=no,toolbar=no,width=640,height=670'},
                        { icon: 'rocketnews.gif', width: 16, height: 16,
                          name: 'RocketNews',
                          url: 'http://www.rocketnews.com'},
                        { icon: 'scuttle.gif', width: 16, height: 16,
                          name: 'Scuttle',
                          url: 'http://scuttle.org'},
                        { icon: 'scoopt.gif', width: 15, height: 11,
                          name: 'Scoopt',
                          url: 'http://www.scoopt.com/words'},
                        { icon: 'shadows.gif', width: 16, height: 16,
                          name: 'Shadows',
                          url: 'http://www.shadows.com'},
                        { icon: 'simpy.gif', width: 16, height: 16,
                          name: 'Simpy',
                          url: 'http://www.simpy.com',
                          action: 'http://www.simpy.com/simpy/LinkAdd.do?href=%URL%&title=%TITLE%'},
                        { icon: 'slashdot.gif', width: 17, height: 17,
                          name: 'Slashdot',
                          url: 'http://slashdot.org',
                          action: 'http://slashdot.org/slashdot-it.pl?op=basic&url=%URL%'},
                        { icon: 'smarking.gif', width: 16, height: 16,
                          name: 'Smarking',
                          url: 'http://www.smarking.com'},
                        { icon: 'sphere.gif', width: 16, height: 16,
                          name: 'Sphere',
                          url: 'http://www.sphere.com'},
                        { icon: 'spurl.gif', width: 16, height: 16,
                          name: 'Spurl',
                          url: 'http://www.spurl.net'},
                        { icon: 'stumbleupon.gif', width: 16, height: 16,
                          name: 'StumbleUpon',
                          url: 'http://www.stumbleupon.com',
                          action: 'http://www.stumbleupon.com/submit?url=%URL%&title=%TITLE%'},
                        { icon: 'technorati.gif', width: 14, height: 17,
                          name: 'Technorati',
                          url: 'http://www.technorati.com'},
                        { icon: 'webride.gif', width: 16, height: 16,
                          name: 'Webride',
                          url: 'http://webride.org'},
                        { icon: 'wists.gif', width: 16, height: 16,
                          name: 'Wists',
                          url: 'http://www.wists.com'},
                        { icon: 'yahoo.png', width: 16, height: 16,
                          name: 'Yahoo',
                          url: 'http://www.yahoo.com',
                          action: 'http://beta.bookmarks.yahoo.com/toolbar/savebm?t=%TITLE%&u=%URL%',
                          window: 'location=yes,links=no,scrollbars=no,toolbar=no,width=400,height=300'} ];


function current_url()
{
    return window.location.href.replace("#", "");
}

function current_image_url()
{
    return document.location.protocol
        + '//' + document.location.host + (document.location.port ? ':' + document.location.port : '')
        + '/image/' + current_image.name;
}

function make_shout_form() {

    function bookmark_link(image, action) {
        return A({ href: '#' + document.current_path, onclick: action },
                 IMG({ src: '/image/' + image }));
    }

    function submit_bookmark(site) {
        var url = site.action;
        url = url.replace("%IMAGEURL%", encodeURIComponent(current_image_url()));
        url = url.replace("%URL%", encodeURIComponent(current_url()));
        url = url.replace("%TITLE%", encodeURIComponent(compute_title()));
        window.open(url,
                    'submit-bookmark',
                    site.window);
    }

    make_overlay('shoot', 'Shout', 320,
                 DIV(null,
                     map(function (site) {
                             var img = IMG({ src: '/static/icons/' + site.icon,
                                             width: site.width, height: site.height,
                                             title: site.name });
                             img.onclick = partial(submit_bookmark, site);
                             return img;
                         }, filter(function (site) { return site.action; }, social_bookmark_sites))));
}

function make_ipod_image() {
    window.open('/image/'
                + encodeURI(current_image.name)
                + '/cell,,'
                + ((current_image.width < current_image.height) ? '320,480' : '480,320')
                + '/download,' + encodeURI(current_image.name) + '.jpg');
}

NOTICE = partial(SPAN, { 'class': 'notice' });
PRICE = partial(SPAN, { 'class': 'price' });
SPACER = partial(DIV, { 'class': 'spacer' });
ARTWORK_NAME = partial(SPAN, { 'class': 'artwork-name' });
BOLD = partial(SPAN, { 'class': 'bold' });
ITALIC = partial(SPAN, { 'class': 'italic' });

function recolored_image_path(name)
{
    return '/image/' + name + '/color,000000,' + pages[current_directory].link_color;
}

function hidden_IMG(obj) {
    obj.style = 'visibility: hidden';
    return IMG(obj);
}

var action_button_titles = { 'buy-file': 'File',
                             'buy-print': 'Art Print',
                             'buy-t-shirt': 'T-Shirt' };

function make_image_action_buttons()
{
    var buttons = [];

    $('image_action_buttons').style.visibility = 'hidden';

    var groups = [];
    var buyable = intersection(keys(current_image.keywords), keys(action_button_titles)).length > 0;

    function make_image_action_button(keyword, title, action)
    {
        var item = LI({ style: 'background-image: url(' + recolored_image_path(keyword) + ')' },
                      A({ href: '#' }, title));
        item.onclick = function() { action(); return false; };
        return item;
    }

    function make_buy_button(keyword) {
        return make_image_action_button(keyword,
                                        action_button_titles[keyword],
                                        partial(make_buy_form, keyword));
    }

    if (buyable) {
        groups.push(DIV({ 'class': 'button-section' },
                        hidden_IMG({ src: recolored_image_path('buy'), 'class': 'flag', width: 37, height: 16}),
                        DIV({ 'class': 'button-section-items' },
                            UL(null,
                               map(make_buy_button, keys(current_image.keywords))))));
    }

    groups.push(DIV({ 'class': 'button-section' },
                    hidden_IMG({ src: recolored_image_path('hey'), 'class': 'flag', width: 37, height: 16}),
                    DIV({ 'class': 'button-section-items last' },
                        UL(null,
                           make_image_action_button('ipod', 'iPhone', make_ipod_image),
                           make_image_action_button('comment', 'Comment', make_comment_form),
                           make_image_action_button('shoot', 'Shoot', make_shout_form)))));

    replaceChildNodes('image_action_buttons', groups);
    log('image_action_buttons width: ', getElementDimensions('image_action_buttons').w);

    var animator = new YAHOO.util.Anim('image_action_buttons', {}, 0.3,
                                       YAHOO.util.Easing.easeBoth);
    $('image_action_buttons').onmouseover = function () {
        animator.stop();
        animator.attributes = { width: { to: 132 }, left: { to: 553 } };
        animator.animate();
    };
    $('image_action_buttons').onmouseout = function () {
        animator.stop();
        animator.attributes = { width: { to: 61 }, left: { to: 623 } };
        animator.animate();
    };

    wait_for_images(function () { $('image_action_buttons').style.visibility = 'inherit'; });
}

