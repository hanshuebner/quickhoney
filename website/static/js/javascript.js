// Copyright 2005-2017 Hans Huebner, hans.huebner@gmail.com
// All rights reserved.

// You will find that this program is kind of ugly, full of global
// variables and overall badly structured.  To my excuse, I want to
// say that when I started writing it back in 2005, JavaScript was not
// generally considered to be a real programming language.  Also,
// maybe surprisingly, this system was substantially changed and
// updated a few times over the years, which despite all the ugliness
// worked well.

/* safari global variable - used to trigger some compatibility hacks */

var safari = false;

/* configuration */

var max_news_items = 50;        /* maximum number of news items to display */

/* home button definitions */

var home_buttons = ['pixel', 'vector', 'pen', 'news'];

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
    
    var elements = $("cms").childNodes;

    if (logged_in) {
        
	for (var i = 0; i < elements.length; i++) {
	    if (elements[i].id) {
		elements[i].style.visibility = (elements[i].id == name) ? "visible" : "hidden";
	    }
	}
        
	$("login_status").style.visibility = 'visible';
	
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
    current_image.center_x = $("edit_center_x").value;
    current_image.center_y = $("edit_center_y").value;

    show_cms_window('saving_edits_form'); // hide edit window until server replies

    console.log('saving', current_image);
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
                   H1(null,
                      A({ href: '/#' + path, onclick: function () { jump_to(path); } },
                        item.name)),
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
    var path = 'news/' + encodeURI(item.name);
    text_div.innerHTML = item.text;
    return DIV({ 'class': 'newsentry' },
               A({ href: '/#' + path, onclick: function () { jump_to(path); } },
                 IMG({ src: "/image/" + encodeURI(item.name) })),
               DIV(null,
                   A({ href: '/#' + path, onclick: function () { jump_to(path); } },
                     H1(null, item.title)),
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
        $('tweet_news_form_element').style.visibility = 'inherit';

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
        $('tweet_news_form_element').style.visibility = 'hidden';
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
	display_thumbnail_page();
    }
}

function show_cue() {
    $("cue").style.visibility = 'visible';
}

function hide_cue() {
    $("cue").style.visibility = 'hidden';
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

    log('got', query_result.length, 'images');
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
	loadJSONDoc("/json-layout-query/" + key)
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

    log('show_page ' + pagename + ' subpath ' + subpath + ' current_directory ' + current_directory);

    $('loading').style.display = 'none';

    /* workaround for IE, which does not display the overlapping menu items expectedly */
    map(function (keyword) {
            $('m_' + keyword).style.zIndex = (keyword == pagename) ? 101 : 100;
        }, ['pixel', 'vector', 'pen', 'news', 'contact']);

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

/* path display (not currently used) */

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
                       src: random_button_image(category, subcategory, 276, height || 276, category),
                       style: 'visibility: hidden' }));
    }

    var buttons;

    var subs = subcategories[category];
    if (category == 'vector') {
        buttons = [ DIV(null,
                        make_subcategory_button(subs[0], 0, 568),
                        make_subcategory_button(subs[1], 1, 568),
                        make_subcategory_button(subs[2], 2)),
                    DIV(null,
                        make_subcategory_button(subs[3], 5)),
                    DIV(null,
                        make_subcategory_button(subs[4], 8),
                        make_subcategory_button(subs[5], 7),
                        make_subcategory_button(subs[6], 6)) ];
        rows = 3;
    } else if (category == 'pen' || category == 'pixel') {
        buttons = [ DIV(null,
                        make_subcategory_button(subs[0], 0, 568),
                        make_subcategory_button(subs[1], 1, 568),
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
    setStyle($('directory'), { height: (100 + rows * 279) + 'px' });
    replaceChildNodes('directory', buttons);

    wait_for_images(function () { reveal_buttons_nicely(map(partial(operator['add'], 'hidden-button'), seq(0, subs.length))); });
}

function show_home_buttons() {

    show_cue();

    function make_category_button(category) {
        return A({ href: '#' + category, 'class': 'home_button'},
                 IMG({ id: 'home_' + category,
                       'class': 'button-image',
                       width: 414, height: 414,
                       src: random_button_image('home', category, 414, 414, category),
                       style: 'visibility: hidden' }));
    }

    var buttons = [ DIV(null, map(make_category_button, home_buttons.slice(0, 2))),
                    DIV(null, map(make_category_button, home_buttons.slice(2, 4))) ];

    replaceChildNodes('home',
                      buttons,
                      DIV({ class: 'footer' },
                          '©1998-2017 QuickHoney Nana Rausch & Peter Stemmler. No part of ',
                          'this website may be reproduced in any manner without permission.  ',
                          'Programming by Hübner/Odendahl'));

    wait_for_images(function () { reveal_buttons_nicely(map(partial(operator['add'], 'home_'), home_buttons)); });
}

function stop_revealing_buttons() {
    window.button_reveal_timer && window.button_reveal_timer.cancel();
}

function reveal_buttons_nicely(images, n) {
    if (n == null) {
        stop_revealing_buttons();
        n = 0;
        hide_cue()
    }
    $(images[n]).style.visibility = 'inherit';
    n++;
    if (n != images.length) {
        window.button_reveal_timer = callLater(0.1, partial(reveal_buttons_nicely, images, n));
    }
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
	if (image_name != undefined) {
	    document.show_picture = image_name;	    
	} else {
            document.show_picture = components[1];
        }
        subdirectory(components[0]);
    } else {
        show_directory_buttons(directory_name);
        $('image_browser').className = 'directory page';
        window.current_scroll_position = 0;
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

function display_thumbnail_page() {
    log('display_thumbnail_page, current_scroll_position', window.current_scroll_position);
    if (window.current_scroll_position) {
        setTimeout("window.scrollTo(0, window.current_scroll_position)", 300);
    }

    $('image_browser').className = 'results page';
    overlay_remove();

    var page = query_result_pages[0];
    var row_index = 0;

    replaceChildNodes("thumbnails");

    function next_row() {
        var row_nodes = [];
	var row = page[row_index];
	var cell_width = row[0];
	var cell_height = row[1];
	for (var image_index = 2; image_index < row.length; image_index++) {
	    var image = row[image_index];
            var imageElement = IMG({ 'class': 'inherited_image',
                                     width: cell_width,
                                     height: cell_height});
            imageElement.src = '/image/' + encodeURI(image.name) + '/cell,ffffff,' + cell_width + ',' + cell_height + ',8';
	    var imageLink = A({ href: '#' + current_directory + '/' + current_subdirectory + '/' + encodeURI(image.name),
                                onclick: function () { window.current_scroll_position = getScrollXY().y; } },
                              imageElement);
	    var imageSpan = SPAN({'style': "position:relative"}, imageLink);
	    row_nodes.push(imageSpan);
        }
        row_nodes.push(BR());
        appendChildNodes("thumbnails", row_nodes);
        row_index++;
    }

    show_cue();
    function display_next_row() {
        next_row();
        if (row_index < page.length) {
            wait_for_images(display_next_row);
        } else {
            log('done with thumbnails');
            hide_cue();
        }
    }
    display_next_row();
}

/* image browser - displaying one image */

function make_images_navbar() {
    var result_links = [];

    if (query_position > 0) {
        replaceChildNodes("previous_image",
                          internal_link(current_directory + '/' + current_subdirectory + '/' + query_result[query_position - 1].name,
                                        "<<"));
    } else {
        replaceChildNodes("previous_image", "<<");
    }
    if (query_position < (query_result.length - 1)) {
        replaceChildNodes("next_image",
                          internal_link(current_directory + '/' + current_subdirectory + '/' + query_result[query_position + 1].name,
                                        ">>"));
    } else {
        replaceChildNodes("next_image", ">>");
    }
    $('back_to_results_link').href = '#' + current_directory + '/' + current_subdirectory;

    replaceChildNodes("result_image_count", "result " + (query_position + 1) + " of " + query_result.length);
}

function right_arrow_pressed() {
    if (document.location.hash.match(/\/.*\//) && (query_position < (query_result.length - 1))) {
        document.location.hash = current_directory + '/' + current_subdirectory + '/' + query_result[query_position + 1].name;
    }
}

function left_arrow_pressed() {
    if (document.location.hash.match(/\/.*\//) && (query_position > 0)) {
        document.location.hash = current_directory + '/' + current_subdirectory + '/' + query_result[query_position - 1].name;
    }
}

function up_arrow_pressed() {
    if (document.location.hash.match(/\/.*\//)) {
        document.location.hash = document.location.hash.replace(/\/[^\/]*$/, '');
    }
}

function display_image(index) {

    $('image_browser').className = 'browse page';

    index = index - 0; // ensure integer
    query_position = index;
    current_image = query_result[index];

    log('display_image index ' + index);

    display_current_image();
}

function cutout_button_dimensions(image, ratio)
{
    var width = Math.round((image.width / 2) * ratio);
    var height = 120;
    var left = Math.max(0, Math.min(image.center_x * ratio - width / 2, image.width * ratio - width));
    var top = Math.max(0, Math.min(image.center_y * ratio - height / 2, image.height * ratio - height));
    return { left: left,
             top: top,
             width: width,
             height: height };
}

function make_button_cutout_box(image, ratio) {
    var dims = cutout_button_dimensions(image, ratio);

    return DIV({ id: 'button_cutout_box',
                 style: 'position: absolute;'
                 + 'top: ' + dims.top + 'px; left: ' + dims.left + 'px;'
                 + 'width: ' + dims.width + 'px; height: ' + dims.height + 'px;'
                 + 'border: 1px solid black;'
                 + 'cursor: crosshair;'
                 + 'pointer-events: none;'
                 + 'box-shadow: 0 0 10px rgba(255,255,255,1)'});
}

function display_current_image() {
    overlay_remove();
    make_images_navbar();

    var ratio = 1 / Math.max(current_image.width / 860, current_image.height / 860);
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

    appendChildNodes("metadata", makeSocialIcons(query_result[query_position]));

    if (may_enlarge) {
	replaceChildNodes("full_click", A({ href: '#' + link, onclick: enlarge },
                                          "full size"));
    } else {
	replaceChildNodes("full_click", SPAN({ style: "visibility: hidden" }, "full size"));
    }

    var content;
    if (current_directory == 'pixel' && current_subdirectory == 'animation') {
        switch (current_image.animation_type) {
        case "video/quicktime":
            if (!detectQuickTime()) {
                content = "<p>To display this content, the <a href='http://www.apple.com/quicktime/download/' target='_new'>Apple Quicktime plugin</a> is required.</p>";
            } else {
                content = "<div style='height: 81px'> </div>"
                    + _QTGenerate("QT_GenerateOBJECTText_XHTML", true, ['/animation/' + current_image.id, '860', '486', '',
                                                                        'scale', 'aspect',
                                                                        'showlogo', 'false',
                                                                        'loop', 'true',
                                                                        'bgcolor', 'white']);
            }
            break;
        case "application/x-director":
            if (!detectDirector()) {
                content = "<p>To display this content, the <a href='http://www.adobe.com/shockwave/download/' target='_new'>Adobe Shockwave plugin</a> is required.</p>";
            } else {
                content
                    = '<object width="860" height="860" classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,0,0">'
                    + ' <param name="movie" value="/animation/' + current_image.id + '" />'
                    + ' <embed src="/animation/' + current_image.id + '" width="860" height="860">'
                    + ' </embed>'
                    + '</object>';
            }
            break;
        case "application/x-shockwave-flash":
            if (!detectFlash()) {
                content = "<p>To display this content, the <a href='http://www.adobe.com/products/flashplayer/' target='_new'>Adobe Flash Player</a> is required.</p>";
            } else {
                content
                    = '<object width="860" height="860" classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,0,0">'
                    + ' <param name="movie" value="/animation/' + current_image.id + '" />'
                    + ' <embed src="/animation/' + current_image.id + '" width="860" height="860">'
                    + ' </embed>'
                    + '</object>';
            }
            break;
        }
    }

    window.scrollTo(0, 0);

    if (content) {
        $("image_detail").innerHTML = content;
    } else {
        show_cue();

	var top_padding = Math.round(860 - display_height) / 2;
	var left_padding = Math.round(860 - display_width) / 2;
        var img = IMG({ width: display_width,
                        height: display_height,
                        style: 'visibility: hidden',
                        src: '/image/' + encodeURI(current_image.name) + imageproc_ops });

        var button_cutout_box = '';
        if (logged_in) {
            img.style.cursor = 'crosshair';
            $(img)
                .onclick = function (e) {
                    console.log('set image center', current_image.name, e.offsetX, e.offsetY);
                    var center_x = Math.floor(e.offsetX / ratio);
                    var center_y = Math.floor(e.offsetY / ratio);
                    $("edit_center_x").value = current_image.center_x = center_x;
                    $("edit_center_y").value = current_image.center_y = center_y;
                    var dims = cutout_button_dimensions(current_image, ratio);
                    $("button_cutout_box").style.left = dims.left + 'px';
                    $("button_cutout_box").style.top = dims.top + 'px';
                    submit_json('/json-edit-image/' + current_image.id + '?action=set-center&center-x=' + center_x + '&center-y=' + center_y, null, image_edited);
                }
            button_cutout_box = make_button_cutout_box(current_image, ratio);
        }
	var divNode = DIV({ style: 'position: relative; margin-left: ' + left_padding + 'px' },
                          (may_enlarge && !logged_in) ? A({ onclick: 'enlarge()', href: '#' }, img) : img,
                          button_cutout_box);
        replaceChildNodes('image_detail', divNode);
        wait_for_images(function () {
            hide_cue();
            img.style.visibility = 'inherit';
        });
    }

    if (logged_in) {
	$("edit_client").value = current_image.client;
        $("edit_keywords").value = current_image.spider_keywords || "";
        $("edit_description").value = current_image.description || "";
        $("edit_center_x").value = current_image.center_x;
        $("edit_center_y").value = current_image.center_y;

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

    var path = 'home';
    if (document.location.pathname != '/') {
        path = document.location.pathname.substr(1);
    } else if (document.location.href.match(/#./)) {
        path = document.location.href.substr(document.location.href.indexOf("#") + 1);
    }
    document.location.href = "/#" + path;

    poll_path();

    window.addEventListener('keydown', function (e) {
        switch (e.keyCode) {
        case 37: /* left */
            left_arrow_pressed();
            break;
        case 38: /* up */
            up_arrow_pressed();
            break;
        case 39: /* right */
            right_arrow_pressed();
            break;
        }
    }, false);

    application_initialized = true;
    log('init_application done');
}

function init() {
    log('init');
    show_cue();

    wait_for_images(init_application);
}

function jump_to(path) {
    document.current_path = path;

    log('jump_to - path is', path, 'current_scroll_position', window.current_scroll_position);

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
    fade('image_browser', {to: to,
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
};

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

NOTICE = partial(SPAN, { 'class': 'notice' });
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
