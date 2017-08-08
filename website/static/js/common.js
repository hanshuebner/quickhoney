if (!Element.prototype.scrollIntoViewIfNeeded) {
  Element.prototype.scrollIntoViewIfNeeded = function (centerIfNeeded) {
    centerIfNeeded = arguments.length === 0 ? true : !!centerIfNeeded;

    var parent = this.parentNode,
        parentComputedStyle = window.getComputedStyle(parent, null),
        parentBorderTopWidth = parseInt(parentComputedStyle.getPropertyValue('border-top-width')),
        parentBorderLeftWidth = parseInt(parentComputedStyle.getPropertyValue('border-left-width')),
        overTop = this.offsetTop - parent.offsetTop < parent.scrollTop,
        overBottom = (this.offsetTop - parent.offsetTop + this.clientHeight - parentBorderTopWidth) > (parent.scrollTop + parent.clientHeight),
        overLeft = this.offsetLeft - parent.offsetLeft < parent.scrollLeft,
        overRight = (this.offsetLeft - parent.offsetLeft + this.clientWidth - parentBorderLeftWidth) > (parent.scrollLeft + parent.clientWidth),
        alignWithTop = overTop && !overBottom;

    if ((overTop || overBottom) && centerIfNeeded) {
      parent.scrollTop = this.offsetTop - parent.offsetTop - parent.clientHeight / 2 - parentBorderTopWidth + this.clientHeight / 2;
    }

    if ((overLeft || overRight) && centerIfNeeded) {
      parent.scrollLeft = this.offsetLeft - parent.offsetLeft - parent.clientWidth / 2 - parentBorderLeftWidth + this.clientWidth / 2;
    }

    if ((overTop || overBottom || overLeft || overRight) && !centerIfNeeded) {
      this.scrollIntoView(alignWithTop);
    }
  };
}

/* directory - first level category */

var last_category_buttons = {};

function random_button_image(category, subcategory, width, height, cut_category) {

    /* We want the last used button for a category used on the home
     * page to also appear on that categories page next.  For that, we
     * save the previously displayed home image button here.  Not pretty.
     */

    var key = category + '/' + subcategory;
    var ids = button_images[key];
    if (ids && ids.length) {
        var image_id = ids[Math.floor(Math.random() * ids.length)];
        if (last_category_buttons[category] && ids.indexOf(last_category_buttons[category]) != -1) {
            image_id = last_category_buttons[category];
            last_category_buttons[category] = null;
        }
        if (category == 'home') {
            last_category_buttons[subcategory] = image_id;
        }
        return '/image/' + image_id + '/cutout-button,' + subcategory + ',ffffff,' + width + ',' + height + ',8,' + cut_category;
    } else {
        console.log('no button image for ' + key + ' found');
    }
}

function ownerTwitterHandle(image)
{
    switch (image.owner) {
    case "nana": return "nanowska";
    case "p": return "peekasso";
    default: return "quickhoney";
    }
}

function makeSocialIcons(image, mobile)
{
    var link = image.category + '/' + image.subcategory + '/' + encodeURI(image.name);
    var mobile = mobile ? '-mobile' : '';
    var twitterHandle = ownerTwitterHandle(image);
    with (DOMBuilder.dom) {
        return DIV({ 'class': 'social-icons' },
                   A({ href: 'mailto:?subject=QuickHoney%20Image%20' + encodeURI(image.name) + '&body=http://quickhoney.com/' + link,
                       title: 'Share by Email' },
                     IMG({src: '/static/images/social/' + image.category + mobile + '-mail.png'})),
                   A({ href: 'https://www.facebook.com/sharer/sharer.php?u=http://quickhoney.com/' + link,
                       title: 'Share on Facebook',
                       target: '_new'},
                     IMG({src: '/static/images/social/' + image.category + mobile + '-facebook.png'})),
                   A({ href: 'https://twitter.com/share?'
                       + 'text=' + encodeURI(image.description || '')
                       + '&url=http%3A//quickhoney.com/' + link
                       + (ownerTwitterHandle ? ('&via=' + twitterHandle) : ''),
                       title: 'Tweet',
                       target: '_new'},
                     IMG({src: '/static/images/social/' + image.category + mobile + '-twitter.png'})),
                   A({ href: 'http://tumblr.com/widgets/share/tool?canonicalUrl=http://quickhoney.com/' + link,
                       title: 'Share on Tumblr',
                       target: '_new'},
                     IMG({src: '/static/images/social/' + image.category + mobile + '-tumblr.png'})),
                   A({ href: 'https://pinterest.com/pin/create/button/?url=http://quickhoney.com/' + link + '&media=http://quickhoney.com/image/' + image.name + '&description=',
                       title: 'Share on Pinterest',
                       target: '_new'},
                     IMG({src: '/static/images/social/' + image.category + mobile + '-pinterest.png'})));
    }
}

function format_clients(client_names)
{
    var rendered_clients = [];
    for (var i = 0; i < client_names.length; i++) {
	var client_name = client_names[i];
	if (client_name.search(/,/)) {
	    rendered_clients[i] = client_name.replace(/^(.*)(\s\S+,\s*.*)$/, "<b>$1</b>$2");
	} else {
	    rendered_clients[i] = "<b>" + client_name + "</b>";
	}
    }
    return rendered_clients.join("; ");
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

