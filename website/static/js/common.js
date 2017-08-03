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

function makeSocialIcons(image, mobile)
{
    var link = image.category + '/' + image.subcategory + '/' + encodeURI(image.name);
    var mobile = mobile ? '-mobile' : '';
    with (DOMBuilder.dom) {
        return DIV({ 'class': 'social-icons' },
                   A({ href: '#',
                       title: 'Share by Email' },
                     IMG({src: '/static/images/social/' + image.category + mobile + '-mail.png'})),
                   A({ href: 'https://www.facebook.com/sharer/sharer.php?u=http://quickhoney.com/' + link,
                       title: 'Share on Facebook',
                       target: '_new'},
                     IMG({src: '/static/images/social/' + image.category + mobile + '-facebook.png'})),
                   A({ href: 'https://twitter.com/home?status=http%3A//quickhoney.com/' + link,
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
