
/* Number of images to preload when displaying a category */
var preloadCount = 5;

function setPage(name, category)
{
    console.log('setPage', name, category);
    category = category || name;
    $('nav.menu menu img')
        .each(function () {
            var match = this.src.match(/\/image\/(.*)-(|un)selected/);
            if (match) {
                var menuCategory = match[1];
                this.src = '/image/'
                    + menuCategory
                    + '-'
                    + ((menuCategory == category) ? "selected" : "unselected");
            } else {
                console.log('unmatched menu image name in image path', this.src);
            }
        });
    if (this.category) {
        $('body').removeClass(this.category);
    }
    this.category = category;
    $('body').addClass(category);
    $('.page').css('display', 'none');
    $('#' + name).css('display', 'block');
}

function screenWidth()
{
    return $(window).width() - 40;
}

function buttonWidth()
{
    if (window.mode == 'wide') {
        return Math.floor((screenWidth() - 12) / 2);
    } else {
        return screenWidth();
    }
}

function buttonHeight(count)
{
    return Math.max(120,
                    Math.floor(($(window).height() - 170) / ((window.mode == 'wide') ? Math.ceil(count / 2) : count)));
}

function home()
{
    $('#home .button')
        .css('width', buttonWidth())
        .css('height', buttonHeight(4));
    setPage('home');
    $('#home')
        .empty()
        .append(['pixel', 'vector', 'pen', 'news'].map(function (category) {
            console.log('category', category);
            with (DOMBuilder.dom) {
                return A({ href: '#' + category },
                         IMG({ 'class': 'button',
                               src: random_button_image('home', category, buttonWidth(), buttonHeight(4), category),
                               width: buttonWidth(),
                               height: buttonHeight(4) }));
            }
        }));
}

function gotoCategory(category)
{
    setPage('category', category);
    $('#category')
        .empty()
        .append(subcategories[category].map(function (subcategory) {
            with (DOMBuilder.dom) {
                return A({ href: '#' + category + '/' + subcategory },
                         IMG({ 'class': 'button',
                               src: random_button_image(category, subcategory, buttonWidth(), buttonHeight(subcategories[category].length), subcategory),
                               width: buttonWidth(),
                               height: buttonHeight(subcategories[category].length) }));
            }
        }));
}

function setHash(hash) {
    var oldHandler = window.onhashchange;
    window.onhashchange = null;
    location.hash = '#' + hash;
    setTimeout(function () { window.onhashchange = oldHandler; }, 0);
}

function toggleImageInfo(e)
{
    var image = e.data;
    var imageInfo = $(e.delegateTarget).find('.image-info');
    if (imageInfo.length) {
        imageInfo.remove();
        setHash(image.category + '/' + image.subcategory);
    } else {
        $('.image-info').remove(); /* remove other image infos */
        with (DOMBuilder.dom) {
            var info = [];
            if (image.description) {
                info = info.concat([image.description, BR()]);
            }
            info = info.concat(['Image name: ', image.name, BR()]);
            if (image.client) {
                info = info.concat(['Client: ', image.client]);
            }
            $(e.delegateTarget).append(DIV({ 'class': 'image-info' },
                                           info,
                                           makeSocialIcons(image, true)));
            setHash(image.category + '/' + image.subcategory + '/' + encodeURI(image.name));
        }
        e.delegateTarget.scrollIntoViewIfNeeded();
    }
    e.stopPropagation();
}

function makeImageDisplay(image)
{
    var ratio = screenWidth() / image.width;
    var width = Math.round(image.width * ratio);
    var height = Math.round(image.height * ratio);
    var proc;
    if (image.category == 'pixel' && ratio >= 2) {
        proc = 'double,' + Math.floor(ratio);
    } else {
        if (ratio > 1) {
            height = Math.max(image.height, 120);
        }
        proc = 'cell,,' + width + ',' + height;
    }
    with (DOMBuilder.dom) {
        return $(DIV({ 'class': 'image-display' },
                     IMG({ src: '/static/images/transparent.gif',
                           'data-src': '/image/' + image.name + '/' + proc,
                           width: screenWidth(),
                           height: height})))
            .on('click', image, toggleImageInfo);
    }
}

$.fn.reveal = function ()
{
    return this.find('img').each(function () {
        var url = $(this).attr('data-src');
        if (url) {
            console.log('reveal', url);
            $(this)
                .attr('src', url)
                .removeAttr('data-src');
        }
    });
}

function gotoSubcategory(category, subcategory, imageName)
{
    setPage('images', category);
    console.log('images', subcategory);
    $('#images').empty();
    if (imageName) {
        $.get('/json-image-info/' + imageName,
              function (data) {
                  var image = data.image;
                  $('#images')
                      .append(makeImageDisplay(image))
                      .reveal();
              });
    } else {
        $.get('/json-image-query/' + category + '/' + subcategory,
              function (data) {
                  var images = data.queryResult;

                  $('#images')
                      .append(images.map(makeImageDisplay));
                  $('#images div')
                      .waypoint(function () {
                          $(this.element).nextAll().slice(0, preloadCount + 1).reveal();
                      });
                  $('#images div').slice(0, preloadCount + 1).reveal();
              });
    }
}

function saveScrollPosition()
{
    console.log('save scroll position', getScrollXY());
    window.savedScrollPosition = getScrollXY().y;
}

function makeNewsItem(item) {
    var categoryColors = { pixel: 'ff00ff',
                           vector: '00ccff',
                           pen: 'ff0000',
                           news: '30be01' };
    with (DOMBuilder.dom) {
        switch (item.type) {
        case 'upload':
            var path = item.category + '/' + item.subcategory + '/' + encodeURI(item.name);
            return DIV({ 'class': 'upload-item ' + item.category },
                       A({ href: '#' + path },
                         IMG({ src: '/image/' + encodeURI(item.name) + '/cutout-button,,' + categoryColors[item.category] + ',120,120,0,' + item.category,
                               width: 120, height: 120 })),
                       DIV(H1(A({ href: '#' + path },
                                item.name)),
                           P(item.date, ' by ', item.owner),
                           item.description ? P(item.description) : ''));
        case 'news':
            var ratio = (item.width < screenWidth()) ? 1 : (item.width / screenWidth());
            return DIV({ 'class': 'news-item' },
                       DIV(IMG({ src: '/image/' + item.id,
                                 width: item.width / ratio,
                                 height: item.height / ratio
                               })),
                       DIV(H1(item.title),
                           P(item.date, ' by ', item.owner)));
        }
    }
}

function news()
{
    setPage('news');
    $('#news')
        .empty();
    $.get('/json-news-items/quickhoney',
          function (data) {
              data.items.map(function (item) {
                  with (DOMBuilder.dom) {
                      $('#news').append([ makeNewsItem(item),
                                          DIV({ 'class': 'sep' })]);
                  }});
              $('#news a').on('click', saveScrollPosition);
              if (window.savedScrollPosition) {
                  setTimeout(function () {
                      window.scrollTo(0, window.savedScrollPosition)
                  }, 300);
              }
          });
}

function contact()
{
    setPage('contact');
}

function toggleMenu(e)
{
    console.log('toggle menu');
    var status = $('nav menu').css('visibility');
    var newStatus = (status == 'visible') ? 'hidden' : 'visible'
    $('nav menu').css('visibility', newStatus);
    e.stopPropagation();
}

function closeMenu()
{
    console.log('close menu');
    $('nav menu').css('visibility', 'hidden');
}

function main()
{
    window.mode = (screenWidth() <= 428) ? 'narrow' : 'wide';
    $('body').addClass(window.mode);

    if (screenWidth() <= 648) {
        $('.open-menu')
            .css('display', 'block')
            .on('click', toggleMenu);
        $('body').on('click', closeMenu);
        $('body').addClass('collapsed');
    } else {
        $('body').addClass('expanded');
    }

    Path.map('#home').to(home);
    ['pixel', 'vector', 'pen'].forEach(function (category) {
        Path.map('#' + category).to(function () {
            gotoCategory(category);
        });
        Path.map('#' + category + '/:subcategory(/:image)').to(function () {
            gotoSubcategory(category, this.params.subcategory, this.params.image);
        });
    });
    Path.map('#news').to(news);
    Path.map('#contact').to(contact);
    Path.root("#home");
    Path.listen();

    $.get('/json-clients',
          function (data) {
        $('#client_names').html(format_clients(data.clients));
    });

    var currentWidth = screenWidth();
    $(window).resize(function () {
        if (currentWidth != screenWidth()) {
            location.reload();
        }
    });
}

$(document).ready(main);
