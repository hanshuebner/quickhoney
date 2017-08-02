
var preloadCount = 5;

function setPage(name, category)
{
    console.log('page', name, category);
    $('body').attr('class', category || name);
    $('.page').css('display', 'none');
    $('#' + name).css('display', 'block');
}

function screenWidth()
{
    return $(window).width() - 40;
}

function homeButtonHeight()
{
    return Math.floor(($(window).height() - 170) / 4);
}

function home()
{
    $('#home .button').css('height', homeButtonHeight());
    setPage('home');
    ['pixel', 'vector', 'pen', 'news'].forEach(function (page) {
        $('#home .' + page)
            .attr('src',
                  random_button_image('home', page, screenWidth(), homeButtonHeight(), page))
            .attr('width', screenWidth());
    });
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
                               src: random_button_image(category, subcategory, screenWidth(), homeButtonHeight(), subcategory),
                               width: screenWidth(),
                               height: homeButtonHeight() }));
            }
        }));
}

function toggleImageInfo(e)
{
    var image = e.data;
    var imageInfo = $(e.delegateTarget).find('.image-info');
    if (imageInfo.length) {
        imageInfo.remove();
    } else {
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

function gotoSubcategory(category, subcategory)
{
    setPage('images', category);
    console.log('images', subcategory);
    $('#images').empty();
    $.get('/json-image-query/' + category + '/' + subcategory,
          function (data) {
              $('#images')
                  .append(data.queryResult.map(makeImageDisplay));
              $('#images div')
                  .waypoint(function () {
                      $(this.element).nextAll().slice(0, preloadCount + 1).reveal();
                  });
              $('#images div').slice(0, preloadCount + 1).reveal();
          });
}

function news()
{
    setPage('news');
}

function contact()
{
    setPage('contact');
}

function openMenu(e)
{
    console.log('open menu');
    $('nav menu').css('visibility', 'visible');
    e.stopPropagation();
}

function closeMenu()
{
    console.log('close menu');
    $('nav menu').css('visibility', 'hidden');
}

function main()
{
    $('.open-menu').on('click', openMenu);
    $('body').on('click', closeMenu);

    Path.map('#home').to(home);
    ['pixel', 'vector', 'pen'].forEach(function (category) {
        Path.map('#' + category).to(function () {
            gotoCategory(category);
        });
        Path.map('#' + category + '/:subcategory').to(function () {
            gotoSubcategory(category, this.params['subcategory']);
        });
    });
    Path.map('#news').to(news);
    Path.map('#contact').to(contact);
    Path.root("#home");
    Path.listen();
    $(window).resize(function () { location.reload() });
}

$(document).ready(main);
