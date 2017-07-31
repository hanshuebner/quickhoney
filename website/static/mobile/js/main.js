
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

function home()
{
    setPage('home');
    ['pixel', 'vector', 'pen', 'news'].forEach(function (page) {
        $('#home .' + page)
            .attr('src',
                  random_button_image('home', page, screenWidth(), 120, page))
            .attr('width', screenWidth());
    });
}

function gotoCategory(category)
{
    setPage('category', category);
    $('#category')
        .empty()
        .append(subcategories[category].map(function (subcategory) {
            with(DOMBuilder.dom) {
                return A({ href: '#' + category + '/' + subcategory },
                         IMG({ 'class': 'button',
                               src: random_button_image(category, subcategory, screenWidth(), 120, subcategory),
                               width: screenWidth(),
                               height: 120 }));
            }
        }));
}

function gotoSubcategory(category, subcategory)
{
    setPage('subcategory', category);
    console.log('subcategory', subcategory);
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
    Path.listen();
}

$(document).ready(main);
