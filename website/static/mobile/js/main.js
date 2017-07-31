
function setSection(name)
{
    console.log('section', name);
    $('body').attr('class', name);
}

function home()
{
    setSection('home');
    var screenWidth = $(window).width() - 40;
    ['pixel', 'vector', 'pen', 'news'].forEach(function (section) {
        $('#home .' + section)
            .attr('src',
                  random_button_image('home', section, screenWidth, 120, section))
            .attr('width', screenWidth);
    });
}

function gotoCategory(category)
{
    setSection(category);
}

function news()
{
    setSection('news');
}

function contact()
{
    setSection('contact');
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
    Path.map('#pixel').to(function () { gotoCategory('pixel'); });
    Path.map('#vector').to(function () { gotoCategory('vector'); });
    Path.map('#pen').to(function () { gotoCategory('pen'); });
    Path.map('#news').to(news);
    Path.map('#contact').to(contact);
    Path.listen();
}

$(document).ready(main);
