function buy_file()
{
    cart.add([ 'file', current_image.name ]);
    return "File added to shopping cart";
}

function buy_print()
{
    cart.add([ 'print', current_image.name ]);
    return "Print added to shopping cart";
}

function buy_t_shirt()
{
    cart.add([ 't-shirt', current_image.name ]);
    return "T-Shirt added to shopping cart";
}

function buy_product_button (buy_function) {
    var button = BUTTON({ id: 'buy_product_button' }, IMG({ src: '/image/add-to-cart', width: 102, height: 40 }));
    button.onclick = function () {
        var confirmation = buy_function();
        var top = (elementPosition('buy_product_button').y - 21) + 'px';
        replaceChildNodes('overlay', H1({ 'class': 'confirmation' }, confirmation));
        $('overlay').style.top = top;
        callLater(2, remove_overlay);
        return false;
    };
    return button;
}

var make_buy_forms = {
    'buy-file' : function () {
        make_overlay('buy-file', 'Buy Art as Vector PDF File', 426,
                     FORM({ action: '#', onsubmit: 'return false' },
                          SPACER("Download Artwork ", ARTWORK_NAME(current_image.name), " for one-time private use only.  ",
                                 "Please read our ",
                                 A({ href: '/static/user-agreement.html', target: 'user-agreement' }, "User Agreement"),
                                 " and tick the box below to indicate that you agree to be bound to it.",
                                 BR(), BR(),
                                 INPUT({ type: 'checkbox', name: 'agree-to-license'}),
                                 " I have read and understood the 'User Agreement' and agree to be bound to the terms set forth in it",
                                 BR(), BR(),
                                 PRICE('45 €'), NOTICE(' (inside EU, incl. tax)*'), BR(),
                                 PRICE('37.82 €'), NOTICE(' (outside EU, tax free)*'), BR(),
                                 BR(),
                                 buy_product_button(buy_file),
                                 BR(), BR(),
                                 NOTICE("Please note:  Our shop is operating from Germany, that's why there is a sales tax within Europe and none outside"))));
    },
    'buy-print' : function () {
        make_overlay('buy-print', 'Buy Art Floating Gallery Plexiglas', 426,
                     FORM({ action: '#', onsubmit: 'return false' },
                          SPACER("Fineart print mounted behind plexiglas on aluminium board with a 3/4'' wood brace. ",
                                 NOTICE('Please allow 4-6 days for production in addition to the shipping time')),
                          IMG({ src: '/image/print-sample', width: 426, height: 428 }),
                          SPACER(ARTWORK_NAME(current_image.name),
                                 TABLE(null,
                                       TBODY(null,
                                             TR(null,
                                                TD(null, INPUT({ type: "radio", name: "size", value: "small"})),
                                                TD(null,
                                                   "Small 20cm x 30cm (7.8'' x 11.8'')", BR(),
                                                   PRICE('150 €'), NOTICE(' (inside EU, incl. tax)*'), BR(),
                                                   PRICE('128.32 €'), NOTICE(' (outside EU, tax free)*'), BR())),
                                             TR(null,
                                                TD(null, INPUT({ type: "radio", name: "size", value: "large"})),
                                                TD(null,
                                                   "Small 33cm x 50cm (13'' x 19.7'')", BR(),
                                                   PRICE('190 €'), NOTICE('(inside EU, incl. tax)*'), BR(),
                                                   PRICE('159.66 €'), NOTICE('(outside EU, tax free)*'), BR())))),
                                 BR(),
                                 buy_product_button(buy_print),
                                 BR(), BR(),
                                 NOTICE("Please note:  We are shipping from Germany, that's why there is a sales tax within Europe and none outside"))));
    },
    'buy-t-shirt' : function () {
        make_overlay('buy-t-shirt', 'Buy Art on T-Shirt', 426,
                     FORM({ action: '#', onsubmit: 'return false' },
                          SPACER("Artwork ", ARTWORK_NAME(current_image.name), " in colored flocked foil ",
                                 "hot pressed on an American Apparel T-Shirt. ",
                                 NOTICE('These Tees are custom made, so please allow 4-6 days for production in addition to the shipping time')),
                          SPACER(TABLE(null,
                                       TBODY(null,
                                             TR(null,
                                                TD({ 'class': 't-shirt-sample', valign: 'top' },
                                                   IMG({ id: 't-shirt-sample',
                                                         src: ('/image/'
                                                               + encodeURI(current_image.name)
                                                               + '/cell,ffffff,100,100,10')})),
                                                TD(null,
                                                   "Color:", BR(),
                                                   "Tee - white", BR(),
                                                   "Art - as in original", BR(),
                                                   BR(),
                                                   "Price:", BR(),
                                                   PRICE('35.00 €'), NOTICE(' (inside EU, incl. tax)*'), BR(),
                                                   PRICE('29.41 €'), NOTICE(' (outside EU, tax free)*'), BR(),
                                                   BR(),
                                                   "Size:", BR(),
                                                   SELECT({ name: 'size' },
                                                          OPTION({ value: "" }, "Choose Size"),
                                                          OPTION({ value: "XS" }, "XS"),
                                                          OPTION({ value: "S" }, "S"),
                                                          OPTION({ value: "M" }, "M"),
                                                          OPTION({ value: "L" }, "L"),
                                                          OPTION({ value: "XL" }, "XL")),
                                                   BR(),
                                                   BR(),
                                                   "Quantity:", BR(),
                                                   INPUT({ name: 'quantity', size: 1, maxlength: 1 }),
                                                   BR(),
                                                   BR(),
                                                   buy_product_button(buy_t_shirt))))),
                                 BR(), BR(),
                                 NOTICE("Please note:  We are shipping from Germany, that's why there is a sales tax within Europe and none outside"))));
    }
};

function make_buy_form(keyword)
{
    log('make_buy_form: ' + keyword);
    try {
        make_buy_forms[keyword]();
    }
    catch (e) {
        log("error caught while creating buy form: " + e);
    }
}

function show_shopping_cart ()
{
    $('menu').className = 'shop';
    document.body.className = 'shop';
}
