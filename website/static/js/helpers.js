/*
 * javascript helper functions
 */

/* ie 5 / mac compatibility routine */

function push(array, item) {
    array[array.length] = item;
}

function seq(start, end) {
    var retval = [];
    for (var i = start; i < end; i++) {
        retval.push(i);
    }
    return retval;
}

/* DOM / HTML helpers */
/* the usual shortcut */

function $(id) {
    return document.getElementById(id);
}

function wait_for_images(callback) {
    var waiting = 0;
    map(function (image) {
            if (!image.complete) {
                waiting++;
            }
        }, getElementsByTagAndClassName('img', null));

    if (waiting == 0 || window.opera) {
        callback();
    } else {
        callLater(.2, partial(wait_for_images, callback));
    }
}

