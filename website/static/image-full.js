/* Used from image-full to load the current image */

function load_image() {
    var hash_index = document.location.href.indexOf('#');

    if (hash_index != -1) {
	// safari does not seem to support split, ie5mac does not have encodeURI and decodeURI.  oh my!
	var argument = unescape(document.location.href.substr(hash_index + 1));

	document.getElementById("the_image").src = "/image/" + argument;
    } else {
	alert('missing image name');
    }
}

