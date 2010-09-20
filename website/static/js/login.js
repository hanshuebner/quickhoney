/* login stuff */

function init_login () {
    $('login_form').style.display = 'block';
    $('username').focus();
}

function do_login () {

    $('login_form').style.display = 'none';
    $('logging-in').style.display = 'block';

    return true;
}

