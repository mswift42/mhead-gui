function setText(dest, s) {
    // append a string s to a given destionation dest
    document.getElementById(dest).value=s;
};

function appendText(s) {
    // append text s to textarea
    document.getElementById("tarea").value = document.getElementById("tarea").value + s;
};

function getInputText() {
    // return submitted text of submit field.
    var apptext = document.getElementById("inptext").value;
    appendText(apptext);


};

function setOverlay() {
    $('<div id=\"overlay\"><p>some text</p></div>');
};









