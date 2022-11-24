var stringaJSON = null;

function parse() {
    stringaJSON = document.getElementById("areaText").value;
    output = document.getElementById("areaRis");
    var session = pl.create();
    session.consult("../libraries/ParserProlog.pl", {
        success: function() {
            session.query("jsonparse(" + stringaJSON + ", Object).", {
            success: function(goal) {output.value = "Stringa JSON inserita con successo";},
            error: function(err) {output.value = "Stringa JSON inserita con sintassi errata impossibile tradurla";}
        });},
        error: function(err) {alert("Errore nel caricamento della libreria")}
    });
    
}