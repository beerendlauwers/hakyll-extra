// This file belongs to the ToJavascript.hs example.
// It would be included just after the inclusions of the generated translation.js files.

var gTranslations = {};

// Process a single translation set by fetching the gRawTranslation_ variable for that language and adding them to gTranslations;
function processTranslation(lang) {
    
    var rawTranslations = window['gRawTranslation_'+lang];

    var extract = function(translations, elem) {
        var identifier = elem[0];
        var translation = elem[1];
        translations[identifier] = translation;
        return translations;
    };
    var translations = R.reduce(extract,{},rawTranslations);
    gTranslations[lang] = translations;
}

// Parse the translations for all defined languages.
function parseAllTranslations() {
    var allTranslations = $js.allLanguages$;
    R.map(processTranslation,allTranslations);
}

// Get the currently defined language.
function getCurrentLanguage() {
    var lang = jQuery('html').attr('lang');
    
    if (lang == undefined) {
        throw "Page does not have a language defined, cannot use hakyllTranslate.";
    }
    
    return lang;
}

// A function you can call in Javascript and it will return the translation for the identifier.
function hakyllTranslate( identifier ) {
    var lang = getCurrentLanguage();
    
    if ( !(lang in gTranslations ) ) {
        processTranslation(lang);
        
        if ( !(lang in gTranslations ) ) {
            throw "No translation set available for language " + lang + " !";
        }
    }
    
    if ( !(identifier in gTranslations[lang] ) ) {
        throw "Identifier " + identifier + " not in " + lang + " translation set!";
    }
    
    return gTranslations[lang][identifier];
}

jQuery(document).ready(function() {
    parseAllTranslations();
});