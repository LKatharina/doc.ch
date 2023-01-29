(function ($) {
    "use strict";

    /* ---------------------------------------------------------------------
    We add css classes to the existing elements of a form
    which we can style. For the style see the main.css style sheet */
    $("select").addClass("bg1 select2-selection select2-selection--single");
    $(".form-group").addClass("bg1");
    $(".form-check-inline > label").addClass("label-radio100");
    $(".form-check-inline > input").addClass("input-radio100");
    $(".controls > ul > li > label").addClass("label-radio100");
    $(".controls > ul > li > label > input").addClass("input-radio100");
    $("input[type=text]").attr("placeholder", "Answer");
    $("input[type=number]").attr("placeholder", "Number");
    $("input[type=textarea]").attr("placeholder", "Answer");
    $(".required > label").append( " *");

    /* To style dropdown elements, we use the select2 library from the web:
       https://select2.org/getting-started/basic-usage */
    $("select").addClass("js-example-basic-single");
    $(".js-example-basic-single").select2({
        placeholder: "Choose an option",
        minimumResultsForSearch: -1
    });

    /* To toggle the vertical radio button checkmark, we need a 
       script, bbecause the input element is a child of the label element, and the label element holds the radio circle, but a parent element cannot be selected based on the status of the input with pure css selectors */
    let form = document.querySelector( "form" );
    form.addEventListener( "change", ( evt ) => {
      let trg = evt.target,
          trg_par = trg.parentElement;
      if ( trg.type === "radio" && trg_par && trg_par.tagName.toLowerCase() === "label" ) {
        let prior = form.querySelector( "label.checked input[name='" + trg.name + "']" );
        if ( prior ) {
          prior.parentElement.classList.remove( "checked" );
        }
        trg_par.classList.add( "checked" );
      }
    }, false );

})(jQuery);