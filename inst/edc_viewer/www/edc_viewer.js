
//Typing Enter in #search_input validate the input
$(document).on('keyup', '#search_input', function(e) {
  if (e.which == 13) {
    Shiny.setInputValue('search_validate', true, {priority: 'event'});
  }
});

//Activate JQuery tooltips for Data headers
$(document).on('shiny:value', function(e) {
  if(e.name != 'table') return(null)
  setTimeout(function() {
    $('.edc_label').tooltip();
  }, 300);
})