
//Typing Enter in #search_input validate the input
$(document).on('keyup', '#search_input', function(e) {
  if (e.which == 13) {
    Shiny.setInputValue('search_validate', true, {priority: 'event'});
  }
});

//At each shiny input
$(document).on('shiny:value', function(e) {
  //But really, only for the main datatable
  if(e.name != 'table') return(null) 
  setTimeout(function() {
    console.log("shiny:value")
    //Activate JQuery tooltips for Data headers
    $('.edc_label').tooltip();
    //Initialize the context menu
    init_context_menu();
    //Fix the horizontal scroller for column searchboxes
    fix_header_scroll();
  }, 300);
})



//https://datatables.net/extensions/buttons/custom
test_callback = function ( e, dt, node, config ) {
  console.log(e)
  debugger;
}

// Context menu for dataset column headers
fix_header_scroll = function(){
  //console.log("fix_header_scroll ------------")
  const tbl = $('#table table');
  const n_fixed = tbl.find('thead tr:first th.dtfc-fixed-left').length
  let left = 0;
  tbl.find('thead tr:last td').slice(0, n_fixed).each(function() {
    $(this).addClass('dtfc-fixed-left').css({
      position: 'sticky',
      left: left + 'px'
    });
    left += $(this).outerWidth();
  });
}

// Context menu for dataset column headers
init_context_menu = function(){
  //console.log("init_context_menu ------------")
  
  const menuHtml = `
  <div id="context-menu"
       style="display:none; position:absolute; background:#fff; border:1px solid #ccc; 
              box-shadow:2px 2px 5px rgba(0,0,0,0.2); z-index:1000;">
    <div class="item" data-action="hide">Hide column</div>
    <div class="item" data-action="fix">Fix column to the left</div>
    <div class="item" data-action="color">Set "color" column</div>
    <div class="item" data-action="group">Set "group" column</div>
  </div>
  `
  $('body').append(menuHtml);
  
  $(document).on('click', function () {
    $('#context-menu').hide();
  });
  
  $('#context-menu .item').off().on('click', function (e) {
    const action = $(this).data('action');
    const target = $('#context-menu').data('target');
    console.log("---- CONTEXT MENU ----" + target.innerText);
    console.log(e);
    //TODO pass action & target to shiny handler
    
    
        //Shiny.setInputValue('hidden_group', target.innerText, {priority: 'event'});
    switch (action) {
      case 'hide':
        console.log('Hide column:', target.innerText);
        current = $("#hidden_hide").val().split("___");
        current.push(target.innerText);
        new_val = current.join('___');
        $("#hidden_hide").val(new_val).trigger("change");
        break;
      case 'fix':
        console.log('Fix column to the left:', target.innerText);
        current = $("#hidden_fixed").val().split("___");
        console.log('append to :', current);
        if(current.includes(target.innerText)){
          current.splice(current.indexOf(target.innerText), 1);
        } else {
          current.push(target.innerText);
        }
        new_val = current.join('___');
        $("#hidden_fixed").val(new_val).trigger("change");
        break;
      case 'color':
        console.log('Set color column:', target.innerText);
        $("#hidden_color").val(target.innerText).trigger("change");
        break;
      case 'group':
        console.log('Set group column:', target.innerText);
        $("#hidden_group").val(target.innerText).trigger("change");
        break;
    }
    
    $('#context-menu').hide();
  });
  
  
  
  $(".data_column").off().on("contextmenu", function(e) {
    //console.log(e)
    e.preventDefault();
    $('#context-menu').data('target', this);
    $('#context-menu').css({ 
      top: e.pageY, left: e.pageX, display: 'block',
    });
    $('#context-menu .item').css({ 
      cursor:"pointer",
      //TODO align right si right of page ? ou alors xmax = length of block?
    });
    return false;
  });

}
