function draw_trials(x) {
  $.each(Array(x), function(i) {
    var elem = '<div class="trial-border" id="trial'+(i+1)+'">'+(i+1)+'</div>';
    $('.trial-container').append(elem);
  })
}

function highlight_trial(x) {
  $('.trial-border').each(function(index, item) {
    $(item).toggleClass('highlight', $(item).attr('id') == "trial" + x);
  })
}