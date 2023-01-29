/* animation speed of the bar in millisec. per unit of the state */
var speed_per_unit = 30;
/* 1 unit = res pixel on the screen, will be set dynamically */
var res = 7;

function draw_progress(s, b, direction) {
  console.log(direction);
  $('.progress-total').css('width', (xmax-xmin) * res + 'px');
  var s_in = s,
      b_in = b;
  state = s;
  var rel_start = (xmax + s) / (xmax-xmin) * 100;
  b = b * res;
  s = s * res;
  $('#line1').
    css('margin-left', (-xmin * res + b) + 'px').
    html(b_in);
  $('#line2').  
    css('margin-left', (-xmin * res + s) + 'px').
    html("Start " + s_in);
  $('.progress-bar').
    css('margin-left', (-xmin * res + s) + 'px')
  $('.progress-container .progress-total').
    css('background-size', res + 'px auto');
  $('.progress-bar').
    toggleClass('progress-bar-negative, success', direction == -1).
    toggleClass('failure', direction == 1).
    css({"margin-left": rel_start +"%",
         "margin-right": 100 - rel_start +"%",
     });    
}

function set_state(diff, state) {
  $('#line2').html('');
  var direction = diff > 0 ? +1 : -1;
  s = (state - start) * res;
  /* fade the state object out*/
  $('.state').fadeOut(10);
  /*let the state bar begin at the start (rather than centered around start)*/
  $('.progress-bar').css('width', '0px').css('-webkit-transform', 'none');
  /* do the animation*/
  $('.progress-bar').animate(
    {
      width: (diff > 0 ? '+' + Math.abs(s) + 'px' :  Math.abs(s) + 'px')
    },
    {
      duration: Math.abs(diff/res) * speed_per_unit,
        complete: function() {
          // show the label of the state 's_in'
          $('.state').html(state).fadeIn("slow");
          // change  color upon success
          $('.progress-bar').
            toggleClass('progress-bar-negative', diff<0 || state<0 && diff==0);
          if (direction == 1 && state >= b) {
            $('.progress-bar').
              removeClass("failure").
              addClass("success");
          }
          if (direction == -1 && state < b) {
            $('.progress-bar').
              removeClass("success").
              addClass("failure");
          }
        }
    }
    );
}

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


var clickNext = function() {
   $('.otree-btn-next').click();
};


var hideActions = function() {
  $('.rsft-btn-choices').prop('disabled', true);
  $('#option1').hide();
  $('#option2').hide();
};

var showActions = function() {
  $('.rsft-btn-choices').prop('disabled', false);       
  $('#option1').show();
  $('#option2').show();      
};

var disableAction = function(x) {
  $('#button'+x).prop('disabled', true);
};

var showOutcome = function(x) {
  x.append('<p class="outcome">' + (outcome > 0 ? '+' : '') + outcome + '</p>' );
  x.addClass('rsft-btn-chosen')
};

var hideOutcome = function(x) {
  x.find('.outcome').remove();
  x.removeClass('rsft-btn-chosen');
};