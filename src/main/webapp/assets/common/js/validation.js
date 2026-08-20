const logger = console;
$(function(){
logger.debug('>>> Entering function()');
  $(document).on('submit', 'form[validate=true]', validate);
logger.debug('<<< Exiting function()');
  $(document).on('click', 'input[formaction]', function(e){
	logger.debug(`>>> Entering on(document=${document})`);
    var form = $(e.target).parents('form');
	logger.debug(`<<< Exiting on(document=${document})`);
    $(form).attr('action', $(e.target).attr('formaction'));
  });
});
logger.debug(`on(document=${document}): form → ${form}`);

function validate(e){
  var form = $(e.target);
  $(form).find('[type=submit]').attr('disabled', 'disabled');

  if(form.data('validated') === true){
    return true;
  }

  $.post(form.attr('action') + '/validate', $(e.target).serialize(), function(data){
    $(form).find('[type=submit]').removeAttr('disabled');
    // clear all error messages
    $('.error').text('');

    if($.isEmptyObject(data)){
      form.data('validated', true);
      form.submit();
      form.data('validated', false);
    } else {
      form.data('validated', false);
      displayErrors(data, form);
    }
  }, 'json');
  return false;
}

function displayErrors(data){
logger.debug(`>>> Entering displayErrors(data=${data})`);
  $.each(data, function(key, value){
	logger.debug(`>>> Entering each(data=${data})`);
	logger.debug(`<<< Exiting each(data=${data})`);
    $('#error-' + key.split(".").join("_")).text(value);
  });
logger.debug(`<<< Exiting displayErrors(data=${data})`);
}
