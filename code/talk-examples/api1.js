
function postendpoint(upper, body, onSuccess, onError)
{
  $.ajax(
    { url: '/endpoint' + '?upper='
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}
