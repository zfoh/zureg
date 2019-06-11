function getParameterByName(name, url) {
  /* <https://stackoverflow.com/questions/901115/how-can-i-get-query-string-values-in-javascript> */
  if (!url) url = window.location.href;
  name = name.replace(/[\[\]]/g, '\\$&');
  var regex = new RegExp('[?&]' + name + '(=([^&#]*)|&|#|$)');
  var results = regex.exec(url);
  if (!results) return null;
  if (!results[2]) return '';
  return decodeURIComponent(results[2].replace(/\+/g, ' '));
}

function scanner() {
  var video = document.createElement("video");
  var canvas = document.createElement("canvas");
  var context = canvas.getContext("2d");
  document.body.appendChild(canvas);

  var message = document.createElement("div");
  message.innerText =
    'Unable to access video stream (please make sure you have a camera enabled)';
  document.body.appendChild(message);

  var output = document.createElement("div");
  document.body.appendChild(output);

  var secret = getParameterByName('secret');
  var code = null;

  function scanned(uuid) {
    xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (xhttp.readyState == 4 && xhttp.status == 200) {
        message.innerText = 'Loaded ' + uuid;
        output.innerHTML = xhttp.responseText;
      } else if (xhttp.readyState == 4) {
        message.innerText = 'Error ' + xhttp.status + ': ' + xhttp.responseText;
        output.innerText = '';
      }
    }
    xhttp.open("GET", 'scan?uuid=' + uuid + '&secret=' + secret, true);
    xhttp.send();
  }

  function tick() {
    if (video.readyState === video.HAVE_ENOUGH_DATA) {
      canvas.hidden = false;
      output.hidden = false;

      canvas.height = video.videoHeight;
      canvas.width = video.videoWidth;
      context.drawImage(video, 0, 0, canvas.width, canvas.height);
      var imageData = context.getImageData(0, 0, canvas.width, canvas.height);
      var newCode = jsQR(imageData.data, imageData.width, imageData.height);
      if (newCode && (!code || code.data != newCode.data) && newCode.data) {
        code = newCode;
        message.innerText = 'Found a code: ' + code.data;
        output.innerText = 'Loading ticket...';
        scanned(code.data);
      }
    }
    requestAnimationFrame(tick);
  }

  // Use facingMode: environment to attemt to get the front camera on phones
  navigator.mediaDevices
    .getUserMedia({video: {facingMode: "environment"}})
    .then(function(stream) {
      message.innerText = "Started camera..."
      video.srcObject = stream;
      // required to tell iOS safari we don't want fullscreen
      video.setAttribute("playsinline", true);
      video.play();
      requestAnimationFrame(tick);
  });
}
