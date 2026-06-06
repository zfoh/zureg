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
  var beep = new Audio("data:audio/mpeg;base64,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");
  document.body.appendChild(canvas);

  var ui = document.createElement('div');
  document.body.appendChild(ui);

  document.body.style = 'margin: 0px; overflow: hidden';
  ui.style = 'position: absolute;' +
      'top: 20px; left: 20px; right: 20px;' +
      'background-color: white;' +
      'opacity: 0.7;' +
      'padding: 20px;' +
      'z-index: 1';

  var message = document.createElement("div");
  message.innerText =
    'Unable to access video stream (please make sure you have a camera enabled)';
  ui.appendChild(message);

  var output = document.createElement("div");
  ui.appendChild(output);

  var secret = getParameterByName('secret');
  var code = null;

  function scanned(uuid) {
    xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (xhttp.readyState == 4 && xhttp.status == 200) {
        beep.play();
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

      /* Compute aspect ratios... */
      var videoWidth = video.videoWidth;
      var intendedWidth = document.body.clientWidth;
      var ratio = intendedWidth / videoWidth;

      /* Scale the whole canvas to make the video "full-screen". */
      canvas.height = video.videoHeight;
      canvas.width = videoWidth;
      canvas.style = 'transform: scale(' + ratio + ');' +
          'transform-origin: 0% 0%;';

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
