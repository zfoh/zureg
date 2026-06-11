function getParameterByName(name, url) {
  /* <https://stackoverflow.com/questions/901115/how-can-i-get-query-string-values-in-javascript> */
  if (!url) url = window.location.href;
  name = name.replace(/[\[\]]/g, '\\$&');
  const regex = new RegExp('[?&]' + name + '(=([^&#]*)|&|#|$)');
  const results = regex.exec(url);
  if (!results) return null;
  if (!results[2]) return '';
  return decodeURIComponent(results[2].replace(/\+/g, ' '));
}

function scanner() {
  const video = document.createElement("video");
  const canvas = document.createElement("canvas");
  const context = canvas.getContext("2d");
  const beep = document.getElementById("beep");
  document.body.appendChild(canvas);

  const ui = document.createElement('div');
  document.body.appendChild(ui);

  document.body.style = 'margin: 0px; overflow: hidden';
  ui.style = 'position: absolute;' +
      'top: 20px; left: 20px; right: 20px;' +
      'background-color: white;' +
      'opacity: 0.7;' +
      'padding: 20px;' +
      'z-index: 1';

  const message = document.createElement("div");
  message.innerText =
    'Unable to access video stream (please make sure you have a camera enabled)';
  ui.appendChild(message);

  const output = document.createElement("div");
  ui.appendChild(output);

  const secret = getParameterByName('secret');
  let code = null;

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
      const videoWidth = video.videoWidth;
      const intendedWidth = document.body.clientWidth;
      const ratio = intendedWidth / videoWidth;

      /* Scale the whole canvas to make the video "full-screen". */
      canvas.height = video.videoHeight;
      canvas.width = videoWidth;
      canvas.style = 'transform: scale(' + ratio + ');' +
          'transform-origin: 0% 0%;';

      context.drawImage(video, 0, 0, canvas.width, canvas.height);
      const imageData = context.getImageData(0, 0, canvas.width, canvas.height);
      const newCode = jsQR(imageData.data, imageData.width, imageData.height);
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
