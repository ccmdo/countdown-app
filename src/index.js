import { Elm } from './Main.elm';

var app = Elm.Main.init({
  node: document.getElementById('root')
});


// Subscriptions to Elm events

app.ports.playMusic.subscribe(function(message) {
  let audioElement = document.querySelector('audio')
  if (!audioElement) { return; }

  audioElement.play();
});

app.ports.pauseMusic.subscribe(function(message) {
  let audioElement = document.querySelector('audio')
  if (!audioElement) { return; }

  audioElement.pause();
});

app.ports.stopMusic.subscribe(function(message) {
  let audioElement = document.querySelector('audio')
  if (!audioElement) { return; }

  audioElement.pause();
  audioElement.currentTime = 0;
});


// Ports to notify Elm

document.querySelector('audio').addEventListener("play", function(event) {
  console.log("play");
  app.ports.audioStarted.send(null);
})

document.querySelector('audio').addEventListener("ended", function(event) {
  console.log("ended");
  app.ports.audioEnded.send(null);
})
