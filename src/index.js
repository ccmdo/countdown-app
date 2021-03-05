import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

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

document.querySelector('audio').addEventListener("play", function(event) {
  console.log("play");
  app.ports.audioStarted.send(null);
})

document.querySelector('audio').addEventListener("ended", function(event) {
  console.log("ended");
  app.ports.audioEnded.send(null);
})

app.ports.musicFinished

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
