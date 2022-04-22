import { Elm } from './Main.elm'

Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    seed: Date.now()
  }
})