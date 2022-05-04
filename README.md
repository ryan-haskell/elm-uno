# @ryannhg/elm-uno

## live demo

Check out the latest progress here: 
https://uno.rhg.dev/

## running locally

If you have Node.js installed, you can run this command:

```
npm start
```

## deploying to Netlify

If you want to deploy a custom version of this to Netlify, use this build command, and point to the `dist` directory:

```
npm run deploy
```

( The `deploy` command will run the test suite before building the Elm app! )

## running the test suite

In development, watching for code changes as you go:

```
npm run test:watch
```

In production, running the test suite once (and failing if something's wrong!)

```
npm run test
```
