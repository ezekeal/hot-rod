'use strict'
/* global Elm */
// Major borrowing from npm for package.json reading
const fs = require('fs')
const path = require('path')

// start the elm app in a container div
let container = document.getElementById('container')
let hotrod = Elm.embed(Elm.Main, container, {
  packageJson: {}
})

hotrod.ports.fetchPackageJsonSignal.subscribe(fetchPackageJson)

function fetchPackageJson () {
  fs.readFile(path.join(__dirname, 'package.json') + '',
    (err, data) => {
      if (err) {
        hotrod.ports.packageJson.send({ error: err })
      }
      let pj = parseJSON(data)
      console.log('sending', pj)
      hotrod.ports.packageJson.send(pj)
    })
}

function parseJSON (content) {
  return JSON.parse(stripBOM(content))
}

function stripBOM (content) {
  content = content.toString()
  // Remove byte order marker. I'm taking npm's word for it
  if (content.charCodeAt(0) === 0xFEFF) {
    content = content.slice(1)
  }
  return content
}
