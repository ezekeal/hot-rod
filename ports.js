'use strict'
/* global Elm */
// Major borrowing from npm for package.json reading
const fs = require('fs')
const path = require('path')

// start the elm app in a container div
let container = document.getElementById('container')
let hotrod = Elm.embed(Elm.Main, container, {
  file: { name: '', fileName: '', contents: '' }
})

hotrod.ports.fetchFile.subscribe((filePath) => {
  let extension = path.extname(filePath)
  let fileName = path.basename(filePath)
  fs.readFile(path.join(__dirname, filePath), 'utf8', (error, data) => {
    if (error) {
      console.log('error', error)
      hotrod.ports.fileError.send(error)
    }
    let file = {
      name: fileName,
      extension: extension,
      contents: data
    }
    console.log('sending', file)
    hotrod.ports.file.send(file)
  })
})
