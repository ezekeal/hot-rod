'use strict'
/* global Elm */
// Major borrowing from npm for package.json reading
const fs = require('fs')
const path = require('path')
const ipcRenderer = require('electron').ipcRenderer

// start the elm app in a container div
let container = document.getElementById('container')
let hotrod = Elm.embed(Elm.Main, container, {
  file: { name: '', fileName: '', contents: '' },
  fileError: null
})

hotrod.ports.fetchFile.subscribe((otherPath) => {
  ipcRenderer.send('get-file')

  ipcRenderer.on('get-file-reply', (event, result) => {
    let filePath = result[0]
    let extension = path.extname(filePath)
    let fileName = path.basename(filePath)

    fs.readFile(filePath, 'utf8', (error, data) => {
      if (error) {
        console.log('error', error)
        hotrod.ports.fileError.send(`Error fetching: ${filePath}`)
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
})
