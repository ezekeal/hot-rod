'use strict'

const electron = require('electron')
const chokidar = require('chokidar')
const open = require('open')
const app = electron.app // Module to control application life.
const BrowserWindow = electron.BrowserWindow // Module to create native browser window.
const nativeImage = electron.nativeImage
const dialog = electron.dialog
const ipcMain = electron.ipcMain

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow
let appIcon = nativeImage.createFromPath(__dirname + '/wombat.png')

function createWindow () {
  // app.tray = new Tray(appIcon)
  // Create the browser window.
  mainWindow = new BrowserWindow({icon: appIcon, width: 1024, height: 768})
  mainWindow.setProgressBar(-1)

  mainWindow.webContents.on('will-navigate', (e, url) => {
    e.preventDefault()
    open(url)
  })

  // and load the index.html of the app.
  mainWindow.loadURL(`file://${ __dirname }/index.html`)

  // Open the DevTools.
  //mainWindow.webContents.openDevTools()

  // Emitted when the window is closed.
  mainWindow.on('closed', function () {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null
  })
}

ipcMain.on('get-file', (event, arg) => {
  let path = dialog.showOpenDialog({
    properties: [ 'openFile' ],
    filters: [{ name: 'JSON', extensions: ['json'] }]
  })
  event.sender.send('get-file-reply', path)
})

// Quit when all windows are closed.
app.on('window-all-closed', function () {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

app.on('activate', function () {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow()
  }
})

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
app.on('ready', createWindow)

chokidar.watch(['ports.js', 'index.html', 'elm.js', 'styles.css']).on('change', () => {
  if (mainWindow) {
    mainWindow.reload()
  }
})
