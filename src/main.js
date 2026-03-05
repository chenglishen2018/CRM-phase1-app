const { app, BrowserWindow, Menu } = require('electron');
const path = require('path');
const { spawn } = require('child_process');

let mainWindow;
let rProcess;

const createWindow = () => {
  mainWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    webPreferences: {
      preload: path.join(__dirname, '../public/preload.js'),
      nodeIntegration: false,
      contextIsolation: true
    },
    icon: path.join(__dirname, '../public/icon.png')
  });

  mainWindow.loadURL('http://localhost:3838');
  
  // Open developer tools for debugging (remove for production)
  // mainWindow.webContents.openDevTools();
};

const startRShiny = () => {
  const rPath = process.platform === 'win32' ? 'Rscript.exe' : 'Rscript';
  const shinyPath = path.join(__dirname, '../shiny_app/run_app.R');
  
  rProcess = spawn(rPath, [shinyPath], {
    cwd: path.join(__dirname, '../shiny_app'),
    stdio: 'pipe'
  });

  rProcess.stdout.on('data', (data) => {
    console.log(`R: ${data}`);
  });

  rProcess.stderr.on('data', (data) => {
    console.error(`R Error: ${data}`);
  });

  rProcess.on('error', (err) => {
    console.error('Failed to start R process:', err);
  });
};

app.on('ready', () => {
  startRShiny();
  
  // Wait for R Shiny to start
  setTimeout(() => {
    createWindow();
  }, 3000);
});

app.on('window-all-closed', () => {
  if (rProcess) {
    rProcess.kill('SIGTERM');
  }
  
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', () => {
  if (mainWindow === null) {
    createWindow();
  }
});

// Handle any uncaught exceptions
process.on('uncaughtException', (error) => {
  console.error('Uncaught Exception:', error);
});
