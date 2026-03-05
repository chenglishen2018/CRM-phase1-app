const { contextBridge } = require('electron');

contextBridge.exposeInMainWorld('electron', {
  appName: 'CRM Phase 1 Trial',
  version: '1.0.0',
  description: 'Phase I Dose-Escalation Clinical Trial Management'
});
