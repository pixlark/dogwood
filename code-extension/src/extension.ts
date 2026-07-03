import * as path from 'path';
import { workspace, ExtensionContext, window } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
  let output = window.createOutputChannel("Dogwood");

  const config = workspace.getConfiguration('dogwood');
  const serverPath = config.get<string>('serverPath', 'dogwood');

  // Get the current active file path, or use a placeholder
  const activeEditor = window.activeTextEditor;
  const filePath = activeEditor?.document.uri.fsPath ?? '';

  output.appendLine(filePath);

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: ['lsp', filePath],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'dogwood' }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/*.pr'),
    },
  };

  client = new LanguageClient(
    'dogwood',
    'Dogwood Language Server',
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
