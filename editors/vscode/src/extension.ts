import * as path from "path";
import { workspace, ExtensionContext } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  // Find the adam-lsp binary. Check PATH first, then workspace setting.
  const config = workspace.getConfiguration("adam");
  const lspPath = config.get<string>("lspPath") || "adam-lsp";

  const serverOptions: ServerOptions = {
    run: { command: lspPath, args: [] },
    debug: { command: lspPath, args: ["--verbose"] },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "adam" }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.adam"),
    },
  };

  client = new LanguageClient(
    "adam-lsp",
    "Adam Language Server",
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
