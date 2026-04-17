import * as path from "path";
import { workspace, window, ExtensionContext } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext): void {
  const config = workspace.getConfiguration("organon-syl");
  const serverPath = config.get<string>("serverPath", "organon-syl-lsp");

  // Resolve relative paths against the extension directory.
  const command = path.isAbsolute(serverPath) ? serverPath : serverPath;

  const serverOptions: ServerOptions = {
    command,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "syl" }],
  };

  client = new LanguageClient(
    "organon-syl",
    "Organon Syllogistic",
    serverOptions,
    clientOptions,
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (client) {
    return client.stop();
  }
  return undefined;
}
