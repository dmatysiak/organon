declare module "monaco-editor/esm/vs/editor/editor.worker?worker" {
  const WorkerFactory: new () => Worker;
  export default WorkerFactory;
}

declare module "*.syl?raw" {
  const content: string;
  export default content;
}

declare module "*.tfl?raw" {
  const content: string;
  export default content;
}

declare module "*.json" {
  const value: unknown;
  export default value;
}
