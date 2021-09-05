import React, { ReactNode } from "react";
import * as Texcraft from "../../wasm/pkg/texcraft_playground_wasm";
import * as Content from "./Content";

type ElementProps = {
  header: string;
  rightHeader?: React.ReactNode;
  children: React.ReactNode;
}

function Element(props: ElementProps) {
  return (
    <div className="page">
      <div className="pageHeader">
        <div className="pageHeaderLeft monospace">{props.header}</div>
        <div className="pageHeaderRight">{props.rightHeader}</div>
      </div>
      <div className="pageFrame">{props.children}</div>
    </div>
  )
}

type EditorProps = {
  file: Content.TexFile;
  shareMode: string;
  shareKey: string;
  handleShare: () => void;
  onChange: (event: React.ChangeEvent<HTMLTextAreaElement>) => void;
}

export function Editor(props: EditorProps) {
  var shareWidget: ReactNode = ""
  if (props.shareMode == "toShare") {
    shareWidget = (<a onClick={props.handleShare}>share</a>);
  }
  if (props.shareMode == "sharing") {
    shareWidget = "generating url"
  }
  if (props.shareMode == "shared") {
    shareWidget = (
      <span>
        <a
          style={{ textDecoration: "none" }}
          onClick={function () { copyToClipboard("https://play.texcraft.dev/s/" + props.shareKey) }}>
          play.texcraft.dev/s/{props.shareKey}
        </a> (click to copy)</span>
    );
  }
  return (
    <Element header={props.file.filename} rightHeader={shareWidget}>
      <textarea id="inputtex" spellCheck="false" value={props.file.content} onChange={props.onChange} />
    </Element>
  )
}

function copyToClipboard(text: string) {
  console.log('Will try to copy ', text, ' to clipboard');
  navigator.clipboard.writeText(text).then(function () {
    console.log('Async: Copying to clipboard was successful!');
  }, function (err) {
    console.error('Async: Could not copy text: ', err);
  });
}


type TexcraftExecProps = {
  inputFile: Content.TexFile;
}

function TexcraftExecRaw(props: TexcraftExecProps) {
    const date = new Date();
    const day = date.getDate();
    const month = date.getMonth() + 1;
    const year = date.getFullYear();
    var midnight = new Date(date);
    const minutes_since_midnight = (date.getTime() - midnight.setHours(0, 0, 0, 0)) / 60000;
    console.log("Running Texcraft");
    var output = Texcraft.greet(props.inputFile.content, minutes_since_midnight, day, month, year);
    return (
      <Element header={"$ texcraft exec " + props.inputFile.filename}>
        <div id="output" className="monospace">{output}</div>
      </Element>
    );
}

// We memoize this element as it calls into Texcraft in render
export const TexcraftExec = React.memo(TexcraftExecRaw);

export function TexcraftCommands(props: {}) {
  return (
    <Element header="$ texcraft commands">
      <div className="commands">
        <b><a>\advance</a></b>  Add an integer to a variable<br />
        <br />
        <b>\catcode</b>  Get or set a catcode register<br />
        <b>\count</b>  Get or set an integer register<br />
        <b>\countdef</b>  Bind an integer register to a control sequence<br />
      </div>
    </Element>
  )
}


type MarkdownViewerProps = {
  file: Content.MarkdownFile;
}

export function MarkdownViewer(props: MarkdownViewerProps) {
  return (
    <Element header={"$ cat " + props.file.filename}>
      <div className="markdown">
        {props.file.content}
      </div>
    </Element>
  )
}

type DirectoryEntryThing = {
  filename: string;
  description: string;
  symlink?: string;
}

export class DirectoryEntry {
  thing: DirectoryEntryThing;
  handler: () => void;

  constructor(thing: DirectoryEntryThing, handler: () => void) {
    this.thing = thing;
    this.handler = handler;
  }
}

type DirectoryViewerProps = {
  entries: DirectoryEntry[];
  path: string;
}

export function DirectoryViewer(props: DirectoryViewerProps) {
  const entries: ReactNode[] = [];
  for (const entry of props.entries) {
    var link: ReactNode = <h3><a onClick={function () { entry.handler() }}>{entry.thing.filename}</a></h3>;
    if (entry.thing.symlink !== undefined) {
      var link: ReactNode = <h3>{entry.thing.filename} → <a href={entry.thing.symlink}>{entry.thing.symlink}</a></h3>
    }
    entries.push(
      <div key={entry.thing.filename}>
        {link}
        <p>{entry.thing.description}</p>
      </div>
    )
  }
  return (
    <Element header={"ls " + props.path}>
      <div className="ls">
        {entries}
      </div>
    </Element>
  )
}
