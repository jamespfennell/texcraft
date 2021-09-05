import React, { Component, ReactNode } from "react";

import * as Content from "./Content";
import { DirectoryEntry, DirectoryViewer, Editor, MarkdownViewer, TexcraftCommands, TexcraftExec } from "./Elements";

enum OpenOnRight {
  Exec,
  Commands,
  Examples,
  Root,
  Markdown,
}

type AppProps = {
  initialFilename: string;
  initialTex: string;
}

type AppState = {
  // The number of times the run button has been pressed
  numRuns: number;

  // The element to open on the right hand side
  openOnRight: OpenOnRight;

  // The TeX file that is open in the editor
  editorFile: Content.TexFile;

  // The TeX file that is currently used as input to `texcraft exec`
  execInputFile: Content.TexFile;

  // The markdown file open in the Markdown viewer
  markdownFile: Content.MarkdownFile;

  shareMode: string;
  shareKey: string;
}

class App extends Component {
  state: AppState;
  constructor(props: AppProps) {
    super(props)
    var initialFile = new Content.TexFile(
      props.initialFilename !== "" ? props.initialFilename : Content.WelcomeDotTex.filename,
      props.initialTex !== "" ? props.initialTex : Content.WelcomeDotTex.content,
    );
    this.state = {
      numRuns: 0,
      openOnRight: OpenOnRight.Exec,
      editorFile: initialFile,
      execInputFile: initialFile,
      markdownFile: Content.HowItWorks,
      shareMode: "toShare", // or sharing or shared TODO: enum
      shareKey: ""
    };
    this.openExamplesPage = this.openExamplesPage.bind(this);
    this.openMarkdownFile = this.openMarkdownFile.bind(this);
    this.openTexcraftCommandsPage = this.openTexcraftCommandsPage.bind(this);
    this.runTex = this.runTex.bind(this);
    this.handleTexInputChange = this.handleTexInputChange.bind(this);
    this.handleKeyPress = this.handleKeyPress.bind(this);
    this.handleShare = this.handleShare.bind(this);
    this.openTexFile = this.openTexFile.bind(this);
  }

  handleKeyPress(e: KeyboardEvent) {
    if (e.key === 'Enter' && e.shiftKey) {
      e.preventDefault();
      this.runTex();
    }
  }

  componentDidMount() {
    document.addEventListener("keydown", this.handleKeyPress, false);
  }

  componentWillUnmount() {
    document.removeEventListener("keydown", this.handleKeyPress, false);
  }

  openExamplesPage() {
    this.setState({
      openOnRight: OpenOnRight.Examples,
    });
  }

  openMarkdownFile(file: Content.MarkdownFile) {
    this.setState({
      markdownFile: file,
      openOnRight: OpenOnRight.Markdown,
    });
  }

  openTexcraftCommandsPage() {
    this.setState({
      openOnRight: OpenOnRight.Commands,
    });
  }

  openTexFile(file: Content.TexFile) {
    this.setState({
      openOnRight: OpenOnRight.Exec,
      editorFile: file,
      execInputFile: file,
      shareMode: "toShare"
    });
  }

  runTex() {
    this.state.numRuns += 1;
    this.setState({
      openOnRight: OpenOnRight.Exec,
      execInputFile: this.state.editorFile,
    });
  }

  handleTexInputChange(event: React.ChangeEvent<HTMLTextAreaElement>) {
    this.setState({
      editorFile: new Content.TexFile("input.tex", event.target.value),
      shareMode: "toShare"
    });
  }

  handleShare() {
    this.setState({
      shareMode: "sharing"
    });
    fetch('/share', {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ texContent: this.state.editorFile.content })
    })
      .then(response => response.json())
      .then(data => this.setState({
        shareMode: "shared",
        shareKey: data.Key
      }));
  }

  render() {
    let that = this;
    let rightPage = (): ReactNode => {
      switch (this.state.openOnRight) {
        case OpenOnRight.Exec:
          return <TexcraftExec inputFile={this.state.execInputFile} />;
        case OpenOnRight.Examples:
          var entries: DirectoryEntry[] = [];
          if (this.state.numRuns >= 0) {
            this.state.numRuns = 0;
            entries.push(new DirectoryEntry({
              filename: "..",
              description: "",
            }, function () {
              that.setState({
                openOnRight: OpenOnRight.Root,
              });
            }));
          }
          for (const texFile of [Content.WelcomeDotTex, Content.PrimesDotTex, Content.UndefinedcsDotTex]) {
            entries.push(
              new DirectoryEntry(texFile, function () { that.openTexFile(texFile) })
            );
          }
          return <DirectoryViewer path="/tex" entries={entries} />;
        case OpenOnRight.Root:
          var entries: DirectoryEntry[] = [
            new DirectoryEntry({
              filename: "tex/",
              description: "TeX files",
            }, function () { that.openExamplesPage() }),
            new DirectoryEntry(Content.HowItWorks, function () { that.openMarkdownFile(Content.HowItWorks) }),
            new DirectoryEntry(Content.Tutorial, function () { that.openMarkdownFile(Content.Tutorial) }),
            new DirectoryEntry({
              "filename": "git",
              "description": "Git repository",
              "symlink": "https://github.com/jamespfennell/texcraft"
            }, function(){}),
          ];
          return <DirectoryViewer path="/" entries={entries} />;
        case OpenOnRight.Commands:
          return <TexcraftCommands />;
        case OpenOnRight.Markdown:
          return <MarkdownViewer file={this.state.markdownFile} />
      }
    };
    return (
      <div id="grid">
        <div id="gridTop">
          <div id="runButton" className="button" onClick={this.runTex}>Run&nbsp;<span className="small">(⇧ return)</span></div>
          <div className="button whiteButton" onClick={this.openExamplesPage}>Examples</div>
          <div className="button whiteButton" onClick={function() {that.openMarkdownFile(Content.Tutorial)}}>Tutorial</div>
          <div className="button whiteButton" onClick={this.openTexcraftCommandsPage}>Available commands</div>
          <h1>Texcraft Playground</h1>
          <div className="text"><a onClick={function() {that.openMarkdownFile(Content.HowItWorks)}}>How it works</a></div>
          <div className="text"><a href="https://github.com/jamespfennell/texcraft">GitHub</a></div>
        </div>
        <div id="gridLeft">
          <Editor
            file={this.state.editorFile}
            onChange={this.handleTexInputChange}
            shareMode={this.state.shareMode}
            shareKey={this.state.shareKey}
            handleShare={this.handleShare}
          />
        </div>
        <div id="gridRight">
          {rightPage()}
        </div>
      </div>
    );
  }
}

export default App;
