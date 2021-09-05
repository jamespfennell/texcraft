import React, { ReactNode } from "react";

export class TexFile {
  filename: string;
  content: string;
  description: string;

  constructor(filename: string, content: string, description?: string) {
    this.filename = filename;
    this.content = content;
    this.description = description === undefined ? "" : description;
  }
}

export const WelcomeDotTex = new TexFile("welcome.tex", String.raw
  `\def\welcome#1{Welcome to #1!}
\welcome{the Texcraft Playground}

\def\twodigit#1{\ifnum#1 < 10 0\fi\the#1}
Today's date is \the\year-\twodigit\month-\twodigit\day.

Hint:\newline
You can add single new lines using the \\newline command.
`, "a welcome message from Texcraft");

export const PrimesDotTex = new TexFile("primes.tex", "TODO", "calculate the first N prime numbers");

export const UndefinedcsDotTex = new TexFile("undefinedcs.tex", String.raw
  `\def\fibonnaci#1{(See fibonnaci.tex for an implementation)}
  
% Fibonnaci is hard to spell...
\fibonaci{20}
`, "see how Texcraft reports errors");


export class MarkdownFile {
  filename: string;
  content: ReactNode;
  description: string;

  constructor(filename: string, content: ReactNode) {
    this.filename = filename;
    this.content = content;
    this.description = "TODO";
  }
}

export const HowItWorks = new MarkdownFile("how-it-works.md",
  <div>
    <h2>How it works</h2>

    <p>
      The Texcraft Playground is made with three parts.
      Most of the work is done by the Texcraft library itself, which is included in the first part.
    </p>
    <ol>
      <li>
        The core of the app is a small TeX engine built with Texcraft.
        This engine is [a single Rust source file]().
        It only includes TeX features relevent for the "Turing complete subset" of the TeX language;
        in particular, it doesn't include any typesetting features.
        The engine is compiled to WebAssembly/WASM so that it can run in the browser.
      </li>
      <li>
        The UI is built using React. The source is basically a single file.
        This file imports the WASM code and invokes it when a TeX script is run.
        The WASM code is also used for generating the commands documentation.
        This functionality plugs into Texcraft's documentation system
        (accesible in the Texcraft CLI using `texcraft commands`).
      </li>
      <li>
        The backend of the app is a small Go script that serves the static
        files and manages the share feature.
        The share feature works by persisting shared TeX scripts in a Redis instance.
      </li>
    </ol>
  </div>
);

export const Tutorial = new MarkdownFile("tutorial.md",
  <div>
    <h2>Tutorial</h2>
  </div>
);
