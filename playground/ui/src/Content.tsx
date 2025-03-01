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

export const DigitsOfPiDotTex = new TexFile("digits-of-pi.tex", String.raw
`% Digits of pi using the Rabinowitz and Wagon spigot algorithm [1]
%
% This TeX script calculates the first n digits of pi. It uses \count registers
% for working memory and is runnable on both Texcraft and pdfTeX. The variable n 
% is specified at the end of the script, and is restricted as follows:
%
% - pdfTeX: n cannot be much larger than 1000 as pdfTeX runs out of stack space.
%
% - Texcraft: n cannot be larger than 7559 as the algorithm requires n + (10n)//3 + 12
%   integers in memory, and we assume 2^15 registers, as in pdfTeX.
%   But, for some currently unknown value of n, 32-bit integer overflow will occur
%   and the result will be incorrect. From the original paper [1] we know that this n
%   is at least 5000. 
%
% \count register layout:
%
% - [0, 25196): used for the length 10n/3 working array
% - [25196, 32755): used for storing the n results
% - [32755, 32768=2^15): used for 13 named variables via \countdef
%
% [1] http://www.cs.williams.edu/~heeringa/classes/cs135/s15/readings/spigot.pdf

% While loop: repeatedly executes #2 while \`\ifnum #1\` is true
\def\while#1#2{%
  \ifnum #1%
    #2%
    \while{#1}{#2}\fi%
}

% Modulus: calculates \`#1 % #2\` and puts the result in #1
\countdef \tempTwo 32756
\def\modulus#1#2{%
  \tempTwo = #1%
  \divide \tempTwo by #2%
  \multiply \tempTwo by #2%
  \multiply \tempTwo by -1%
  \advance \tempTwo by #1%
  #1= \tempTwo%
}

\countdef \n 32767
% Any changes to the following line will break the benchmark Rust code as it assumes
% the string '\n = 100' appears exactly.
\n = 100

\def\result{\count}
\countdef \resultIndex 32766
\resultIndex = 25196

% allocate an array of length (10n)/3
\countdef \len 32765
\len = \n
\multiply \len by 10
\divide \len by 3
\def\r{\count}

% initialize each element of the array to 2
\countdef \i 32764
\i = 0
\while{\i < \len}{
  \r \i = 2
  \advance \i by 1
}

\countdef \j 32763
\countdef \carry 32761
\countdef \preDigit 32760
\countdef \firstPreDigit 32759
\firstPreDigit = -1
\countdef \numTrailingPreDigits 32758
\numTrailingPreDigits = 0

\countdef \outerLoopIndex 32757
\outerLoopIndex = 0
\while{\outerLoopIndex < \n}{
  \advance \outerLoopIndex by 1
  %
  \i = \len
  \while{\i > 0}{
    \advance \i by -1
    %
    % r[i] = r[i] * 10 + carry
    \multiply \r \i by 10
    \advance \r \i by \carry
    %
    % Calculate j = 2i+1
    \j = 2
    \multiply \j by \i
    \advance \j by 1
    %
    % carry = (r[i]/j)(i)
    \carry = \r \i
    \divide \carry by \j
    \multiply \carry by \i
    %
    % r[i] = r[i] % j
    \ifnum \i > 0 \modulus{\r\i}{\j} \fi
  }
  %
  \preDigit = \r 0
  \divide \preDigit by 10
  %
  \ifnum \preDigit < 9
    \ifnum \firstPreDigit > -1
      \result\resultIndex = \firstPreDigit
      \advance\resultIndex by 1
    \fi
    \while{\numTrailingPreDigits > 0}{
      \result\resultIndex = 9
      \advance\resultIndex by 1
      \advance\numTrailingPreDigits by -1
    }
    \firstPreDigit = \preDigit
  \fi
  \ifnum \preDigit = 9
    \advance\numTrailingPreDigits by 1
  \fi
  \ifnum \preDigit = 10
    \ifnum \firstPreDigit < 9
      \advance \firstPreDigit by 1
    \else 
      \firstPreDigit = 0
    \fi
    \result\resultIndex = \firstPreDigit
    \advance\resultIndex by 1
    \while{\numTrailingPreDigits > 0}{
      \result\resultIndex = 0
      \advance\resultIndex by 1
      \advance\numTrailingPreDigits by -1
    }
    \firstPreDigit = 0
  \fi
  \modulus{\r 0}{10}
}

\while{\resultIndex < \n}{\result\resultIndex-1\advance\resultIndex1}

\advance\n by 25196
\countdef \s 32755
\i=25196
pi =

\while{\i<\n}{%
  \ifnum \result\i>-1 \the\result\i\fi%
  \ifnum \i=25196 .\fi %
  \advance\i by 1%
  \s=\i%
  \modulus{\s}{20}%
  \ifnum \s=0\newline \fi%
}...
`, "compute the first n digits of pi");


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
