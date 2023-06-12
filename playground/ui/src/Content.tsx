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
% The number of digits to produce is configured at the end of the script.
%
% Notes:
%
% - The algorithm is O(n^2) and currently quite slow in Texcraft! Calculating
%   5000 digits can take up to an hour depending on the hardware.
%
% - TeX uses 32 bit integers and for large enough n overflow will occur and the
%   result will be incorrect. This n is at least 5000. Its exact value is not
%   known yet.
%
% - This script uses Texcraft's allocation commands (\newInt and \newIntArray) and
%   thus does not work on existing TeX engines like pdfTeX. There is a version
%   of this script that uses \count registers instead and that can run on both
%   pdfTeX and Texcraft [2]. The use of allocation commands allows for arbitrary
%   amounts of memory to be used and for the typical memory bounds in TeX to be
%   bypassed.
%
% [1] http://www.cs.williams.edu/~heeringa/classes/cs135/s15/readings/spigot.pdf
% [2] https://github.com/jamespfennell/texcraft/blob/main/performance/digits-of-pi.tex

% While loop: repeatedly executes #2 while \`\ifnum #1\` is true
\def\while#1#2{
  \ifnum #1
    #2%
    \while{#1}{#2}\fi
}

% Modulus: calculates \`#1 % #2\` and puts the result in #1
\newInt \modulusTemp
\def\modulus#1#2{
  \modulusTemp = #1
  \divide \modulusTemp by #2
  \multiply \modulusTemp by #2
  \multiply \modulusTemp by -1
  \advance \modulusTemp by #1
  #1= \modulusTemp
}

% Computes n digits of pi and stores the result in a provided array.
%
% The first argument is the number of digits to generate. It can be provided
% as a constant or as an integer variable. The second argument is a control
% sequence pointing to an array in which the result will be stored. The array
% must enough space to store n digits
%
% The algorithm sometimes produces less than n digits. The tail of the array
% may contain -1 values indicating those digits were no calculated.
\def\computeDigitsOfPi#1#2{
  \newInt \n
  \n = #1
  \let \result = #2
  \newInt\resultIndex

  % allocate an array of length (10n)/3
  \newInt \len
  \len = \n
  \multiply \len by 10
  \divide \len by 3
  \newIntArray \r \len

  % initialize each element of the array to 2
  \newInt \i
  \i = 0
  \while{\i < \len}{
    \r \i = 2
    \advance \i by 1
  }

  \newInt \j
  \newInt \k
  \newInt \carry
  \newInt \preDigit
  \newInt \firstPreDigit
  \firstPreDigit = -1
  \newInt \numTrailingPreDigits
  \numTrailingPreDigits = 0

  \newInt \outerLoopIndex
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
}

\newInt \m
\m = 50
\newIntArray \digits \m
\computeDigitsOfPi \m \digits

\i=0
\newInt \temp
\while{\i<\m}{%
  \ifnum \result\i > -1 \the\digits\i \fi
  \ifnum \i=0.\fi
  \advance\i by 1%
}
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
