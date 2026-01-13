#!/usr/bin/env bun
// clasp: a simple CLI tool for working with text

const VERSION = "0.1.0";

function usage(): void {
  console.log(`clasp ${VERSION} - text processing utility

Usage: clasp <command> [args]

Commands:
  lines <file>       Count lines in file
  words <file>       Count words in file
  json <file>        Validate and format JSON
  version            Show version
  help               Show this help

Examples:
  clasp lines file.txt
  clasp json data.json
`);
}

async function countLines(file: string): Promise<number> {
  const text = await Bun.file(file).text();
  return text.split("\n").length - (text.endsWith("\n") ? 1 : 0);
}

async function countWords(file: string): Promise<number> {
  const text = await Bun.file(file).text();
  return text.trim().split(/\s+/).filter(Boolean).length;
}

async function formatJson(file: string): Promise<string> {
  const text = await Bun.file(file).text();
  const parsed = JSON.parse(text);
  return JSON.stringify(parsed, null, 2);
}

async function main(): Promise<void> {
  const args = process.argv.slice(2);
  const command = args[0];

  if (!command || command === "help") {
    usage();
    process.exit(0);
  }

  if (command === "version") {
    console.log(VERSION);
    process.exit(0);
  }

  const file = args[1];
  if (!file && ["lines", "words", "json"].includes(command)) {
    console.error(`Error: ${command} requires a file argument`);
    process.exit(1);
  }

  try {
    switch (command) {
      case "lines":
        console.log(await countLines(file));
        break;
      case "words":
        console.log(await countWords(file));
        break;
      case "json":
        console.log(await formatJson(file));
        break;
      default:
        console.error(`Unknown command: ${command}`);
        usage();
        process.exit(1);
    }
  } catch (err) {
    console.error(`Error: ${err instanceof Error ? err.message : err}`);
    process.exit(1);
  }
}

main();
