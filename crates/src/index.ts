#!/usr/bin/env node
'use strict';

import { Command } from 'commander';
import { searchCrates } from './api';
import { SearchOptions } from './types';

const cli = new Command();

async function main() {
  cli.version("0.0.1")
    .description("Search crates on crates.io")
    .name("crateio")
    .argument('<string>', 'search query')
    .option('-p, --page <number>', 'result page number', parseInt, 1)
    .option('-P, --per-page <number>', 'numer of results per page', parseInt, 30)
    .option('-s, --sort <string>', 'sort results');

  cli.parse();
  const opts = cli.opts<SearchOptions>();
  delete Object.assign(opts, { ["per_page"]: opts["perPage"] })["perPage"];

  try {
    const res = await searchCrates(cli.args[0], opts);
    console.log(res);
  } catch (err) {
    // FIXME: handle error
    console.error(err);
    return 1;
  }
  return 0;
}

main();
