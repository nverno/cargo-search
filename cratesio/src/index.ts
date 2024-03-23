import { Command, Option } from 'commander';
import { searchCrates } from './api';
import { Crate, CliOptions, SearchResponse, sortOptions, SearchOptions, Sort } from './types';

const cli = new Command();

async function main() {
  cli.version("0.0.1")
    .description("Search crates on crates.io")
    .name("cratesio")
    .requiredOption('-q, --query <string>', 'search query')
    .option('-p, --page <number>', 'page number', '1')
    .option('-c, --per-page <number>', 'results per page', '10')
    .addOption(new Option('-s, --sort <string>', 'sort results')
      .choices(sortOptions)
      .default('relevance'))
    .option('--fields <fields...>', 'data fields to keep', 'name')
    .parse();

  const { query, perPage: per_page, page, sort, fields }: CliOptions = cli.opts();
  // const [ query, ..._ignored ] = cli.args

  const searchOpts: SearchOptions = {
    per_page,
    page,
    sort: sort as Sort
  };

  try {
    const resp = await searchCrates<SearchResponse>(query, searchOpts);
    const crates: Crate[] = resp['crates'];

    let res = typeof fields === 'string'
      ? crates.map((o) => o[fields as string]).join('\n')
      : crates.map((o) => (fields as string[])
        .reduce((acc, f) => ({ ...acc, [f]: o[f] }), {}));

    console.log(res);
  } catch (err) {
    console.error(err);
    return 1;
  }
  return 0;
}

main();
