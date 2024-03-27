import { Command, Option } from 'commander';
import { searchCrates } from './api';
import { Crate, CliOptions, sortOptions, SearchOptions, Sort, CrateField, crateFields } from './types';

const defaultFields: CrateField[] = [
  'description',
  'documentation',
  'exact_match',
  'homepage',
  'id',
  'max_stable_version',
  'max_version',
  'name',
  'repository',
];

const cli = new Command();

async function main() {
  cli.version("0.0.1")
    .description("Search crates on crates.io")
    .name("cratesio")
    .requiredOption('-q, --query <string>', 'search query')
    .option('-p, --page <number>', 'page number', '1')
    .option('-c, --per-page <number>', 'results per page', '1')
    .addOption(new Option('-s, --sort <string>', 'sort results')
      .choices(sortOptions)
      .default('relevance'))
    .addOption(new Option('--fields <fields...>', 'data fields to keep')
      .choices(['default'].concat(crateFields))
      .default(defaultFields))
    .option('--raw', 'dont serialize ouput as JSON', false)
    .parse();

  let { query, perPage: per_page, page, sort, fields, raw }: CliOptions = cli.opts();

  if (fields.length === 1 && fields[0] === 'default')
    fields = defaultFields;
  
  const searchOpts: SearchOptions = {
    per_page,
    page,
    sort: sort as Sort
  };

  try {
    const resp = await searchCrates(query, searchOpts);
    const crates: Crate[] = resp['crates'];

    let res: any = typeof fields === 'string'
      ? crates.map((o) => o[fields as string])
      : crates.map((o) => (fields as string[])
        .reduce((acc, f) => ({ ...acc, [f]: o[f] }), {}));

    if (!raw)
      res = JSON.stringify(res);

    console.log(res);
  } catch (err) {
    console.error(err);
    return 1;
  }
  return 0;
}

main();
