export interface Crate {
  name: string;
  description: string | null;
  documentation: string | null;
  homepage: string | null;
  repository: string | null;
  versions: number[] | null;
  [key: string]: any;
}

export interface Meta {
  next_page: string | null;
  prev_page: string | null;
  total: number;
}

export interface SearchResponse {
  crates: Crate[];
  meta: Meta;
}

export const sortOptions = [
  'alpha',
  'relevance',
  'downloads',
  'recent-downloads',
  'recent-updates',
  'new'
] as const;
export type Sort = typeof sortOptions[number]

export interface SearchOptions {
  page?: number | string;
  per_page?: number | string;
  sort?: Sort;
}

export interface CliOptions {
  query: string;
  page?: string;
  perPage?: string;
  sort?: string;
  fields?: string | string[];
}
