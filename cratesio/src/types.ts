export const crateFields = [
  "categories",
  "created_at",
  "description",
  "documentation",
  "downloads",
  "exact_match",
  "homepage",
  "id",
  "keywords",
  "max_stable_version",
  "max_version",
  "name",
  "recent_downloads",
  "repository",
  "updated_at",
  "versions",
] as const;
export type CrateField = typeof crateFields[number];

export interface Crate {
  id: string;
  name: string;
  description: string | null;
  documentation: string | null;
  homepage: string | null;
  repository: string | null;
  versions: number[] | null;
  downloads: number;
  recent_downloads: number | null;
  categories: string[] | null;
  keywords: string[] | null;
  max_version: string;
  max_stable_version: string;
  created_at: string;                             // datetime utc
  updated_at: string;
  exact_match: boolean | null;
  // [key: string]: any;
}

export interface Meta {
  next_page: string | null;
  prev_page: string | null;
  total: number;
}

export interface SearchResult {
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
  raw?: boolean;
  page?: string;
  perPage?: string;
  sort?: Sort;
  fields?: 'default' | CrateField | CrateField[];
}
