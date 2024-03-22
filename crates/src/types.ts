export interface Crate {
  name: string;
  description: string | null;
  documentation: string | null;
  homepage: string | null;
  repository: string | null;
  versions: number[] | null;
  [key: string]: any;
}

export interface SearchOptions {
  page?: number;
  per_page?: number;
  sort?: string;
}

export interface Meta {
  next_page: string | null;
  prev_page: string | null;
  total: number;
}
