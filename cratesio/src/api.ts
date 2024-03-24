import { SearchOptions, SearchResult } from "types";

const baseUrl = 'https://crates.io/api/v1';

const headers = new Headers();
headers.append("User-Agent",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0");

export async function searchCrates(query: string, options?: SearchOptions): Promise<SearchResult> {
  const url = new URL(`${baseUrl}/crates`);
  url.searchParams.set("q", encodeURIComponent(query));

  if (options)
    Object.entries(options).forEach(([k, v]) => v && url.searchParams.set(k, v.toString()));

  const res = await fetch(url, {
    method: "GET",
    headers,
    // credentials: "include",
  });

  if (!res.ok) {
    const txt = await res.text();
    const resp = JSON.parse(txt) as unknown;
    throw resp;
  }

  return res.json();
}
