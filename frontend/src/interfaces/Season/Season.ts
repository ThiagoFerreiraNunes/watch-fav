import type { Episode } from "../Episode/Episode";

export interface Season {
  id: number;
  number: number;
  releaseYear: number;
  episodes: Episode[];
}
