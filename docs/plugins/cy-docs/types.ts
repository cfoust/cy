export interface Symbol {
  Name: string;
  Docstring: string;
  Link: string;
  Macro: boolean;
}

export interface Param {
  Name: string;
  Type: string;
  Default: string;
  Docstring: string;
}

export interface Binding {
  Tag: string;
  Source: string;
  Function: string;
  Sequence: string[];
}

export interface RawApiData {
  Symbols: Symbol[];
  Parameters: Param[];
  Binds: Binding[];
  Frames: string[];
  Animations: string[];
}

export interface ResolvedBinding {
  Tag: string;
  Source: string;
  Function: Symbol | null;
  Sequence: string[];
}

export interface ApiData {
  symbols: Symbol[];
  params: Param[];
  bindings: ResolvedBinding[];
  frames: string[];
  animations: string[];
  symbolLookup: Record<string, Symbol>;
  paramLookup: Record<string, Param>;
}

export type StoryAssetType = 'png' | 'gif' | 'cast';
