declare module 'asciinema-player' {
  export function create(
    src: string,
    element: HTMLElement,
    opts?: Record<string, any>,
  ): {dispose: () => void};
}
