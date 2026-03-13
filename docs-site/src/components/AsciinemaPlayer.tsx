import React, {useEffect, useRef, useState} from 'react';
import 'asciinema-player/dist/bundle/asciinema-player.css';

interface Props {
  cast: string;
  isStatic?: boolean;
}

export default function AsciinemaPlayer({cast, isStatic}: Props) {
  const ref = useRef<HTMLDivElement>(null);
  const playerRef = useRef<any>(null);
  const [error, setError] = useState(false);

  useEffect(() => {
    if (!ref.current) return;

    let disposed = false;
    const url = `/cy/images/${cast}.cast`;

    // Check that the .cast file actually exists before creating the player.
    // In dev mode the SPA fallback returns 200 with HTML for missing files,
    // so we verify the Content-Type is not HTML.
    fetch(url, {method: 'HEAD'}).then((res) => {
      if (disposed) return;
      const ct = res.headers.get('content-type') || '';
      if (!res.ok || ct.includes('text/html')) {
        setError(true);
        return;
      }

      import('asciinema-player').then((AsciinemaPlayerLib) => {
        if (disposed || !ref.current) return;

        const options: Record<string, any> = {
          autoPlay: true,
          loop: true,
        };

        if (isStatic) {
          options.speed = 5;
          options.markers = [[1.0, '']];
          options.pauseOnMarkers = true;
          options.controls = false;
        }

        playerRef.current = AsciinemaPlayerLib.create(
          url,
          ref.current,
          options,
        );
      });
    });

    return () => {
      disposed = true;
      if (playerRef.current?.dispose) {
        playerRef.current.dispose();
        playerRef.current = null;
      }
    };
  }, [cast, isStatic]);

  if (error) {
    return (
      <div style={{
        padding: '1rem',
        border: '1px solid var(--ifm-color-emphasis-300)',
        borderRadius: '4px',
        color: 'var(--ifm-color-emphasis-600)',
        fontFamily: 'var(--ifm-font-family-monospace)',
        fontSize: '0.85rem',
      }}>
        cast not found: {cast}.cast
      </div>
    );
  }

  return <div ref={ref} />;
}
