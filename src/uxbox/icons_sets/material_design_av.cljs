(ns uxbox.icons-sets.material-design-av
  (:require [uxbox.icons-sets.register]
            [uxbox.pubsub :as pubsub]))

(def material-design-av (sorted-map
   :album {
    :name "Album"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M24 4C12.95 4 4 12.95 4 24s8.95 20 20 20 20-8.95 20-20S35.05 4 24 4zm0 29c-4.97 0-9-4.03-9-9s4.03-9 9-9 9 4.03 9 9-4.03 9-9 9zm0-11c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2z"}]}
   :av-timer {
    :name "AV Timer"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M22 34c0 1.1.9 2 2 2s2-.9 2-2-.9-2-2-2-2 .9-2 2zm0-28v8h4v-3.84c6.78.97 12 6.79 12 13.84 0 7.73-6.27 14-14 14s-14-6.27-14-14c0-3.36 1.18-6.43 3.15-8.85L24 26l2.83-2.83-13.6-13.6-.02.04C8.84 12.89 6 18.11 6 24c0 9.94 8.04 18 17.99 18S42 33.94 42 24 33.94 6 23.99 6H22zm14 18c0-1.1-.9-2-2-2s-2 .9-2 2 .9 2 2 2 2-.9 2-2zm-24 0c0 1.1.9 2 2 2s2-.9 2-2-.9-2-2-2-2 .9-2 2z"}]}
   :closed-caption {
    :name "Closed Caption"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M38 8H10c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h28c2.21 0 4-1.79 4-4V12c0-2.21-1.79-4-4-4zM22 22h-3v-1h-4v6h4v-1h3v2c0 1.1-.89 2-2 2h-6c-1.11 0-2-.9-2-2v-8c0-1.1.89-2 2-2h6c1.11 0 2 .9 2 2v2zm14 0h-3v-1h-4v6h4v-1h3v2c0 1.1-.89 2-2 2h-6c-1.11 0-2-.9-2-2v-8c0-1.1.89-2 2-2h6c1.11 0 2 .9 2 2v2z"}]}
   :equalizer {
    :name "Equalizer"
    :svg [:path
     {:style {:stroke nil}
      :d "M20 40h8V8h-8v32zM8 40h8V24H8v16zm24-22v22h8V18h-8z"}]}
   :explicit {
    :name "Explicit"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M38 6H10c-2.21 0-4 1.79-4 4v28c0 2.21 1.79 4 4 4h28c2.21 0 4-1.79 4-4V10c0-2.21-1.79-4-4-4zm-8 12h-8v4h8v4h-8v4h8v4H18V14h12v4z"}]}
   :fast-forward {
    :name "Fast Forward"
    :svg [:path
     {:style {:stroke nil}
      :d "M8 36l17-12L8 12v24zm18-24v24l17-12-17-12z"}]}
   :fast-rewind {
    :name "Fast Rewind"
    :svg [:path
     {:style {:stroke nil}
      :d "M22 36V12L5 24l17 12zm1-12l17 12V12L23 24z"}]}
   :games {
    :name "Games"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M30 15V4H18v11l6 6 6-6zm-15 3H4v12h11l6-6-6-6zm3 15v11h12V33l-6-6-6 6zm15-15l-6 6 6 6h11V18H33z"}]}
   :hearing {
    :name "Hearing"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M34 40c-.57 0-1.13-.12-1.53-.3-1.41-.75-2.43-1.77-3.42-4.77-1.03-3.11-2.94-4.58-4.79-6.01-1.58-1.22-3.22-2.48-4.63-5.05C18.58 21.95 18 19.86 18 18c0-5.61 4.39-10 10-10s10 4.39 10 10h4c0-7.85-6.15-14-14-14s-14 6.15-14 14c0 2.53.76 5.3 2.13 7.8 1.82 3.31 3.97 4.96 5.7 6.3 1.62 1.25 2.79 2.15 3.43 4.09 1.2 3.63 2.75 5.68 5.45 7.1 1.04.47 2.14.71 3.29.71 4.41 0 8-3.59 8-8h-4c0 2.21-1.79 4-4 4zM15.27 5.27l-2.83-2.83C8.46 6.42 6 11.92 6 18s2.46 11.58 6.44 15.56l2.83-2.83C12.01 27.47 10 22.97 10 18s2.01-9.47 5.27-12.73zM23 18c0 2.76 2.24 5 5 5s5-2.24 5-5-2.24-5-5-5-5 2.24-5 5z"}]}
   :high-quality {
    :name "High Quality"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M38 8H10c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h28c2.21 0 4-1.79 4-4V12c0-2.21-1.79-4-4-4zM22 30h-3v-4h-4v4h-3V18h3v5h4v-5h3v12zm14-2c0 1.1-.89 2-2 2h-1.5v3h-3v-3H28c-1.11 0-2-.9-2-2v-8c0-1.1.89-2 2-2h6c1.11 0 2 .9 2 2v8zm-7-1h4v-6h-4v6z"}]}
   :loop {
    :name "Loop"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M24 8V2l-8 8 8 8v-6c6.63 0 12 5.37 12 12 0 2.03-.51 3.93-1.39 5.61l2.92 2.92C39.08 30.05 40 27.14 40 24c0-8.84-7.16-16-16-16zm0 28c-6.63 0-12-5.37-12-12 0-2.03.51-3.93 1.39-5.61l-2.92-2.92C8.92 17.95 8 20.86 8 24c0 8.84 7.16 16 16 16v6l8-8-8-8v6z"}]}
   :mic {
    :name "Mic"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M24 28c3.31 0 5.98-2.69 5.98-6L30 10c0-3.32-2.68-6-6-6-3.31 0-6 2.68-6 6v12c0 3.31 2.69 6 6 6zm10.6-6c0 6-5.07 10.2-10.6 10.2-5.52 0-10.6-4.2-10.6-10.2H10c0 6.83 5.44 12.47 12 13.44V42h4v-6.56c6.56-.97 12-6.61 12-13.44h-3.4z"}]}
   :mic-none {
    :name "Mic None"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M24 28c3.31 0 5.98-2.69 5.98-6L30 10c0-3.32-2.68-6-6-6-3.31 0-6 2.68-6 6v12c0 3.31 2.69 6 6 6zM21.6 9.8c0-1.32 1.08-2.4 2.4-2.4 1.32 0 2.4 1.08 2.4 2.4l-.02 12.4c0 1.32-1.07 2.4-2.38 2.4-1.32 0-2.4-1.08-2.4-2.4V9.8zm13 12.2c0 6-5.07 10.2-10.6 10.2-5.52 0-10.6-4.2-10.6-10.2H10c0 6.83 5.44 12.47 12 13.44V42h4v-6.56c6.56-.97 12-6.61 12-13.44h-3.4z"}]}
   :mic-off {
    :name "Mic Off"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M38 22h-3.4c0 1.49-.31 2.87-.87 4.1l2.46 2.46C37.33 26.61 38 24.38 38 22zm-8.03.33c0-.11.03-.22.03-.33V10c0-3.32-2.69-6-6-6s-6 2.68-6 6v.37l11.97 11.96zM8.55 6L6 8.55l12.02 12.02v1.44c0 3.31 2.67 6 5.98 6 .45 0 .88-.06 1.3-.15l3.32 3.32c-1.43.66-3 1.03-4.62 1.03-5.52 0-10.6-4.2-10.6-10.2H10c0 6.83 5.44 12.47 12 13.44V42h4v-6.56c1.81-.27 3.53-.9 5.08-1.81L39.45 42 42 39.46 8.55 6z"}]}
   :movie {
    :name "Movie"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M36 8l4 8h-6l-4-8h-4l4 8h-6l-4-8h-4l4 8h-6l-4-8H8c-2.21 0-3.98 1.79-3.98 4L4 36c0 2.21 1.79 4 4 4h32c2.21 0 4-1.79 4-4V8h-8z"}]}
   :my-library-add {
    :name "My Library Add"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M8 12H4v28c0 2.21 1.79 4 4 4h28v-4H8V12zm32-8H16c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h24c2.21 0 4-1.79 4-4V8c0-2.21-1.79-4-4-4zm-2 18h-8v8h-4v-8h-8v-4h8v-8h4v8h8v4z"}]}
   :my-library-books {
    :name "My Library Books"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M8 12H4v28c0 2.21 1.79 4 4 4h28v-4H8V12zm32-8H16c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h24c2.21 0 4-1.79 4-4V8c0-2.21-1.79-4-4-4zm-2 18H18v-4h20v4zm-8 8H18v-4h12v4zm8-16H18v-4h20v4z"}]}
   :my-library-music {
    :name "My Library Music"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M40 4H16c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h24c2.21 0 4-1.79 4-4V8c0-2.21-1.79-4-4-4zm-4 10h-6v11c0 2.76-2.24 5-5 5s-5-2.24-5-5 2.24-5 5-5c1.13 0 2.16.39 3 1.02V10h8v4zM8 12H4v28c0 2.21 1.79 4 4 4h28v-4H8V12z"}]}
   :new-releases {
    :name "New Releases"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M46 24l-4.88-5.56.68-7.37-7.22-1.63-3.78-6.36L24 6l-6.8-2.92-3.78 6.36-7.22 1.63.68 7.37L2 24l4.88 5.56-.68 7.37 7.22 1.63 3.78 6.36L24 42l6.8 2.92 3.78-6.36 7.22-1.63-.68-7.37L46 24zM26 34h-4v-4h4v4zm0-8h-4V14h4v12z"}]}
   :not-interested {
    :name "Not Interested"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M24 4C12.95 4 4 12.95 4 24s8.95 20 20 20 20-8.95 20-20S35.05 4 24 4zm0 36c-8.84 0-16-7.16-16-16 0-3.7 1.27-7.09 3.37-9.8L33.8 36.63C31.09 38.73 27.7 40 24 40zm12.63-6.2L14.2 11.37C16.91 9.27 20.3 8 24 8c8.84 0 16 7.16 16 16 0 3.7-1.27 7.09-3.37 9.8z"}]}
   :pause {
    :name "Pause"
    :svg [:path
     {:style {:stroke nil}
      :d "M12 38h8V10h-8v28zm16-28v28h8V10h-8z"}]}
   :pause-circle-fill {
    :name "Pause Circle Fill"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M24 4C12.95 4 4 12.95 4 24s8.95 20 20 20 20-8.95 20-20S35.05 4 24 4zm-2 28h-4V16h4v16zm8 0h-4V16h4v16z"}]}
   :pause-circle-outline {
    :name "Pause Circle Outline"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M18 32h4V16h-4v16zm6-28C12.95 4 4 12.95 4 24s8.95 20 20 20 20-8.95 20-20S35.05 4 24 4zm0 36c-8.82 0-16-7.18-16-16S15.18 8 24 8s16 7.18 16 16-7.18 16-16 16zm2-8h4V16h-4v16z"}]}
   :play {
    :name "Play"
    :svg [:path {:style {:stroke nil} :d "M16 10v28l22-14z"}]}
   :play-circle-fill {
    :name "Play Circle Fill"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M24 4C12.95 4 4 12.95 4 24s8.95 20 20 20 20-8.95 20-20S35.05 4 24 4zm-4 29V15l12 9-12 9z"}]}
   :play-circle-outline {
    :name "Play Circle Outline"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M20 33l12-9-12-9v18zm4-29C12.95 4 4 12.95 4 24s8.95 20 20 20 20-8.95 20-20S35.05 4 24 4zm0 36c-8.82 0-16-7.18-16-16S15.18 8 24 8s16 7.18 16 16-7.18 16-16 16z"}]}
   :play-shopping-bag {
    :name "Play Shopping Bag"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M32 12V8c0-2.21-1.79-4-4-4h-8c-2.21 0-4 1.79-4 4v4H4v26c0 2.21 1.79 4 4 4h32c2.21 0 4-1.79 4-4V12H32zM20 8h8v4h-8V8zm-2 28V18l15 8-15 10z"}]}
   :playlist-add {
    :name "Playlist Add"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M28 20H4v4h24v-4zm0-8H4v4h24v-4zm8 16v-8h-4v8h-8v4h8v8h4v-8h8v-4h-8zM4 32h16v-4H4v4z"}]}
   :queue {
    :name "Queue"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M8 12H4v28c0 2.21 1.79 4 4 4h28v-4H8V12zm32-8H16c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h24c2.21 0 4-1.79 4-4V8c0-2.21-1.79-4-4-4zm-2 18h-8v8h-4v-8h-8v-4h8v-8h4v8h8v4z"}]}
   :queue-music {
    :name "Queue Music"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M30 12H6v4h24v-4zm0 8H6v4h24v-4zM6 32h16v-4H6v4zm28-20v16.37c-.63-.23-1.29-.37-2-.37-3.31 0-6 2.69-6 6s2.69 6 6 6 6-2.69 6-6V16h6v-4H34z"}]}
   :radio {
    :name "Radio"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M6.47 12.3C5.02 12.87 4 14.33 4 16v24c0 2.21 1.79 4 4 4h32c2.21 0 4-1.79 4-4V16c0-2.21-1.79-4-4-4H16.61l16.53-6.67L31.76 2 6.47 12.3zM14 40c-3.31 0-6-2.69-6-6s2.69-6 6-6 6 2.69 6 6-2.69 6-6 6zm26-16h-4v-4h-4v4H8v-8h32v8z"}]}
   :recent-actors {
    :name "Recent Actors"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M42 10v28h4V10h-4zm-8 28h4V10h-4v28zm-6-28H4c-1.1 0-2 .9-2 2v24c0 1.1.9 2 2 2h24c1.1 0 2-.9 2-2V12c0-1.1-.9-2-2-2zm-12 5.5c2.48 0 4.5 2.02 4.5 4.5 0 2.49-2.02 4.5-4.5 4.5s-4.5-2.01-4.5-4.5c0-2.48 2.02-4.5 4.5-4.5zM25 34H7v-1.5c0-3 6-4.5 9-4.5s9 1.5 9 4.5V34z"}]}
   :repeat {
    :name "Repeat"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M14 14h20v6l8-8-8-8v6H10v12h4v-8zm20 20H14v-6l-8 8 8 8v-6h24V26h-4v8z"}]}
   :repeat-one {
    :name "Repeat One"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M14 14h20v6l8-8-8-8v6H10v12h4v-8zm20 20H14v-6l-8 8 8 8v-6h24V26h-4v8zm-8-4V18h-2l-4 2v2h3v8h3z"}]}
   :replay {
    :name "Replay"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M24 10V2L14 12l10 10v-8c6.63 0 12 5.37 12 12s-5.37 12-12 12-12-5.37-12-12H8c0 8.84 7.16 16 16 16s16-7.16 16-16-7.16-16-16-16z"}]}
   :shuffle {
    :name "Shuffle"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M21.17 18.34L10.83 8 8 10.83l10.34 10.34 2.83-2.83zM29 8l4.09 4.09L8 37.17 10.83 40l25.09-25.09L40 19V8H29zm.66 18.83l-2.83 2.83 6.26 6.26L29 40h11V29l-4.09 4.09-6.25-6.26z"}]}
   :skip-next {
    :name "Skip Next"
    :svg [:path
     {:style {:stroke nil}
      :d "M12 36l17-12-17-12v24zm20-24v24h4V12h-4z"}]}
   :skip-previous {
    :name "Skip Previous"
    :svg [:path
     {:style {:stroke nil} :d "M12 12h4v24h-4zm7 12l17 12V12z"}]}
   :snooze {
    :name "Snooze"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M15.76 6.78l-2.57-3.06L4 11.43l2.57 3.06 9.19-7.71zM44 11.44l-9.19-7.71-2.57 3.06 9.19 7.71L44 11.44zM23.99 8C14.04 8 6 16.06 6 26s8.04 18 17.99 18S42 35.94 42 26 33.94 8 23.99 8zM24 40c-7.73 0-14-6.27-14-14s6.27-14 14-14 14 6.27 14 14-6.26 14-14 14zm-6-18h7.25L18 30.4V34h12v-4h-7.25L30 21.6V18H18v4z"}]}
   :stop {
    :name "Stop"
    :svg [:path {:style {:stroke nil} :d "M12 12h24v24H12z"}]}
   :subtitles {
    :name "Subtitles"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M40 8H8c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h32c2.21 0 4-1.79 4-4V12c0-2.21-1.79-4-4-4zM8 24h8v4H8v-4zm20 12H8v-4h20v4zm12 0h-8v-4h8v4zm0-8H20v-4h20v4z"}]}
   :surround-sound {
    :name "Surround Sound"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M40 8H8c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h32c2.21 0 4-1.79 4-4V12c0-2.21-1.79-4-4-4zM15.51 32.49l-2.83 2.83C9.57 32.19 8 28.1 8 24c0-4.1 1.57-8.19 4.69-11.31l2.83 2.83C13.18 17.85 12 20.93 12 24c0 3.07 1.17 6.15 3.51 8.49zM24 32c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8zm11.31 3.31l-2.83-2.83C34.83 30.15 36 27.07 36 24c0-3.07-1.18-6.15-3.51-8.49l2.83-2.83C38.43 15.81 40 19.9 40 24c0 4.1-1.57 8.19-4.69 11.31zM24 20c-2.21 0-4 1.79-4 4s1.79 4 4 4 4-1.79 4-4-1.79-4-4-4z"}]}
   :video-collection {
    :name "Video Collection"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M8 12H4v28c0 2.21 1.79 4 4 4h28v-4H8V12zm32-8H16c-2.21 0-4 1.79-4 4v24c0 2.21 1.79 4 4 4h24c2.21 0 4-1.79 4-4V8c0-2.21-1.79-4-4-4zM24 29V11l12 9-12 9z"}]}
   :videocam {
    :name "Videocam"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M34 21v-7c0-1.1-.9-2-2-2H8c-1.1 0-2 .9-2 2v20c0 1.1.9 2 2 2h24c1.1 0 2-.9 2-2v-7l8 8V13l-8 8z"}]}
   :videocam-off {
    :name "Videocam Off"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M42 13l-8 8v-7c0-1.1-.9-2-2-2H19.64L42 34.36V13zM6.55 4L4 6.55 9.45 12H8c-1.1 0-2 .9-2 2v20c0 1.1.9 2 2 2h24c.41 0 .77-.15 1.09-.37L39.46 42 42 39.45 6.55 4z"}]}
   :volume-down {
    :name "Volume Down"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M37 24c0-3.53-2.04-6.58-5-8.05v16.11c2.96-1.48 5-4.53 5-8.06zm-27-6v12h8l10 10V8L18 18h-8z"}]}
   :volume-mute {
    :name "Volume Mute"
    :svg [:path {:style {:stroke nil} :d "M14 18v12h8l10 10V8L22 18h-8z"}]}
   :volume-off {
    :name "Volume Off"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M33 24c0-3.53-2.04-6.58-5-8.05v4.42l4.91 4.91c.06-.42.09-.85.09-1.28zm5 0c0 1.88-.41 3.65-1.08 5.28l3.03 3.03C41.25 29.82 42 27 42 24c0-8.56-5.99-15.72-14-17.54v4.13c5.78 1.72 10 7.07 10 13.41zM8.55 6L6 8.55 15.45 18H6v12h8l10 10V26.55l8.51 8.51c-1.34 1.03-2.85 1.86-4.51 2.36v4.13c2.75-.63 5.26-1.89 7.37-3.62L39.45 42 42 39.45l-18-18L8.55 6zM24 8l-4.18 4.18L24 16.36V8z"}]}
   :volume-up {
    :name "Volume Up"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M6 18v12h8l10 10V8L14 18H6zm27 6c0-3.53-2.04-6.58-5-8.05v16.11c2.96-1.48 5-4.53 5-8.06zM28 6.46v4.13c5.78 1.72 10 7.07 10 13.41s-4.22 11.69-10 13.41v4.13c8.01-1.82 14-8.97 14-17.54S36.01 8.28 28 6.46z"}]}
   :web {
    :name "Web"
    :svg [:path
     {:style {:stroke nil}
      :d
      "M40 8H8c-2.21 0-3.98 1.79-3.98 4L4 36c0 2.21 1.79 4 4 4h32c2.21 0 4-1.79 4-4V12c0-2.21-1.79-4-4-4zM30 36H8v-8h22v8zm0-10H8v-8h22v8zm10 10h-8V18h8v18z"}]}))

(pubsub/publish! [:register-icons-set {:key :material-design-av
                                       :name "Material Design (Av)"
                                       :icons material-design-av}])
