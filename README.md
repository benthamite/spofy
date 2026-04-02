# `spofy`: Spotify player for Emacs

`spofy` is a full-featured Spotify client that runs entirely inside Emacs. It communicates with the [Spotify Web API](https://developer.spotify.com/documentation/web-api) to let you control playback, search for music, browse albums and artists, manage playlists, and curate your library — all without leaving the editor.

## Overview

The main entry point is `M-x spofy`, which opens a dashboard buffer showing the currently playing track, your recently played tracks, and your playlists. From there you can search for content, browse into albums and artists, or manage your playlists with full CRUD operations.

A transient popup (bound to `C-c s` by default) provides quick access to playback controls, search, and browsing from any buffer. A configurable mode-line segment keeps you informed of the current track at all times.

Optional integrations extend `spofy` into the broader Emacs ecosystem:

- **Consult** — five async completion sources for searching tracks, albums, artists, playlists, and devices from the minibuffer.
- **Embark** — contextual actions (play, save, browse, copy URI) on any Spotify entity.
- **Org-mode** — a `spotify:` link type for storing, following, and exporting links to Spotify content.

## Installation

`spofy` requires Emacs 30.1 or later and depends on [transient](https://github.com/magit/transient) (available on GNU ELPA).

### package-vc (built-in since Emacs 30)

```emacs-lisp
(package-vc-install "https://github.com/benthamite/spofy")
```

### Elpaca

```emacs-lisp
(use-package spofy
  :ensure (spofy :host github :repo "benthamite/spofy"))
```

### straight.el

```emacs-lisp
(straight-use-package
 '(spofy :type git :host github :repo "benthamite/spofy"))
```

### Optional dependencies

- [consult](https://github.com/minad/consult) — for minibuffer completion sources (`spofy-consult.el`)
- [embark](https://github.com/oantolin/embark) — for contextual actions (`spofy-embark.el`)

## Quick start

1. Go to the [Spotify Developer Dashboard](https://developer.spotify.com/dashboard), create a new application, and add `http://127.0.0.1:8080/spofy/callback` as a redirect URI.

2. Configure your credentials:

   ```emacs-lisp
   (setopt spofy-client-id "your-client-id")
   (setopt spofy-client-secret "your-client-secret")
   ```

3. Run `M-x spofy-authenticate` to authorize via OAuth2 (opens your browser).

4. Run `M-x spofy` to open the dashboard, or enable `spofy-global-mode` to get the transient popup on `C-c s` and a mode-line display of the current track.

## Roadmap

- [ ] Personalized recommendations and radio based on seed tracks, artists, or genres
- [ ] Queue management (view upcoming tracks, add to queue)
- [ ] Album art display in GUI Emacs frames
- [ ] Social features (follow users, real-time collaborative playlist editing)

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](README.org).
