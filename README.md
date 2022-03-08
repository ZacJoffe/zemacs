# zemacs
A (highly) opinionated and evil Emacs configuration, inspired by [Doom](https://github.com/hlissner/doom-emacs).

## Features
- Package management with `straight.el` and configuration with `use-package`
- Completion with `vertico`, `consult`, `marginalia`, and `orderless`
- Key bindings defined through `general.el`
  - `doom`/`spacemacs`-esqe mnemonic bindings with `space` leader
- `evil` everything!
- Workspaces hacked on top of `perspective.el` (similar to Doom's workspace module)
- Pretty theming with custom `doom-modeline` and `doom-themes` integration
- Organize notes with `org-roam`

### Motivation and Philosophy
This project was mainly for learning elisp and the inner-workings of Emacs. Not unlike most Emacs users, I have a specific workflow that I rely on to do my job, and this distribution is modeled around that. I try to keep the amount of packages minimal, though often heavily configure the packages. I rely on `use-package` to conveniently load and configure packages, and `general.el` to simplify key bindings (which can be especially arduous when evil is involved). Code snippets taken from other repos, wiki pages, Reddit threads, GitHub issues, etc. are linked for accreditation and posterity.

<!-- However, this is first and foremost a tool that I use to write code and do my job, so it needs to be  There's still a lot of work to be done, but this configuration has reached parity to the features I relied on in Doom. -->

TODO
