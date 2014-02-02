#moedict.el
###Looking up Chinese vocabulary via moedict API.

>啊就是一個拿來查萌典用的 Emacs major mode。

Moedict-mode is a major mode for looking up Traditional Chinese vocabulary in Emacs via moedict API (萌典, http://moedict.tw/).

#Screenshot
<a href="https://raw.github.com/kuanyui/moedict.el/master/screenshot.png"><img src="screenshot.png" width="710" height="379"/></a>

#Installation
**Note: Support Emacs 24.3 and above.**

Add following to your `~/.emacs.d/init.el` or `~/.emacs`:
```lisp
(add-to-list 'load-path "~/path/to/moedict/")
(require 'moedict)
(global-set-key (kbd "C-c m l") 'moedict-lookup)
(global-set-key (kbd "C-c m r") 'moedict-lookup-region)
```

If you're using Un*x-like OS and `curl` is available on your system, we suggest using it instead of `url.el` to avoid some strange GnuTLS errors:

```lisp
(setq moedict-use-curl t)
```

#Usage
- `moedict-lookup` to look up vocabulary via minibuffer.
- `moedict-lookup-region` to look up vocabulary under the selected region.

## Key-binding
If under `*moedict*` buffer:

| Key       | Command                        |
|-----------|--------------------------------|
| l         | `moedict-lookup`               |
| r         | `moedict-lookup-region`        |
| C-c C-b   | `moedict-backward-history`     |
| C-c C-f   | `moedict-forward-history`      |
| C-c D     | `moedict-clear-history`        |
| TAB       | `moedict-cursor-forward-word`  |
| Shift-TAB | `moedict-cursor-backward-word` |
| q         | Close/bury the buffer.         |
| h         | Display help.                  |

#License
BSD License.
