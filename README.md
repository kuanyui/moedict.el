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
(global-set-key (kbd "C-c d m") 'moedict-lookup) ;[memorization] Dictionary => Moedict 
```

If you're using Un*x-like OS and `curl` is available on your system, we suggest using it instead of `url.el` to avoid some strange GnuTLS errors:

```lisp
(setq moedict-use-curl t)
```

#Usage
Press `C-c d m` `moedict-lookup` to look up vocabulary via minibuffer; when you've selected a region, this will lookup the selected word.

## Key-binding
If under `*moedict*` buffer:

| Key       | Command                        |
|-----------|--------------------------------|
| l         | `moedict-lookup`               |
| r         | `moedict-lookup-region`        |
| C-c C-b   | `moedict-history-backward`     |
| C-c C-f   | `moedict-history-forward`      |
| C-c D     | `moedict-history-clean`        |
| TAB       | `moedict-cursor-forward-word`  |
| Shift-TAB | `moedict-cursor-backward-word` |
| q         | Close/bury the buffer.         |
| h         | Display help.                  |

#License
Revised BSD License (3-Clause)

Copyright (c) 2014, kuanyui
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the organization nor the names of its contributors
      may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL KUANYUI BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
