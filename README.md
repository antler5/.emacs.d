<!-- SPDX-FileCopyrightText: 2024 antlers <antlers@illucid.net> -->
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- This file is generated from $< -->
<!-- Do not edit it directly -->
<h1>.emacs.d</h1>

## Cloning and Authenticating

``` bash
git clone 'https://github.com/antler5/.emacs.d' && cd .emacs.d
```

You may verify that each commit in this branch has been signed by an
authorized contributer via GNU Guix's
[authentication](https://guix.gnu.org/manual/en/html_node/Invoking-guix-git-authenticate.html)
mechanism.

``` bash
git fetch origin keyring:keyring
guix git authenticate \
  '91ab69b25fb8d64d6e73d2eab3b2ad008a829a16' \
  'DACB 035F B9B0 EE9C 7E13  1AAA C310 15D9 6620 A955'
```

## Running

I use a custom use-package keyword to pull in guix packages. They just
get stripped from the elisp and "tangled" out into a command like this:

> [!NOTE]
> I don't run this (probably out-of-date) psudo-code directly.</br>
> This is a representation of the command assembled by <code>run.sh</code>,</br>
> extracted via `set -x` and edited for clarity.

``` bash
antlers@citrus ~/.emacs.d$ ./run.sh
+ [...]
+ guix shell --pure --container --network                                \
+   emacs emacs-use-package emacs-dash alsa-lib at-spi2-core bzip2 cairo \
+   cups dbus gcc-toolchain gdk-pixbuf glib gst-plugins-base gstreamer   \
+   gtk+ jq libxcomposite libxkbcommon libxkbfile libxrandr libxrender   \
+   libxtst mesa mit-krb5 mysql node nss pango patchelf pcsc-lite        \
+   postgresql pulseaudio python speech-dispatcher unixodbc wmctrl       \
+   xcb-util-cursor xcb-util-image xcb-util-keysyms xcb-util-wm xdotool  \
+   python-psutil python-pygments python-qtconsole                       \
+   -- emacs $@
```

## Defined Symbols

<table>
<thead>
<tr class="header">
<th>Symbol</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/mode-line-percent&amp;type=code">antlers/mode-line-percent</a></td>
<td>Return <code class="verbatim">point</code> position (as a
percentage) and buffer length (in lines).</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/mode-line-format&amp;type=code">antlers/mode-line-format</a></td>
<td>Return <code class="verbatim">mode-line-format</code> with <code
class="verbatim">TITLE</code> and widgets <code
class="verbatim">CENTER</code>, <code class="verbatim">RIGHT</code>, and
<code class="verbatim">END</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/find-file&amp;type=code">antlers/find-file</a></td>
<td>Edit file <code class="verbatim">FILENAME</code> (it’s <code
class="verbatim">find-file</code>, but a <code
class="verbatim">command</code>).</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/clear-vc-state-cache&amp;type=code">antlers/clear-vc-state-cache</a></td>
<td>Clear <code class="verbatim">antlers/vc-state-cache</code> and
trigger gutter refresh.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/define-icon-adapter&amp;type=code">antlers/define-icon-adapter</a></td>
<td>Install broad <code class="verbatim">all-the-icons</code> &lt;-&gt;
<code class="verbatim">nerd-icons</code> shims.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/magit-post-refresh-hook&amp;type=code">antlers/magit-post-refresh-hook</a></td>
<td>Revert <code class="verbatim">dired</code> buffers for <code
class="verbatim">magit</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/set-mode-line-format&amp;type=code">antlers/set-mode-line-format</a></td>
<td>Sets the default <code class="verbatim">mode-line-format</code> and
updates open buffers.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/vc-state-cache&amp;type=code">antlers/vc-state-cache</a></td>
<td><code class="verbatim">vc-state</code> cache for <code
class="verbatim">dirvish-git-gutter</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/grep-elisp-load-path&amp;type=code">antlers/grep-elisp-load-path</a></td>
<td>Run <code class="verbatim">grep</code>, searching for <code
class="verbatim">REGEX</code> in <code
class="verbatim">elisp-load-path-roots</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/symbol-to-gh-search-link&amp;type=code">antlers/symbol-to-gh-search-link</a></td>
<td>Return an Org-formatted link to a GitHub search for <code
class="verbatim">SYM</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/dirvish--mode-line-fmt-setter&amp;type=code">antlers/dirvish–mode-line-fmt-setter</a></td>
<td>Call <code class="verbatim">antlers/mode-line-format</code> for
dirvish buffers.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/all-the-icons-fileicon-jupyter&amp;type=code">antlers/all-the-icons-fileicon-jupyter</a></td>
<td><code class="verbatim">all-the-icons</code> shim powered by <code
class="verbatim">nerd-icons</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/dirvish-file-modes-ml&amp;type=code">antlers/dirvish-file-modes-ml</a></td>
<td>Replicates <code class="verbatim">diredfl</code> colors for <code
class="verbatim">dirvish-file-modes-ml</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/collect-plist&amp;type=code">antlers/collect-plist</a></td>
<td>Destructively collect leading <code class="verbatim">plist</code>
from <code class="verbatim">ARGS</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/dirvish-free-space-ml&amp;type=code">antlers/dirvish-free-space-ml</a></td>
<td>Propertize <code class="verbatim">STR</code> with face <code
class="verbatim">dired-ignore</code> for <code
class="verbatim">dirvish-free-space-ml</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/mode-line-file-size&amp;type=code">antlers/mode-line-file-size</a></td>
<td>Return the file-size of <code
class="verbatim">buffer-file-name</code>, formatted for the
mode-line.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/all-the-icons-alltheicon-html5&amp;type=code">antlers/all-the-icons-alltheicon-html5</a></td>
<td><code class="verbatim">all-the-icons</code> shim powered by <code
class="verbatim">nerd-icons</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/mode-line-status&amp;type=code">antlers/mode-line-status</a></td>
<td>Return a <code class="verbatim">nerd-icon</code> representing the
state of the current buffer.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/restore-frame-title-format&amp;type=code">antlers/restore-frame-title-format</a></td>
<td>Restore <code class="verbatim">frame-title-format</code> once Emacs
has initialized.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/all-the-icons-fileicon-org&amp;type=code">antlers/all-the-icons-fileicon-org</a></td>
<td><code class="verbatim">all-the-icons</code> shim powered by <code
class="verbatim">nerd-icons</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/dirvish-collapse--cache&amp;type=code">antlers/dirvish-collapse–cache</a></td>
<td>Replace <code class="verbatim">backslash</code> with <code
class="verbatim">vertical-pipe</code> for <code
class="verbatim">dirvish-collapse--cache</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/symbol-to-synopsis&amp;type=code">antlers/symbol-to-synopsis</a></td>
<td>Return a short description of <code
class="verbatim">SYM</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/s-subtract&amp;type=code">antlers/s-subtract</a></td>
<td>Return a numeric string which is <code class="verbatim">N</code>
less than numeric string <code class="verbatim">STR</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/disable-indicate-buffer-boundaries&amp;type=code">antlers/disable-indicate-buffer-boundaries</a></td>
<td>Disable <code class="verbatim">indicate-buffer-boundaries</code> in
<code class="verbatim">Dired</code> buffers.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/symbol-concat&amp;type=code">antlers/symbol-concat</a></td>
<td>Flatten symbols in list <code class="verbatim">SYMS</code> into a
new symbol.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/mode-line-vcs&amp;type=code">antlers/mode-line-vcs</a></td>
<td>Return current buffer’s <code class="verbatim">vc-state</code>,
truncated, with a <code class="verbatim">nerd-icon</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/append-to-path&amp;type=code">antlers/append-to-path</a></td>
<td>Add <code class="verbatim">DIR</code> to <code
class="verbatim">PATH</code>, duplicating it and updating <code
class="verbatim">exec-path</code> when appropriate.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/dirvish--render-attrs&amp;type=code">antlers/dirvish–render-attrs</a></td>
<td>Mark gutter up-to-date for <code
class="verbatim">render-attrs</code>.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/dirvish-subtree-remove&amp;type=code">antlers/dirvish-subtree-remove</a></td>
<td>Clear gutter for <code
class="verbatim">dirvish-subtree-remove</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/clear&amp;type=code">antlers/clear</a></td>
<td>Clear <code class="verbatim">eshell</code> (<code
class="verbatim">eshell/clear</code> errors out).</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/dirvish-find-entry-hook&amp;type=code">antlers/dirvish-find-entry-hook</a></td>
<td>Clear gutter for <code
class="verbatim">dirvish-find-entry-hook</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/eaf-install-and-update&amp;type=code">antlers/eaf-install-and-update</a></td>
<td>Install and update <code class="verbatim">EAF Core</code> and the
subset of modules that I use.</td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/mode-line-dedicated&amp;type=code">antlers/mode-line-dedicated</a></td>
<td>Return a pin <code class="verbatim">nerd-icon</code> when <code
class="verbatim">current-buffer</code> is <code
class="verbatim">dedicated</code>.</td>
</tr>
<tr class="even">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/init.el&amp;type=code">antlers/init.el</a></td>
<td>Path to <code class="verbatim">init.el</code></td>
</tr>
<tr class="odd">
<td><a
href="https://github.com/search?q=repo%3Aantler5%2F.emacs.d%20+NOT+path%3AREADME.md+antlers/define-icon-mappings&amp;type=code">antlers/define-icon-mappings</a></td>
<td>Install individual <code class="verbatim">all-the-icons</code>
&lt;-&gt; <code class="verbatim">nerd-icons</code> shims.</td>
</tr>
</tbody>
</table>
