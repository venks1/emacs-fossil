<title>GNU Emacs Integration for Fossil</title>

This file contains a GNU Emacs VC backend for the Fossil version
control system.  You need GNU Emacs version 25.1 or later.

<h4>How Do I load Fossil support into emacs?</h4>

Short answer:

Install the vc-fossil package from the non-GNU ELPA package repository
then customize the vc-handled-backend variable by adding "Fossil" to
its list of names.

Alternative, slightly longer, answer:

Instead of installing from non-GNU ELPA you may clone this source
repository directly then add it manually to your Emacs setup as
follows.

Add this to your .emacs file, or cut and paste this into the *scratch*
buffer and do <code>C-x C-e</code> to execute it.

<pre>
(add-to-list 'vc-handled-backends 'Fossil t)
(load-file "/path/to/this/project/vc/el/vc-fossil.el")
</pre>

or if you would rather load the file lazily, compile the file (see
below) and use this instead:

<pre>
(add-to-list 'vc-handled-backends 'Fossil t)
(autoload 'vc-fossil-registered "/path/to/this/project/vc/el/vc-fossil.elc")
</pre>

To compile the file (with warnings) do the following:

<pre>
emacs -batch -q -no-site-file -eval '(byte-compile-file "vc-fossil.el")'
</pre>

then you can load vc-fossil.elc, which should be nominally faster.

<h5>Using <code>use-package</code></h5>

If you want to use <code>use-package</code> to setup
<code>vc-fossil</code>; then this snippet might be helpfull:

<pre>
(use-package vc-fossil
  ;; Keep from loading unnecessarily at startup.
  :defer t
  ;; This allows VC to load vc-fossil when needed.
  :init (add-to-list 'vc-handled-backends 'Fossil t))
</pre>

<h4>What emacs keys do I need?</h4>

This integrates fossil into emacs vc builtin mode.  The same keys will
work for fossil inside a fossil tree as for other SCMs.

A few are:

<table>
<tr><td><code>C-x v d</code></td><td>Run Fossil dired - like cvs-examine</td></tr>
<tr><td><code>C-x v l</code></td><td>When in a file, show log</td></tr>
<tr><td><code>v</code></td><td>In VC, do next action: merge/update/checkin</td></tr>
</table>

<h4>Credits</h4>

<ul>
  <li>Venkat Iyer &lt;venkat@comit.com&gt;:
    Original author.</li>
  <li>Alfred M. Szmidt &lt;ams@gnu.org&gt;:
    Random stuff.</li>
  <li>Barak A. Pearlmutter &lt;barak@pearlmutter.net&gt;:
    Major cleanup of my initial versions,</li>
  <li>Frank Fischer &lt;frank-fischer@shadow-soft.de&gt;:
    Colorized logs, annotate, fixes for diff output.</li>
  <li>Paul Onions &lt;ponions37@gmail.com&gt;:
    GNU emacs 25 support.</li>
</ul>
    
<h4>Mirrors</h4>

If you're reading this on github, this is mirrored for MELPA from
<a href="https://chiselapp.com/user/venks/repository/emacs-fossil">https://chiselapp.com/user/venks/repository/emacs-fossil</a>.

There is also a mirror at
<a href="https://tumbleweed.nu/r/vc-fossil">https://tumbleweed.nu/r/vc-fossil</a>
maintained by Alfred M. Szmidt.