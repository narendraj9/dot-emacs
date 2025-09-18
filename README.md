narendraj9's dot-emacs 
=====================================

My Emacs configuration.


Setup
-----

If you would like to try it, use the following commands:

```bash
$ git clone "https://github.com/narendraj9/dot-emacs.git" "~/Downloads/dot-emacs"
$ emacs -Q -l "~/Downloads/dot-emacs/init.el"

```

The above commands should not affect your existing Emacs
configuration.

The first time you start Emacs with this configuration, it would try
to install all the packages from Melpa, which will take some time.


Browser Integration
-------------------

This configuration includes atomic-chrome support for editing browser text areas directly in Emacs. To use this feature:

### For Chrome
Install the [GhostText](https://chrome.google.com/webstore/detail/ghosttext/godiecgffnchndlihlpaajjcplehddca) Chrome extension.

### For Firefox  
Install the [GhostText](https://addons.mozilla.org/en-US/firefox/addon/ghosttext/) Firefox extension.

### Usage
1. Start Emacs with this configuration
2. Focus on any text area in your browser
3. Click the GhostText extension icon
4. The text area content will open in Emacs for editing
5. Changes are synchronized in real-time between browser and Emacs
6. Press `C-c C-c` in Emacs to finish editing


