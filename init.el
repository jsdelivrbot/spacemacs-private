;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults

     ;; Chat
     erc

     ;; Checkers
     syntax-checking

     ;; Completion
     (auto-completion
      :variables
      auto-completion-enable-snippets-in-popup t
      auto-completion-enable-help-tooltip t
      auto-completion-enable-sort-by-usage t
      auto-completion-tab-key-behavior 'complete)
     ivy

     ;; Configuration files
     nginx

     ;; Emacs
     better-defaults
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          ;org-projectile-file "~/workspace/todos.org"
          )
                                        ; semantic

     ;; filetree
     treemacs

     ;; Frameworks
     ruby-on-rails

     ;; International support
     (chinese :variables
              chinese-enable-fcitx t)

     ;; Programming and markup languages
     (c-c++ :variables
             c-c++-enable-cmake-ide-support t
             c-c++-enable-rtags-support t
             c-c++-enable-google-style t
             c-c++-enable-google-newline t
             )
     (cmake :variables cmake-enable-cmake-ide-support t)
     emacs-lisp
     erlang
     (go :variables
         gofmt-command "goimports"
         go-use-gometalinter t)
     html
     (java)
     (javascript :variables javascript-disable-tern-port-files nil)
     lua
     markdown
     (ruby :variables
           ruby-enable-enh-ruby-mode t
           ruby-version-manager 'rvm
           ruby-test-runner 'chruby)
     (python :variables
             python-test-runner '(nose pytest)
             python-enable-yapf-format-on-save t
             python-sort-imports-on-save t)
     (rust :variables rust-format-on-save t)
     (scala :variables
            scala-indent:use-javadoc-style t
            scala-enable-eldoc t
            scala-auto-insert-asterisk-in-comments t
            scala-use-unicode-arrows t
            scala-auto-start-ensime t)
     scheme
     shell-scripts
     yaml

     ;; Operating systems
     osx

     ;; Pair programming
     floobits

     ;; Source control
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          ;; This is really creepy magit
          magit-revision-show-gravatars nil)
     ;; github
     (version-control :variables version-control-diff-tool 'git-gutter+
                      version-control-global-margin t)

     ;; Spacemacs distribution layers
     (spacemacs-layouts :variables layouts-enable-autosave t
                        layouts-autosave-delay 300)

     ;; Tags
     cscope
     (gtags :variables gtags-enable-by-default t)


     ;; Tools
     (dash
      :variables
      helm-dash-docset-newpath "~/Library/Application Support/Dash/DocSets")
     docker
     fasd
     imenu-list
     ranger
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)
     tmux
     vagrant
     (ycmd :variables
           ycmd-server-command '("python" "/Users/zwb/.bin/ycmd/ycmd")
           ycmd-force-semantic-completion t)

     ;; Web services
     (wakatime :variables
               wakatime-api-key  "b0d4cb91-e437-470c-b54c-72d92716287c"
               wakatime-cli-path "/usr/local/bin/wakatime")

     ;; Blog
     blog

     ;; GTD
     ;; gtd

     ;; ZWB
     zwb
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the lastest
   ;; version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil
   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner "/Users/zwb/.spacemacs.d/banner/spacemacs.png"
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((projects . 10))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(seti)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(all-the-icons :separator wave)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco"
                               :size 13
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle 300
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "https://elpa.emacs-china.org/melpa/")
          ("org-cn"   . "https://elpa.emacs-china.org/org/")
          ("gnu-cn"   . "https://elpa.emacs-china.org/gnu/")))
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  (push '(ensime . "melpa-stable") package-pinned-packages)

  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq byte-compile-warnings '(not obsolete))
  (setq warning-minimum-level :error)

  ;; mode-line
  (setq spaceline-org-clock-p t)
  (with-eval-after-load 'spaceline-segments
    (spaceline-toggle-minor-modes-off)
    )
  ; (setq-default flycheck-scalastylerc "/usr/local/etc/scalastyle_config.xml")
  )

(defun dotspacemacs/user-config ()
  "This function is called at the very end of Spacemacs initialization after
  layers configuration.
  This is the place where most of your configurations should be done. Unless it is
  explicitly specified that a variable should be set before a package is loaded,
  you should place your code here."

  ;; c style setting
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; Add kernel style
              (c-add-style
               "linux-tabs-only"
               '("linux" (c-offsets-alist
                          (arglist-cont-nonempty
                           c-lineup-gcc-asm-reg
                           c-lineup-arglist-tabs-only))))))

  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)

  (add-hook 'c-mode-hook
            (lambda ()
              (c-set-style "linux-tabs-only")))

  ;; email address
  (setq user-mail-address "zwb.ict@gmail.com")

  ;; org-page
  (setq op/confound-email nil)

  ;; auto-completion
  (setq company--auto-completion t
        company-show-numbers t)

  ;; flycheck-pos-tip
  (setq flycheck-pos-tip-timeout 4)

  ;; hs-minor
  (add-hook 'prog-mode-hook (lambda () (unless (eq major-mode 'web-mode)
                                         (hs-minor-mode))))

  ;; eyebrowse
  (setq eyebrowse-new-workspace (lambda () (counsel-find-file)))

  (define-key evil-hybrid-state-map (kbd "C-h") 'backward-delete-char)
  (define-key evil-emacs-state-map (kbd "C-h") 'backward-delete-char)

  ;; org
  (with-eval-after-load 'org
    (setq org-agenda-files (file-expand-wildcards "~/GTD/*.org"))
    (setq org-journal-dir "~/GTD/journal/")
    (setq org-journal-file-format "%Y-%m-%d")
    (setq org-journal-date-prefix "#+TITLE: ")
    (setq org-journal-date-format "%A, %B %d %Y")
    (setq org-journal-time-prefix "* ")
    (setq org-journal-time-format "")
    (setq spaceline-org-clock-p t)

    (add-hook 'org-mode-hook (lambda () (autoload 'space-doc-mode "space-doc" nil 'interactive)))
    )

  ;; (with-eval-after-load 'org-agenda
  ;;   (require 'org-projectile)
  ;;   (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

  ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (setq flycheck-gometalinter-vendor t)
  ;; only run fast linters
  (setq flycheck-gometalinter-fast t)
  ;; use in tests files
  (setq flycheck-gometalinter-test t)
  ;; disable some linters
  (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
  (setq flycheck-gometalinter-deadline "10s")

  ;; ycmd
  (setq url-show-status nil)
  (setq request-message-level -1)
  (set-variable 'ycmd-extra-conf-whitelist '("~/*"))
  )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (all-the-icons-ivy all-the-icons-dired yasnippet-snippets yapfify yaml-mode xterm-color xcscope ws-butler winum which-key wgrep web-mode web-beautify wakatime-mode volatile-highlights vi-tilde-fringe vagrant-tramp vagrant uuidgen use-package unfill treemacs-projectile treemacs-evil toml-mode toc-org tagedit symon string-inflection spaceline-all-the-icons smex smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe reveal-in-osx-finder restart-emacs realgud rbenv ranger rainbow-delimiters racer pyvenv pytest pyim pyenv-mode py-isort pug-mode projectile-rails popwin pippel pipenv pip-requirements persp-mode pbcopy password-generator paradox pangu-spacing ox-twbs ox-reveal ox-gfm overseer osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-page org-mime org-journal org-download org-bullets org-brain open-junk-file noflet nginx-mode nameless mwim mvn multi-term move-text mmm-mode minitest meghanada maven-test-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl js2-refactor js-doc ivy-xref ivy-rtags ivy-purpose ivy-hydra insert-shebang indent-guide importmagic impatient-mode hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-make groovy-mode groovy-imports gradle-mode google-translate google-c-style golden-ratio godoctor go-tag go-rename go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md ggtags geiser fuzzy font-lock+ flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-gometalinter flycheck-bashate flx-ido floobits fish-mode find-by-pinyin-dired fill-column-indicator feature-mode fcitx fasd fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erlang erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks ensime enh-ruby-mode emmet-mode elisp-slime-nav editorconfig dumb-jump dockerfile-mode docker disaster diminish diff-hl dash-at-point cython-mode counsel-projectile counsel-gtags counsel-dash counsel-css company-ycmd company-web company-tern company-statistics company-shell company-rtags company-quickhelp company-lua company-go company-emacs-eclim company-c-headers company-anaconda column-enforce-mode coffee-mode cmake-mode cmake-ide clean-aindent-mode clang-format chruby centered-cursor-mode cargo bundler browse-at-remote blog-admin auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-pinyin ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
