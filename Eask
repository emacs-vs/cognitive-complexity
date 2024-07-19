(package "cognitive-complexity"
         "1.0.0"
         "Plugin shows complexity information")

(website-url "https://github.com/abougouffa/cognitive-complexity")
(keywords "convenience" "complexity")

(package-file "cognitive-complexity.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "29.1")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(development
 (depends-on "kotlin-mode")
 (depends-on "lua-mode")
 (depends-on "treesit-auto"))
