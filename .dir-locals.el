;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((c++-mode
  (eval . (let ((dir
                 (replace-regexp-in-string "\n$" "/"
                  (shell-command-to-string "git rev-parse --show-toplevel"))))
            (setq flycheck-clang-include-path
                  (append (list (concat dir "lib/include"))
                          (delete "" (split-string
                                      (replace-regexp-in-string "\s" ""
                                       (shell-command-to-string "pkg-config --cflags pangocairo"))
                                      "-I"))))))
  (flycheck-clang-language-standard . "c++1y")))
