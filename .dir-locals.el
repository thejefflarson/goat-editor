;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((c++-mode
  (eval . (let ((dir (file-name-directory (expand-file-name ".dir-locals.el"))))
            (setq flycheck-clang-include-path
                  (append (list (concat dir "lib/include"))
                          (delete "" (split-string
                                      (replace-regexp-in-string "\s" ""
                                       (shell-command-to-string "pkg-config --cflags pangocairo"))
                                      "-I"))))))
  (flycheck-clang-language-standard . "c++1y")))
