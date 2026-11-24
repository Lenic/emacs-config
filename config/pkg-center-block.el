;; -*- lexical-binding: t -*-

;;;; M-o：把光标所在的代码块在窗口里上下居中
;;
;; 一、基本行为
;;   1. 按 M-o，把光标所在的代码块在当前窗口里上下居中。
;;   2. 连按效果不变，不会逐级扩大范围。
;;   3. 居中后用选区的颜色高亮这个代码块，300 毫秒后消失；高亮要盖过 hl-line。
;;   4. 只处理编程语言的 buffer，其它 buffer 居中当前行。
;;
;; 二、查找的起点
;;   5. 光标在行首缩进里时，从这一行第一个字符开始找；否则从光标位置开始找。
;;
;; 三、收集候选代码块
;;   6. 从起点往外，逐层收集包着光标的语法结构。
;;   7. 有 tree-sitter 时用语法树，从嵌得最深的语言开始（比如 Vue 文件里 script 中的
;;      TypeScript），再到外层的语言。整个文件、整个嵌入区域本身不算代码块。
;;   8. 没有 tree-sitter 时，用包着光标的各层括号，再加上光标所在的函数。
;;   9. 候选块都扩展成整行；结束位置正好在下一行行首时，把那一行也算进来。
;;  10. 只占一行的块不算候选。
;;  11. 行范围相同的块只算一个，其中有一个是函数，合并后就算函数。
;;  12. 块的高度按屏幕上实际显示的行数算，折叠后显示几行就算几行。
;;
;; 四、阈值（都按当前窗口的高度算）
;;  13. 小阈值 = 窗口高度的 1/4。
;;  14. 半屏 = 窗口高度的 1/2。
;;  15. 放得下 = 块的高度不超过窗口高度。
;;
;; 五、什么样的函数算数
;;  16. 函数满足下面任意一条才算数：
;;      - 高度超过小阈值；
;;      - 是独立函数：第一行的上一行和最后一行的下一行都是边界行。
;;  17. 边界行是：
;;      - 空行；
;;      - 只有括号、逗号、分号的行，比如 `{'、`}'、`},'、`]);'；
;;      - 文件或嵌入区域之外的行：文件开头之前、结尾之后，以及 `<script setup>'、
;;        `</script>' 这样的标签行；
;;      - 往上查时先跳过紧挨着函数的注释行，再看注释上面那一行。
;;  18. 注释只参与判断，不算进函数的范围，居中和高亮都从函数本身的第一行开始。
;;  19. Vue 文件里 TypeScript 的哪些节点算函数，在 pkg-web.el 的
;;      `my/vue-ts-mode-setup-sexp' 里设置。
;;
;; 六、挑选规则（按顺序，前面的命中就不再往下）
;;  20. 放得下并且算数的函数里，最外层的那个。
;;  21. 从内往外，第一个高度大于小阈值、小于半屏的块。
;;  22. 放得下、高度大于小阈值的块里，最小的那个。
;;  23. 高度不超过小阈值的块里，最大的那个。
;;  24. 都没有时，用光标所在行。
;;
;; 七、居中方式
;;  25. 块上方和下方的空白行数一样，除不尽时多出的一行放在下方。
;;  26. 挑出的块都放得下一屏；只有光标所在行折行后比窗口还高时，才把开头放在窗口顶部。
;;
;; 八、代码组织
;;  27. 这个文件自成一体，除了 cl-lib、seq、treesit 不依赖别的代码。这三个包在启动时
;;      已经加载过（seq 是预加载的，cl-lib 随 use-package 加载，treesit 随 pkg-dev 里的
;;      treesit-auto 加载），这里 require 不会拖慢启动。cl-defstruct 在加载文件时
;;      就要展开，所以 cl-lib 不能推迟到第一次调用时再加载。

(require 'cl-lib)
(require 'seq)
(require 'treesit)

(cl-defstruct (my/center-block--block (:constructor my/center-block--make))
  "一个候选代码块。BEG、END 对齐到整行，HEIGHT 是屏幕行数，
DEFUN 表示是不是函数，PARSER 是找到它的 tree-sitter parser（没有时为 nil）。"
  beg end height defun parser)

;;; 查找候选代码块

(defun my/center-block--treesit-p ()
  "当前 buffer 是否能用 tree-sitter。"
  (and (treesit-available-p) (treesit-parser-list)))

(defun my/center-block--lines (beg end)
  "把 BEG 到 END 扩展成整行，返回 (开始 . 结束)。"
  (save-excursion
    (cons (progn (goto-char beg) (line-beginning-position))
          (progn (goto-char end) (line-end-position)))))

(defun my/center-block--start-pos ()
  "查找的起点：光标在行首缩进里时，用这一行第一个字符的位置。"
  (if (<= (current-column) (current-indentation))
      (save-excursion (back-to-indentation) (point))
    (point)))

(defun my/center-block--defun-p (node)
  "NODE 是否是函数级别的 tree-sitter 节点。"
  (or (ignore-errors (treesit-node-match-p node 'defun t))
      (let ((type (treesit-node-type node)))
        (pcase treesit-defun-type-regexp
          ((and (pred stringp) re) (string-match-p re type))
          (`(,re . ,pred) (and (string-match-p re type) (funcall pred node)))))))

(defun my/center-block--treesit-candidates (pos)
  "tree-sitter 下 POS 外面的所有节点，返回 ((开始 结束 是否函数 parser) ...)。
从嵌得最深的语言开始找，像 Vue 文件里 script 中的代码也能找到。
根节点是整个文件或整个嵌入区域，不算代码块。"
  (cl-loop
   for parser in (treesit-parsers-at pos)
   nconc (cl-loop
          for node = (treesit-node-descendant-for-range
                      (treesit-parser-root-node parser) pos pos)
          then (treesit-node-parent node)
          while (and node (treesit-node-parent node))
          collect (list (treesit-node-start node) (treesit-node-end node)
                        (my/center-block--defun-p node) parser))))

(defun my/center-block--syntax-candidates (pos)
  "没有 tree-sitter 时，用 POS 外面的各层括号和所在的函数，格式同上。"
  (append
   (cl-loop
    ;; 第 9 项是外层在前的左括号位置，反过来从最内层开始找；
    ;; 起点正好是左括号时，从这个括号开始
    for open in (append (when (eq (syntax-class (syntax-after pos)) 4)
                          (list pos))
                        (reverse (nth 9 (syntax-ppss pos))))
    for close = (ignore-errors (scan-lists open 1 0))
    when close collect (list open close nil nil))
   (ignore-errors
     (save-excursion
       (goto-char pos)
       (end-of-defun)
       (let ((end (point)))
         (beginning-of-defun)
         (when (<= (point) pos end)
           (list (list (point) end t nil))))))))

(defun my/center-block--blocks ()
  "光标外面所有跨多行的代码块，按高度从小到大排列，也就是从内往外。
行范围相同的合并成一个，其中有一个是函数，合并后就算函数。"
  (let ((pos (my/center-block--start-pos))
        blocks)
    (pcase-dolist (`(,beg ,end ,defun ,parser)
                   (if (my/center-block--treesit-p)
                       (my/center-block--treesit-candidates pos)
                     (my/center-block--syntax-candidates pos)))
      (pcase-let ((`(,beg . ,end) (my/center-block--lines beg end)))
        (when (> end (save-excursion (goto-char beg) (line-end-position)))
          (if-let* ((same (seq-find (lambda (b)
                                      (and (= (my/center-block--block-beg b) beg)
                                           (= (my/center-block--block-end b) end)))
                                    blocks)))
              (when defun (setf (my/center-block--block-defun same) t))
            (push (my/center-block--make :beg beg :end end
                                         :height (count-screen-lines beg end)
                                         :defun defun :parser parser)
                  blocks)))))
    (sort blocks (lambda (a b) (< (my/center-block--block-height a)
                                  (my/center-block--block-height b))))))

;;; 判断独立函数

(defun my/center-block--comment-line-p (parser)
  "当前行是否只有注释。PARSER 非 nil 时用 tree-sitter 判断，否则用语法表。"
  (save-excursion
    (back-to-indentation)
    (and (not (eolp))
         (if parser
             (when-let* ((node (treesit-node-at (point) parser)))
               (and (<= (treesit-node-start node) (point))
                    (string-match-p "comment" (treesit-node-type node))))
           (let ((eol (line-end-position)))
             (or (nth 4 (syntax-ppss))
                 (and (forward-comment 1) (>= (point) eol))))))))

(defun my/center-block--boundary-line-p (parser)
  "当前行是否是边界行：空行、只有括号逗号分号的行，或者不在 PARSER 的范围里。"
  (save-excursion
    (back-to-indentation)
    (or (looking-at-p "[][(){},; \t]*$")
        (and parser (not (memq parser (treesit-parsers-at (point))))))))

(defun my/center-block--standalone-p (block)
  "BLOCK 是否是独立函数：上一行（跳过紧挨着的注释）和下一行都是边界行。
文件开头之前、结尾之后也算边界。"
  (let ((parser (my/center-block--block-parser block)))
    (save-excursion
      (and (progn
             (goto-char (my/center-block--block-beg block))
             (cl-loop
              do (when (bobp) (cl-return t))
              do (forward-line -1)
              unless (my/center-block--comment-line-p parser)
              return (my/center-block--boundary-line-p parser)))
           (progn
             (goto-char (my/center-block--block-end block))
             (or (eobp)
                 (progn (forward-line 1)
                        (my/center-block--boundary-line-p parser))))))))

;;; 挑选并居中

(defun my/center-block--pick (blocks)
  "按 M-o 的规则从 BLOCKS 里挑一个代码块，没有合适的返回 nil。"
  (let* ((win-h (window-body-height))
         (small (/ win-h 4))
         (half (/ win-h 2))
         (height #'my/center-block--block-height)
         (fits (seq-filter (lambda (b) (<= (funcall height b) win-h)) blocks)))
    (or (car (last (seq-filter
                    (lambda (b)
                      (and (my/center-block--block-defun b)
                           (or (> (funcall height b) small)
                               (my/center-block--standalone-p b))))
                    fits)))
        (seq-find (lambda (b) (< small (funcall height b) half)) blocks)
        (seq-find (lambda (b) (> (funcall height b) small)) fits)
        (car (last (seq-filter (lambda (b) (<= (funcall height b) small)) blocks))))))

(defvar my/center-block--overlay nil
  "居中后的高亮 overlay。")

(defun my/center-block--unflash ()
  "删除高亮。"
  (when (overlayp my/center-block--overlay)
    (delete-overlay my/center-block--overlay)
    (setq my/center-block--overlay nil)))

(defun my/center-block--flash (beg end)
  "高亮 BEG 到 END 所在的整行，300 毫秒后清除。"
  ;; zenburn 里 `highlight' 和 hl-line 同色，看不出来，所以用和选区一样的 `region' 颜色
  (my/center-block--unflash)
  (let ((ov (make-overlay beg (min (1+ end) (point-max)))))
    ;; 盖过 hl-line（优先级 -50）
    (overlay-put ov 'priority 1)
    (overlay-put ov 'face '((:extend t) region))
    (setq my/center-block--overlay ov)
    (run-with-timer 0.3 nil #'my/center-block--unflash)))

(defun my/center-block ()
  "把光标所在的代码块在窗口里上下居中，挑选规则见文件开头的说明。"
  (interactive)
  (let* ((block (and (derived-mode-p 'prog-mode)
                     (my/center-block--pick (my/center-block--blocks))))
         (beg (if block (my/center-block--block-beg block) (line-beginning-position)))
         (end (if block (my/center-block--block-end block) (line-end-position)))
         ;; 除不尽时多出的一行放在下方；比窗口高时开头放在顶部
         (above (max 0 (/ (- (window-body-height) (count-screen-lines beg end)) 2))))
    (set-window-start nil (save-excursion
                            (goto-char beg)
                            (vertical-motion (- above))
                            (point)))
    (my/center-block--flash beg end)))

(global-set-key (kbd "M-o") #'my/center-block)

(provide 'pkg-center-block)
