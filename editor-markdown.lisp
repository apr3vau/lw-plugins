;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

;; Markdown syntax highlighting for LispWorks Editor, Under the
;; specification of Github-Flavored Markdown.

;; What we does not support currently:
;; - Raw HTML
;; - Task list item
;; - Extended Autolinks

;; What we did not implement well:
;; - Autolinks (in pointy brackets) are lack of verification
;; - Tables are not parsed strictly
;; - Highlight region refreshment are not strict enough
;;   (sometimes will cause regions outdated)

;; Usage: Configure the font family & size, load this file,
;;        eval (EDITOR-MARKDOWN:UPDATE-FACE) after CAPI initialized.

;; 07Dec24:
;; - Fix wrongly extended code block region when editing under a code
;; block;
;; - Improve tilde / backquote code span recognition
;; - Inline code span now overrides other inlines correctly

(defpackage editor-markdown
  (:add-use-defaults))
(in-package editor-markdown)

(declaim (special *bold-font* *italic-font* *bold-italic-font* *strikethrough-font* *header-font* *mono-font* *mono-bold-font*))

;; Font family & size

(defvar *sans-serif-font-family-name*
  #+mswindows "Arial"
  #+macosx ".AppleSystemUIFont"
  #+linux "Liberation Sans")

(defvar *monospace-font-family-name*
  #+mswindows "Courier New"
  #+macosx "Monaco"
  #+linux "Liberation Mono")

(defvar *font-size* 12)
(defvar *header-font-size-increasement* 2)

;; Face

(defun update-face ()
  "Update face properties.

Should be called once after the CAPI has initialized, and after the
change of font family or size to adapt the change."
  (let ((port (capi:create-dummy-graphics-port)))
    (editor:make-face 'md-default-face
                      :font (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*))
                      :foreground nil :background nil
                      :if-exists :overwrite)
    (defparameter *bold-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*
                                                        :weight :bold)))
    (defparameter *italic-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*
                                                        :slant :italic)))
    (defparameter *bold-italic-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*
                                                        :weight :bold :slant :italic)))
    (defparameter *strikethrough-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size *font-size*
                                                        :slant :italic)))
    (defparameter *header-font*
      (gp:find-best-font port (gp:make-font-description :family *sans-serif-font-family-name* :size (+ *font-size* *header-font-size-increasement*)
                                                        :weight :bold)))
    (defparameter *mono-font*
      (gp:find-best-font port (gp:make-font-description :family *monospace-font-family-name* :size *font-size*)))
    (defparameter *mono-bold-font*
      (gp:find-best-font port (gp:make-font-description :family *monospace-font-family-name* :size *font-size*
                                                        :weight :bold))))
  
  (editor:make-face 'md-code-block-face
                    :font *mono-font*
                    :background (editor::create-dark-background-switchable-color :lightcyan2 #(:RGB 0 0.2 0))
                    :if-exists :overwrite)
  (editor:make-face 'md-thematic-break-face
                    :background (editor::create-dark-background-switchable-color :grey70 :grey38)
                    :if-exists :overwrite)
  (editor:make-face 'md-header-face
                    :font *header-font*
                    :foreground (editor::create-dark-background-switchable-color :blue :lightblue)
                    :if-exists :overwrite)
  (editor:make-face 'md-list-face
                    :font *italic-font*
                    :foreground (editor::create-dark-background-switchable-color :darkgoldenrod :lightgoldenrod)
                    :if-exists :overwrite)
  (editor:make-face 'md-quote-face
                    :background (editor::create-dark-background-switchable-color :gray90 :gray20)
                    :foreground (editor::create-dark-background-switchable-color :red4 :pink)
                    :if-exists :overwrite)
  (editor:make-face 'md-link-face
                    :font *italic-font*
                    :foreground (editor::create-dark-background-switchable-color :dodgerblue :skyblue)
                    :if-exists :overwrite)
  (editor:make-face 'md-image-face
                    :font *bold-italic-font*
                    :foreground (editor::create-dark-background-switchable-color :dodgerblue :skyblue)
                    :if-exists :overwrite)
  (editor:make-face 'md-bold-face
                    :font *bold-font*
                    :foreground (editor::create-dark-background-switchable-color :orchid :orchid1)
                    :if-exists :overwrite)
  (editor:make-face 'md-italic-face
                    :font *italic-font*
                    :foreground (editor::create-dark-background-switchable-color :orchid :orchid1)
                    :if-exists :overwrite)
  (editor:make-face 'md-bold-italic-face
                    :font *bold-italic-font*
                    :foreground (editor::create-dark-background-switchable-color :orchid :orchid1)
                    :if-exists :overwrite)
  (editor:make-face 'md-strikethrough-face
                    :font *strikethrough-font*
                    :foreground (editor::create-dark-background-switchable-color :gray20 :gray80)
                    :background (editor::create-dark-background-switchable-color :gray80 :gray20)
                    :if-exists :overwrite)
  (editor:make-face 'md-code-span-face
                    :font *mono-font*
                    :foreground (editor::create-dark-background-switchable-color :darkgreen :palegreen)
                    :if-exists :overwrite)
  (editor:make-face 'md-table-face
                    :font *mono-font*
                    :foreground (editor::create-dark-background-switchable-color :cadetblue4 :cadetblue1)
                    :if-exists :overwrite)
  )

;; Util

;; We use following code to generate forms of *PUNCT-CHAR-CODES* and
;; *UNICODE-WS-CHAR-CODES*

;; Source of data: ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt

#|
(let (punct-char-codes
      (ws-char-codes '(#x09 #x0d #x0a #x0c)))
  (loop for i from #x21 to #x2f do (push-end i punct-char-codes))
  (loop for i from #x3a to #x40 do (push-end i punct-char-codes))
  (loop for i from #x5b to #x60 do (push-end i punct-char-codes))
  (loop for i from #x7b to #x7e do (push-end i punct-char-codes))
  (with-open-file (in (merge-pathnames "UnicodeData.txt" (asdf:system-source-directory :lw-plugins)))
    (loop for line = (read-line in nil)
          until (null line)
          for split = (split-sequence '(#\;) line)
          when (eql (char (third split) 0) #\P)
            do (push-end-new (parse-integer (first split) :radix 16) punct-char-codes)
          when (equal (third split) "Zs")
            do (push-end-new (parse-integer (first split) :radix 16) ws-char-codes)))
  (editor:insert-string
   (editor:current-point)
   (string-append
    (format nil "~%(defparameter *punct-char-codes*~%  '(~@?))"
            (format nil "~A~{~A~}~A" "~{" (make-list 12 :initial-element "~6D~^ ") "~%    ~}")
            punct-char-codes)
    (string #\Newline)
    (format nil "~%(defparameter *unicode-ws-char-codes*~%  '(~@?))"
            (format nil "~A~{~A~}~A" "~{" (make-list 12 :initial-element "~6D~^ ") "~%    ~}")
            ws-char-codes))))
|#

(defparameter *punct-char-codes*
  '(    33     34     35     36     37     38     39     40     41     42     43     44 
        45     46     47     58     59     60     61     62     63     64     91     92 
        93     94     95     96    123    124    125    126    161    167    171    182 
       183    187    191    894    903   1370   1371   1372   1373   1374   1375   1417 
      1418   1470   1472   1475   1478   1523   1524   1545   1546   1548   1549   1563 
      1565   1566   1567   1642   1643   1644   1645   1748   1792   1793   1794   1795 
      1796   1797   1798   1799   1800   1801   1802   1803   1804   1805   2039   2040 
      2041   2096   2097   2098   2099   2100   2101   2102   2103   2104   2105   2106 
      2107   2108   2109   2110   2142   2404   2405   2416   2557   2678   2800   3191 
      3204   3572   3663   3674   3675   3844   3845   3846   3847   3848   3849   3850 
      3851   3852   3853   3854   3855   3856   3857   3858   3860   3898   3899   3900 
      3901   3973   4048   4049   4050   4051   4052   4057   4058   4170   4171   4172 
      4173   4174   4175   4347   4960   4961   4962   4963   4964   4965   4966   4967 
      4968   5120   5742   5787   5788   5867   5868   5869   5941   5942   6100   6101 
      6102   6104   6105   6106   6144   6145   6146   6147   6148   6149   6150   6151 
      6152   6153   6154   6468   6469   6686   6687   6816   6817   6818   6819   6820 
      6821   6822   6824   6825   6826   6827   6828   6829   6990   6991   7002   7003 
      7004   7005   7006   7007   7008   7037   7038   7039   7164   7165   7166   7167 
      7227   7228   7229   7230   7231   7294   7295   7360   7361   7362   7363   7364 
      7365   7366   7367   7379   8208   8209   8210   8211   8212   8213   8214   8215 
      8216   8217   8218   8219   8220   8221   8222   8223   8224   8225   8226   8227 
      8228   8229   8230   8231   8240   8241   8242   8243   8244   8245   8246   8247 
      8248   8249   8250   8251   8252   8253   8254   8255   8256   8257   8258   8259 
      8261   8262   8263   8264   8265   8266   8267   8268   8269   8270   8271   8272 
      8273   8275   8276   8277   8278   8279   8280   8281   8282   8283   8284   8285 
      8286   8317   8318   8333   8334   8968   8969   8970   8971   9001   9002  10088 
     10089  10090  10091  10092  10093  10094  10095  10096  10097  10098  10099  10100 
     10101  10181  10182  10214  10215  10216  10217  10218  10219  10220  10221  10222 
     10223  10627  10628  10629  10630  10631  10632  10633  10634  10635  10636  10637 
     10638  10639  10640  10641  10642  10643  10644  10645  10646  10647  10648  10712 
     10713  10714  10715  10748  10749  11513  11514  11515  11516  11518  11519  11632 
     11776  11777  11778  11779  11780  11781  11782  11783  11784  11785  11786  11787 
     11788  11789  11790  11791  11792  11793  11794  11795  11796  11797  11798  11799 
     11800  11801  11802  11803  11804  11805  11806  11807  11808  11809  11810  11811 
     11812  11813  11814  11815  11816  11817  11818  11819  11820  11821  11822  11824 
     11825  11826  11827  11828  11829  11830  11831  11832  11833  11834  11835  11836 
     11837  11838  11839  11840  11841  11842  11843  11844  11845  11846  11847  11848 
     11849  11850  11851  11852  11853  11854  11855  11858  11859  11860  11861  11862 
     11863  11864  11865  11866  11867  11868  11869  12289  12290  12291  12296  12297 
     12298  12299  12300  12301  12302  12303  12304  12305  12308  12309  12310  12311 
     12312  12313  12314  12315  12316  12317  12318  12319  12336  12349  12448  12539 
     42238  42239  42509  42510  42511  42611  42622  42738  42739  42740  42741  42742 
     42743  43124  43125  43126  43127  43214  43215  43256  43257  43258  43260  43310 
     43311  43359  43457  43458  43459  43460  43461  43462  43463  43464  43465  43466 
     43467  43468  43469  43486  43487  43612  43613  43614  43615  43742  43743  43760 
     43761  44011  64830  64831  65040  65041  65042  65043  65044  65045  65046  65047 
     65048  65049  65072  65073  65074  65075  65076  65077  65078  65079  65080  65081 
     65082  65083  65084  65085  65086  65087  65088  65089  65090  65091  65092  65093 
     65094  65095  65096  65097  65098  65099  65100  65101  65102  65103  65104  65105 
     65106  65108  65109  65110  65111  65112  65113  65114  65115  65116  65117  65118 
     65119  65120  65121  65123  65128  65130  65131  65281  65282  65283  65285  65286 
     65287  65288  65289  65290  65292  65293  65294  65295  65306  65307  65311  65312 
     65339  65340  65341  65343  65371  65373  65375  65376  65377  65378  65379  65380 
     65381  65792  65793  65794  66463  66512  66927  67671  67871  67903  68176  68177 
     68178  68179  68180  68181  68182  68183  68184  68223  68336  68337  68338  68339 
     68340  68341  68342  68409  68410  68411  68412  68413  68414  68415  68505  68506 
     68507  68508  68974  69293  69461  69462  69463  69464  69465  69510  69511  69512 
     69513  69703  69704  69705  69706  69707  69708  69709  69819  69820  69822  69823 
     69824  69825  69952  69953  69954  69955  70004  70005  70085  70086  70087  70088 
     70093  70107  70109  70110  70111  70200  70201  70202  70203  70204  70205  70313 
     70612  70613  70615  70616  70731  70732  70733  70734  70735  70746  70747  70749 
     70854  71105  71106  71107  71108  71109  71110  71111  71112  71113  71114  71115 
     71116  71117  71118  71119  71120  71121  71122  71123  71124  71125  71126  71127 
     71233  71234  71235  71264  71265  71266  71267  71268  71269  71270  71271  71272 
     71273  71274  71275  71276  71353  71484  71485  71486  71739  72004  72005  72006 
     72162  72255  72256  72257  72258  72259  72260  72261  72262  72346  72347  72348 
     72350  72351  72352  72353  72354  72448  72449  72450  72451  72452  72453  72454 
     72455  72456  72457  72673  72769  72770  72771  72772  72773  72816  72817  73463 
     73464  73539  73540  73541  73542  73543  73544  73545  73546  73547  73548  73549 
     73550  73551  73727  74864  74865  74866  74867  74868  77809  77810  92782  92783 
     92917  92983  92984  92985  92986  92987  92996  93549  93550  93551  93847  93848 
     93849  93850  94178 113823 121479 121480 121481 121482 121483 124415 125278 125279))

(defparameter *unicode-ws-char-codes*
  '(     9     13     10     12     32    160   5760   8192   8193   8194   8195   8196 
      8197   8198   8199   8200   8201   8202   8239   8287  12288))

(defun punct-char-p (char)
  "Check if CHAR is a puctuation character."
  (member (char-code char) *punct-char-codes*))

(defun unicode-ws-char-p (char)
  "Check if CHAR is an unicode whitespace character"
  (member (char-code char) *unicode-ws-char-codes*))

(defun count-left-whitespaces (line)
  (let ((result 0))
    (loop for c across line
          do (cond ((eql c #\Tab) (incf result 4))
                   ((unicode-ws-char-p c) (incf result))
                   (t (return))))
    result))

(defun identical-char-length (text start)
  (if (> (length text) 0)
      (if (= start (1- (length text)))
          1
        (let ((char (char text start)))
          (loop for i from 1
                until (= (+ i start) (length text))
                for c = (char text (+ start i))
                until (not (eql c char))
                finally (return i))))
    nil))

;; Line-wise predicting function

;; We use these functions to find
;; - Headings
;; - Quote block
;; - Code block
;; - List item
;; - Thematic break

(defun line-atx-heading-p (line)
  (and (<= (count-left-whitespaces line) 3)
       (loop for i from 0
             for c across (string-trim-whitespace line)
             count (eql c #\#) into result
             until (not (eql c #\#))
             finally (return (and (or (and c (unicode-ws-char-p c))
                                      (= i (length line)))
                                  (<= 1 result 6))))))

(defun line-quotes-p (line)
  (let ((trimed (string-trim-whitespace line)))
    (and (<= (count-left-whitespaces line) 3)
         (> (length trimed) 0)
         (eql (char trimed 0) #\>))))

(defun line-list-item-p (line)
  (let ((1st (first (split-sequence-if #'unicode-ws-char-p line :coalesce-separators t))))
    (and 1st
         (or (and (member (char 1st (1- (length 1st))) '(#\. #\)))
                  (< 1 (length 1st) 10)
                  (every #'digit-char-p (subseq 1st 0 (1- (length 1st)))))
             (and (= (length 1st) 1)
                  (member (char 1st (1- (length 1st))) '(#\+ #\- #\*)))))))

(defun line-setext-underline-p (line)
  (let ((trimed (string-trim-whitespace line)))
    (and (<= (count-left-whitespaces line) 3)
         (> (length trimed) 0)
         (or (every (lambda (c) (eql c #\=)) trimed)
             (every (lambda (c) (eql c #\-)) trimed)))))

(defun line-thematic-break-p (line)
  (and (<= (count-left-whitespaces line) 3)
       (or (every (lambda (c) (or (eql c #\*) (unicode-ws-char-p c))) line)
           (every (lambda (c) (or (eql c #\-) (unicode-ws-char-p c))) line)
           (every (lambda (c) (or (eql c #\_) (unicode-ws-char-p c))) line))
       (>= (count-if (lambda (c) (member c '(#\* #\- #\_))) line) 3)))

(defun line-indented-code-p (line)
  (>= (count-left-whitespaces line) 4))

(defun line-backquote-fence-p (line)
  (let* ((trimed (string-trim-whitespace line))
         (count (loop for c across trimed
                      if (eql c #\`) sum 1
                      else do (loop-finish))))
    (and (>= count 3)
         (<= (count-left-whitespaces line) 3))))

(defun line-tilde-fence-p (line)
  (let* ((trimed (string-trim-whitespace line))
         (count (loop for c across trimed
                      if (eql c #\~) sum 1
                      else do (loop-finish))))
    (and (>= count 3)
         (<= (count-left-whitespaces line) 3))))

(defun line-empty-p (line)
  (= (length (string-trim-whitespace line)) 0))

;; Multi-line predicting function

;; Giving a list of lines, find the pattern at the beginning of the
;; lines. If found, return a list of lines contained in the pattern.

;; We use these functions to find:
;; - Link definitions
;; - Tables

(defun find-link-def-at (lines start)
  (let ((line (nth start lines))
        label-start label-end dest-start dest-end title-start title-end title-in-quote-p
        (newline-count 0)
        (i 0))
    (loop (let ((char (if (= i (length line))
                          #\Newline
                        (char line i))))
            (cond ((null label-start) (if (eql char #\[)
                                          (setq label-start start)
                                        (return)))
                  ((null label-end) (cond ((eql char #\])
                                           (if (and (< (1+ i) (length line))
                                                    (eql (char line (1+ i)) #\:))
                                               (progn
                                                 (setq label-end start)
                                                 (incf i))
                                             (return)))
                                          ((member char '(#\Return #\Newline))
                                           (if (> newline-count 0)
                                               (return)
                                             (incf newline-count)))
                                          ((unicode-ws-char-p char) nil)
                                          (t (setq newline-count 0))))
                  ((null dest-start) (cond ((member char '(#\Return #\Newline))
                                            (if (> newline-count 0)
                                                (return)
                                              (incf newline-count)))
                                           ((unicode-ws-char-p char) nil)
                                           (t (setq newline-count 0
                                                    dest-start start))))
                  ((null dest-end) (cond ((unicode-ws-char-p char) (setq dest-end start))
                                         (t (setq newline-count 0))))
                  ((null title-start) (cond ((member char '(#\Return #\Newline))
                                             (if (> newline-count 0)
                                                 (return)
                                               (incf newline-count)))
                                            ((unicode-ws-char-p char) nil)
                                            (t (when (eql char #\')
                                                 (setq title-in-quote-p t))
                                               (setq newline-count 0
                                                     title-start start))))
                  ((null title-end) (cond ((eql char #\') (if title-in-quote-p
                                                              (setq title-end start)
                                                            (setq title-in-quote-p t)))
                                          ((unicode-ws-char-p char) (unless title-in-quote-p
                                                                      (setq title-end start)))))
                  (t (if (= start title-end)
                         (if (= (length (string-trim-whitespace (subseq line i (length line)))) 0)
                             (return)
                           (return-from find-link-def-at))
                       (return)))))
          (if (= i (length line))
              (progn
                (incf start)
                (setq line (nth start lines))
                (setq i 0)
                (when (= start (length lines))
                  (return))
                (when (or (line-quotes-p line) (line-atx-heading-p line)
                          (line-list-item-p line) (line-indented-code-p line))
                  (return)))
            (incf i)))
    (when dest-start
      (unless dest-end
        (setq dest-end (1- start)))
      (when title-start
        (unless title-end
          (setq title-end (1- start))))
      (loop for i from label-start to (or title-end dest-end)
            collect i))))

(defun find-table-at (lines start)
  (let (result header-row-p in-table-p)
    (loop for i from start to (1- (length lines))
          for line = (nth i lines)
          for trimed = (string-trim-whitespace line)
          for row-p = (and (> (length trimed) 0)
                           (eql (char trimed 0) #\|)
                           (eql (char trimed (1- (length trimed))) #\|))
          do (if row-p
               (cond (in-table-p (push i result))
                     (header-row-p
                      (if (every (lambda (c) (member c '(#\| #\: #\- #\Space))) trimed)
                          (progn
                            (setq in-table-p t
                                  header-row-p nil)
                            (push (1- i) result)
                            (push i result))
                        (setq header-row-p nil)))
                     (t (setq header-row-p t)))
               (return)))
    result))

;; Main search function

(defun search-lines (lines &optional tilde-fence-opening backquote-fence-opening)
  "Search through a list of lines, return a plist, each key
corresponding a kind of block, each value is a list of line numbers
that belong to this block."
  (let ((paragraph-lines 0)
        headings quotes list-items codes link-defs thematic-breaks tables
        paragraph-under-quote
        (i 0))
    (loop (let ((line (nth i lines))
                (link-def (find-link-def-at lines i))
                (table (find-table-at lines i)))
            (cond ((line-tilde-fence-p line) (if backquote-fence-opening
                                               (push i codes)
                                               (if tilde-fence-opening
                                                 (progn
                                                   (push i codes)
                                                   (setq tilde-fence-opening nil))
                                                 (progn
                                                   (setq paragraph-lines 0
                                                         paragraph-under-quote nil
                                                         tilde-fence-opening i)
                                                   (push i codes)))))
                  ((line-backquote-fence-p line) (if tilde-fence-opening
                                                   (push i codes)
                                                   (if backquote-fence-opening
                                                     (progn
                                                       (push i codes)
                                                       (setq backquote-fence-opening nil))
                                                     (progn
                                                       (setq paragraph-lines 0
                                                             paragraph-under-quote nil
                                                             backquote-fence-opening i)
                                                       (push i codes)))))
                  ((or tilde-fence-opening backquote-fence-opening) (push i codes))
                  ((line-atx-heading-p line) (progn
                                               (push i headings)
                                               (setq paragraph-lines 0
                                                     paragraph-under-quote nil)))
                  ((line-setext-underline-p line) (if (> paragraph-lines 0)
                                                    (loop for j from i downto (- i paragraph-lines)
                                                          do (push j headings))
                                                    (if (line-thematic-break-p line)
                                                      (progn
                                                        (push i thematic-breaks)
                                                        (setq paragraph-lines 0
                                                              paragraph-under-quote nil))
                                                      (incf paragraph-lines))))
                  ((line-thematic-break-p line) (progn
                                                  (push i thematic-breaks)
                                                  (setq paragraph-lines 0
                                                        paragraph-under-quote nil)))
                  (link-def (if (= paragraph-lines 0)
                              (progn
                                (loop for j in link-def
                                      do (push j link-defs))
                                (incf i (1- (length link-def))))
                              (incf paragraph-lines)))
                  (table (progn
                           (loop for j in table
                                 do (push j tables))
                           (incf i (1- (length table)))))
                  ((line-list-item-p line) (progn
                                             (setq paragraph-lines 0
                                                   paragraph-under-quote nil)
                                             (push i list-items)))
                  ((line-indented-code-p line) (if (eql paragraph-lines 0)
                                                 (push i codes)
                                                 (incf paragraph-lines)))
                  ((line-quotes-p line) (progn
                                          (setq paragraph-lines 1
                                                paragraph-under-quote t)
                                          (push i quotes)))
                  ((line-empty-p line) (setq paragraph-lines 0
                                             paragraph-under-quote nil))
                  (t (when paragraph-under-quote
                       (push i quotes))
                     (incf paragraph-lines)))
            (incf i)
            (when (= i (length lines))
              (return))
            (setq line (nth i lines))))
    (list :headings headings :quotes    quotes    :list-items list-items
          :codes    codes    :link-defs link-defs :breaks     thematic-breaks
          :tables   tables)))

;; 07Dec24: complete edge case checking for left/right delimiter run

(defun search-inlines (text)
  "Search through TEXT, return a plist, each key
corresponding a kind of inline-component, each value is a list of (START END) offsets
that contain the inline-component."
  (let (code-spans strikethroughs bolds italics bold-italics images links
                   
        single-code-span-opening double-code-span-opening
        single-strikethrough-opening double-strikethrough-opening
        
        *-italic-opening *-bold-opening *-bold-italic-opening
        _-italic-opening _-bold-opening _-bold-italic-opening
        
        image-opening link-title-opening link-title-closing link-dest-opening
        pointy-opening
        (i 0))
    (symbol-macrolet ((inline-code-override-any
                       (setq single-strikethrough-opening nil double-strikethrough-opening nil
                             *-italic-opening nil *-bold-opening nil *-bold-italic-opening nil
                             _-italic-opening nil _-bold-opening nil _-bold-italic-opening nil
                             image-opening nil link-title-opening nil link-title-closing nil
                             link-dest-opening nil))
                      (left-flanking-delimiter-run-p
                       (and (not (unicode-ws-char-p follow-char))
                            (or (not (punct-char-p follow-char))
                                (or (unicode-ws-char-p precede-char)
                                    (punct-char-p precede-char)))))
                      (right-flanking-delimiter-run-p
                       (and (not (unicode-ws-char-p precede-char))
                            (or (not (punct-char-p precede-char))
                                (or (unicode-ws-char-p follow-char)
                                    (punct-char-p follow-char))))))
      (when (> (length text) 0)
        (loop (let ((char (char text i)))
                (case char
                  (#\\ (incf i))
                  (#\` (let ((len (identical-char-length text i)))
                         (case len
                           (1 inline-code-override-any
                              (if single-code-span-opening
                                (progn
                                  (push (list single-code-span-opening (1+ i)) code-spans)
                                  (setq single-code-span-opening nil))
                                (setq single-code-span-opening i)))
                           (2 inline-code-override-any
                              (if double-code-span-opening
                                (progn
                                  (push (list double-code-span-opening (+ 2 i)) code-spans)
                                  (setq double-code-span-opening nil))
                                (setq double-code-span-opening i))))
                         (incf i (1- len))))
                  (#\~ (unless (or single-code-span-opening double-code-span-opening)
                         (let* ((len (identical-char-length text i))
                                (precede-char (char text (1- i)))
                                (follow-char (char text (+ i len))))
                           (case len
                             (1 (if single-strikethrough-opening
                                  (when right-flanking-delimiter-run-p
                                    (push (list single-strikethrough-opening (1+ i)) strikethroughs)
                                    (setq single-strikethrough-opening nil))
                                  (when left-flanking-delimiter-run-p
                                    (setq single-strikethrough-opening i))))
                             (2 (if double-code-span-opening
                                  (when right-flanking-delimiter-run-p
                                    (push (list double-strikethrough-opening (+ 2 i)) strikethroughs)
                                    (setq double-strikethrough-opening nil))
                                  (when left-flanking-delimiter-run-p
                                    (setq double-strikethrough-opening i)))))
                           (incf i (1- len)))))
                  (#\* (unless (or single-code-span-opening double-code-span-opening)
                         (let* ((len (identical-char-length text i))
                                (precede-char (char text (1- i)))
                                (follow-char (char text (+ i len))))
                           (case len
                             (1 (if *-italic-opening
                                  (when right-flanking-delimiter-run-p
                                    (push (list *-italic-opening (1+ i)) italics)
                                    (setq *-italic-opening nil))
                                  (when left-flanking-delimiter-run-p
                                    (setq *-italic-opening i))))
                             (2 (if *-bold-opening
                                  (when right-flanking-delimiter-run-p
                                    (push (list *-bold-opening (+ 2 i)) bolds)
                                    (setq *-bold-opening nil))
                                  (when left-flanking-delimiter-run-p
                                    (setq *-bold-opening i))))
                             (3 (if *-bold-italic-opening
                                  (when right-flanking-delimiter-run-p
                                    (push (list *-bold-italic-opening (+ 3 i)) bold-italics)
                                    (setq *-bold-italic-opening nil))
                                  (when left-flanking-delimiter-run-p
                                    (setq *-bold-italic-opening i)))))
                           (incf i (1- len)))))
                  (#\_ (unless (or single-code-span-opening double-code-span-opening)
                         (let* ((len (identical-char-length text i))
                                (precede-char (char text (1- i)))
                                (follow-char (char text (+ i len))))
                           (case len
                             (1 (if _-italic-opening
                                  (when right-flanking-delimiter-run-p
                                    (push (list _-italic-opening (1+ i)) italics)
                                    (setq _-italic-opening nil))
                                  (when left-flanking-delimiter-run-p
                                    (setq _-italic-opening i))))
                             (2 (if _-bold-opening
                                  (when right-flanking-delimiter-run-p
                                    (push (list _-bold-opening (+ 2 i)) bolds)
                                    (setq _-bold-opening nil))
                                  (when left-flanking-delimiter-run-p
                                    (setq _-bold-opening i))))
                             (3 (if _-bold-italic-opening
                                  (when right-flanking-delimiter-run-p
                                    (push (list _-bold-italic-opening (+ 3 i)) bold-italics)
                                    (setq _-bold-italic-opening nil))
                                  (when left-flanking-delimiter-run-p
                                    (setq _-bold-italic-opening i)))))
                           (incf i (1- len)))))
                  (#\[ (setq link-title-opening i)
                       (if (and (> i 0)
                                (eql (char text (1- i)) #\!))
                         (setq image-opening (1- i))
                         (setq image-opening nil)))
                  (#\] (when link-title-opening
                         (unless (and (< i (1- (length text)))
                                      (eql (char text (1+ i)) #\())
                           (if image-opening
                             (push (list image-opening (1+ i)) images)
                             (push (list link-title-opening (1+ i)) links)))
                         (setq link-title-closing i)))
                  (#\< (setq pointy-opening i))
                  (#\> (when pointy-opening
                         (unless link-dest-opening
                           (push (list pointy-opening (1+ i)) links)))
                       (setq pointy-opening nil))
                  (#\( (unless pointy-opening
                         (when (eql link-title-closing (1- i))
                           (setq link-title-closing nil
                                 link-dest-opening i))))
                  (#\) (unless pointy-opening
                         (when link-dest-opening
                           (when link-title-opening
                             (if image-opening
                               (progn
                                 (push (list image-opening (1+ i)) images))
                               (progn
                                 (push (list link-title-opening (1+ i)) links)))
                             (setq image-opening nil
                                   link-title-opening nil
                                   link-dest-opening nil)))))
                  ((or #\Return #\Newline)
                   (setq single-code-span-opening nil double-code-span-opening nil
                         single-strikethrough-opening nil double-strikethrough-opening nil
                         *-italic-opening nil *-bold-opening nil *-bold-italic-opening nil
                         _-italic-opening nil _-bold-opening nil _-bold-italic-opening nil
                         image-opening nil link-title-opening nil link-title-closing nil
                         link-dest-opening nil pointy-opening nil))))
              (incf i)
              (when (= i (length text)) (return)))))
    (list :code-spans code-spans :strikethroughs strikethroughs
          :bolds      bolds      :italics        italics        :bold-italics bold-italics
          :images     images     :links          links)))

;; Font-lock Functions

(defun fontify-syntactically-region (start end)
  (editor::paragraph-offset start -1)
  (editor::paragraph-offset end 1)
  (editor:with-point ((s start)
                      (e end))
    (let* ((str (editor:points-to-string start end))
           (lines (split-sequence '(#\Newline) str))
           (prev-lines (split-sequence '(#\Newline)
                                       (editor:points-to-string
                                        (editor:buffers-start (editor:point-buffer start)) start)))
           (tilde-fence-opening (oddp (count-if #'line-tilde-fence-p prev-lines)))
           (backquote-fence-opening (oddp (count-if #'line-backquote-fence-p prev-lines)))
           (result (search-lines lines tilde-fence-opening backquote-fence-opening)))
      
      (editor::merge-face-property s e 'md-default-face :modification nil)
      (loop for key in '(:quotes :headings :list-items :codes :link-defs :breaks :tables)
            for face in '(md-quote-face md-header-face md-list-face md-code-block-face md-link-face md-thematic-break-face md-table-face)
            do (loop for prev-line = 0 then line
                     for line in (sort (getf result key) #'<)
                     do (editor:line-offset s (- line prev-line))
                        (editor:move-point e s)
                        (editor:line-end e)
                        (editor::merge-face-property s e face :modification nil))
               (editor:move-point s start)
               (editor:move-point e start))
      )))

(defun fontify-keywords-function (start end)
  (editor::paragraph-offset start -1)
  (editor::paragraph-offset end 1)
  (editor:with-point ((s start) (e end))
    (let* ((str (editor:points-to-string start end))
           (pos (editor:point-position start))
           (result (search-inlines str)))
      (loop for key in '(:code-spans :strikethroughs :bolds :italics :bold-italics :images :links)
            for face in '(md-code-span-face md-strikethrough-face md-bold-face md-italic-face md-bold-italic-face md-image-face md-link-face)
            do (loop for (pos-start pos-end) in (getf result key) 
                     do (setf (editor:point-position s) (+ pos pos-start)
                              (editor:point-position e) (+ pos pos-end))
                        (unless (or (member 'md-code-block-face (editor:get-text-property s 'editor:face))
                                    (member 'md-code-block-face (editor:get-text-property e 'editor:face)))
                          (editor::merge-face-property s e face :modification nil)
                          (editor:move-point s start)
                          (editor:move-point e start)))))))

;; Mode definition

(editor::defmode "Markdown"
  :major-p t
  :vars '((editor::font-lock-fontify-syntactically-region-function
           . fontify-syntactically-region)
          (editor::font-lock-fontify-keywords-region-function
           . fontify-keywords-function)))

(setf (editor:variable-value 'editor::font-lock-fontify-by-default :mode "Markdown") t)

(editor:add-global-hook editor::markdown-mode-hook #'editor:turn-on-font-lock)

(editor:defcommand "Markdown Mode" (p)
     "Enable Markdown Mode."
     "Enable Markdown Mode."
  (let ((buffer (editor:current-buffer)))
    (when (if p (plusp p) (not (equal (editor:buffer-major-mode buffer) "Markdown")))
      (setf (editor:buffer-major-mode buffer) "Markdown"))))

(editor:define-file-type-hook
    ("md" "markdown")
    (buffer type)
  (declare (ignore type))
  (setf (editor:buffer-major-mode buffer) "Markdown"))

;; For the use within IDE

(define-action "Initialize LispWorks Tools" "Initialize Editor-Markdown"
               (lambda (&rest args)
                 (declare (ignore args))
                 (update-face))
               :after "Create default the tools")

(export '(*sans-serif-font-family-name*
          *monospace-font-family-name*
          *font-size*
          *header-font-size-increasement*
          md-link-face
          md-bold-face
          md-list-face
          md-quote-face
          md-table-face
          md-image-face
          md-header-face
          md-italic-face
          md-default-face
          md-strikeout-face
          md-code-span-face
          md-code-block-face
          md-bold-italic-face
          md-strikethrough-face
          update-face
          fontify-syntactically-region
          fontify-keywords-function))
