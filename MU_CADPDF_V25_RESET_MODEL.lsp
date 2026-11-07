 ; MU CADPDF V25: Model / Layout | Reset selection after closing dialog.
 ; Uses MU24CORE (.NET), no PDF24 Creator, no temporary DWG.
 ; COMMAND: MUIN25 / MU25CHECK (diagnostics).
(vl-load-com)
(setq MU25:pages nil MU25:pos 0 MU25:dev nil MU25:paper nil MU25:style nil
      MU25:orientation 0 MU25:fit 0 MU25:output 0)

(defun MU25:note (s) (princ (strcat "\n[MU CADPDF V25] " s)))
(defun MU25:array-list (v)
  (cond ((= (type v) 'VARIANT) (MU25:array-list (vlax-variant-value v)))
        ((= (type v) 'SAFEARRAY) (vlax-safearray->list v))
        ((listp v) v) (T nil)))
(defun MU25:call (fn args) (vl-catch-all-apply fn args))
(defun MU25:ok (v) (not (vl-catch-all-error-p v)))
(defun MU25:tryget (obj prop / r)
  (setq r (MU25:call 'vlax-get-property (list obj prop)))
  (if (MU25:ok r) r nil))
(defun MU25:tryput (obj prop value)
  (MU25:call 'vlax-put-property (list obj prop value)))
(defun MU25:2d (p / arr)
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-put-element arr 0 (float (car p)))
  (vlax-safearray-put-element arr 1 (float (cadr p))) arr)
(defun MU25:rect (pts / xx yy)
  (if pts
    (progn (setq xx (mapcar 'car pts) yy (mapcar 'cadr pts))
           (list (list (apply 'min xx) (apply 'min yy))
                 (list (apply 'max xx) (apply 'max yy))))))
(defun MU25:width (w) (- (caadr w) (caar w)))
(defun MU25:height (w) (- (cadadr w) (cadar w)))
(defun MU25:centerx (w) (/ (+ (caar w) (caadr w)) 2.0))
(defun MU25:centery (w) (/ (+ (cadar w) (cadadr w)) 2.0))
(defun MU25:close (a b tol) (< (abs (- a b)) tol))
(defun MU25:same (a b)
  (and (MU25:close (MU25:centerx a) (MU25:centerx b) 0.5)
       (MU25:close (MU25:centery a) (MU25:centery b) 0.5)))
(defun MU25:polypts (en / d pts code f n bulges)
  (if (= (type en) 'ENAME)
    (progn
      (setq d (entget en) pts nil)
      (if (and (= (cdr (assoc 0 d)) "LWPOLYLINE")
               (= (cdr (assoc 90 d)) 4)
               (= 1 (logand 1 (if (assoc 70 d) (cdr (assoc 70 d)) 0))))
          (progn
            (foreach code d
              (if (= (car code) 10) (setq pts (cons (cdr code) pts))))
            (setq pts (reverse pts))
            (setq bulges nil)
            (foreach code d
              (if (and (= (car code) 42) (> (abs (cdr code)) 0.000001))
                (setq bulges T)))
            (if (or bulges (/= (length pts) 4)) nil pts)
          ) nil))))
(defun MU25:axisrect (pts / xs ys xmin xmax ymin ymax pass p)
  (if (= (length pts) 4)
    (progn
      (setq xs (mapcar 'car pts) ys (mapcar 'cadr pts)
            xmin (apply 'min xs) xmax (apply 'max xs)
            ymin (apply 'min ys) ymax (apply 'max ys)
            pass (and (> (- xmax xmin) 1.0) (> (- ymax ymin) 1.0)))
      (foreach p pts
        (if (not (and (or (MU25:close (car p) xmin 0.01)
                          (MU25:close (car p) xmax 0.01))
                      (or (MU25:close (cadr p) ymin 0.01)
                          (MU25:close (cadr p) ymax 0.01))))
          (setq pass nil)))
      pass)))
(defun MU25:sheetlike (win / w h ratio)
  (if win
    (progn
      (setq w (MU25:width win) h (MU25:height win))
      (and (> w 140.0) (> h 95.0)
           (setq ratio (/ (max w h) (min w h)))
           (> ratio 1.25) (< ratio 1.68)))))
(defun MU25:score (win layer / s w h)
  (setq w (MU25:width win) h (MU25:height win) s (* w h))
  (if (wcmatch (strcase layer) "*BODER*,*BORDER*,*KHUNG*,*FRAME*")
    (setq s (+ s 1000000.0)))
  (if (and (MU25:close w 420.0 22.0) (MU25:close h 297.0 22.0))
    (setq s (+ s 2000000.0)))
  s)
(defun MU25:matrix-point (p m / x y z)
  ; NENTSEL returns MCS-to-WCS rows: X axis, Y axis, Z axis, origin.
  (setq x (car p) y (cadr p) z 0.0)
  (list (+ (* x (car (nth 0 m))) (* y (car (nth 1 m)))
           (* z (car (nth 2 m))) (car (nth 3 m)))
        (+ (* x (cadr (nth 0 m))) (* y (cadr (nth 1 m)))
           (* z (cadr (nth 2 m))) (cadr (nth 3 m)))))
(defun MU25:insert-point (p ins base sx sy ang / dx dy c s)
  (setq dx (* sx (- (car p) (car base)))
        dy (* sy (- (cadr p) (cadr base)))
        c (cos ang) s (sin ang))
  (list (+ (car ins) (- (* dx c) (* dy s)))
        (+ (cadr ins) (+ (* dx s) (* dy c)))))
(defun MU25:block-frame (parent / d bnm block bp ins sx sy ang e pts transformed
                               win sc best bestsc layer normal)
  (if (and (= (type parent) 'ENAME)
           (= (cdr (assoc 0 (setq d (entget parent)))) "INSERT"))
    (progn
      (setq bnm (cdr (assoc 2 d))
            block (tblobjname "BLOCK" bnm)
            ins (cdr (assoc 10 d))
            sx (if (assoc 41 d) (cdr (assoc 41 d)) 1.0)
            sy (if (assoc 42 d) (cdr (assoc 42 d)) 1.0)
            ang (if (assoc 50 d) (cdr (assoc 50 d)) 0.0)
            normal (if (assoc 210 d) (cdr (assoc 210 d)) '(0.0 0.0 1.0))
            best nil bestsc nil)
      ; Selection report shows ordinary 2D paper-space frames.
      ; Only transform by hand for standard Z-normal inserts.
      (if (and block (MU25:close (car normal) 0.0 0.001)
               (MU25:close (cadr normal) 0.0 0.001)
               (> (caddr normal) 0.999))
        (progn
          (setq bp (cdr (assoc 10 (entget block))))
          (if (null bp) (setq bp '(0.0 0.0 0.0)))
          (setq e (entnext block))
          (while (and e (/= (cdr (assoc 0 (entget e))) "ENDBLK"))
            (setq pts (MU25:polypts e))
            (if (and pts (MU25:axisrect pts))
              (progn
                (setq transformed (mapcar '(lambda (p)
                  (MU25:insert-point p ins bp sx sy ang)) pts))
                (setq win (MU25:rect transformed))
                (if (MU25:sheetlike win)
                  (progn
                    (setq layer (cdr (assoc 8 (entget e)))
                          sc (MU25:score win layer))
                    (if (or (null bestsc) (> sc bestsc))
                      (setq best win bestsc sc))))))
            (setq e (entnext e)))))
      best)))
(defun MU25:bounding (en / ob r lo hi)
  (if (= (type en) 'ENAME)
    (progn
      (setq ob (vlax-ename->vla-object en)
            r (MU25:call 'vla-GetBoundingBox (list ob 'lo 'hi)))
      (if (MU25:ok r) (MU25:rect (list (MU25:array-list lo) (MU25:array-list hi)))))))
(defun MU25:selectframe (pick / en d pts matrix parent parents win direct layer)
  (setq en (car pick) win nil)
  (if (= (type en) 'ENAME)
    (progn
      (setq d (entget en) pts (MU25:polypts en)
            matrix (if (> (length pick) 2) (nth 2 pick) nil)
            parents (if (> (length pick) 3) (nth 3 pick) nil)
            parent (if (and (listp parents) parents) (car (last parents)) nil))
      ; Prefer selected OUTER rectangle, not titlebox lines.
      (if (and pts (MU25:axisrect pts))
        (progn
          (setq direct (MU25:rect
            (mapcar '(lambda (p)
              (if (and (listp matrix) (= (length matrix) 4))
                (MU25:matrix-point p matrix)
                (trans (list (car p) (cadr p) 0.0) en 0))) pts)))
          (if (MU25:sheetlike direct) (setq win direct))))
      ; If the clicked geometry is a child of an XREF, find outer BODER.
      (if (and (null win) (= (type parent) 'ENAME))
        (setq win (MU25:block-frame parent)))
      ; Clicking an INSERT directly also works.
      (if (and (null win) (= (cdr (assoc 0 d)) "INSERT"))
        (setq win (MU25:block-frame en) parent en))
      ; Only use INSERT extents as clearly announced fallback.
      (if (and (null win) (= (type parent) 'ENAME))
        (progn
          (setq direct (MU25:bounding parent))
          (if (MU25:sheetlike direct)
            (progn (MU25:note "Canh bao: dung bounding box Block thay cho BODER.")
                   (setq win direct)))))
      (if (and (null win) (= (cdr (assoc 0 d)) "INSERT"))
        (progn
          (setq direct (MU25:bounding en))
          (if (MU25:sheetlike direct)
            (progn (MU25:note "Canh bao: dung bounding box Block thay cho BODER.")
                   (setq win direct)))))
      (if win
        (list win
              (if (= (type parent) 'ENAME)
                (cdr (assoc 2 (entget parent)))
                (if (= (cdr (assoc 0 d)) "INSERT")
                  (cdr (assoc 2 d)) (cdr (assoc 0 d)))))))))
(defun MU25:exists (win / found p)
  (setq found nil)
  (foreach p MU25:pages
    (if (MU25:same win (car p)) (setq found T))) found)
(defun MU25:add-page (rec)
  (cond
    ((null rec) (MU25:note "Khong tim thay BODER A3 ben trong Block. Click vao NET KHUNG NGOAI."))
    ((MU25:exists (car rec)) (MU25:note "Khung nay da chon - bo qua."))
    (T (setq MU25:pages (append MU25:pages (list rec))
             MU25:pos (1- (length MU25:pages)))
       (MU25:mark-new (car rec) (length MU25:pages))
       (MU25:note (strcat "Da them to " (itoa (length MU25:pages))
              " | " (rtos (MU25:width (car rec)) 2 1) " x "
              (rtos (MU25:height (car rec)) 2 1) " | " (cadr rec))))))
; ---------------------------------------------------------------------------
; Paper-space scanner.  The clicked point may be BLANK inside the Xref frame.
; It does not call ENTSEL.  Works with locked/unselectable references as long
; as their INSERT reference and nested BODER polyline are present in DWG.
; ---------------------------------------------------------------------------
(defun MU25:scan-uncached (/ ss sp i en d nm win preferred other polys pts layer cand)
  ; Both Model and Paper: scan inserted XREF frames. Model also scans
  ; independent RECTANG/LWPOLYLINE frames on BORDER or A3-sized layers.
  (setq sp (if (= (getvar "TILEMODE") 1) "Model" (getvar "CTAB"))
        preferred nil other nil)
  (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 410 sp))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i) d (entget en) nm (cdr (assoc 2 d)))
        (if (and nm (or (wcmatch (strcase nm) "*KTA3*KT*")
                        (wcmatch (strcase nm) "*KHUNG*TEN*")))
          (progn
            (setq win (MU25:block-frame en))
            (if win (setq preferred (cons (list win nm en) preferred)))))
        (setq i (1+ i)))
      (if (null preferred)
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq en (ssname ss i) d (entget en)
                  nm (cdr (assoc 2 d)) win (MU25:block-frame en))
            (if win (setq other (cons (list win nm en) other)))
            (setq i (1+ i)))))))
  (setq cand (if preferred (reverse preferred) (reverse other)))
  (if (= sp "Model")
    (progn
      (setq polys (ssget "_X" '((0 . "LWPOLYLINE") (410 . "Model"))))
      (if polys
        (progn
          (setq i 0)
          (while (< i (sslength polys))
            (setq en (ssname polys i) d (entget en)
                  pts (MU25:polypts en)
                  layer (strcase (cdr (assoc 8 d))))
            (if (and pts (MU25:axisrect pts)
                     (setq win (MU25:rect pts)) (MU25:sheetlike win)
                     (or (wcmatch layer "*BODER*,*BORDER*,*KHUNG*,*FRAME*")
                         (and (MU25:close (MU25:width win) 420.0 3.0)
                              (MU25:close (MU25:height win) 297.0 3.0))))
              (setq cand (append cand (list (list win "RECTANG MODEL" en)))))
            (setq i (1+ i)))))))
  cand)

(defun MU25:scan (/ key)
  (setq key (strcat (strcase (getvar "DWGPREFIX")) "|" (strcase (getvar "DWGNAME"))
                    "|" (strcase (getvar "CTAB")) "|" (itoa (getvar "TILEMODE"))))
  (if (or (null MU25:scan-cache) (not (equal key MU25:scan-key)))
    (setq MU25:scan-key key MU25:scan-cache (MU25:scan-uncached)))
  MU25:scan-cache)

(defun MU25:inside (pt win / lo hi fuzz)
  (setq lo (car win) hi (cadr win) fuzz 0.0001)
  (and (<= (- (car lo) fuzz) (car pt))
       (<= (car pt) (+ (car hi) fuzz))
       (<= (- (cadr lo) fuzz) (cadr pt))
       (<= (cadr pt) (+ (cadr hi) fuzz))))
(defun MU25:at-point (pt frames / hit item a amin)
  (setq hit nil amin nil)
  (foreach item frames
    (if (MU25:inside pt (car item))
      (progn
        (setq a (* (MU25:width (car item)) (MU25:height (car item))))
        (if (or (null amin) (< a amin))
          (setq hit item amin a)))))
  hit)
(defun MU25:pickpages (/ frames pt rec count)
  (setq frames (MU25:scan))
  (cond
    ((null frames)
     (alert (strcat
       "Khong tim thay khung KTA3 KT co duong bao BODER trong tab "
       (getvar "CTAB") ".\n"
       "Kiem tra Xref da LOAD va o dung Model/Layout.\n"
       "Chay MU25NEST de chon truc tiep mot NET cua Block.")))
    (T
      (MU25:note (strcat "Tim thay " (itoa (length frames))
        " khung Block/Xref. NHAP 1 DIEM BEN TRONG MOI TO."))
      (MU25:note "Thu tu in la thu tu ban click. ENTER de ket thuc.")
      (setq count 0)
      (while (setq pt (getpoint "\nClick TRONG to can in <ENTER = xong>: "))
        (setq rec (MU25:at-point (trans pt 1 0) frames))
        (if rec
          (progn
            (MU25:add-page (list (car rec) (cadr rec)))
            (setq count (1+ count)))
          (MU25:note "Diem nay KHONG nam trong khung duoc tim thay. Click lai.")))
      (MU25:note (strcat "Da ket thuc chon. Tong: "
         (itoa (length MU25:pages)) " to."))))
  (princ))
(defun c:MU25QUET (/ frames f out i)
  (vl-load-com)
  (if (and (= (getvar "TILEMODE") 0) (> (getvar "CVPORT") 1))
    (alert "Trong Layout, nhap PSPACE de quet khung.")
    (progn
      (setq frames (MU25:scan) i 0)
      (MU25:note (strcat "So khung nhan dien: " (itoa (length frames))))
      (foreach f frames
        (setq i (1+ i))
        (MU25:note (strcat "TO " (itoa i) " | " (cadr f)
           " | " (rtos (MU25:width (car f)) 2 2) " x "
           (rtos (MU25:height (car f)) 2 2))))
      (alert (strcat "MU25 QUET KHUNG TRONG MODEL/LAYOUT: " (getvar "CTAB")
         "\nDa tim thay: " (itoa (length frames)) " khung."
         "\nF2 de xem ten va kich thuoc."
         "\nNeu >0: go INXREF de in theo thu tu tu chon."))))
  (princ))
(defun c:MU25NEST (/ item rec)
  (vl-load-com)
  (setq item (nentsel "\nChon 1 NET cua khung ten / XREF: "))
  (if item
    (progn
      (setq rec (MU25:selectframe item))
      (if rec
        (progn (MU25:add-page rec)
               (alert (strcat "Da them khung " (cadr rec)
                 ". Go MUIN16 de mo bang in.")))
        (alert "Khong xac dinh duoc duong BODER tu NET duoc chon."))))
  (princ))
(defun MU25:swap (items a b / i out p)
  (setq i 0 out nil)
  (foreach p items
    (setq out (cons (cond ((= i a) (nth b items))
                          ((= i b) (nth a items)) (T p)) out)
          i (1+ i))) (reverse out))
(defun MU25:delete (items pos / i out p)
  (setq i 0 out nil)
  (foreach p items
    (if (/= i pos) (setq out (cons p out)))
    (setq i (1+ i))) (reverse out))
(defun MU25:dialog-source ()
  (list
    "mucadpdfv25 : dialog {"
    " : image { key=\"mubanner\"; width=84; height=5; fixed_width=true; fixed_height=true; }"
    " : text { label=\"MANCHESTER UNITED  |  CAD PDF EXPORT\"; alignment=centered; width=72; }"
    " : text { label=\"Tàn nhẫn với bản thân mình là fan MU. Mà sao cố chấp vẫn yêu\"; alignment=centered; width=72; }"
    " label = \"MU CADPDF V25 - CHON KHUNG MODEL / LAYOUT\";"
    " : text { label = \"THEM > click TRONG to (khong can chon net). Chon thu tu tuy y.\"; }"
    " : text { key=\"mode\"; width=68; label=\"\"; }"
    " : boxed_column { label = \"DANH SACH TO BAN VE\";"
    " : list_box { key = \"pages\"; width = 60; height = 12; }"
    " : text { key = \"count\"; width=56; }"
    " : row {"
    "   : button { key=\"add\"; label=\"+ CHON TO\"; width=17; }"
    "   : button { key=\"up\"; label=\"LEN\"; width=9; }"
    "   : button { key=\"down\"; label=\"XUONG\"; width=9; }"
    "   : button { key=\"remove\"; label=\"XOA\"; width=9; }"
    "   : button { key=\"view\"; label=\"XEM TO\"; width=12; }"
    " }"
    " }"
    " : text { label=\"Khung CHON hien khung mau do va so thu tu mau vang; khong in ra PDF.\"; }"
    " : row {"
    "   : button { key=\"plot\"; label=\"CHON GIAY & IN / XUAT PDF\"; is_default=true; width=31; }"
    "   : button { key=\"cancel\"; label=\"DONG\"; is_cancel=true; width=12; }"
    " }"
    "}"
    "mucadpdfv25plot : dialog {"
    " : image { key=\"mubanner\"; width=84; height=5; fixed_width=true; fixed_height=true; }"
    " : text { label=\"MANCHESTER UNITED  |  CAD PDF EXPORT\"; alignment=centered; width=72; }"
    " : text { label=\"Tàn nhẫn với bản thân mình là fan MU. Mà sao cố chấp vẫn yêu\"; alignment=centered; width=72; }"
    " label = \"MU CADPDF V25 - THIET LAP IN\";"
    " : boxed_column { label = \"THONG SO IN\";"
    "   : popup_list { key=\"device\"; label=\"Printer\"; width=48; }"
    "   : popup_list { key=\"media\"; label=\"Paper size\"; width=48; }"
    "   : popup_list { key=\"style\"; label=\"Plot style\"; width=48; }"
    "   : popup_list { key=\"orient\"; label=\"Chieu giay\"; width=34; }"
    "   : popup_list { key=\"scale\"; label=\"Ti le\"; width=34; }"
    "   : text { label=\"Dau ra: 1 file PDF nhieu trang (AutoCAD .NET)\"; }"
    " }"
    " : text { label=\"Doi Printer se cap nhat danh sach kho giay.\"; }"
    " : row {"
    "   : button { key=\"accept\"; label=\"BAT DAU IN\"; is_default=true; width=20; }"
    "   : button { key=\"cancel\"; label=\"QUAY LAI\"; is_cancel=true; width=16; }"
    " }"
    "}"
  ))
(defun MU25:make-dcl (/ path f lines)
  (setq path (vl-filename-mktemp "mucadpdfv25_" nil ".dcl")
        f (open path "w"))
  (if f
    (progn (foreach lines (MU25:dialog-source) (write-line lines f))
           (close f) path)
    nil))
(defun MU25:setlist (tile arr / x)
  (start_list tile 3)
  (foreach x arr (add_list x))
  (end_list))
(defun MU25:idx (v items / i)
  (setq i (vl-position v items)) (if i i 0))
(defun MU25:savepos ()
  (if (and (get_tile "pages") (/= (get_tile "pages") ""))
    (setq MU25:pos (atoi (get_tile "pages")))))
(defun MU25:manager (dcl / code i rec str w)
  (setq code 0)
  (if (new_dialog "mucadpdfv25" dcl)
    (progn
      (MU25:paint-brand)
      (set_tile "mode" (if (= (getvar "TILEMODE") 1)
          "DANG CHON TRONG MODEL | khung duoc danh so vang"
          "DANG CHON TRONG LAYOUT (PAPER) | khung duoc danh so vang"))
      (setq i 1)
      (MU25:setlist "pages"
        (mapcar '(lambda (rec)
          (setq w (car rec)
                str (strcat (itoa i) "  | "
                    (rtos (MU25:width w) 2 1) " x "
                    (rtos (MU25:height w) 2 1) " mm  | " (cadr rec))
                i (1+ i)) str) MU25:pages))
      (set_tile "count" (strcat "DA CHON: " (itoa (length MU25:pages)) " TO"))
      (if MU25:pages
        (set_tile "pages" (itoa (max 0 (min MU25:pos (1- (length MU25:pages)))))))
      (action_tile "pages" "(setq MU25:pos (atoi $value))")
      (action_tile "add" "(MU25:savepos) (done_dialog 2)")
      (action_tile "up" "(MU25:savepos) (done_dialog 3)")
      (action_tile "down" "(MU25:savepos) (done_dialog 4)")
      (action_tile "remove" "(MU25:savepos) (done_dialog 5)")
      (action_tile "view" "(MU25:savepos) (done_dialog 6)")
      (action_tile "plot" "(MU25:savepos) (done_dialog 7)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq code (start_dialog)))) code)
(defun MU25:show-page (/ w p q)
  (if (and MU25:pages (< MU25:pos (length MU25:pages)))
    (progn
      (setq w (car (nth MU25:pos MU25:pages))
            p (car w) q (cadr w))
      (vl-cmdf "_.ZOOM" "_W"
        (list (- (car p) 12.0) (- (cadr p) 12.0))
        (list (+ (car q) 12.0) (+ (cadr q) 12.0))))
    (MU25:note "Chua chon to nao.")))
(defun MU25:get-devices (lay / v)
  (MU25:call 'vla-RefreshPlotDeviceInfo (list lay))
  (setq v (MU25:call 'vla-GetPlotDeviceNames (list lay)))
  (if (MU25:ok v) (MU25:array-list v) nil))
(defun MU25:media (lay / v)
  (MU25:call 'vla-RefreshPlotDeviceInfo (list lay))
  (setq v (MU25:call 'vla-GetCanonicalMediaNames (list lay)))
  (if (MU25:ok v) (MU25:array-list v) nil))
(defun MU25:get-styles (lay / v all target)
  (setq v (MU25:call 'vla-GetPlotStyleTableNames (list lay)))
  (setq all (if (MU25:ok v) (MU25:array-list v) nil)
        target (if (= (getvar "PSTYLEMODE") 1) "*.CTB" "*.STB"))
  (cons "(Khong ap dung)"
    (vl-remove-if-not '(lambda (s) (wcmatch (strcase s) target)) all)))
(defun MU25:select-default-pdf (items / out p)
  (setq out nil)
  (foreach p items
    (if (and (null out) (wcmatch (strcase p) "*DWG*TO*PDF*.PC3"))
      (setq out p)))
  (if (null out)
    (foreach p items
      (if (and (null out) (wcmatch (strcase p) "*PDF*.PC3"))
        (setq out p))))
  (if out out (car items)))
(defun MU25:default-media (items / p result)
  (setq result nil)
  (foreach p items
    (if (and (null result) (wcmatch (strcase p) "*FULL*BLEED*A3*"))
      (setq result p)))
  (if (null result)
    (foreach p items
      (if (and (null result) (wcmatch (strcase p) "*A3*"))
        (setq result p))))
  (if result result (car items)))
(defun MU25:settings-load-device (lay / r)
  (setq r (MU25:call 'vla-put-ConfigName (list lay MU25:dev)))
  (if (MU25:ok r)
    (progn
      (MU25:call 'vla-RefreshPlotDeviceInfo (list lay))
      (setq MU25:medias (MU25:media lay))
      (if (not (member MU25:paper MU25:medias))
        (setq MU25:paper (MU25:default-media MU25:medias)))
      T)
    (progn
      (MU25:note (strcat "Printer khong ho tro: " MU25:dev)) nil)))
(defun MU25:save-print-tiles ()
  (setq MU25:dev (nth (atoi (get_tile "device")) MU25:device-list)
        MU25:paper (nth (atoi (get_tile "media")) MU25:medias)
        MU25:style (nth (atoi (get_tile "style")) MU25:style-list)
        MU25:orientation (atoi (get_tile "orient"))
        MU25:fit (atoi (get_tile "scale"))
        MU25:output 0))
(defun MU25:settings (dcl lay / code has-settings done)
  (setq MU25:device-list (MU25:get-devices lay))
  (if (not MU25:device-list)
    (progn (alert "Khong tim thay may in trong AutoCAD.") nil)
    (progn
      (if (not (member MU25:dev MU25:device-list))
        (setq MU25:dev (MU25:select-default-pdf MU25:device-list)))
      (setq MU25:style-list (MU25:get-styles lay))
      (if (not (member MU25:style MU25:style-list))
        (setq MU25:style (if (member "monochrome.ctb" MU25:style-list)
                          "monochrome.ctb" (car MU25:style-list))))
      (setq done nil has-settings nil)
      (while (not done)
        (if (not (MU25:settings-load-device lay))
          (setq done T)
          (if (new_dialog "mucadpdfv25plot" dcl)
            (progn
              (MU25:paint-brand)
              (MU25:setlist "device" MU25:device-list)
              (MU25:setlist "media" MU25:medias)
              (MU25:setlist "style" MU25:style-list)
              (MU25:setlist "orient" '("Ngang" "Doc"))
              (MU25:setlist "scale" '("Fit to paper" "1:1"))
              (set_tile "device" (itoa (MU25:idx MU25:dev MU25:device-list)))
              (set_tile "media" (itoa (MU25:idx MU25:paper MU25:medias)))
              (set_tile "style" (itoa (MU25:idx MU25:style MU25:style-list)))
              (set_tile "orient" (itoa MU25:orientation))
              (set_tile "scale" (itoa MU25:fit))
              (action_tile "device" "(setq MU25:dev (nth (atoi $value) MU25:device-list) MU25:paper nil) (done_dialog 3)")
              (action_tile "accept" "(MU25:save-print-tiles) (done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")
              (setq code (start_dialog))
              (cond ((= code 1) (setq has-settings T done T))
                    ((= code 3) nil)
                    (T (setq done T))))
            (progn (MU25:note "Khong mo duoc bang cai dat in DCL") (setq done T)))))
      has-settings)))

(defun MU25:snapshot (lay / props one values a b r)
  (setq props '(ConfigName CanonicalMediaName StyleSheet PaperUnits PlotRotation
                PlotType CenterPlot UseStandardScale StandardScale
                PlotWithPlotStyles PlotWithLineweights)
        values nil)
  (foreach one props
    (setq values (cons (cons one (MU25:tryget lay one)) values)))
  (setq r (MU25:call 'vla-GetWindowToPlot (list lay 'a 'b)))
  (if (MU25:ok r)
    (setq values (cons (cons 'win (list (MU25:array-list a) (MU25:array-list b))) values)))
  (setq r (MU25:call 'vla-GetCustomScale (list lay 'a 'b)))
  (if (MU25:ok r)
    (setq values (cons (cons 'custom (list a b)) values)))
  values)
(defun MU25:restore (lay sn / p v)
  (setq v (cdr (assoc 'ConfigName sn)))
  (if v (MU25:tryput lay 'ConfigName v))
  (MU25:call 'vla-RefreshPlotDeviceInfo (list lay))
  (foreach p '(CanonicalMediaName StyleSheet PaperUnits PlotRotation
               PlotWithPlotStyles PlotWithLineweights)
    (setq v (cdr (assoc p sn)))
    (if v (MU25:tryput lay p v)))
  (setq v (cdr (assoc 'win sn)))
  (if v (MU25:call 'vla-SetWindowToPlot (list lay (MU25:2d (car v)) (MU25:2d (cadr v)))))
  (foreach p '(PlotType UseStandardScale StandardScale)
    (setq v (cdr (assoc p sn)))
    (if v (MU25:tryput lay p v)))
  (setq v (cdr (assoc 'custom sn)))
  (if (and v (= (cdr (assoc 'UseStandardScale sn)) :vlax-false))
    (MU25:call 'vla-SetCustomScale (list lay (car v) (cadr v))))
  (setq v (cdr (assoc 'CenterPlot sn)))
  (if v (MU25:tryput lay 'CenterPlot v))
  (princ))


(defun MU25:pad (n / s)
  (setq s (itoa n))
  (while (< (strlen s) 3) (setq s (strcat "0" s))) s)



(defun MU25:ensure-source (/ now)
  (setq now (strcase (strcat (getvar "DWGPREFIX") "|"
                             (getvar "DWGNAME") "|" (getvar "CTAB")
                             "|" (itoa (getvar "TILEMODE")))))
  (if (not (equal now MU25:source))
    (progn (setq MU25:source now MU25:pages nil MU25:pos 0 MU25:scan-cache nil MU25:scan-key nil))))

(defun MU25:reset-session ()
  ; The temporary red frames and yellow numbers are transient selection aids.
  (MU25:clear-marks)
  (setq MU25:pages nil
        MU25:pos 0
        MU25:mark-objs nil
        MU25:scan-cache nil
        MU25:scan-key nil
        MU25:source nil)
  (princ))

(defun c:MU25OPEN (/ *error* doc lay dclpath dclid running action snap res)
  (vl-load-com)
  (defun *error* (message)
    (MU25:reset-session)
    (if (and dclid (> dclid 0))
      (MU25:call 'unload_dialog (list dclid)))
    (if (and lay snap) (MU25:restore lay snap))
    (if (and dclpath (findfile dclpath)) (vl-file-delete dclpath))
    (if (and message (not (wcmatch (strcase message) "*CANCEL*,*QUIT*,*EXIT*")))
      (progn (MU25:note (strcat "LOI: " message))
             (alert (strcat "MU CADPDF V25: " message "\nNhan F2 de xem loi."))))
    (princ))
  (MU25:reset-session)
  (MU25:ensure-source)
  (cond
    (T
      (if (and (= (getvar "TILEMODE") 0) (> (getvar "CVPORT") 1)) (vl-cmdf "_.PSPACE"))
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
            lay (vla-get-ActiveLayout doc)
            snap (MU25:snapshot lay)
            dclpath (MU25:make-dcl)
            running T)
      (if (null dclpath)
        (alert "Khong tao duoc DCL tam. Kiem tra quyen ghi TEMP.")
        (progn
          (setq dclid (load_dialog dclpath))
          (if (and dclid (> dclid 0))
            (progn
              (while running
                (setq action (MU25:manager dclid))
                (cond
                  ((= action 2) (MU25:pickpages))
                  ((= action 3)
                   (if (and MU25:pages (> MU25:pos 0))
                     (progn (setq MU25:pages
                         (MU25:swap MU25:pages MU25:pos (1- MU25:pos)))
                       (setq MU25:pos (1- MU25:pos)))))
                  ((= action 4)
                   (if (and MU25:pages (< MU25:pos (1- (length MU25:pages))))
                     (progn (setq MU25:pages
                         (MU25:swap MU25:pages MU25:pos (1+ MU25:pos)))
                       (setq MU25:pos (1+ MU25:pos)))))
                  ((= action 5)
                   (if MU25:pages
                     (progn (setq MU25:pages (MU25:delete MU25:pages MU25:pos))
                       (setq MU25:pos (max 0 (min MU25:pos (1- (length MU25:pages))))))))
                  ((= action 6) (MU25:show-page))
                  ((= action 7)
                   (if MU25:pages
                     (progn
                       (setq res (MU25:settings dclid lay))
                       (if res (progn (MU25:do-print) (MU25:draw-marks))))
                     (alert "Chua chon khung nao.")))
                  (T (setq running nil)))
                (if (member action '(3 4 5)) (MU25:draw-marks)))
              (unload_dialog dclid))
            (alert "Khong load duoc DCL. Kiem tra AutoCAD DCL."))
          ; Restore settings changed for fetching paper lists, even on Cancel.
          (MU25:reset-session)
          (MU25:restore lay snap)
          (vl-file-delete dclpath)))))
  (princ))
(defun c:MU25KHUNG (/ picked rec win)
  (vl-load-com)
  (if (and (= (getvar "TILEMODE") 0) (> (getvar "CVPORT") 1))
    (alert "Hay nhap PSPACE de chon khung trong Layout.")
    (progn
      (setq picked (nentsel "\nMU25KHUNG: CLICK 1 NET cua khung/XREF KTA3 KT: "))
      (if picked
        (progn
          (setq rec (MU25:selectframe picked))
          (if rec
            (progn
              (setq win (car rec))
              (alert (strcat "DA NHAN DIEN KHUNG\n\nBlock: " (cadr rec)
                "\nRong: " (rtos (MU25:width win) 2 2) " mm"
                "\nCao: " (rtos (MU25:height win) 2 2) " mm"
                "\nChay MUIN25 de chon nhieu to va in."))
              (MU25:note (strcat "KHUNG OK " (cadr rec))))
            (alert "Chua tim duoc duong bao. Hay CLICK truc tiep vao NET KHUNG NGOAI (layer BODER).")))
        (MU25:note "Da huy chon."))))
  (princ))



(princ "\n[MU CADPDF V25] DA NAP. LENH: MUIN25 / MU25CHECK")
(princ)

; Independent debug command: does not modify current plot setup.
(defun c:MU25MAYIN (/ doc lay names)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        lay (vla-get-ActiveLayout doc)
        names (MU25:get-devices lay))
  (alert (strcat "MU CADPDF V25 - Kiem tra may in"
                 "\nSo may in: " (itoa (length names))
                 "\nPDF: " (vl-princ-to-string (MU25:select-default-pdf names))
                 "\nKhong thay doi cai dat in."))
  (princ))

; ==============================================================
; MU CADPDF V25 - visual selection marks, Model support, lightweight in-CAD selection
; ==============================================================
(defun MU25:q (s) (strcat "\"" s "\""))
(defun MU25:nonplot-layer (/ doc lays lay r)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        lays (vla-get-Layers doc)
        r (MU25:call 'vla-Item (list lays "MU25-SELECT-NONPLOT")))
  (setq lay (if (MU25:ok r) r (vla-Add lays "MU25-SELECT-NONPLOT")))
  (MU25:tryput lay 'LayerOn :vlax-true)
  (MU25:tryput lay 'Freeze :vlax-false)
  (MU25:tryput lay 'Lock :vlax-false)
  (MU25:tryput lay 'Plottable :vlax-false)
  (MU25:tryput lay 'Color 1)
  lay)
(defun MU25:clear-marks (/ o)
  (foreach o MU25:mark-objs
    (if o (MU25:call 'vla-Delete (list o))))
  (setq MU25:mark-objs nil)
  (princ))
(defun MU25:markrect (owner win order / lo hi offset x1 y1 x2 y2 arr ob textob sz)
  (setq lo (car win) hi (cadr win)
        offset (max 1.3 (* 0.004 (max (MU25:width win) (MU25:height win))))
        x1 (- (car lo) offset) x2 (+ (car hi) offset)
        y1 (- (cadr lo) offset) y2 (+ (cadr hi) offset)
        arr (vlax-make-safearray vlax-vbDouble '(0 . 7)))
  (vlax-safearray-fill arr (list x1 y1 x2 y1 x2 y2 x1 y2))
  (setq ob (MU25:call 'vla-AddLightWeightPolyline (list owner arr)))
  (if (MU25:ok ob)
    (progn
      (MU25:tryput ob 'Layer "MU25-SELECT-NONPLOT")
      (MU25:tryput ob 'Closed :vlax-true)
      (MU25:tryput ob 'Color 1)
      (MU25:tryput ob 'ConstantWidth (* offset 0.18))
      (setq MU25:mark-objs (cons ob MU25:mark-objs))
      (MU25:call 'vla-Update (list ob))))
  (setq sz (max 2.4 (* 0.022 (MU25:width win))))
  (setq textob (MU25:call 'vla-AddText
    (list owner (strcat "CHON " (MU25:pad order))
      (vlax-3d-point (+ x1 (* offset 0.3)) (+ y2 sz) 0.0) sz)))
  (if (MU25:ok textob)
    (progn
      (MU25:tryput textob 'Layer "MU25-SELECT-NONPLOT")
      (MU25:tryput textob 'Color 2)
      (setq MU25:mark-objs (cons textob MU25:mark-objs))
      (MU25:call 'vla-Update (list textob)))))
(defun MU25:draw-marks (/ doc owner idx it)
  (MU25:clear-marks)
  (if MU25:pages
    (progn
      (MU25:nonplot-layer)
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
            owner (if (= (getvar "TILEMODE") 1)
                    (vla-get-ModelSpace doc) (vla-get-PaperSpace doc))
            idx 0)
      (foreach it MU25:pages
        (setq idx (1+ idx))
        (MU25:markrect owner (car it) idx))
      (redraw)))
  (princ))
(defun MU25:mark-new (win order / doc owner)
  (MU25:nonplot-layer)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        owner (if (= (getvar "TILEMODE") 1)
                (vla-get-ModelSpace doc) (vla-get-PaperSpace doc)))
  (MU25:markrect owner win order)
  ; Refresh current screen only, no expensive database regeneration.
  (redraw)
  (princ))


; V25 commands; V14 short aliases are not overwritten.






(defun MU25:paint-brand (/ x y cx cy rx ry)
  ; Lightweight vector brand drawn with ~20 DCL primitives, not 1334 rectangles.
  ; Native DCL image tiles cannot display a 4K PNG directly; see bundled image.
  (start_image "mubanner")
  (setq x (dimx_tile "mubanner") y (dimy_tile "mubanner"))
  (fill_image 0 0 x y 1)
  (fill_image 0 0 x 3 2)
  (fill_image 0 (- y 3) x 3 2)
  (fill_image 4 5 (max 2 (- x 8)) (max 2 (- y 10)) 12)
  (setq cx (fix (* x 0.075)) cy (fix (* y 0.52))
        rx (max 6 (fix (* x 0.055))) ry (max 5 (fix (* y 0.31))))
  ; stylized crisp shield/crest on the left, and scalable gold separators
  (vector_image (- cx rx) (- cy (fix (* ry 0.5))) (- cx (fix (* rx 0.75))) (+ cy (fix (* ry 0.5))) 2)
  (vector_image (- cx (fix (* rx 0.75))) (+ cy (fix (* ry 0.5))) cx (+ cy ry) 2)
  (vector_image cx (+ cy ry) (+ cx (fix (* rx 0.75))) (+ cy (fix (* ry 0.5))) 2)
  (vector_image (+ cx (fix (* rx 0.75))) (+ cy (fix (* ry 0.5))) (+ cx rx) (- cy (fix (* ry 0.5))) 2)
  (vector_image (+ cx rx) (- cy (fix (* ry 0.5))) cx (- cy ry) 2)
  (vector_image cx (- cy ry) (- cx rx) (- cy (fix (* ry 0.5))) 2)
  (vector_image (- cx (fix (* rx 0.4))) cy (+ cx (fix (* rx 0.4))) cy 2)
  (vector_image cx (- cy (fix (* ry 0.5))) cx (+ cy (fix (* ry 0.5))) 2)
  (setq cx (fix (* x 0.155)))
  (vector_image cx 6 cx (- y 6) 2)
  (vector_image (fix (* x 0.19)) (fix (* y 0.70)) (fix (* x 0.94)) (fix (* y 0.70)) 2)
  (end_image)
  (princ))

; V25 optional manual rescan after attaching/moving new Xrefs while the tool is open.
(defun c:MU25LAMMOI ()
  (setq MU25:scan-cache nil MU25:scan-key nil)
  (MU25:note "Da xoa bo nho tim khung. Lan CHON TO tiep theo se quet lai.")
  (princ))

(princ "\n[MU CADPDF V25] NAP XONG. Go MUIN25 / MU25CHECK.")
(princ)



; =======================================================================
; MU CADPDF V25 - PROVEN V16 UI + NATIVE .NET PLOTENGINE HANDOFF
; No temporary DWG, no PDF24, no intermediary PDFs.
; Requires MU24Direct.dll to be loaded with NETLOAD once.
; =======================================================================
(defun MU25F:outfile (/ dir stem path n)
  (setq dir (getvar "DWGPREFIX")
        stem (vl-filename-base (getvar "DWGNAME"))
        path (strcat dir stem "_TONG_HOP.pdf")
        n 1)
  (while (findfile path)
    (setq path (strcat dir stem "_TONG_HOP_" (itoa n) ".pdf")
          n (1+ n)))
  path)

(defun MU25F:jobfile (/ temp)
  (setq temp (getenv "TEMP"))
  (if (and temp (/= temp ""))
    (strcat (vl-string-right-trim "\\/" temp) "\\MU24_ACTIVE_JOB.txt")
    nil))

(defun MU25:do-print (/ path dest fh item win result ok)
  (cond
    ((null MU25:pages)
     (alert "Chua chon to ban ve."))
    ((= (getvar "DWGTITLED") 0)
     (alert "Hay SAVE file DWG truoc khi xuat PDF."))
    ((not (and MU25:dev
               (wcmatch (strcase MU25:dev) "*PDF*.PC3")))
     (alert "Hay chon AutoCAD PDF.pc3 hoac DWG To PDF.pc3."))
    (T
     (setq path (MU25F:jobfile)
           dest (MU25F:outfile)
           fh (if path (open path "w") nil))
     (if (not fh)
       (alert "Khong ghi duoc file job trong thu muc TEMP cua Windows.")
       (progn
         (foreach item
           (list "MU24-1"
                 (strcat (getvar "DWGPREFIX") (getvar "DWGNAME"))
                 (getvar "CTAB")
                 dest
                 MU25:dev
                 MU25:paper
                 (if MU25:style MU25:style "")
                 (itoa MU25:orientation)
                 (itoa MU25:fit)
                 (itoa (length MU25:pages)))
           (write-line item fh))
         (foreach item MU25:pages
           (setq win (car item))
           (write-line
             (strcat
               (rtos (caar win) 2 10) (chr 9)
               (rtos (cadar win) 2 10) (chr 9)
               (rtos (caadr win) 2 10) (chr 9)
               (rtos (cadadr win) 2 10))
             fh))
         (close fh)
         (MU25:clear-marks)
         (princ
           (strcat "\n[MU25] Da chon "
                   (itoa (length MU25:pages))
                   " to. Dang goi MU24CORE (DLL .NET)..."))
         (setq result
           (vl-catch-all-apply 'vl-cmdf (list "MU24CORE")))
         (if (vl-catch-all-error-p result)
           (alert
             (strcat
               "Khong chay duoc MU24CORE."
               "\nGo NETLOAD va nap MU24Direct.dll truoc."
               "\nChi tiet: "
               (vl-catch-all-error-message result))))))))
  (princ))

(defun c:MUIN25 ()
  (c:MU25OPEN)
  (princ))

(defun c:MU25TEST ()
  (alert
    (strcat
      "MU CADPDF V25: da nap AutoLISP thanh cong."
      "\nTab hien tai: " (getvar "CTAB")
      "\nTiep theo: NETLOAD MU24Direct.dll"
      "\nSau do go MUIN25 de chon to."))
  (princ))

(princ "\n[MU CADPDF V25] LISP DA NAP. GO MU25TEST / MUIN25.")
(princ)


(defun c:MU25CHECK (/ mode frames)
  (vl-load-com)
  (setq mode (if (= (getvar "TILEMODE") 1) "MODEL" "LAYOUT/PAPER"))
  (if (and (= (getvar "TILEMODE") 0) (> (getvar "CVPORT") 1))
    (alert "Ban dang o MODEL TRONG VIEWPORT cua Layout. Go PSPACE hoac chuyen tab Model.")
    (progn
      (setq frames (MU25:scan))
      (alert (strcat "MU CADPDF V25 DA NAP."
         "\nMoi truong: " mode
         "\nSo khung nhan dien: " (itoa (length frames))
         "\nGo MUIN25 de chon ban ve."))))
  (princ))

(princ)
