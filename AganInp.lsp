;阿甘输入法自动切换V2.5   by 阿甘 2021.9.3
;使用方法：
;1、修改第9行dll路径。
;2、将本lisp文件设为CAD自动加载，ok搞定。
;命令反应器中的命令请根据需要自行修改。
(vl-load-com);加载VL函数

;↓↓↓以下为自动加载dll代码，修改为你的AganInp.dll路径（注意路径为双斜杠）
(vl-cmdf "netload" "G:\\CAD\\AganInp.dll")

;↓↓↓以下为快捷键（c:后为快捷键，;号后为注释），自己可根据需要修改
(defun c:pz() (vl-cmdf "agpz")(princ));打开设置
(defun c:gbs() (vl-cmdf "GC:UnloadInp")(princ));手动关闭输入法自动切换
(defun c:dks() (vl-cmdf "GC:loadInp")(princ));手动打开输入法自动切换

(defun S::STARTUP () ;此函数在加载菜单之后加载，避免与天正部分版本加载时冲突，导致功能不全
  (if (not MSrct) (setq MSrct (vlr-mouse-reactor nil '((:vlr-beginDoubleClick . double_click)))))        ;鼠标反应器-双击事件
  (if (not CMDSTrct) (setq CMDSTrct (vlr-command-reactor nil '((:vlr-commandWillStart . cmdstart)))))    ;CMD反应器-CMD命令
  (if (not LSPSTrct) (setq LSPSTrct (vlr-lisp-reactor nil '((:vlr-lispWillStart . lispstart)))))         ;LSP反应器-lisp自定义命令
  (if (not CMDEDrct) (setq CMDEDrct (vlr-command-reactor nil '((:vlr-commandEnded . change2en)))))       ;CMD反应器-命令完成后切换为英文
  (if (not CMDCErct) (setq CMDCErct (vlr-command-reactor nil '((:vlr-commandCancelled . change2en)))))   ;CMD反应器-命令取消后切换为英文
  (if (not UNCMDrct) (setq UNCMDrct (vlr-command-reactor nil '((:vlr-unknownCommand . change2en)))))     ;CMD反应器-输入未知命令后切换为英文
)

;手动加载用
(if (not MSrct) (setq MSrct (vlr-mouse-reactor nil '((:vlr-beginDoubleClick . double_click)))))        ;鼠标反应器-双击事件
(if (not CMDSTrct) (setq CMDSTrct (vlr-command-reactor nil '((:vlr-commandWillStart . cmdstart)))))    ;CMD反应器-CMD命令
(if (not LSPSTrct) (setq LSPSTrct (vlr-lisp-reactor nil '((:vlr-lispWillStart . lispstart)))))         ;LSP反应器-lisp自定义命令
(if (not CMDEDrct) (setq CMDEDrct (vlr-command-reactor nil '((:vlr-commandEnded . change2en)))))       ;CMD反应器-命令完成后切换为英文
(if (not CMDCErct) (setq CMDCErct (vlr-command-reactor nil '((:vlr-commandCancelled . change2en)))))   ;CMD反应器-命令取消后切换为英文
(if (not UNCMDrct) (setq UNCMDrct (vlr-command-reactor nil '((:vlr-unknownCommand . change2en)))))     ;CMD反应器-输入未知命令后切换为英文
  
;鼠标双击
(defun double_click (a b / en end entype text) 
  (if (setq en (nentselp (trans (car b) 0 1))) 
    (progn 
      (setq end (entget (car en)))
      (setq entype (cdr (assoc 0 end)))
      (if (wcmatch entype "*TEXT") 
        (progn 
          (setq text (cdr (assoc 1 end)))
          (if (= entype "MTEXT")
            (setq text (DK:mtext2text text))
          )
          (GC:doubleclick text);调用C#判断中英文，切换输入法
        )
      )
    )
  )
  (princ)
)

;CMD命令反应器
;功能：无条件切换中文或英文
(defun cmdstart (a b / cmd) 
  (setq cmd (car b))
  (if (or (= cmd "TOBJEDIT") (= cmd "SAVEAS") (= cmd "TSAVEAS") (= cmd "EATTEDIT"))  ;无条件切换为中文，自己根据需要修改命令
    (GC:cmdstart "中文") ;调用C#，无条件切换为中文
  )
  (if (or (= cmd "TABLEDIT"))  ;无条件切换为英文，自己根据需要修改命令
    (GC:cmdstart "EN") ;调用C#，无条件切换为英文
  )
  (princ)
)

;lisp自定义命令反应器
;功能：无条件切换中文或英文
(defun lispstart (a b / lsp) 
  (setq lsp (car b))
  (if (or (= lsp "(C:G1)") (= lsp "(C:G2)"))  ;无条件切换为中文，G1、G2为自定义命令，自己根据需要修改命令
    (GC:cmdstart "中文") ;调用C#，无条件切换为中文
  )
  (if (or (= lsp "(C:G3)") (= lsp "(C:G4)"))  ;无条件切换为英文，G3、G4为自定义命令，自己根据需要修改命令
    (GC:cmdstart "EN") ;调用C#，无条件切换为英文
  )
  (princ)
)

;CMD命令反应器
;功能：切换为英文
(defun change2en (a b)
 (GC:cmdend "EN") ;C#切换输入法为英文
 (princ)
)

;提取多行文字,去除无用格式符号--来自明经
(defun DK:mtext2text(MTextString / regex s)
  (setq regex(vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (vlax-put-property regex "IgnoreCase" 0) ;不忽略大小写
  (vlax-put-property regex "Global" 1) ;匹配方式，全文字匹配
  (setq s MTextString)
     ;替换\\字符
  (vlax-put-property regex "Pattern" "\\\\\\\\")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 1)))
     ;替换\{字符
  (vlax-put-property regex "Pattern" "\\\\{")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 2)))
     ;替换\}字符
  (vlax-put-property regex "Pattern" "\\\\}")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 3)))
     ;删除段落缩进格式
  (vlax-put-property regex "Pattern" "\\\\pi(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除制表符格式
  (vlax-put-property regex "Pattern" "\\\\pt(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除堆迭格式
  (vlax-put-property regex "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除字体、颜色、字高、字距、倾斜、字宽、对齐格式
  (vlax-put-property regex "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除下划线、删除线格式
  (vlax-put-property regex "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除不间断空格格式
  (vlax-put-property regex "Pattern" "\\\\~")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除换行符格式
  (vlax-put-property regex "Pattern" "\\\\P")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除换行符格式(针对Shift+Enter格式)
  (vlax-put-property regex "Pattern" "\n")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除{}
  (vlax-put-property regex "Pattern" "({|})")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除%< >%
  (vlax-put-property regex "Pattern" "(%<|>%)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     
     ;替换回\\,\{,\}字符
  (vlax-put-property regex "Pattern" "\\x01")
  (setq s(vlax-invoke-method  regex "Replace" s "\\"))
  (vlax-put-property regex "Pattern" "\\x02")
  (setq s(vlax-invoke-method  regex "Replace" s "{"))
  (vlax-put-property regex "Pattern" "\\x03")
  (setq s(vlax-invoke-method  regex "Replace" s "}"))
     
  (vlax-release-object regex)
  s
)
