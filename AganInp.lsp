;�������뷨�Զ��л�V2.5   by ���� 2021.9.3
;ʹ�÷�����
;1���޸ĵ�9��dll·����
;2������lisp�ļ���ΪCAD�Զ����أ�ok�㶨��
;���Ӧ���е������������Ҫ�����޸ġ�
(vl-load-com);����VL����

;����������Ϊ�Զ�����dll���룬�޸�Ϊ���AganInp.dll·����ע��·��Ϊ˫б�ܣ�
(vl-cmdf "netload" "G:\\CAD\\AganInp.dll")

;����������Ϊ��ݼ���c:��Ϊ��ݼ���;�ź�Ϊע�ͣ����Լ��ɸ�����Ҫ�޸�
(defun c:pz() (vl-cmdf "agpz")(princ));������
(defun c:gbs() (vl-cmdf "GC:UnloadInp")(princ));�ֶ��ر����뷨�Զ��л�
(defun c:dks() (vl-cmdf "GC:loadInp")(princ));�ֶ������뷨�Զ��л�

(defun S::STARTUP () ;�˺����ڼ��ز˵�֮����أ��������������ְ汾����ʱ��ͻ�����¹��ܲ�ȫ
  (if (not MSrct) (setq MSrct (vlr-mouse-reactor nil '((:vlr-beginDoubleClick . double_click)))))        ;��귴Ӧ��-˫���¼�
  (if (not CMDSTrct) (setq CMDSTrct (vlr-command-reactor nil '((:vlr-commandWillStart . cmdstart)))))    ;CMD��Ӧ��-CMD����
  (if (not LSPSTrct) (setq LSPSTrct (vlr-lisp-reactor nil '((:vlr-lispWillStart . lispstart)))))         ;LSP��Ӧ��-lisp�Զ�������
  (if (not CMDEDrct) (setq CMDEDrct (vlr-command-reactor nil '((:vlr-commandEnded . change2en)))))       ;CMD��Ӧ��-������ɺ��л�ΪӢ��
  (if (not CMDCErct) (setq CMDCErct (vlr-command-reactor nil '((:vlr-commandCancelled . change2en)))))   ;CMD��Ӧ��-����ȡ�����л�ΪӢ��
  (if (not UNCMDrct) (setq UNCMDrct (vlr-command-reactor nil '((:vlr-unknownCommand . change2en)))))     ;CMD��Ӧ��-����δ֪������л�ΪӢ��
)

;�ֶ�������
(if (not MSrct) (setq MSrct (vlr-mouse-reactor nil '((:vlr-beginDoubleClick . double_click)))))        ;��귴Ӧ��-˫���¼�
(if (not CMDSTrct) (setq CMDSTrct (vlr-command-reactor nil '((:vlr-commandWillStart . cmdstart)))))    ;CMD��Ӧ��-CMD����
(if (not LSPSTrct) (setq LSPSTrct (vlr-lisp-reactor nil '((:vlr-lispWillStart . lispstart)))))         ;LSP��Ӧ��-lisp�Զ�������
(if (not CMDEDrct) (setq CMDEDrct (vlr-command-reactor nil '((:vlr-commandEnded . change2en)))))       ;CMD��Ӧ��-������ɺ��л�ΪӢ��
(if (not CMDCErct) (setq CMDCErct (vlr-command-reactor nil '((:vlr-commandCancelled . change2en)))))   ;CMD��Ӧ��-����ȡ�����л�ΪӢ��
(if (not UNCMDrct) (setq UNCMDrct (vlr-command-reactor nil '((:vlr-unknownCommand . change2en)))))     ;CMD��Ӧ��-����δ֪������л�ΪӢ��
  
;���˫��
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
          (GC:doubleclick text);����C#�ж���Ӣ�ģ��л����뷨
        )
      )
    )
  )
  (princ)
)

;CMD���Ӧ��
;���ܣ��������л����Ļ�Ӣ��
(defun cmdstart (a b / cmd) 
  (setq cmd (car b))
  (if (or (= cmd "TOBJEDIT") (= cmd "SAVEAS") (= cmd "TSAVEAS") (= cmd "EATTEDIT"))  ;�������л�Ϊ���ģ��Լ�������Ҫ�޸�����
    (GC:cmdstart "����") ;����C#���������л�Ϊ����
  )
  (if (or (= cmd "TABLEDIT"))  ;�������л�ΪӢ�ģ��Լ�������Ҫ�޸�����
    (GC:cmdstart "EN") ;����C#���������л�ΪӢ��
  )
  (princ)
)

;lisp�Զ������Ӧ��
;���ܣ��������л����Ļ�Ӣ��
(defun lispstart (a b / lsp) 
  (setq lsp (car b))
  (if (or (= lsp "(C:G1)") (= lsp "(C:G2)"))  ;�������л�Ϊ���ģ�G1��G2Ϊ�Զ�������Լ�������Ҫ�޸�����
    (GC:cmdstart "����") ;����C#���������л�Ϊ����
  )
  (if (or (= lsp "(C:G3)") (= lsp "(C:G4)"))  ;�������л�ΪӢ�ģ�G3��G4Ϊ�Զ�������Լ�������Ҫ�޸�����
    (GC:cmdstart "EN") ;����C#���������л�ΪӢ��
  )
  (princ)
)

;CMD���Ӧ��
;���ܣ��л�ΪӢ��
(defun change2en (a b)
 (GC:cmdend "EN") ;C#�л����뷨ΪӢ��
 (princ)
)

;��ȡ��������,ȥ�����ø�ʽ����--��������
(defun DK:mtext2text(MTextString / regex s)
  (setq regex(vlax-create-object "Vbscript.RegExp")) ;����������ʽ�ؼ�
  (vlax-put-property regex "IgnoreCase" 0) ;�����Դ�Сд
  (vlax-put-property regex "Global" 1) ;ƥ�䷽ʽ��ȫ����ƥ��
  (setq s MTextString)
     ;�滻\\�ַ�
  (vlax-put-property regex "Pattern" "\\\\\\\\")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 1)))
     ;�滻\{�ַ�
  (vlax-put-property regex "Pattern" "\\\\{")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 2)))
     ;�滻\}�ַ�
  (vlax-put-property regex "Pattern" "\\\\}")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 3)))
     ;ɾ������������ʽ
  (vlax-put-property regex "Pattern" "\\\\pi(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���Ʊ����ʽ
  (vlax-put-property regex "Pattern" "\\\\pt(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���ѵ���ʽ
  (vlax-put-property regex "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����塢��ɫ���ָߡ��־ࡢ��б���ֿ������ʽ
  (vlax-put-property regex "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���»��ߡ�ɾ���߸�ʽ
  (vlax-put-property regex "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ������Ͽո��ʽ
  (vlax-put-property regex "Pattern" "\\\\~")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����з���ʽ
  (vlax-put-property regex "Pattern" "\\\\P")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����з���ʽ(���Shift+Enter��ʽ)
  (vlax-put-property regex "Pattern" "\n")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ��{}
  (vlax-put-property regex "Pattern" "({|})")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ��%< >%
  (vlax-put-property regex "Pattern" "(%<|>%)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     
     ;�滻��\\,\{,\}�ַ�
  (vlax-put-property regex "Pattern" "\\x01")
  (setq s(vlax-invoke-method  regex "Replace" s "\\"))
  (vlax-put-property regex "Pattern" "\\x02")
  (setq s(vlax-invoke-method  regex "Replace" s "{"))
  (vlax-put-property regex "Pattern" "\\x03")
  (setq s(vlax-invoke-method  regex "Replace" s "}"))
     
  (vlax-release-object regex)
  s
)
