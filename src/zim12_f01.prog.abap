*&---------------------------------------------------------------------*
*&  Include           ZIM12_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_X  text
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT  USING P_EDIT.
  REFRESH T_FIELDCATALOG.
  PERFORM F_MONTAR_ESTRUTURA USING:
        1   ''         ''       'TG_AVAL'  'STATUS'     'St'                   '03' ' '    ' ' 'X',
        1   ''         ''       'TG_AVAL'  'LINHA'      'Linha'                '06' ' '    ' ' 'X',
        1   'T001'     'BUKRS'  'TG_AVAL'  'BUKRS'      'Empresa'              '08' P_EDIT ' ' 'X',
        1   ''         ''       'TG_AVAL'  'NIVEL'      'Nível'                '05' P_EDIT ' ' 'X',
        1   'USR21'    'BNAME'  'TG_AVAL'  'APROVADOR'  'Aprovador'            '15' P_EDIT ' ' 'X',
        1   ''         ''       'TG_AVAL'  'GPO_CMP'    'Gpo.Compra'           '05' P_EDIT ' ' 'X',
        1   ''         ''       'TG_AVAL'  'DESC_USNAM' 'Nome'                 '25' ' '    ' ' 'X',
        1   ''         ''       'TG_AVAL'  'DATA_ATUAL' 'Data'                 '12' ' '    ' ' 'X',
        1   ''         ''       'TG_AVAL'  'HORA_ATUAL' 'Hora'                 '12' ' '    ' ' 'X',
        1   ''         ''       'TG_AVAL'  'USUARIO'    'Usuário'              '15' ' '    ' ' 'X'.
ENDFORM.

FORM F_MONTAR_LAYOUT2  USING P_EDIT.
  REFRESH T_FIELDCATALOG.
  PERFORM F_MONTAR_ESTRUTURA USING:
        1   ''         ''       'TG_APROV'  'STATUS'     'St'                   '03' ' '    ' ' '',
        1   ''         ''       'TG_APROV'  'LINHA'      'Linha'                '06' ' '    ' ' '',
        1   'T001'     'BUKRS'  'TG_APROV'  'BUKRS'      'Empresa'              '08' P_EDIT ' ' 'X',
        1   'T001W'    'WERKS'  'TG_APROV'  'WERKS'      'Filial'               '08' P_EDIT ' ' 'X',
        1   'CSKS'     'KOSTL'  'TG_APROV'  'KOSTL'      'C.Custo'              '15' P_EDIT ' ' 'X',
        1   ''         ''       'TG_APROV'  'FASE'       'Fase'                 '08' P_EDIT ' ' 'X',
        1   ''         ''       'TG_APROV'  'NIVEL'      'Nível'                '05' P_EDIT ' ' 'X',
        1   'USR21'    'BNAME'  'TG_APROV'  'APROVADOR'  'Aprovador'            '15' P_EDIT ' ' 'X',
        1   ''         ''       'TG_APROV'  'DESC_USNAM' 'Nome'                 '25' ' '    ' ' 'X',
        1   ''         ''       'TG_APROV'  'DATA_ATUAL' 'Data'                 '12' ' '    ' ' 'X',
        1   ''         ''       'TG_APROV'  'HORA_ATUAL' 'Hora'                 '12' ' '    ' ' 'X',
        1   ''         ''       'TG_APROV'  'USUARIO'    'Usuário'              '15' ' '    ' ' 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MONTAR_ESTRUTURA  USING  P_COL_POS   P_REF_TABNAME P_REF_FIELDNAME P_TABNAME P_FIELD
                                P_SCRTEXT_L P_OUTPUTLEN   P_EDIT          P_SUM     P_EMPHASIZE.

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.

  W_FIELDCATALOG-KEY           = ' '.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS       = P_COL_POS.

  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  ENDIF.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  IF P_FIELD = 'FASE' OR P_FIELD = 'GPO_CMP'.
    W_FIELDCATALOG-F4AVAILABL = 'X'.
  ENDIF.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.
ENDFORM.                    " F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LIMPA_CAMPOS .

ENDFORM.

FORM F_TRATA_CAMPOS  USING P_FIELD P_GROUP1 P_VALUE P_INVISIBLE.

  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.

ENDFORM.                    " F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS .
  DATA:   WL_LINHA(6),
          WL_CONT     TYPE I,
          WL_CSKS     TYPE CSKS,
          VAPROVADOR  TYPE ZIM12_APROV-APROVADOR.


  CLEAR:    TG_MSG_RET.
  REFRESH:  TG_MSG_RET.

  IF G_TS_100-SUBSCREEN = '0101'.
    TG_AVAL_AUX[] = TG_AVAL[].
    SORT   TG_AVAL_AUX BY BUKRS NIVEL.
    CLEAR WG_AVAL.
    LOOP AT TG_AVAL_AUX INTO WG_AVAL_AUX.
      IF WG_AVAL_AUX-MODI = 'D'.
        CONTINUE.
      ENDIF.
      WL_LINHA =  WG_AVAL_AUX-LINHA.
      IF WG_AVAL_AUX-DESC_USNAM IS INITIAL.
        MOVE:  C_TS_100-TAB1 TO TG_MSG_RET-ABA.
        CONCATENATE 'Usuário inválido'  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
      IF WG_AVAL_AUX-BUKRS IS INITIAL OR
         WG_AVAL_AUX-NIVEL IS INITIAL OR
         WG_AVAL_AUX-APROVADOR IS INITIAL.
        MOVE:  C_TS_100-TAB1 TO TG_MSG_RET-ABA.
        CONCATENATE 'Estratégia incompleta'  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
      IF WG_AVAL_AUX-BUKRS = WG_AVAL-BUKRS AND
         WG_AVAL_AUX-NIVEL = WG_AVAL-NIVEL AND
         WG_AVAL_AUX-STATUS IS INITIAL.

        MOVE:  C_TS_100-TAB1 TO TG_MSG_RET-ABA.
        CONCATENATE 'Chave duplicada'  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
      LOOP AT TG_AVAL INTO WG_AVAL  WHERE BUKRS     = WG_AVAL_AUX-BUKRS
                                    AND   APROVADOR = WG_APROV_AUX-APROVADOR.
        ADD 1 TO WL_CONT.
      ENDLOOP.
      IF WL_CONT GT 1.
        MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE 'Aprovador duplicado para a mesma empresa '  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
      MOVE-CORRESPONDING WG_AVAL_AUX TO WG_AVAL.
    ENDLOOP.
  ELSE.
    SORT   TG_APROV BY BUKRS WERKS KOSTL FASE NIVEL .
    TG_APROV_AUX[] = TG_APROV[].

    CLEAR WG_APROV.
    LOOP AT TG_APROV_AUX INTO WG_APROV_AUX.
      IF WG_APROV_AUX-MODI = 'D'.
        CONTINUE.
      ENDIF.
      WL_LINHA =  WG_APROV_AUX-LINHA.

      IF WG_APROV_AUX-BUKRS IS INITIAL OR
         WG_APROV_AUX-WERKS IS INITIAL OR
         WG_APROV_AUX-KOSTL IS INITIAL OR
         WG_APROV_AUX-FASE  IS INITIAL OR
         WG_APROV_AUX-NIVEL IS INITIAL OR
         WG_APROV_AUX-APROVADOR IS INITIAL.
        MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE 'Estratégia incompleta'  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      SELECT SINGLE *
        FROM CSKS
        INTO WL_CSKS
        WHERE KOSTL = WG_APROV_AUX-KOSTL.
      IF SY-SUBRC NE 0.
        MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE 'Centro de custo inválido'  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
      IF WL_CSKS-BUKRS NE  WG_APROV_AUX-BUKRS  .
        MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE 'Empresa inválida para Centro de custo '  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      IF WL_CSKS-GSBER NE  WG_APROV_AUX-WERKS  .
        MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE 'Centro inválido para Centro de custo '  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      IF WG_APROV_AUX-BUKRS = WG_APROV-BUKRS AND
         WG_APROV_AUX-WERKS = WG_APROV-WERKS AND
         WG_APROV_AUX-KOSTL = WG_APROV-KOSTL AND
         WG_APROV_AUX-FASE  = WG_APROV-FASE  AND
         WG_APROV_AUX-NIVEL = WG_APROV-NIVEL AND
         WG_APROV_AUX-STATUS IS INITIAL.
        "
        MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
        CONCATENATE 'Chave duplicada'  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
*      WL_CONT = 0.
*      CLEAR VAPROVADOR.
*      LOOP AT TG_APROV INTO WG_APROV WHERE KOSTL     = WG_APROV_AUX-KOSTL
*                                     AND   APROVADOR = WG_APROV_AUX-APROVADOR.
*        IF VAPROVADOR NE WG_APROV-APROVADOR.
*          ADD 1 TO WL_CONT.
*        ENDIF.
*      ENDLOOP.
*      IF WL_CONT GT 1.
*        MOVE:  C_TS_100-TAB2 TO TG_MSG_RET-ABA.
*        CONCATENATE 'Aprovador duplicado para o mesmo Centro de Custo '  WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.
      MOVE-CORRESPONDING WG_APROV_AUX TO WG_APROV.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVA_DADOS .

  DATA: WL_ZIM12_AVAL TYPE ZIM12_AVAL,
        TL_ZIM12_AVAL TYPE TABLE OF ZIM12_AVAL.

  DATA: WL_ZIM12_APROV TYPE ZIM12_APROV,
        TL_ZIM12_APROV TYPE TABLE OF ZIM12_APROV.

  "Avaliadores
  IF G_TS_100-SUBSCREEN = '0101'.
    REFRESH TL_ZIM12_AVAL.

    DELETE TG_AVAL WHERE MODI EQ ''.
    LOOP AT TG_AVAL INTO WG_AVAL.
      IF WG_AVAL-MODI = 'X'.
        MOVE-CORRESPONDING WG_AVAL TO WL_ZIM12_AVAL.
        WL_ZIM12_AVAL-DATA_ATUAL = SY-DATUM.
        WL_ZIM12_AVAL-HORA_ATUAL = SY-UZEIT.
        WL_ZIM12_AVAL-USUARIO    = SY-UNAME.
        APPEND WL_ZIM12_AVAL TO TL_ZIM12_AVAL.
      ENDIF.
    ENDLOOP.
    LOOP AT TG_AVAL INTO WG_AVAL.
      IF WG_AVAL-MODI NE ''.
        DELETE FROM ZIM12_AVAL WHERE BUKRS      = WG_AVAL-BUKRS
                               AND   NIVEL      = WG_AVAL-NIVEL
                               AND   APROVADOR  = WG_AVAL-APROVADOR.
      ENDIF.
    ENDLOOP.

    MODIFY ZIM12_AVAL FROM TABLE TL_ZIM12_AVAL.
    COMMIT WORK.
    SELECT *
      FROM ZIM12_AVAL
      INTO CORRESPONDING FIELDS OF TABLE TG_AVAL.
  ELSE. "Aprovadores
    REFRESH TL_ZIM12_APROV.

    DELETE TG_APROV WHERE MODI EQ ''.
    LOOP AT TG_APROV INTO WG_APROV.
      IF WG_APROV-MODI = 'X'.
        MOVE-CORRESPONDING WG_APROV TO WL_ZIM12_APROV.
        WL_ZIM12_APROV-DATA_ATUAL = SY-DATUM.
        WL_ZIM12_APROV-HORA_ATUAL = SY-UZEIT.
        WL_ZIM12_APROV-USUARIO    = SY-UNAME.
        APPEND WL_ZIM12_APROV TO TL_ZIM12_APROV.
      ENDIF.
    ENDLOOP.
    LOOP AT TG_APROV INTO WG_APROV.
      IF WG_APROV-MODI NE ''.
        DELETE FROM ZIM12_APROV WHERE BUKRS      = WG_APROV-BUKRS
                                AND   WERKS      = WG_APROV-WERKS
                                AND   KOSTL      = WG_APROV-KOSTL
                                AND   FASE       = WG_APROV-FASE
                                AND   NIVEL      = WG_APROV-NIVEL
                                AND   APROVADOR  = WG_APROV-APROVADOR.
      ENDIF.
    ENDLOOP.

    MODIFY ZIM12_APROV FROM TABLE TL_ZIM12_APROV.
    COMMIT WORK.
    SELECT *
      FROM ZIM12_APROV
      INTO CORRESPONDING FIELDS OF TABLE TG_APROV.
  ENDIF.
  "

  PERFORM F_ATUALIZA.
  REFRESH TG_MSG_RET.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '0100'
      I_SHOW        = ' '
      I_REPID       = SY-REPID
      I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
      I_SET_FIELD   = 'X_FIELD'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.
  "
  MESSAGE S836(SD) WITH TEXT-M01 TEXT-M02.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ATUALIZA .
  DATA: WL_USREFUS TYPE TY_USREFUS,
        TL_USREFUS TYPE TABLE OF TY_USREFUS,
        IT_USR21   TYPE STANDARD TABLE OF USR21,
        IT_ADRP    TYPE STANDARD TABLE OF ADRP,
        WA_USR21   TYPE USR21,
        WA_ADRP    TYPE ADRP,
        TABIX      TYPE SY-TABIX.

  REFRESH STYLE.
*  REFRESH: TG_AVAL, TG_APROV .

  WA_STYLE-FIELDNAME = 'BUKRS'.
  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
  INSERT  WA_STYLE INTO TABLE STYLE .

  WA_STYLE-FIELDNAME = 'NIVEL'.
  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
  INSERT  WA_STYLE INTO TABLE STYLE .

  WA_STYLE-FIELDNAME = 'APROVADOR'.
  WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
  INSERT  WA_STYLE INTO TABLE STYLE .
  IF G_TS_100-SUBSCREEN = '0101'.

    IF TG_AVAL[] IS NOT INITIAL.
      SELECT * FROM USR21 INTO TABLE IT_USR21
        FOR ALL ENTRIES IN  TG_AVAL
        WHERE BNAME  EQ TG_AVAL-APROVADOR.

      IF IT_USR21 IS NOT INITIAL.
        SELECT * FROM ADRP INTO TABLE IT_ADRP
          FOR ALL ENTRIES IN IT_USR21
          WHERE PERSNUMBER = IT_USR21-PERSNUMBER.
      ENDIF.
    ENDIF.
    SORT IT_USR21 BY BNAME.

    LOOP AT TG_AVAL INTO WG_AVAL.
      IF WG_AVAL-MODI IS INITIAL.
        TABIX = SY-TABIX.
        WG_AVAL-STYLE[] = STYLE[].
        READ TABLE IT_USR21 INTO WA_USR21 WITH KEY BNAME = WG_AVAL-APROVADOR BINARY SEARCH.
        IF SY-SUBRC = 0.
          READ TABLE IT_ADRP INTO WA_ADRP WITH KEY PERSNUMBER = WA_USR21-PERSNUMBER.
          IF SY-SUBRC EQ 0.
            WG_AVAL-DESC_USNAM = WA_ADRP-NAME_FIRST && '&' && WA_ADRP-NAME_LAST.
            REPLACE '&' WITH SPACE INTO WG_AVAL-DESC_USNAM.
          ENDIF.
        ENDIF.
        WG_AVAL-LINHA = TABIX.
        MODIFY TG_AVAL FROM WG_AVAL INDEX TABIX  TRANSPORTING STYLE DESC_USNAM LINHA.
      ENDIF.
    ENDLOOP.
  ELSE.
    WA_STYLE-FIELDNAME = 'FASE'.
    WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
    INSERT  WA_STYLE INTO TABLE STYLE .

    WA_STYLE-FIELDNAME = 'WERKS'.
    WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
    INSERT  WA_STYLE INTO TABLE STYLE .

    WA_STYLE-FIELDNAME = 'KOSTL'.
    WA_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED +  ALV_STYLE_FONT_BOLD.
    INSERT  WA_STYLE INTO TABLE STYLE .

    IF TG_APROV[] IS NOT INITIAL.
      SELECT * FROM USR21 INTO TABLE IT_USR21
        FOR ALL ENTRIES IN  TG_APROV
        WHERE BNAME  EQ TG_APROV-APROVADOR.

      IF IT_USR21 IS NOT INITIAL.
        SELECT * FROM ADRP INTO TABLE IT_ADRP
          FOR ALL ENTRIES IN IT_USR21
          WHERE PERSNUMBER = IT_USR21-PERSNUMBER.
      ENDIF.
    ENDIF.
    SORT IT_USR21 BY BNAME.

    LOOP AT TG_APROV INTO WG_APROV.
      IF WG_APROV-MODI IS INITIAL.
        TABIX = SY-TABIX.
        WG_APROV-STYLE[] = STYLE[].
        READ TABLE IT_USR21 INTO WA_USR21 WITH KEY BNAME = WG_APROV-APROVADOR BINARY SEARCH.
        IF SY-SUBRC = 0.
          READ TABLE IT_ADRP INTO WA_ADRP WITH KEY PERSNUMBER = WA_USR21-PERSNUMBER.
          IF SY-SUBRC EQ 0.
            WG_APROV-DESC_USNAM = WA_ADRP-NAME_FIRST && '&' && WA_ADRP-NAME_LAST.
            REPLACE '&' WITH SPACE INTO WG_APROV-DESC_USNAM.
          ENDIF.
        ENDIF.
        WG_APROV-LINHA = TABIX.
        MODIFY TG_APROV FROM WG_APROV INDEX TABIX TRANSPORTING STYLE DESC_USNAM LINHA.
      ENDIF.
    ENDLOOP.
    SORT   TG_APROV BY BUKRS WERKS KOSTL NIVEL FASE.
  ENDIF.
ENDFORM.
