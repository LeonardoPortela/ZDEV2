*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 05/03/2013                                              &*
*& Descrição: Cadastro de Cód. de cadastro de itens de formação de Preço&*
*& Transação: zsdt0063                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         03.08.2010                            &*
*&--------------------------------------------------------------------&*

REPORT  ZSDR0024.

CONTROLS: TC_ITENS TYPE TABLEVIEW USING SCREEN 100.

INCLUDE <ICON>.
TYPE-POOLS: VRM, USTYP.


TYPES: BEGIN OF TY_ITENS,
        FIELD       TYPE ZSDT0070-FIELD,
        TIPO_CALC   TYPE ZSDT0070-TIPO_CALC,
        C_DECIMAIS  TYPE ZSDT0070-C_DECIMAIS,
        VLR_FIXO    TYPE ZSDT0070-VLR_FIXO,
        PRECO       TYPE ZSDT0070-PRECO,
        MARK,
      END OF TY_ITENS.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_X               TYPE C VALUE 'X',
           C_ADD(3)          TYPE C VALUE 'ADD',
           C_ADD_I(5)        TYPE C VALUE 'ADD_I',
           C_DEL(3)          TYPE C VALUE 'DEL',
           C_DEL_I(5)        TYPE C VALUE 'DEL_I',
           C_EXIT(4)         TYPE C VALUE 'EXIT',
           C_BACK(4)         TYPE C VALUE 'BACK',
           C_SAVE(4)         TYPE C VALUE 'SAVE',
           C_BLOQ(4)         TYPE C VALUE 'BLOQ',
           C_ATUAL(5)        TYPE C VALUE 'ATUAL',
           C_MODIF(5)        TYPE C VALUE 'MODIF',
           C_SEARCH(6)       TYPE C VALUE 'SEARCH',
           C_CANCEL(6)       TYPE C VALUE 'CANCEL',
           C_COND_FORM(9)    TYPE C VALUE 'COND_FORM',
           C_CDECIMAIS(9)    TYPE C VALUE 'CDECIMAIS',
           C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
*** Declaracoes referente a logica do programa ***
DATA: WG_HEADER    TYPE ZSDT0056,
      TG_ITENS     TYPE TABLE OF TY_ITENS WITH HEADER LINE,
      TG_ITENS_AUX TYPE TABLE OF TY_ITENS WITH HEADER LINE,
      WG_INPUT_MASK.

*** Declaracoes referente ao template do programa ***
DATA: OK-CODE     TYPE SY-UCOMM,
      WG_INIT     TYPE C,
      WG_DISPLAY,
      WG_ACAO(10),
      WG_FLAG,
      INIT,
      TG_SELECTEDCELL       TYPE LVC_T_CELL,
      WG_SELECTEDCELL       TYPE LVC_S_CELL.

***** Funcao de Z_DOC_CHECK_NEW
DATA: X_FIELD(30),
      WG_MENSAGEM(30).
DATA: TG_MSG_RET TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE,
      WG_CELL TYPE LVC_S_CELL,
      TG_CELL TYPE LVC_T_CELL.

** Criação de tabela dinamica
DATA: T_FIELDCATALOG        TYPE LVC_T_FCAT,
      W_FIELDCATALOG        TYPE LVC_S_FCAT,
      WA_LAYOUT             TYPE LVC_S_LAYO,
      WA_STABLE             TYPE LVC_S_STBL.


*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  VERIFICA_ERROS .
  DATA: WL_TCURC TYPE TCURC,
        WL_LINHA(6),
        WL_FIELD TYPE I.

  CLEAR: WL_TCURC.
  REFRESH: TG_MSG_RET.

  SELECT SINGLE *
    FROM TCURC
    INTO WL_TCURC
     WHERE WAERS EQ WG_HEADER-WAERS.


*** Descrição (BEZEI)
  IF WG_HEADER-BEZEI IS INITIAL.
    MOVE: 'WG_HEADER-BEZEI'       TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01 'Descrição.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*** Tipo de Calculo (TIPO_CALC)
*  IF WG_HEADER-TP_COD_FP NE 'C'.
*    IF WG_HEADER-TIPO_CALC IS INITIAL.
*      MOVE: 'WG_HEADER-TIPO_CALC'       TO TG_MSG_RET-FIELD.
*      CONCATENATE TEXT-E01 'Tipo de Calculo.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.

*** Moeda (WAERS)
  IF WG_HEADER-WAERS IS INITIAL.
    MOVE: 'WG_HEADER-WAERS'       TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01 'Moeda.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WL_TCURC-WAERS IS INITIAL.
    MOVE: 'WG_HEADER-WAERS'       TO TG_MSG_RET-FIELD.
    CONCATENATE 'Moeda' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.
*  ENDIF.

**** Casas Decimais (C_DECIMAIS)
*    IF WG_HEADER-C_DECIMAIS IS INITIAL.
*      MOVE: 'WG_HEADER-C_DECIMAIS'       TO TG_MSG_RET-FIELD.
*      CONCATENATE TEXT-E01 'Casas Decimais.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*
**** Valor Fixo (VLR_FIXO)
*    IF WG_HEADER-VLR_FIXO IS INITIAL
*    AND WG_HEADER-TIPO_CALC EQ 'F'.
*      MOVE: 'WG_HEADER-VLR_FIXO'       TO TG_MSG_RET-FIELD.
*      CONCATENATE TEXT-E01 'Valor Fixo.' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*
*
*  ENDIF.

*  IF WG_HEADER-TP_COD_FP EQ 'C'.
  IF TG_ITENS[] IS INITIAL.
    MOVE: TEXT-E01 TO TG_MSG_RET-MSG.

    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.
  LOOP AT TG_ITENS.
    WL_LINHA = SY-TABIX.






*** Campo (FIELD)
    IF TG_ITENS-FIELD IS NOT INITIAL.

      TG_ITENS_AUX[] = TG_ITENS[].

      DELETE TG_ITENS_AUX
        WHERE FIELD NE TG_ITENS-FIELD.

      SORT TG_ITENS_AUX BY FIELD.

      DELETE ADJACENT DUPLICATES FROM      TG_ITENS_AUX
                                 COMPARING FIELD.

      IF SY-SUBRC IS INITIAL.

        MOVE: 'TG_ITENS-FIELD'       TO TG_MSG_RET-FIELD,
               WL_LINHA              TO TG_MSG_RET-TABIX.
        CONCATENATE TEXT-E04 'Campo. LINHA:' WL_LINHA INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.


      ELSE.

      ENDIF.

    ENDIF.





*** Tipo de Calculo (TIPO_CALC)
    IF TG_ITENS-TIPO_CALC IS INITIAL.
      MOVE: 'TG_ITENS-TIPO_CALC'       TO TG_MSG_RET-FIELD,
             WL_LINHA                  TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-E01 'Tipo de Calculo. LINHA:' WL_LINHA INTO TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

*** Casas Decimais (C_DECIMAIS)
    IF TG_ITENS-C_DECIMAIS IS INITIAL.
      MOVE: 'TG_ITENS-C_DECIMAIS'       TO TG_MSG_RET-FIELD.
      CONCATENATE TEXT-E01 'Casas Decimais. LINHA:' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

*** Valor Fixo (VLR_FIXO)
    IF TG_ITENS-VLR_FIXO IS INITIAL
    AND TG_ITENS-TIPO_CALC EQ 'F'.
      MOVE: 'TG_ITENS-VLR_FIXO'       TO TG_MSG_RET-FIELD.
      CONCATENATE TEXT-E01 'Valor Fixo. LINHA:' WL_LINHA  INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.

      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

  ENDLOOP.
*
*  ENDIF.

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
*        I_SHOW        = C_X
        I_REPID       = SY-REPID
*        I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA:  BEGIN OF TL_UCOMM OCCURS 0,
        UCOMM TYPE  SY-UCOMM,
       END OF TL_UCOMM.

  REFRESH:TL_UCOMM.
  CLEAR: TL_UCOMM.

  IF WG_ACAO NE C_ADD
  AND WG_ACAO NE C_MODIF
  AND WG_ACAO NE C_COND_FORM.
    MOVE: C_SAVE TO TL_UCOMM.

    APPEND TL_UCOMM.
    CLEAR: TL_UCOMM.

  ELSEIF WG_ACAO EQ C_ADD
      OR WG_ACAO EQ C_MODIF
      OR WG_ACAO EQ C_COND_FORM.

    MOVE: C_ATUAL TO TL_UCOMM.

    APPEND TL_UCOMM.
    CLEAR: TL_UCOMM.

  ENDIF.

  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET PF-STATUS 'Z001' EXCLUDING TL_UCOMM.
  SET TITLEBAR 'Z001'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.


  CASE SY-UCOMM.
    WHEN C_ADD.
      IF WG_FLAG IS INITIAL.
        PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
        PERFORM GET_NEXT_NUMBER  USING  'ZCOD_FP'
                                        '01'
                               CHANGING WG_HEADER-COD_FP.
      ENDIF.
      MOVE C_ADD TO WG_ACAO.
    WHEN C_ATUAL.
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.
      PERFORM BUSCA_DADOS_DOC.
      PERFORM BUSCA_DADOS.
      MOVE: C_ATUAL TO WG_ACAO.

    WHEN C_SEARCH.
      PERFORM BUSCA_DADOS.
    WHEN C_CDECIMAIS.

*    WHEN C_COND_FORM.
*      WG_ACAO = C_COND_FORM.
*      IF WG_HEADER-TP_COD_FP EQ 'L'.
*        REFRESH: TG_ITENS.
*      ELSEIF WG_HEADER-TP_COD_FP EQ 'C'.
*        CLEAR: WG_HEADER-TIPO_CALC, WG_HEADER-C_DECIMAIS, WG_HEADER-VLR_FIXO, WG_HEADER-PRECO.
*
*      ENDIF.

    WHEN C_ADD_I.
      APPEND INITIAL LINE TO TG_ITENS.

    WHEN C_DEL_I.
      DELETE TG_ITENS WHERE MARK EQ C_X.

    WHEN C_MODIF.
**    Valida se existe documento para ser modificado.
      SELECT SINGLE COD_FP
        FROM ZSDT0056
        INTO WG_HEADER-COD_FP
         WHERE COD_FP EQ WG_HEADER-COD_FP.
      IF SY-SUBRC IS INITIAL.

        PERFORM LIMPA_VARIAVEL USING C_ATUAL.
        PERFORM BUSCA_DADOS_DOC.
        PERFORM BUSCA_DADOS.

        MOVE C_MODIF TO WG_ACAO.

      ENDIF.

    WHEN C_CANCEL.
      PERFORM LIMPA_VARIAVEL USING SY-UCOMM.

    WHEN C_SHOW_MSGRE.
      PERFORM VERIFICA_ERROS.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = 'X'
          I_REPID       = SY-REPID
          I_POPUP       = 0
*          I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
          I_SET_CELL    = 'WG_CELL'
          I_SET_OBJ     = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.

    WHEN C_SAVE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        PERFORM GRAVA_DADOS.

      ELSE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = 'X'
          I_REPID       = SY-REPID
          I_POPUP       = 0
*          I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
          I_SET_CELL    = 'WG_CELL'
          I_SET_OBJ     = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.



    WHEN 'BACK'
      OR 'EXIT'.
*      call function 'DEQUEUE_EZSDT0040'
*        exporting
*          doc_simulacao = wg_header-doc_simulacao.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_VARIAVEL USING P_ACAO.
  DATA: WL_COD_FP TYPE ZSDT0056-COD_FP.


  CLEAR: WL_COD_FP.

  IF P_ACAO EQ C_CANCEL
   OR P_ACAO EQ C_ATUAL.
    WL_COD_FP = WG_HEADER-COD_FP.

*
    CLEAR: WG_HEADER,
           WG_ACAO,
           WG_FLAG,
           X_FIELD,
           TG_MSG_RET,
           WG_CELL.
*           WG_DESC_KUNNR,
*           WG_DESC_VKORG,
*           WG_DESC_VTWEG,
*           WG_DESC_SPART,
*           WG_DESC_VKGRP,
*           WG_DESC_VKBUR,
*           WG_DESC_ZLSCH,
*           WG_DESC_ZTERM.
*
    REFRESH: TG_MSG_RET,
             TG_ITENS.
*             style,
*             tg_itens-style,
*             tg_mglobal.
*

    WG_HEADER-COD_FP = WL_COD_FP.
  ELSEIF P_ACAO EQ C_ADD.
    CLEAR: WG_HEADER,
           WG_ACAO,
           X_FIELD,
           TG_MSG_RET.
*           WG_DESC_KUNNR,
*           WG_DESC_VKORG,
*           WG_DESC_VTWEG,
*           WG_DESC_SPART,
*           WG_DESC_VKGRP,
*           WG_DESC_VKBUR,
*           WG_DESC_ZLSCH,
*           WG_DESC_ZTERM.

    REFRESH: TG_MSG_RET.
*             style,
*             tg_itens-style,
*             tg_mglobal.

  ELSE.
*    clear: tg_itens.
*    refresh: tg_itens, tg_itens-style.
  ENDIF.

ENDFORM.                    " LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  DATA: P_2(13) TYPE P DECIMALS 2,
          P_4(13) TYPE P DECIMALS 4.

*  DATA: VALUES       TYPE VRM_VALUES WITH HEADER LINE,
*        TL_TVLV      TYPE TABLE OF TVLV WITH HEADER LINE,
*        TL_TVLVT      TYPE TABLE OF TVLVT WITH HEADER LINE,
*        WL_T052       TYPE T052.
  FIELD-SYMBOLS: <FS_CAMPO> TYPE ANY.

*  REFRESH: TL_TVLV, TL_TVLVT, VALUES.

  IF WG_ACAO EQ C_ADD
  OR WG_ACAO EQ C_MODIF
  OR WG_ACAO EQ C_COND_FORM.
    LOOP AT SCREEN.
      IF SCREEN-GROUP2 EQ 'A1'.
*        IF SCREEN-NAME EQ 'WG_HEADER-VLR_FIXO'.
*          IF WG_HEADER-TIPO_CALC EQ 'F'.
*            SCREEN-INPUT = 1.
*          ELSE.
*            CLEAR: WG_HEADER-VLR_FIXO.
*            SCREEN-INPUT = 0.
*          ENDIF.
*        ELSE.
        SCREEN-INPUT = 1.
*
*        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF SCREEN-GROUP2 EQ 'A2'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.

*      IF SCREEN-GROUP4 EQ 'B1'.
*        IF WG_HEADER-TP_COD_FP EQ 'C'.
*          SCREEN-INPUT = 1.
*        ELSEIF WG_HEADER-TP_COD_FP EQ 'L'.
*          SCREEN-INPUT = 0.
*        ELSE.
*          SCREEN-INPUT = 0.
*        ENDIF.
*        MODIFY SCREEN.
*      ENDIF.

      IF SCREEN-GROUP3 EQ 'A2'.
*        IF WG_HEADER-TP_COD_FP EQ 'C'.
*          SCREEN-INPUT = 0.
*        ELSEIF WG_HEADER-TP_COD_FP EQ 'L'.
*          SCREEN-INPUT = 1.
*        ELSE.
        SCREEN-INPUT = 0.
*        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP4 EQ 'B1'.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

  IF WG_INPUT_MASK IS NOT INITIAL.
*    PERFORM INPUT_MASK USING    WG_HEADER-C_DECIMAIS
*                       CHANGING WG_HEADER-VLR_FIXO.
    CLEAR: WG_INPUT_MASK.

  ENDIF.

*  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
*                        USING SY-UNAME.

  PERFORM VERIFICA_ERROS.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '100'
*      I_SHOW        = 'X'
      I_REPID       = SY-REPID
      I_POPUP       = 0
*      I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
      I_SET_FIELD   = 'X_FIELD'
*      I_SET_CELL    = 'WG_CELL'
*      I_SET_OBJ     = 'WG_OBJ'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD."'WG_DESC_OPERACAO'.
  ENDIF.

*  IF WG_CELL IS NOT INITIAL .
*    REFRESH: TG_CELL.
*    CALL METHOD GRID1->SET_SELECTED_CELLS
*      EXPORTING
*        IT_CELLS = TG_CELL[].
*
*    APPEND WG_CELL TO TG_CELL.
*    CALL METHOD GRID1->SET_SELECTED_CELLS
*      EXPORTING
*        IT_CELLS = TG_CELL[].
*  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM GET_NEXT_NUMBER  USING    P_OBJECT   "TYPE nrobj
                               P_NR_RANGE "TYPE nrnr
                      CHANGING P_NUMBER.

  CLEAR P_NUMBER.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = P_NR_RANGE
      OBJECT                  = P_OBJECT
    IMPORTING
      NUMBER                  = P_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CLEAR: P_NUMBER.
    MESSAGE E836(SD) WITH 'O intervalo de numeração,'
                      'não foi encontrado!'.
  ELSE.
    WG_FLAG = C_X.
  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS_DOC.
  DATA: WL_0056 TYPE ZSDT0056,
        TL_0070 TYPE TABLE OF ZSDT0070.

  CLEAR: WL_0056.

  SELECT SINGLE *
    FROM ZSDT0056
    INTO WL_0056
     WHERE COD_FP EQ WG_HEADER-COD_FP.
  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM ZSDT0070
      INTO TABLE TL_0070
       WHERE COD_FP EQ WG_HEADER-COD_FP.

    PERFORM MONTA_DADOS_DOC TABLES TL_0070
                            USING  WL_0056.


  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'O documento de Cód.'
                                           'Formação de preço'
                                           'não foi encontrado'.
  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0041  text
*      -->P_WL_0040  text
*----------------------------------------------------------------------*
FORM MONTA_DADOS_DOC TABLES TL_0070 STRUCTURE ZSDT0070
                      USING  WL_0056 TYPE ZSDT0056.

  CLEAR: WG_HEADER.
  REFRESH: TG_ITENS.

* Header
  MOVE-CORRESPONDING: WL_0056 TO WG_HEADER.

  LOOP AT TL_0070.
    MOVE-CORRESPONDING TL_0070 TO TG_ITENS.

    APPEND TG_ITENS.
    CLEAR TG_ITENS.
  ENDLOOP.


*  IF WG_HEADER-C_DECIMAIS EQ '2'.
*    P_2 = WG_HEADER-VLR_FIXO.
*    WRITE P_2 TO WG_HEADER-VLR_FIXO.
*  ELSEIF WG_HEADER-C_DECIMAIS EQ '4'.
*    P_4 = WG_HEADER-VLR_FIXO.
*    WRITE P_4 TO WG_HEADER-VLR_FIXO.
*  ENDIF.

ENDFORM.                    " MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .

  DATA: WL_INPUT_0056 TYPE ZSDT0056,
        WL_0056       TYPE ZSDT0056,
        TL_INPUT_0070 TYPE TABLE OF ZSDT0070 WITH HEADER LINE.

  CLEAR: WL_INPUT_0056, WL_0056.
  REFRESH: TL_INPUT_0070.

  SELECT SINGLE *
    FROM ZSDT0056
    INTO WL_0056
     WHERE  COD_FP EQ WG_HEADER-COD_FP.

  IF SY-SUBRC IS INITIAL.
    MOVE: WL_0056-USNAM      TO WL_INPUT_0056-USNAM,
          WL_0056-DATA_ATUAL TO WL_INPUT_0056-DATA_ATUAL,
          WL_0056-HORA_ATUAL TO WL_INPUT_0056-HORA_ATUAL.
  ELSE.
    MOVE: SY-UNAME     TO WL_INPUT_0056-USNAM,
          SY-DATUM     TO WL_INPUT_0056-DATA_ATUAL,
          SY-UZEIT     TO WL_INPUT_0056-HORA_ATUAL.
  ENDIF.
* Header
  MOVE: WG_HEADER-COD_FP           TO WL_INPUT_0056-COD_FP,
        WG_HEADER-BEZEI            TO WL_INPUT_0056-BEZEI,
*        WG_HEADER-TIPO_CALC        TO WL_INPUT_0056-TIPO_CALC,
        WG_HEADER-WAERS            TO WL_INPUT_0056-WAERS,
*        WG_HEADER-PRECO            TO WL_INPUT_0056-PRECO,
        WG_HEADER-OCBOT            TO WL_INPUT_0056-OCBOT,
        WG_HEADER-INVISIBLE        TO WL_INPUT_0056-INVISIBLE.
*        WG_HEADER-C_DECIMAIS       TO WL_INPUT_0056-C_DECIMAIS,
*        WG_HEADER-VLR_FIXO         TO WL_INPUT_0056-VLR_FIXO,
*        WG_HEADER-TP_COD_FP        TO WL_INPUT_0056-TP_COD_FP.

*  TRANSLATE WL_INPUT_0056-VLR_FIXO USING '. '.
*  TRANSLATE WL_INPUT_0056-VLR_FIXO USING ',.'.
*  CONDENSE WL_INPUT_0056-VLR_FIXO  NO-GAPS.

  LOOP AT TG_ITENS.
    MOVE-CORRESPONDING: TG_ITENS TO TL_INPUT_0070.
    MOVE: WG_HEADER-COD_FP  TO TL_INPUT_0070-COD_FP.

    TRANSLATE TL_INPUT_0070-VLR_FIXO USING '. '.
    TRANSLATE TL_INPUT_0070-VLR_FIXO USING ',.'.
    CONDENSE TL_INPUT_0070-VLR_FIXO  NO-GAPS.


    APPEND TL_INPUT_0070.
    CLEAR: TL_INPUT_0070.
  ENDLOOP.

  DELETE FROM ZSDT0056 WHERE COD_FP EQ WL_INPUT_0056-COD_FP.
  DELETE FROM ZSDT0070 WHERE COD_FP EQ WL_INPUT_0056-COD_FP.

  MODIFY ZSDT0056 FROM WL_INPUT_0056.
  MODIFY ZSDT0070 FROM TABLE TL_INPUT_0070.


  MESSAGE S836(SD) WITH 'Cód. Formação de Preço'
                         WL_INPUT_0056-COD_FP
                         ', criado/modificado com sucesso!'.

*  call function 'DEQUEUE_EZSDT0040'
*    exporting
*      doc_simulacao = wg_header-doc_simulacao.

  PERFORM LIMPA_VARIAVEL USING C_ATUAL.
  WG_ACAO = C_ATUAL.
  LEAVE TO SCREEN 100.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .
*  CLEAR: WG_DESC_AUART.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_T_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM VALIDA_LAYOUT  TABLES   TL_FIELDCATALOG STRUCTURE LVC_S_FCAT
                     USING   UNAME.

  DATA: TL_PARAMETROS TYPE USTYP_T_PARAMETERS,
        WL_PARAMETROS TYPE USTYP_PARAMETERS,
        WL_FIELDCATALOG TYPE LVC_S_FCAT,
        WL_VARIANTE01 TYPE ZVARIANTE01,
        TL_VARIANTE02_ALV    TYPE TABLE OF ZVARIANTE02 WITH HEADER LINE,
        TL_VARIANTE02_SCREEN TYPE TABLE OF ZVARIANTE02 WITH HEADER LINE,
        WL_TABIX TYPE SY-TABIX,
        WL_ATRIBUTO(30).

  REFRESH: TL_PARAMETROS, TL_VARIANTE02_ALV, TL_VARIANTE02_SCREEN.
  FIELD-SYMBOLS: <FS_ATRIBUTOS> TYPE ANY.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      USER_NAME                 = UNAME
*   WITH_TEXT                 =
    TABLES
      USER_PARAMETERS           = TL_PARAMETROS
    EXCEPTIONS
      USER_NAME_NOT_EXIST       = 1
      OTHERS                    = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  READ TABLE TL_PARAMETROS INTO WL_PARAMETROS
    WITH KEY PARID = 'ZVARIANTE'.
  IF SY-SUBRC IS INITIAL.
    SELECT SINGLE *
      FROM ZVARIANTE01
      INTO WL_VARIANTE01
       WHERE GRPVA EQ WL_PARAMETROS-PARVA
         AND TCODE EQ SY-TCODE.

    IF SY-SUBRC IS INITIAL.
      CONDENSE WL_VARIANTE01-GRPVA NO-GAPS.
      SELECT *
        FROM ZVARIANTE02
        INTO TABLE TL_VARIANTE02_ALV
         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
           AND TCODE   EQ SY-TCODE
           AND ATR_TIP EQ 'ALV'
           AND DYNNR   EQ SY-DYNNR.

      SELECT *
        FROM ZVARIANTE02
        INTO TABLE TL_VARIANTE02_SCREEN
         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
           AND TCODE   EQ SY-TCODE
           AND ATR_TIP NE 'ALV'
           AND DYNNR   EQ SY-DYNNR.

    ENDIF.
    IF TL_VARIANTE02_SCREEN[] IS NOT INITIAL
    AND ( SY-TCODE NE 'SE38'
       AND SY-TCODE NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE TL_VARIANTE02_SCREEN
          WITH KEY FIELD = SCREEN-NAME.

        IF SY-SUBRC IS INITIAL.
          IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
          AND TL_VARIANTE02_SCREEN-ACAO EQ WG_ACAO )
            OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
            UNASSIGN <FS_ATRIBUTOS>.
            CONCATENATE 'SCREEN' TL_VARIANTE02_SCREEN-ATR_TIP INTO WL_ATRIBUTO SEPARATED BY '-'.
            ASSIGN (WL_ATRIBUTO) TO <FS_ATRIBUTOS>.
            IF <FS_ATRIBUTOS> IS ASSIGNED.
              <FS_ATRIBUTOS> = TL_VARIANTE02_SCREEN-FATR_VALUE.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF TL_VARIANTE02_ALV[] IS INITIAL
    AND ( SY-TCODE EQ 'SE38'
       OR SY-TCODE EQ 'SE80' ).
      EXIT.
    ENDIF.
    LOOP AT TL_FIELDCATALOG INTO WL_FIELDCATALOG.
      WL_TABIX = SY-TABIX.
      READ TABLE TL_VARIANTE02_ALV
        WITH KEY FIELD = WL_FIELDCATALOG-FIELDNAME.
      IF SY-SUBRC IS NOT  INITIAL.
        IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
            AND TL_VARIANTE02_SCREEN-ACAO EQ WG_ACAO )
              OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
          DELETE TL_FIELDCATALOG INDEX WL_TABIX.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
    SELECT SINGLE *
      FROM ZVARIANTE01
      INTO WL_VARIANTE01
       WHERE DEFAULT_VAR EQ C_X
         AND TCODE EQ SY-TCODE.

    IF SY-SUBRC IS INITIAL.
      SELECT *
        FROM ZVARIANTE02
        INTO TABLE TL_VARIANTE02_ALV
         WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
           AND TCODE   EQ SY-TCODE
           AND ATR_TIP EQ 'ALV'
           AND DYNNR   EQ SY-DYNNR.

      SELECT *
         FROM ZVARIANTE02
         INTO TABLE TL_VARIANTE02_SCREEN
          WHERE GRPVA   EQ WL_VARIANTE01-GRPVA
            AND TCODE   EQ SY-TCODE
            AND ATR_TIP NE 'ALV'
            AND DYNNR   EQ SY-DYNNR.
    ENDIF.
    IF TL_VARIANTE02_SCREEN[] IS NOT INITIAL
        AND ( SY-TCODE NE 'SE38'
           AND SY-TCODE NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE TL_VARIANTE02_SCREEN
          WITH KEY FIELD = SCREEN-NAME.

        IF SY-SUBRC IS INITIAL.
          IF ( TL_VARIANTE02_SCREEN-ACAO IS NOT INITIAL
            AND TL_VARIANTE02_SCREEN-ACAO EQ WG_ACAO )
              OR TL_VARIANTE02_SCREEN-ACAO IS INITIAL.
            UNASSIGN <FS_ATRIBUTOS>.
            CONCATENATE 'SCREEN' TL_VARIANTE02_SCREEN-ATR_TIP INTO WL_ATRIBUTO SEPARATED BY '-'.
            ASSIGN (WL_ATRIBUTO) TO <FS_ATRIBUTOS>.
            IF <FS_ATRIBUTOS> IS ASSIGNED.
              <FS_ATRIBUTOS> = TL_VARIANTE02_SCREEN-FATR_VALUE.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF TL_VARIANTE02_ALV[] IS INITIAL
    AND ( SY-TCODE EQ 'SE38'
       OR SY-TCODE EQ 'SE80' ).
      EXIT.
    ENDIF.
    LOOP AT TL_FIELDCATALOG INTO WL_FIELDCATALOG.
      WL_TABIX = SY-TABIX.
      READ TABLE TL_VARIANTE02_ALV
        WITH KEY FIELD = WL_FIELDCATALOG-FIELDNAME.
      IF SY-SUBRC IS NOT  INITIAL.
        IF ( TL_VARIANTE02_ALV-ACAO IS NOT INITIAL
            AND TL_VARIANTE02_ALV-ACAO EQ WG_ACAO )
              OR TL_VARIANTE02_ALV-ACAO IS INITIAL.
          DELETE TL_FIELDCATALOG INDEX WL_TABIX.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_MASK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_MASK INPUT.
  WG_INPUT_MASK = C_X.
ENDMODULE.                 " INPUT_MASK  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_MARK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_MARK INPUT.
  TG_ITENS-MARK = 'X'.
  MODIFY TG_ITENS INDEX TC_ITENS-CURRENT_LINE.
ENDMODULE.                 " TC_MARK  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_TC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_TC OUTPUT.
  DATA: WL_ITENS LIKE LINE OF TG_ITENS.

*  wl_itens_col-COLS-SCREEN-INPUT = 1.
*  MODIFY TC_ITENS-COLS-SCREEN TRANSPORTING input
*                               WHERE NAME EQ 'TG_ITENS-FIELD'
*                                  OR NAME EQ 'TG_ITENS-TIPO_CALC'
*                                  OR NAME EQ 'TG_ITENS-C_DECIMAIS'.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'TG_ITENS-FIELD'
    OR SCREEN-NAME EQ 'TG_ITENS-PRECO'
    OR SCREEN-NAME EQ 'TG_ITENS-TIPO_CALC'
    OR SCREEN-NAME EQ 'TG_ITENS-C_DECIMAIS'.

      IF WG_ACAO EQ C_ADD
      OR WG_ACAO EQ C_MODIF
      OR WG_ACAO EQ C_COND_FORM.
*        IF WG_HEADER-TP_COD_FP EQ 'C'.

        SCREEN-INPUT = 1.
*        ELSE.
*          SCREEN-INPUT = 0.
*        ENDIF.
      ELSE.
        SCREEN-INPUT = 0.
      ENDIF.

    ELSEIF SCREEN-NAME EQ 'TG_ITENS-VLR_FIXO'.
      IF WG_ACAO EQ C_ADD
      OR WG_ACAO EQ C_MODIF
      OR WG_ACAO EQ C_COND_FORM.
*        IF WG_HEADER-TP_COD_FP EQ 'C'.
        CLEAR: WL_ITENS.
        READ TABLE TG_ITENS INTO WL_ITENS INDEX TC_ITENS-CURRENT_LINE.
        IF  WL_ITENS-TIPO_CALC EQ 'F'.
          SCREEN-INPUT = 1.
        ELSE.
          SCREEN-INPUT = 0.
        ENDIF.
*        ELSE.
*          SCREEN-INPUT = 0.
*        ENDIF.
      ELSE.
        SCREEN-INPUT = 0.
      ENDIF.

    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " MODIFY_TC  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INSERT_ITEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INSERT_ITEM INPUT.
  MODIFY TG_ITENS INDEX TC_ITENS-CURRENT_LINE.
  IF SY-SUBRC IS NOT INITIAL.
    APPEND TG_ITENS.
  ENDIF.
ENDMODULE.                 " INSERT_ITEM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINES_OF_TC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINES_OF_TC OUTPUT.
  DESCRIBE TABLE TG_ITENS LINES TC_ITENS-LINES.
ENDMODULE.                 " GET_LINES_OF_TC  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INPUT_MASK_ITEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_MASK_ITEM INPUT.
  IF TG_ITENS-TIPO_CALC EQ 'F'.
    PERFORM INPUT_MASK USING TG_ITENS-C_DECIMAIS
                       CHANGING TG_ITENS-VLR_FIXO.
  ENDIF.
ENDMODULE.                 " INPUT_MASK_ITEM  INPUT
*&---------------------------------------------------------------------*
*&      Form  INPUT_MASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_HEADER_VLR_FIXO  text
*----------------------------------------------------------------------*
FORM INPUT_MASK USING P_C_DECIMAIS
                CHANGING P_VLR_FIXO.
  TRANSLATE P_VLR_FIXO USING '. '.
  TRANSLATE P_VLR_FIXO USING ',.'.
  CONDENSE  P_VLR_FIXO NO-GAPS.

  IF P_C_DECIMAIS EQ '2'.
    P_2 = P_VLR_FIXO.
    WRITE P_2 TO P_VLR_FIXO.
  ELSEIF P_C_DECIMAIS EQ '4'.
    P_4 = P_VLR_FIXO.
    WRITE P_4 TO P_VLR_FIXO.
  ENDIF.
ENDFORM.                    " INPUT_MASK
