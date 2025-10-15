*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 17/10/2013                                              &*
*& Descrição: Relatório de Aprovação de Recebimento                   &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  ZMMR038.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.
TYPES: BEGIN OF TY_EKKO,
         BUKRS TYPE EKKO-BUKRS,
         LIFNR TYPE EKKO-LIFNR,
         EBELN TYPE EKKO-EBELN,
         LOEKZ TYPE EKKO-LOEKZ,
         EKGRP TYPE EKKO-EKGRP,
         ERNAM TYPE EKKO-ERNAM,
         AEDAT TYPE EKKO-AEDAT,
         FRGKE TYPE EKKO-FRGKE,
         FRGRL TYPE EKKO-FRGRL,
         LIFRE TYPE EKKO-LIFRE,
       END OF TY_EKKO,

       BEGIN OF TY_EKPO,
         EBELN  TYPE EKPO-EBELN,
         EBELP  TYPE EKPO-EBELP,
         MATNR  TYPE EKPO-MATNR,
         WERKS  TYPE EKPO-WERKS,
         LOEKZ  TYPE EKPO-LOEKZ,
         ELIKZ  TYPE EKPO-ELIKZ,
         EREKZ  TYPE EKPO-EREKZ,
         TXZ01  TYPE EKPO-TXZ01,
         KNTTP  TYPE EKPO-KNTTP,
         MENGE  TYPE EKPO-MENGE,
         MEINS  TYPE EKPO-MEINS,
         PSTYP  TYPE EKPO-PSTYP,
         PACKNO TYPE EKPO-PACKNO,
       END OF TY_EKPO,

       BEGIN OF TY_ESLL,
         PACKNO     TYPE ESLL-PACKNO,
         SUB_PACKNO TYPE ESLL-SUB_PACKNO,
         SRVPOS     TYPE ESLL-SRVPOS,
         TDNAME     TYPE STXL-TDNAME,
       END OF TY_ESLL,

       BEGIN OF TY_ESLL_AUX,
         PACKNO     TYPE ESLL-PACKNO,
         SUB_PACKNO TYPE ESLL-SUB_PACKNO,
         SRVPOS     TYPE STXL-TDNAME,
       END OF TY_ESLL_AUX,

       BEGIN OF TY_STXL,
         TDNAME   TYPE STXL-TDNAME,
         TDOBJECT TYPE STXL-TDOBJECT,
         TDSPRAS  TYPE STXL-TDSPRAS,
       END OF TY_STXL,

       BEGIN OF TY_EKBE,
         EBELN TYPE EKBE-EBELN,
         EBELP TYPE EKBE-EBELP,
         VGABE TYPE EKBE-VGABE,
         BELNR TYPE EKBE-BELNR,
         MATNR TYPE EKBE-MATNR,
         MENGE TYPE EKBE-MENGE,
         SHKZG TYPE EKBE-SHKZG,
*        MBLNR TYPE EKBE-MBLNR,
       END OF TY_EKBE,

       BEGIN OF TY_MSEG,
         SMBLN TYPE MSEG-SMBLN,
       END OF TY_MSEG,

       BEGIN OF TY_EKES,
         EBELN TYPE EKES-EBELN,
         EBELP TYPE EKES-EBELP,
         VBELN TYPE EKES-VBELN,
       END OF TY_EKES,

       BEGIN OF TY_EKKN,
         EBELN TYPE EKKN-EBELN,
         EBELP TYPE EKKN-EBELN,
         KOSTL TYPE EKKN-KOSTL,
         ANLN1 TYPE EKKN-ANLN1,
         ANLN2 TYPE EKKN-ANLN2,
       END OF TY_EKKN,

       BEGIN OF TY_J_1BNFDOC,
         BUKRS  TYPE J_1BNFDOC-BUKRS,
         DOCNUM TYPE J_1BNFDOC-DOCNUM,
         NFNUM  TYPE J_1BNFDOC-NFNUM,
         NFENUM TYPE RBKP-XBLNR, "J_1BNFDOC-NFENUM,
         BRANCH TYPE LFA1-LIFNR,
         DOCDAT TYPE J_1BNFDOC-DOCDAT,
         SERIES TYPE J_1BNFDOC-SERIES,
         NFTYPE TYPE J_1BNFDOC-NFTYPE,
       END OF TY_J_1BNFDOC,

       BEGIN OF TY_J_1BNFLIN,
         DOCNUM TYPE J_1BNFLIN-DOCNUM,
         WERKS  TYPE J_1BNFLIN-WERKS,
         ITMNUM TYPE J_1BNFLIN-ITMNUM,
         MATNR  TYPE J_1BNFLIN-MATNR,
         MAKTX  TYPE J_1BNFLIN-MAKTX,
         MEINS  TYPE J_1BNFLIN-MEINS,
       END OF TY_J_1BNFLIN,

       BEGIN OF TY_RELA_DET,
         EBELP LIKE EKPO-EBELP,
         TXZ01 LIKE EKPO-TXZ01,
         LGPBE LIKE MARD-LGPBE,
         VBELN LIKE EKES-VBELN,
         MEINS LIKE EKPO-MEINS,
         ANLN1 LIKE EKKN-ANLN1,
         MATNR LIKE EKPO-MATNR,
         KNTTP LIKE EKPO-KNTTP,
         KOSTL LIKE EKKN-KOSTL,
         MENGE LIKE EKPO-MENGE,
         BELNR LIKE RBSELBEST-BELNR,
         EBELN LIKE RBSELBEST-EBELN,
       END OF TY_RELA_DET,

       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
       END OF TY_LFA1,

       BEGIN OF TY_T001,
         BUKRS TYPE T001-BUKRS,
         BUTXT TYPE T001-BUTXT,
       END OF TY_T001,

       BEGIN OF TY_T001W,
         WERKS TYPE T001W-WERKS,
         NAME1 TYPE T001W-NAME1,
       END OF TY_T001W,

       BEGIN OF TY_T024,
         EKGRP TYPE T024-EKGRP,
         EKNAM TYPE T024-EKNAM,
         EKTEL TYPE T024-EKTEL,
       END OF TY_T024,

       BEGIN OF TY_MARD,
         MATNR TYPE MARD-MATNR,
         WERKS TYPE MARD-WERKS,
         LGPBE TYPE MARD-LGPBE,
       END OF TY_MARD,

       BEGIN OF TY_T163I,
         KNTTP TYPE T163I-KNTTP,
         KNTTX TYPE T163I-KNTTX,
       END OF TY_T163I,

       BEGIN OF TY_J_1BNFE_ACTIVE,
         DOCNUM TYPE J_1BNFE_ACTIVE-DOCNUM,
         DOCSTA TYPE J_1BNFE_ACTIVE-DOCSTA,
       END OF TY_J_1BNFE_ACTIVE,

       BEGIN OF TY_SAIDA,
         BRANCH     TYPE LFA1-LIFNR,
         LGPBE      TYPE MARD-LGPBE,
         KOSTL      TYPE EKKN-KOSTL,
         ANLN1      TYPE EKKN-ANLN1,
         VBELN      TYPE EKES-VBELN,
         KNTTP      TYPE T163I-KNTTX,
         EKNAM      TYPE T024-EKNAM,
         EKTEL      TYPE T024-EKTEL,
         CENTRO     TYPE T001W-NAME1,
         WERKS      TYPE T001W-WERKS,
         BUTXT      TYPE T001-BUTXT,
         MEINS      TYPE EKPO-MEINS,
         MATNR      TYPE EKPO-MATNR,
         AEDAT      TYPE EKKO-AEDAT,
         EKGRP      TYPE EKKO-EKGRP,
         LIFNR      TYPE EKKO-LIFNR,
         BUKRS      TYPE EKKO-BUKRS,
         TXZ01      TYPE EKPO-TXZ01,
         EBELN      TYPE EKPO-EBELN,
         EBELP      TYPE EKPO-EBELP,
         DOCNUM     TYPE J_1BNFDOC-DOCNUM,
         ITMNUM     TYPE J_1BNFLIN-ITMNUM,
         MAKTX      TYPE J_1BNFLIN-MAKTX,
         NFENUM(13) TYPE C,
         DOCDAT     TYPE J_1BNFDOC-DOCDAT,
         NAME1      TYPE LFA1-NAME1,
         MARK,
       END OF TY_SAIDA.

DATA: BEGIN OF WA_EKPO_TEXT,
        EBELN  LIKE EKPO-EBELN,
        EBELP  LIKE EKPO-EBELP,
        TDLINE LIKE TLINE-TDLINE,
        BELNR  LIKE RBSELBEST-BELNR,
      END OF WA_EKPO_TEXT,

      BEGIN OF STXL_ID,
        TDOBJECT LIKE STXL-TDOBJECT,
        TDNAME   LIKE STXL-TDNAME,
        TDID     LIKE STXL-TDID,
        TDSPRAS  LIKE STXL-TDSPRAS,
      END OF STXL_ID.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: T_EKKO       TYPE TABLE OF TY_EKKO,
      T_EKPO       TYPE TABLE OF TY_EKPO,
      T_ESLL       TYPE TABLE OF TY_ESLL,
      T_ESLL_AUX   TYPE TABLE OF TY_ESLL,
      T_STXL       TYPE TABLE OF TY_STXL,
      T_EKBE       TYPE TABLE OF TY_EKBE,
      T_MSEG       TYPE TABLE OF TY_MSEG,
      T_EKES       TYPE TABLE OF TY_EKES,
      T_EKKN       TYPE TABLE OF TY_EKKN,
      T_DOC        TYPE TABLE OF TY_J_1BNFDOC,
      T_LIN        TYPE TABLE OF TY_J_1BNFLIN,
      T_LFA1       TYPE TABLE OF TY_LFA1,
      T_T001       TYPE TABLE OF TY_T001,
      T_T001W      TYPE TABLE OF TY_T001W,
      T_T024       TYPE TABLE OF TY_T024,
      T_MARD       TYPE TABLE OF TY_MARD,
      T_T163I      TYPE TABLE OF TY_T163I,
      T_RELA_DET   TYPE TABLE OF TY_RELA_DET,
      IT_EKPO_TEXT LIKE STANDARD TABLE OF WA_EKPO_TEXT,
      T_ACTIVE     TYPE TABLE OF TY_J_1BNFE_ACTIVE,
      T_SAIDA      TYPE TABLE OF TY_SAIDA.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_EKKO     TYPE TY_EKKO,
      WA_ESLL     TYPE TY_ESLL,
      WA_ESLL_AUX TYPE TY_ESLL,
      WA_STXL     TYPE TY_STXL,
      WA_EKPO     TYPE TY_EKPO,
      WA_EKBE     TYPE TY_EKBE,
      WA_MSEG     TYPE TY_MSEG,
      WA_EKES     TYPE TY_EKES,
      WA_EKKN     TYPE TY_EKKN,
      WA_DOC      TYPE TY_J_1BNFDOC,
      WA_LIN      TYPE TY_J_1BNFLIN,
      WA_LFA1     TYPE TY_LFA1,
      WA_T001     TYPE TY_T001,
      WA_T001W    TYPE TY_T001W,
      WA_T024     TYPE TY_T024,
      WA_MARD     TYPE TY_MARD,
      WA_T163I    TYPE TY_T163I,
      WA_RELA_DET TYPE TY_RELA_DET,
      WA_ACTIVE   TYPE TY_J_1BNFE_ACTIVE,
      WA_SAIDA    TYPE TY_SAIDA.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.


*----------------------------------------------------------------------*
* Variáveis globais
*----------------------------------------------------------------------*
DATA: VG_FM_NAME TYPE RS38L_FNAM, "Nome da função smart form
      WG_XBLNR   TYPE XBLNR1.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULÁRIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_COMPR RADIOBUTTON GROUP A1 USER-COMMAND MUDA_TELA
                                                   DEFAULT 'X',
                P_TRANS RADIOBUTTON GROUP A1.
SELECTION-SCREEN: END OF BLOCK B1.

*   Compras
SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME.
SELECT-OPTIONS: S_WERKS  FOR WA_EKPO-WERKS NO INTERVALS NO-EXTENSION MODIF ID C1,               """ Centro
                S_EBELN  FOR WA_EKKO-EBELN NO INTERVALS NO-EXTENSION MODIF ID C1,               """ Pedido
                S_NFENUM FOR WA_DOC-NFENUM NO INTERVALS NO-EXTENSION MODIF ID C1,               """ Nota Fiscal
                S_DOCDAT FOR WA_DOC-DOCDAT NO INTERVALS NO-EXTENSION MODIF ID C1,               """ Data do documento
                S_NFTYPE FOR WA_DOC-NFTYPE NO INTERVALS NO-EXTENSION MODIF ID C1.               """ Cat. Nota Fiscal
SELECTION-SCREEN: END OF BLOCK B2.

*   Trasferências
SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME.
SELECT-OPTIONS: S_DOCNUM FOR WA_DOC-DOCNUM NO INTERVALS NO-EXTENSION MODIF ID C2,               """ Número do documento
                S_NFE_T  FOR WA_DOC-NFENUM NO INTERVALS NO-EXTENSION MODIF ID C2 .              """ Nota Fiscal
SELECTION-SCREEN: END OF BLOCK B3.


AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFICA_TELA.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
*
  IF P_COMPR IS NOT  INITIAL.
    IF S_WERKS-LOW  IS INITIAL
      OR S_EBELN-LOW  IS INITIAL
      OR S_NFENUM-LOW IS INITIAL
      OR S_DOCDAT-LOW IS INITIAL
      OR S_NFTYPE-LOW IS INITIAL.

      MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'É obrigatório o preenchimento de todos os campos.'.
    ELSE.
      PERFORM SELECIONAR_DADOS.
      PERFORM ORGANIZAR_DADOS.
      PERFORM INICIAR_VARIAVEIS.
      PERFORM IMPRIMIR_DADOS.
    ENDIF.

  ELSEIF P_TRANS IS NOT INITIAL.
    IF  S_DOCNUM-LOW IS INITIAL
      OR S_NFE_T-LOW  IS INITIAL.

      MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'É obrigatório o preenchimento de todos os campos.'.
    ELSE.
      PERFORM SELECIONAR_DADOS.
      PERFORM ORGANIZAR_DADOS.
      PERFORM INICIAR_VARIAVEIS.
      PERFORM IMPRIMIR_DADOS.
    ENDIF.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .
  DATA: WL_BRANCH TYPE LFA1-LIFNR,
        WL_INDEX  TYPE I.

  IF P_COMPR IS NOT INITIAL.
    SELECT BUKRS LIFNR EBELN LOEKZ EKGRP ERNAM AEDAT FRGKE FRGRL LIFRE
      FROM EKKO
        INTO TABLE T_EKKO
          WHERE EBELN IN S_EBELN
            AND LOEKZ EQ SPACE.

    IF SY-SUBRC IS INITIAL.
      SELECT EBELN EBELP MATNR WERKS LOEKZ ELIKZ EREKZ TXZ01 KNTTP MENGE MEINS PSTYP PACKNO
        FROM EKPO
          INTO TABLE T_EKPO
          FOR ALL ENTRIES IN T_EKKO
            WHERE EBELN EQ T_EKKO-EBELN
              AND WERKS IN S_WERKS
              AND LOEKZ EQ SPACE
              AND ELIKZ EQ SPACE
              AND EREKZ EQ SPACE.

      IF SY-SUBRC IS INITIAL.
        SELECT EBELN EBELP VGABE BELNR MATNR MENGE SHKZG
          FROM EKBE
            INTO TABLE T_EKBE
            FOR ALL ENTRIES IN T_EKPO
              WHERE EBELN EQ T_EKPO-EBELN
                AND EBELP EQ T_EKPO-EBELP
                AND VGABE EQ '1'.

        IF SY-SUBRC IS INITIAL.
          SELECT SMBLN
            FROM MSEG
              INTO TABLE T_MSEG
              FOR ALL ENTRIES IN T_EKBE
                WHERE SMBLN EQ T_EKBE-BELNR.

        ENDIF.

        SELECT EBELN EBELP VBELN
          FROM EKES
            INTO TABLE T_EKES
            FOR ALL ENTRIES IN T_EKPO
              WHERE EBELN EQ T_EKPO-EBELN
                AND EBELP EQ T_EKPO-EBELP.

        SELECT EBELN EBELP KOSTL ANLN1 ANLN2
          FROM EKKN
            INTO TABLE T_EKKN
            FOR ALL ENTRIES IN T_EKPO
              WHERE EBELN EQ T_EKPO-EBELN
                AND EBELP EQ T_EKPO-EBELP.

        SELECT MATNR WERKS LGPBE
          FROM MARD
            INTO TABLE T_MARD
            FOR ALL ENTRIES IN T_EKPO
              WHERE MATNR EQ T_EKPO-MATNR
                AND WERKS EQ T_EKPO-WERKS.

        SELECT LIFNR NAME1
          FROM LFA1
            INTO TABLE T_LFA1
            FOR ALL ENTRIES IN T_EKKO
              WHERE LIFNR EQ T_EKKO-LIFNR.

        SELECT BUKRS BUTXT
          FROM T001
            INTO TABLE T_T001
            FOR ALL ENTRIES IN T_EKKO
              WHERE BUKRS EQ T_EKKO-BUKRS.

        SELECT EKGRP EKNAM EKTEL
          FROM T024
            INTO TABLE T_T024
            FOR ALL ENTRIES IN T_EKKO
              WHERE EKGRP EQ T_EKKO-EKGRP.


        IF T_EKPO[] IS NOT INITIAL.
          SELECT WERKS NAME1
            FROM T001W
              INTO TABLE T_T001W
              FOR ALL ENTRIES IN T_EKPO
                WHERE WERKS EQ T_EKPO-WERKS.

          SELECT KNTTP KNTTX
            FROM T163I
              INTO TABLE T_T163I
              FOR ALL ENTRIES IN T_EKPO
                WHERE KNTTP EQ T_EKPO-KNTTP.

          SELECT PACKNO SUB_PACKNO SRVPOS SRVPOS
            FROM ESLL
              INTO TABLE T_ESLL
              FOR ALL ENTRIES IN T_EKPO
                WHERE PACKNO EQ T_EKPO-PACKNO.

          IF SY-SUBRC IS INITIAL.
            SELECT PACKNO SUB_PACKNO SRVPOS SRVPOS
              FROM ESLL
                INTO TABLE T_ESLL_AUX
                FOR ALL ENTRIES IN T_ESLL
                  WHERE PACKNO EQ T_ESLL-SUB_PACKNO.
          ENDIF.

*          IF T_ESLL_aux[] IS not INITIAL.
*            SELECT TDNAME TDOBJECT TDSPRAS
*              FROM STXL
*                INTO TABLE T_STXL
*                FOR ALL ENTRIES IN T_ESLL_AUX
*                  WHERE TDNAME EQ T_ESLL_AUX-TDNAME
*                    AND TDOBJECT EQ 'ASMD'
*                    AND TDSPRAS  EQ SY-LANGU.
*
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSEIF P_TRANS IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = S_NFE_T-LOW
      IMPORTING
        OUTPUT = S_NFE_T-LOW.

    MODIFY S_NFE_T INDEX 1 .

    SELECT BUKRS DOCNUM NFNUM NFENUM BRANCH DOCDAT SERIES NFTYPE
      FROM J_1BNFDOC
        INTO TABLE T_DOC
          WHERE DOCNUM IN S_DOCNUM
            AND NFENUM IN S_NFE_T.

    IF SY-SUBRC IS INITIAL.
      SELECT DOCNUM WERKS ITMNUM MATNR MAKTX MEINS
        FROM J_1BNFLIN
          INTO TABLE T_LIN
          FOR ALL ENTRIES IN T_DOC
            WHERE DOCNUM EQ T_DOC-DOCNUM.

      IF SY-SUBRC IS INITIAL.
        SELECT MATNR WERKS LGPBE
          FROM MARD
            INTO TABLE T_MARD
            FOR ALL ENTRIES IN T_LIN
              WHERE MATNR EQ T_LIN-MATNR
                AND WERKS EQ T_LIN-WERKS.

        LOOP AT T_DOC INTO WA_DOC.
          WL_INDEX = SY-TABIX.
          WL_BRANCH = WA_DOC-BRANCH.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WL_BRANCH
            IMPORTING
              OUTPUT = WA_DOC-BRANCH.
          MODIFY T_DOC FROM WA_DOC INDEX WL_INDEX
                                   TRANSPORTING BRANCH.
          CLEAR: WA_DOC.

        ENDLOOP.

        SELECT LIFNR NAME1
          FROM LFA1
            INTO TABLE T_LFA1
            FOR ALL ENTRIES IN T_DOC
              WHERE LIFNR EQ T_DOC-BRANCH.

        SELECT BUKRS BUTXT
          FROM T001
            INTO TABLE T_T001
            FOR ALL ENTRIES IN T_EKKO
              WHERE BUKRS EQ T_EKKO-BUKRS.

        SELECT WERKS NAME1
          FROM T001W
            INTO TABLE T_T001W
            FOR ALL ENTRIES IN T_LIN
              WHERE WERKS EQ T_LIN-WERKS.


      ENDIF.

      SELECT DOCNUM DOCSTA
        FROM J_1BNFE_ACTIVE
          INTO TABLE T_ACTIVE
          FOR ALL ENTRIES IN T_DOC
             WHERE DOCNUM EQ T_DOC-DOCNUM.

    ENDIF.


  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZAR_DADOS .
  DATA: WL_MENGE     TYPE EKPO-MENGE,
        WL_TOT_MENGE TYPE EKBE-MENGE,
        P_FORN       TYPE EKKO-LIFNR,
        W_ANSWER(1).



  WG_XBLNR = S_NFENUM-LOW.

*   Compras
  IF P_COMPR IS NOT INITIAL.
    LOOP AT T_EKPO INTO WA_EKPO.
      READ TABLE T_EKKO INTO WA_EKKO
        WITH KEY EBELN = WA_EKPO-EBELN.

*      IF SY-SUBRC IS INITIAL.
      IF WA_EKKO-FRGRL EQ 'X'.
        MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'Pedido de compras não esta liberado completamente.'.
*
      ELSE.
*          MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'Itens não encontrados para o pedido.'.

        READ TABLE T_T001 INTO WA_T001
          WITH KEY BUKRS = WA_EKKO-BUKRS.

        READ TABLE T_T024 INTO WA_T024
          WITH KEY EKGRP = WA_EKKO-EKGRP.

        READ TABLE T_LFA1 INTO WA_LFA1
          WITH KEY LIFNR = WA_EKKO-LIFNR.

        WA_SAIDA-EKGRP = WA_T024-EKGRP.
        WA_SAIDA-EKNAM = WA_T024-EKNAM.
        WA_SAIDA-EKTEL = WA_T024-EKTEL.

        WA_SAIDA-BUTXT = WA_T001-BUTXT.

        LOOP AT T_EKBE INTO WA_EKBE
            WHERE EBELN EQ WA_EKPO-EBELN
              AND EBELP EQ WA_EKPO-EBELP
              AND VGABE = '1'.

*        READ TABLE T_MSEG INTO WA_MSEG
*          WITH KEY SMBLN = WA_EKBE-BELNR.
*
*        IF SY-SUBRC IS INITIAL.
*          DELETE T_EKBE.
*          CONTINUE.
*        ENDIF.
          IF WA_EKBE-SHKZG EQ 'H'.
            MULTIPLY WA_EKBE-MENGE BY -1.
          ENDIF.
          WL_TOT_MENGE = WL_TOT_MENGE + WA_EKBE-MENGE.

          CLEAR: WA_EKBE, WA_MSEG.
        ENDLOOP.
        WL_MENGE = WA_EKPO-MENGE - WL_TOT_MENGE.

*        IF WL_MENGE LT '0'.
*          DELETE T_EKPO.
*          CONTINUE.
*        ENDIF.

        READ TABLE T_EKES INTO WA_EKES
          WITH KEY EBELN = WA_EKPO-EBELN
                   EBELP = WA_EKPO-EBELP.

        READ TABLE T_EKKN INTO WA_EKKN
          WITH KEY EBELN = WA_EKPO-EBELN
                   EBELP = WA_EKPO-EBELP.

        READ TABLE T_MARD INTO WA_MARD
          WITH KEY MATNR = WA_EKPO-MATNR
                   WERKS = WA_EKPO-WERKS.

        READ TABLE T_T001W INTO WA_T001W
          WITH KEY WERKS = WA_EKPO-WERKS.

        READ TABLE T_T163I INTO WA_T163I
          WITH KEY KNTTP = WA_EKPO-KNTTP.

        P_FORN = WA_EKKO-LIFNR.
        IF WA_EKKO-LIFRE IS NOT INITIAL.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              TEXT_QUESTION         = 'Checar XML como parceiro?'
              TEXT_BUTTON_1         = 'Sim'(100)
              ICON_BUTTON_1         = 'ICON_OKAY '
              TEXT_BUTTON_2         = 'Não'(101)
              ICON_BUTTON_2         = 'ICON_CANCEL'
              DEFAULT_BUTTON        = '1'
              DISPLAY_CANCEL_BUTTON = ' '
              START_COLUMN          = 25
              START_ROW             = 6
            IMPORTING
              ANSWER                = W_ANSWER
            EXCEPTIONS
              TEXT_NOT_FOUND        = 1
              OTHERS                = 2.

          IF W_ANSWER = '1'. "não
            P_FORN = WA_EKKO-LIFRE.
          ENDIF.

        ENDIF.
        CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
          EXPORTING
            P_LIFNR  = P_FORN
            P_NFTYPE = S_NFTYPE-LOW
            P_XBLNR  = WG_XBLNR
            P_DATA   = S_DOCDAT-LOW
            P_WERKS  = WA_EKPO-WERKS
          EXCEPTIONS
            ERROR    = 1
            OTHERS   = 2.

        IF SY-SUBRC IS NOT INITIAL.

          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          CONCATENATE WA_EKKN-ANLN1 WA_EKKN-ANLN2 INTO WA_SAIDA-ANLN1 SEPARATED BY `-`.


          WA_SAIDA-LGPBE  = WA_MARD-LGPBE.
          WA_SAIDA-KOSTL  = WA_EKKN-KOSTL.

          WA_SAIDA-VBELN  = WA_EKES-VBELN.
          WA_SAIDA-MEINS  = WA_EKPO-MEINS.
          WA_SAIDA-KNTTP  = WA_T163I-KNTTX.
          WA_SAIDA-CENTRO = WA_T001W-NAME1.
          WA_SAIDA-WERKS  = WA_T001W-WERKS.
          WA_SAIDA-MEINS  = WA_EKPO-MEINS.
          WA_SAIDA-MATNR  = WA_EKPO-MATNR.
          WA_SAIDA-AEDAT  = WA_EKKO-AEDAT.
          WA_SAIDA-EKGRP  = WA_EKKO-EKGRP.
          WA_SAIDA-LIFNR  = WA_EKKO-LIFNR.
          WA_SAIDA-BUKRS  = WA_EKKO-BUKRS.
          WA_SAIDA-NFENUM = WG_XBLNR.
          WA_SAIDA-DOCDAT = S_DOCDAT-LOW.
          WA_SAIDA-NAME1  = WA_LFA1-NAME1.
          WA_SAIDA-TXZ01  = WA_EKPO-TXZ01.
          WA_SAIDA-EBELP  = WA_EKPO-EBELP.
          WA_SAIDA-EBELN  = WA_EKPO-EBELN.



          APPEND WA_SAIDA TO T_SAIDA.
        ENDIF.
        CLEAR: WA_EKPO, WA_T163I, WA_T001W, WA_T024,WA_T001, WA_LIN, WA_MARD, WA_SAIDA.

      ENDIF.
    ENDLOOP.
*   Transferência
  ELSEIF P_TRANS IS NOT INITIAL.
    LOOP AT T_DOC INTO WA_DOC.
      READ TABLE T_LIN INTO WA_LIN
        WITH KEY DOCNUM = WA_DOC-DOCNUM.

      IF SY-SUBRC IS INITIAL.
        READ TABLE T_MARD INTO WA_MARD
          WITH KEY MATNR = WA_LIN-MATNR
                   WERKS = WA_LIN-WERKS.

        READ TABLE T_LFA1 INTO WA_LFA1
          WITH KEY LIFNR = WA_DOC-BRANCH.

        READ TABLE T_T001 INTO WA_T001
          WITH KEY BUKRS = WA_DOC-BUKRS.

        READ TABLE T_T001W INTO WA_T001W
          WITH KEY WERKS = WA_LIN-WERKS.

        READ TABLE T_ACTIVE INTO WA_ACTIVE
          WITH KEY DOCNUM = WA_DOC-DOCNUM.

        IF WA_ACTIVE-DOCSTA EQ '1'.

          CONCATENATE WA_DOC-NFENUM '-' WA_DOC-SERIES
                 INTO WA_SAIDA-NFENUM.

          WA_SAIDA-LGPBE  = WA_MARD-LGPBE.
          WA_SAIDA-CENTRO = WA_T001W-NAME1.
          WA_SAIDA-WERKS  = WA_T001W-WERKS.
          WA_SAIDA-BUTXT  = WA_T001-BUTXT.
          WA_SAIDA-BUKRS  = WA_T001-BUKRS.
          WA_SAIDA-MEINS  = WA_LIN-MEINS.
          WA_SAIDA-MATNR  = WA_LIN-MATNR.
          WA_SAIDA-BRANCH = WA_DOC-BRANCH.
          WA_SAIDA-DOCDAT = WA_DOC-DOCDAT.
*          WA_SAIDA-NFENUM = WA_DOC-NFENUM.
          WA_SAIDA-NAME1  = WA_LFA1-NAME1.
          WA_SAIDA-MAKTX  = WA_LIN-MAKTX.
          WA_SAIDA-ITMNUM = WA_LIN-ITMNUM. """ ALV
          WA_SAIDA-EBELP  = WA_LIN-ITMNUM. """ SMARTFORMS
          WA_SAIDA-DOCNUM = WA_DOC-DOCNUM. """ ALV
          WA_SAIDA-EBELN  = WA_DOC-DOCNUM. """ SMARTFORMS
          WA_SAIDA-AEDAT  = WA_DOC-DOCDAT.
          APPEND WA_SAIDA TO T_SAIDA.

        ELSE.
          MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'Nota fiscal não está autorizada.'.
          LEAVE LIST-PROCESSING.
        ENDIF.
        CLEAR: WA_LIN, WA_MARD, WA_SAIDA, WA_LFA1, WA_T001, WA_T001W, WA_ACTIVE.
      ENDIF.

    ENDLOOP.

  ENDIF.
ENDFORM.                    " ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT." USING 'T_SAIDA'.
  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
  WL_LAYOUT-BOX_TABNAME  = 'T_SAIDA'.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = V_REPORT
*     IS_VARIANT         = GS_VARIANT_C
*     I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
      IT_FIELDCAT        = ESTRUTURA[]
      IS_LAYOUT          = WL_LAYOUT
      I_SAVE             = 'A'
      IT_EVENTS          = EVENTS
      IS_PRINT           = T_PRINT
    TABLES
      T_OUTTAB           = T_SAIDA.



ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.

  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.



ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.
  IF P_COMPR IS NOT INITIAL.
    PERFORM MONTAR_ESTRUTURA USING:
          1  'EKPO'       'EBELN'            'T_SAIDA' 'EBELN'            'Pedido'          ' ' ,      "   Pedido
          2  'EKPO'       'EBELP'            'T_SAIDA' 'EBELP'            ' '               ' ' ,      "   Item
          3  'EKPO'       'TXZ01'            'T_SAIDA' 'TXZ01'            'Material'        ' ' ,      "   Material
          4  'LFA1'       'NAME1'            'T_SAIDA' 'NAME1'            'Fornecedor'      ' ' ,      "   Fornecedor
          5  'J_1BNFDOC'  'NFENUM'           'T_SAIDA' 'NFENUM'           'Nota Fiscal'     ' ' ,      "   Nota Fiscal
          6  'J_1BNFDOC'  'DOCDAT'           'T_SAIDA' 'DOCDAT'          'Data Doc.'       ' ' .      "   Data Doc.

  ELSEIF P_TRANS IS NOT INITIAL.
    PERFORM MONTAR_ESTRUTURA USING:
         1  'J_1BNFDOC'  'DOCNUM'           'T_SAIDA' 'DOCNUM'           'Número Documento'  ' ' ,      "   Número Documento
         2  'J_1BNFLIN'  'ITMNUM'           'T_SAIDA' 'ITMNUM'           ' '                 ' ' ,      "   Item
         3  'J_1BNFLIN'  'MAKTX'            'T_SAIDA' 'MAKTX'            'Material'          ' ' ,      "   Material
         4  'LFA1'       'NAME1'            'T_SAIDA' 'NAME1'            'Fornecedor'        ' ' ,      "   Fornecedor
         5  'J_1BNFDOC'  'NFENUM'           'T_SAIDA' 'NFENUM'           'Nota Fiscal'       ' ' ,      "   Nota Fiscal
         6  'J_1BNFDOC'  'DOCDAT'           'T_SAIDA' 'DOCDAT'           'Data Doc.'         ' ' .      "   Data Doc.
  ENDIF.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR: WA_ESTRUTURA.


  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

*  IF P_FIELD EQ 'BELNR'
*    OR P_FIELD EQ 'DOC_VARIACAO'.
*    WA_ESTRUTURA-HOTSPOT = 'X'.
*  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.

  V_REPORT = SY-REPID.

  IF P_COMPR IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-004.
  ELSE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-005.
  ENDIF.
ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFICA_TELA .
  LOOP AT SCREEN.

    IF P_COMPR = 'X'.

      IF SCREEN-GROUP1 = 'C2'.
        SCREEN-INVISIBLE = 1.
        SCREEN-INPUT     = 0.
        SCREEN-ACTIVE    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF SCREEN-GROUP1 = 'C1'.
        SCREEN-INVISIBLE = 0.
        SCREEN-INPUT     = 1.
        SCREEN-ACTIVE    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ELSE.

      IF SCREEN-GROUP1 = 'C1'.
        SCREEN-INVISIBLE = 1.
        SCREEN-INPUT     = 0.
        SCREEN-ACTIVE    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF SCREEN-GROUP1 = 'C2'.
        SCREEN-INVISIBLE = 0.
        SCREEN-INPUT     = 1.
        SCREEN-ACTIVE    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFICA_TELA
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XPF_STATUS_SET USING UCOMM TYPE KKBLO_T_EXTAB.         "#EC CALLED
  DATA: TL_FCODE TYPE TABLE OF SY-UCOMM,
        WL_FCODE TYPE SY-UCOMM.

*  IF P_COMPR IS INITIAL.
*    WL_FCODE = '&APR'.
*    APPEND WL_FCODE TO TL_FCODE.
*  ENDIF.

  SET PF-STATUS 'STANDARD_FULLSCREEN'." EXCLUDING TL_FCODE.

ENDFORM. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD..     "#EC CALLED

  DATA: TL_0042      TYPE TABLE OF ZMMT0042,
        RT_LINES     TYPE TABLE OF TLINE WITH HEADER LINE,
        WL_0042      TYPE ZMMT0042,
        WL_MSG,
        P_NUMBER(10) TYPE C,
        WL_XBLNR     TYPE XBLNR1,
        WL_AEDAT     TYPE EKKO-AEDAT.
  DATA BEGIN OF TEXTLINES OCCURS 0.
          INCLUDE STRUCTURE TLINE.
  DATA END OF TEXTLINES.

  REFRESH: TL_0042.

  CASE SY-UCOMM.
*    WHEN '&APR'.
*      LOOP AT T_SAIDA INTO WA_SAIDA
*        WHERE MARK IS NOT INITIAL.

*        CLEAR: P_NUMBER.
*
*        CALL FUNCTION 'NUMBER_GET_NEXT'
*          EXPORTING
*            NR_RANGE_NR             = '01'
*            OBJECT                  = 'Z_NUM_APR'
*          IMPORTING
*            NUMBER                  = P_NUMBER
*          EXCEPTIONS
*            INTERVAL_NOT_FOUND      = 1
*            NUMBER_RANGE_NOT_INTERN = 2
*            OBJECT_NOT_FOUND        = 3
*            QUANTITY_IS_0           = 4
*            QUANTITY_IS_NOT_1       = 5
*            INTERVAL_OVERFLOW       = 6
*            BUFFER_OVERFLOW         = 7
*            OTHERS                  = 8.
*
*        IF SY-SUBRC NE 0.
*          CLEAR: P_NUMBER.
*          MESSAGE E836(SD) WITH 'O intervalo de numeração,'
*                            'não foi encontrado!'.
*
*        ELSE.
*          MOVE: WA_SAIDA-NFENUM TO WL_0042-XBLNR,
*                WA_SAIDA-EBELN  TO WL_0042-EBELN,
*                WA_SAIDA-EBELP  TO WL_0042-EBELP,
*                SY-DATUM        TO WL_0042-DATA_ATUAL,
*                SY-UNAME        TO WL_0042-USNAM,
*                SY-UZEIT        TO WL_0042-HORA_ATUAL,
*                P_NUMBER        TO WL_0042-NUMERO_APR.
*
*          APPEND WL_0042 TO TL_0042.
*          CLEAR: WL_0042.
*        ENDIF.
*      ENDLOOP.
*
*      MODIFY ZMMT0042 FROM TABLE TL_0042.
*      COMMIT WORK.

    WHEN '&SMARTFORM'.
      IF P_COMPR IS NOT INITIAL.
        CLEAR: P_NUMBER.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'Z_NUM_APR'
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

          LOOP AT T_SAIDA INTO WA_SAIDA
             WHERE MARK IS NOT INITIAL.
            MOVE: WA_SAIDA-NFENUM TO WL_0042-XBLNR,
                  WA_SAIDA-EBELN  TO WL_0042-EBELN,
                  WA_SAIDA-EBELP  TO WL_0042-EBELP,
                  SY-DATUM        TO WL_0042-DATA_ATUAL,
                  SY-UNAME        TO WL_0042-USNAM,
                  SY-UZEIT        TO WL_0042-HORA_ATUAL,
                  P_NUMBER        TO WL_0042-NUMERO_APR.


            APPEND WL_0042 TO TL_0042.
            CLEAR: WL_0042.
          ENDLOOP.
        ENDIF.
*      ENDLOOP.

        MODIFY ZMMT0042 FROM TABLE TL_0042.
        COMMIT WORK.

        REFRESH: T_RELA_DET, IT_EKPO_TEXT.

        LOOP AT T_SAIDA INTO WA_SAIDA
          WHERE MARK IS NOT INITIAL.
          WL_MSG = 'X'.

          MOVE-CORRESPONDING WA_SAIDA TO WA_RELA_DET.

          READ TABLE T_EKPO INTO WA_EKPO
            WITH KEY EBELN = WA_SAIDA-EBELN
                     EBELP = WA_SAIDA-EBELP.

          IF WA_EKPO-PSTYP NE '9'.
            PERFORM: TEXTO_MATERIAL_DESC USING WA_SAIDA-MATNR WA_SAIDA-EBELP
                                               WA_SAIDA-EBELN SPACE.
          ELSE.
******************************************************************************
            CLEAR WA_RELA_DET-TXZ01.

            READ TABLE T_ESLL INTO WA_ESLL
              WITH KEY PACKNO = WA_EKPO-PACKNO.

            READ TABLE T_ESLL_AUX INTO WA_ESLL_AUX
              WITH KEY PACKNO = WA_ESLL-SUB_PACKNO.


            STXL_ID-TDOBJECT = 'ASMD'.
            STXL_ID-TDNAME   =  WA_ESLL_AUX-TDNAME.
            STXL_ID-TDID     = 'LTXT'.
            STXL_ID-TDSPRAS  = 'PT'.

            IMPORT TLINE TO RT_LINES
             FROM DATABASE STXL(TX)
                  CLIENT   SY-MANDT
                  ID       STXL_ID
                  ACCEPTING TRUNCATION                     "important for Unicode->Nonunicode
                  IGNORING CONVERSION ERRORS.



            LOOP AT RT_LINES.

              WA_EKPO_TEXT-EBELN = WA_SAIDA-EBELN.
              WA_EKPO_TEXT-EBELP = WA_SAIDA-EBELP.
*        WA_EKPO_TEXT-BELNR = P_BELNR_DESC.
              WA_EKPO_TEXT-TDLINE = RT_LINES-TDLINE.
              APPEND WA_EKPO_TEXT TO IT_EKPO_TEXT.

              CLEAR: WA_EKPO_TEXT.
            ENDLOOP.

            IF NOT ( IT_EKPO_TEXT[] IS INITIAL ).
              DELETE IT_EKPO_TEXT WHERE TDLINE EQ ''.
            ENDIF.
          ENDIF.

*    ENDIF.

          CLEAR: TEXTLINES[].

******************************************************************************



          APPEND WA_RELA_DET TO T_RELA_DET.
          CLEAR: WA_RELA_DET.


        ENDLOOP.

        IF WL_MSG IS INITIAL.
          MESSAGE S000(Z01) WITH 'Selecionar pelo menos uma linha.'.

        ELSE.
*       Chamada de relatório SMARTFORMS
          CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
            EXPORTING
              FORMNAME = 'ZMMR010_SMART_FORM_4'
            IMPORTING
              FM_NAME  = VG_FM_NAME
            EXCEPTIONS
              OTHERS   = 3.

          CHECK SY-SUBRC IS INITIAL.




          CALL FUNCTION VG_FM_NAME
            EXPORTING
              BUKRS        = WA_SAIDA-BUKRS
              EMPRE        = WA_SAIDA-BUTXT
              WERKS        = WA_SAIDA-WERKS
              CENTR        = WA_SAIDA-CENTRO
              CPUDT        = SY-DATUM
              CPUTM        = SY-UZEIT
              BELNR        = P_NUMBER
              XBLNR        = WG_XBLNR
              USNAM        = SY-UNAME
              EBELN        = WA_SAIDA-EBELN
              EKGRP        = WA_SAIDA-EKGRP
              EKNAM        = WA_SAIDA-EKNAM
              EKTEL        = WA_SAIDA-EKTEL
              LIFNR        = WA_SAIDA-LIFNR
              FORNE        = WA_SAIDA-NAME1
              AEDAT        = WA_SAIDA-AEDAT
              KNTTP        = ' '
              VBELN        = WA_SAIDA-VBELN
              ANLN1        = WA_SAIDA-ANLN1
              KOSTL        = WA_SAIDA-KOSTL
              LGPBE        = WA_SAIDA-LGPBE
              MENGE        = ' '
              EBELP        = WA_SAIDA-EBELP
              DOCDAT       = WA_SAIDA-DOCDAT
            TABLES
              ITENS        = T_RELA_DET
              IT_EKPO_TEXT = IT_EKPO_TEXT[].

*        ENDLOOP.
        ENDIF.
*        IF WL_MSG IS INITIAL.
*          MESSAGE S000(Z01) WITH 'Selecionar pelo menos uma linha.'.
*        ENDIF.

*     Transferência
      ELSE.
        REFRESH: T_RELA_DET, IT_EKPO_TEXT.
        CLEAR: P_NUMBER.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'Z_NUM_APR'
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
          MOVE: WA_SAIDA-NFENUM TO WL_0042-XBLNR,
                WA_SAIDA-EBELN  TO WL_0042-EBELN,
                WA_SAIDA-EBELP  TO WL_0042-EBELP,
                SY-DATUM        TO WL_0042-DATA_ATUAL,
                SY-UNAME        TO WL_0042-USNAM,
                SY-UZEIT        TO WL_0042-HORA_ATUAL,
                P_NUMBER        TO WL_0042-NUMERO_APR.


          APPEND WL_0042 TO TL_0042.
          CLEAR: WL_0042.
        ENDIF.

        LOOP AT T_SAIDA INTO WA_SAIDA
          WHERE MARK IS NOT INITIAL.
          WL_MSG = 'X'.
*
*
          MOVE-CORRESPONDING WA_SAIDA TO WA_RELA_DET.
          WA_RELA_DET-EBELP = WA_SAIDA-ITMNUM.
          WA_RELA_DET-TXZ01 = WA_SAIDA-MAKTX.

          CLEAR WA_RELA_DET-TXZ01.
          PERFORM: TEXTO_MATERIAL_DESC USING WA_SAIDA-MATNR WA_RELA_DET-EBELP
                                               SPACE SPACE.
*
*          PERFORM: TEXTO_MATERIAL_DESC IN PROGRAM ZMMR010
*                                       USING WA_SAIDA-MATNR WA_SAIDA-EBELP
*                                             WA_SAIDA-EBELN SPACE.

          APPEND WA_RELA_DET TO T_RELA_DET.
          CLEAR: WA_RELA_DET.
        ENDLOOP.

        IF WL_MSG IS INITIAL.
          MESSAGE S000(Z01) WITH 'Selecionar pelo menos uma linha.'.

        ELSE.
*       Chamada de relatório SMARTFORMS
          CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
            EXPORTING
              FORMNAME = 'ZMMR010_SMART_FORM_4'
            IMPORTING
              FM_NAME  = VG_FM_NAME
            EXCEPTIONS
              OTHERS   = 3.

          CHECK SY-SUBRC IS INITIAL.


          WL_XBLNR = WA_SAIDA-NFENUM.

          CALL FUNCTION VG_FM_NAME
            EXPORTING
*
              BUKRS        = WA_SAIDA-BUKRS
              EMPRE        = WA_SAIDA-BUTXT
              WERKS        = WA_SAIDA-WERKS
              CENTR        = WA_SAIDA-CENTRO
              CPUDT        = SY-DATUM
              CPUTM        = SY-UZEIT
              BELNR        = P_NUMBER
              XBLNR        = WL_XBLNR
              USNAM        = SY-UNAME
              EBELN        = ' '  "WA_SAIDA-EBELN
              EKGRP        = WA_SAIDA-EKGRP
              EKNAM        = WA_SAIDA-EKNAM
              EKTEL        = WA_SAIDA-EKTEL
              LIFNR        = WA_SAIDA-LIFNR
              FORNE        = WA_SAIDA-NAME1
              AEDAT        = WL_AEDAT "WA_SAIDA-AEDAT
              KNTTP        = ' '
              VBELN        = WA_SAIDA-VBELN
              ANLN1        = WA_SAIDA-ANLN1
              KOSTL        = WA_SAIDA-KOSTL
              LGPBE        = WA_SAIDA-LGPBE
              MENGE        = ' '
              EBELP        = WA_SAIDA-EBELP
            TABLES
              ITENS        = T_RELA_DET
              IT_EKPO_TEXT = IT_EKPO_TEXT[].
*
*        ENDLOOP.
        ENDIF.
      ENDIF.

  ENDCASE.


ENDFORM. "XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  TEXTO_MATERIAL
*&---------------------------------------------------------------------*
FORM TEXTO_MATERIAL_DESC USING  P_MATNR_DESC
                                P_EBELP_DESC
                                P_EBELN_DESC
                                P_BELNR_DESC.

  TYPES:
    BEGIN OF STXL_ID,
      TDOBJECT LIKE STXL-TDOBJECT,
      TDNAME   LIKE STXL-TDNAME,
      TDID     LIKE STXL-TDID,
      TDSPRAS  LIKE STXL-TDSPRAS,
    END OF STXL_ID.

  DATA BEGIN OF TEXTHEADER.
          INCLUDE STRUCTURE THEAD.
  DATA END OF TEXTHEADER.

  DATA BEGIN OF TEXTLINES OCCURS 0.
          INCLUDE STRUCTURE TLINE.
  DATA END OF TEXTLINES.

  DATA: RT_LINES  TYPE TABLE OF TLINE WITH HEADER LINE.

  DATA: WL_STXL_ID TYPE STXL_ID,
        V_NAME     TYPE STXH-TDNAME.

  CONCATENATE P_EBELN_DESC P_EBELP_DESC INTO V_NAME.

  IF NOT ( P_MATNR_DESC  IS INITIAL ) AND ( NOT P_EBELP_DESC IS INITIAL ).

    CLEAR: WL_STXL_ID.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID       = 'F03'
        LANGUAGE = 'P'
        NAME     = V_NAME
        OBJECT   = 'EKPO'
      IMPORTING
        HEADER   = TEXTHEADER
      TABLES
        LINES    = TEXTLINES
      EXCEPTIONS
        OTHERS   = 1.


    LOOP  AT TEXTLINES.

      WA_EKPO_TEXT-EBELN = P_EBELN_DESC.
      WA_EKPO_TEXT-EBELP = P_EBELP_DESC.
      WA_EKPO_TEXT-BELNR = P_BELNR_DESC.
      WA_EKPO_TEXT-TDLINE = RT_LINES-TDLINE.
      APPEND WA_EKPO_TEXT TO IT_EKPO_TEXT.

      CLEAR: WA_EKPO_TEXT.

    ENDLOOP.

    IF NOT ( IT_EKPO_TEXT[] IS INITIAL ).
      DELETE IT_EKPO_TEXT WHERE TDLINE EQ ''.
    ENDIF.

    IF ( TEXTLINES[] IS INITIAL ).

      STXL_ID-TDOBJECT = 'MATERIAL'.
      STXL_ID-TDNAME   =  P_MATNR_DESC.
      STXL_ID-TDID     = 'BEST'.
      STXL_ID-TDSPRAS  = 'PT'.

      IMPORT TLINE TO RT_LINES
       FROM DATABASE STXL(TX)
            CLIENT   SY-MANDT
            ID       STXL_ID
            ACCEPTING TRUNCATION                     "important for Unicode->Nonunicode
            IGNORING CONVERSION ERRORS.



      LOOP AT RT_LINES.

        WA_EKPO_TEXT-EBELN = P_EBELN_DESC.
        WA_EKPO_TEXT-EBELP = P_EBELP_DESC.
        WA_EKPO_TEXT-BELNR = P_BELNR_DESC.
        WA_EKPO_TEXT-TDLINE = RT_LINES-TDLINE.
        APPEND WA_EKPO_TEXT TO IT_EKPO_TEXT.

        CLEAR: WA_EKPO_TEXT.
      ENDLOOP.

      IF NOT ( IT_EKPO_TEXT[] IS INITIAL ).
        DELETE IT_EKPO_TEXT WHERE TDLINE EQ ''.
      ENDIF.


    ENDIF.

    CLEAR: TEXTLINES[].
  ELSEIF P_EBELN_DESC IS INITIAL
     AND P_MATNR_DESC IS NOT INITIAL.

    STXL_ID-TDOBJECT = 'MATERIAL'.
    STXL_ID-TDNAME   =  P_MATNR_DESC.
    STXL_ID-TDID     = 'BEST'.
    STXL_ID-TDSPRAS  = 'PT'.

    IMPORT TLINE TO RT_LINES
     FROM DATABASE STXL(TX)
          CLIENT   SY-MANDT
          ID       STXL_ID
          ACCEPTING TRUNCATION                     "important for Unicode->Nonunicode
          IGNORING CONVERSION ERRORS.



    LOOP AT RT_LINES.

      WA_EKPO_TEXT-EBELN = P_EBELN_DESC.
      WA_EKPO_TEXT-EBELP = P_EBELP_DESC.
      WA_EKPO_TEXT-BELNR = P_BELNR_DESC.
      WA_EKPO_TEXT-TDLINE = RT_LINES-TDLINE.
      APPEND WA_EKPO_TEXT TO IT_EKPO_TEXT.

      CLEAR: WA_EKPO_TEXT.
    ENDLOOP.

    IF NOT ( IT_EKPO_TEXT[] IS INITIAL ).
      DELETE IT_EKPO_TEXT WHERE TDLINE EQ ''.
    ENDIF.

  ENDIF.
ENDFORM.                    " TEXTO_MATERIAL
