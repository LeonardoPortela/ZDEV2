*&---------------------------------------------------------------------*
*& Report  ZCOR007
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCOR007.


TABLES: T001,     LIPS, ZCOT0001,
        ZCOT0004, BKPF, VTTK,
        ZIB_CONTABIL.

CONSTANTS: C_M TYPE C LENGTH 1 VALUE '+'.

DATA: CL_ALV TYPE REF TO CL_GUI_ALV_GRID.
TYPE-POOLS: SLIS.
DATA: FIELDCATALOG TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GD_LAYOUT    TYPE SLIS_LAYOUT_ALV,
      GD_REPID     LIKE SY-REPID.
DATA: SORT      TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   E_GROUP_OPENED.
*       message texts
TABLES: T100.

*&---------------------------------------------------------------------*
*& Tabelas Internas / Estruturas                                       *
*&---------------------------------------------------------------------*
TYPES: BEGIN OF TY_032.
TYPES: TKNUM LIKE ZLEST0032-TKNUM,
       FKNUM LIKE ZLEST0032-FKNUM,
       EBELN LIKE ZLEST0032-EBELN,
       EBELP LIKE ZLEST0032-EBELP,
       LBLNI LIKE ZLEST0032-LBLNI,
       BELNR LIKE ZLEST0032-BELNR,
       GJAHR LIKE ZLEST0032-GJAHR.
TYPES END OF TY_032.

DATA: T_032 TYPE TABLE OF TY_032 WITH HEADER LINE.

DATA: BEGIN OF T_001 OCCURS 0.
        INCLUDE STRUCTURE ZCOT0001.
DATA: END OF T_001.

DATA: BEGIN OF T_003 OCCURS 0.
        INCLUDE STRUCTURE ZCOT0003.
DATA: END OF T_003.

TYPES: BEGIN OF TY_VTTK.
TYPES: TKNUM LIKE VTTK-TKNUM,
       SHTYP LIKE VTTK-SHTYP,
       EXTI1 LIKE VTTK-EXTI1,
       EXTI2 LIKE VTTK-EXTI2,
       TPLST LIKE VTTK-TPLST,
       DATBG LIKE VTTK-DATBG.
TYPES END OF TY_VTTK.

DATA: T_VTTK TYPE TABLE OF TY_VTTK WITH HEADER LINE.

TYPES: BEGIN OF TY_VTTP.
TYPES: TKNUM LIKE VTTP-TKNUM,
       TPNUM LIKE VTTP-TPNUM,
       VBELN LIKE VTTP-VBELN.
TYPES END OF TY_VTTP.

DATA: T_VTTP TYPE TABLE OF TY_VTTP WITH HEADER LINE.

*types: begin of ty_vbfa.
*types:  vbelv   like vbfa-vbelv,
*        posnv   like vbfa-posnv,
*        vbeln   like vbfa-vbeln,
*        posnn   like vbfa-posnn,
*        vbtyp_n like vbfa-vbtyp_n,
*        erdat   like vbfa-erdat,
*        vbtyp_v like vbfa-vbtyp_v.
*types end of ty_vbfa.
*
*data: t_vbfa type table of ty_vbfa with header line.

TYPES: BEGIN OF TY_LIKP.
TYPES: VBELN LIKE LIKP-VBELN,
       VKORG LIKE LIKP-VKORG,
       ERDAT LIKE LIKP-ERDAT,
       LFDAT LIKE LIKP-LFDAT.
TYPES  END OF TY_LIKP.

DATA: T_LIKP TYPE TABLE OF TY_LIKP WITH HEADER LINE.

DATA: BEGIN OF T_LIPS OCCURS 0,
        VBELN LIKE LIPS-VBELN,
        POSNR LIKE LIPS-POSNR,
        MATNR LIKE LIPS-MATNR,
        WERKS LIKE LIPS-WERKS,
        VTWEG LIKE LIPS-VTWEG,
        SPART LIKE LIPS-SPART,
        BRGEW LIKE LIPS-BRGEW,
        LGORT LIKE LIPS-LGORT,
        CHARG LIKE LIPS-CHARG,
        VGBEL LIKE LIPS-VGBEL,
        VGPOS LIKE LIPS-VGPOS,
      END OF T_LIPS.

DATA: BEGIN OF T_VBAP OCCURS 0,
        VBELN  LIKE VBAP-VBELN,
        POSNR  LIKE VBAP-POSNR,
        ERDAT  LIKE VBAP-ERDAT,
        KWMENG LIKE VBAP-KWMENG,
        WERKS  LIKE VBAP-WERKS,
        VSTEL  LIKE VBAP-VSTEL,
      END OF T_VBAP.

DATA: BEGIN OF T_VBRP OCCURS 0,
        VBELN  LIKE VBRP-VBELN,
        POSNR  LIKE VBRP-POSNR,
        VGBEL  LIKE VBRP-VGBEL,
        VGPOS  LIKE VBRP-VGPOS,
        WERKS  LIKE VBAP-WERKS,
        ERDAT  LIKE VBAP-ERDAT,
        KWMENG LIKE VBAP-KWMENG,
      END OF T_VBRP.

DATA: BEGIN OF T_EKPO OCCURS 0,
        EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP,
        MATNR LIKE EKPO-MATNR,
        WERKS LIKE EKPO-WERKS,
      END OF T_EKPO.

DATA: BEGIN OF T_RBKP OCCURS 0,
        BELNR LIKE RBKP-BELNR,
        GJAHR LIKE RBKP-GJAHR,
        BUDAT LIKE RBKP-BUDAT,
        TCODE LIKE RBKP-TCODE,
        RMWWR LIKE RBKP-RMWWR,
        BUKRS LIKE RBKP-BUKRS,
      END OF T_RBKP.

DATA: BEGIN OF T_RBCO OCCURS 0,
        BELNR   LIKE RBCO-BELNR,
        GJAHR   LIKE RBCO-GJAHR,
        BUZEI   LIKE RBCO-BUZEI,
        COBL_NR LIKE RBCO-COBL_NR,
        SAKNR   LIKE RBCO-SAKNR,
        WRBTR   LIKE RBCO-WRBTR,
        GSBER   LIKE RBCO-GSBER,
        SGTXT   LIKE RBCO-SGTXT,
      END OF T_RBCO.

TYPES BEGIN OF TY_016.
TYPES: TRANSPORTADOR  LIKE ZLEST0016-TRANSPORTADOR,
       POSTO          LIKE ZLEST0016-POSTO,
       LOTE           LIKE ZLEST0016-LOTE,
       CHVID          LIKE ZLEST0016-CHVID,
       CTAFRETE       LIKE ZLEST0016-CTAFRETE,
       CONHECIMENTO   LIKE ZLEST0016-CONHECIMENTO,
       VLR_CONFIRMADO LIKE ZLEST0016-VLR_CONFIRMADO.
TYPES END OF TY_016.

DATA: T_016 TYPE TABLE OF TY_016 WITH HEADER LINE.

DATA: BEGIN OF T_020 OCCURS 0,
        TRANSPORTADOR LIKE ZLEST0020-TRANSPORTADOR,
        POSTO         LIKE ZLEST0020-POSTO,
        LOTE          LIKE ZLEST0020-LOTE,
        CHVID         LIKE ZLEST0020-CHVID,
        DIFER_TRANSP  LIKE ZLEST0020-DIFER_TRANSP,
        VLRPERDA      LIKE ZLEST0020-VLRPERDA,
      END OF T_020.

DATA: BEGIN OF T_015 OCCURS 0,
        TRANSPORTADOR LIKE ZLEST0015-TRANSPORTADOR,
        POSTO         LIKE ZLEST0015-POSTO,
        LOTE          LIKE ZLEST0015-LOTE,
      END OF T_015.

DATA: BEGIN OF T_022 OCCURS 0,
        TRANSPORTADOR LIKE ZLEST0022-TRANSPORTADOR,
        POSTO         LIKE ZLEST0022-POSTO,
        LOTE          LIKE ZLEST0022-LOTE,
        CHVID         LIKE ZLEST0022-CHVID,
        TIPTRANSP     LIKE ZLEST0022-TIPTRANSP,
        CTLGLANCTO    LIKE ZLEST0022-CTLGLANCTO,
        DOCSAP        LIKE ZLEST0022-DOCSAP,
        GJAHR         LIKE ZLEST0022-GJAHR,
        BUKRS         LIKE ZLEST0022-BUKRS,
      END OF T_022.

DATA: BEGIN OF T_BKPF OCCURS 0,
        BUKRS LIKE BKPF-BUKRS,
        BELNR LIKE BKPF-BELNR,
        GJAHR LIKE BKPF-GJAHR,
        BUDAT LIKE BKPF-BUDAT,
      END OF T_BKPF.

DATA: BEGIN OF T_SAIDA OCCURS 0,
        SEL,
        TPOPER    LIKE ZCOT0001-TP_OPER,
        SHTYP     LIKE VTTK-SHTYP,
        BEZEI     LIKE TVTKT-BEZEI,
        DTMOV     LIKE SY-DATUM,
        DTCHE     LIKE SY-DATUM,
        MATNR     LIKE MARA-MATNR,
        BRGEW     LIKE LIPS-BRGEW,
        PESOC     LIKE LIPS-BRGEW,
        EXTI1     LIKE VTTK-EXTI1,
        TKNUM     LIKE ZLEST0032-TKNUM,
        TPNUM     LIKE VTTP-TPNUM,
        VBELN     LIKE LIPS-VBELN,
        POSNR     LIKE LIPS-POSNR,
        FKNUM     LIKE ZLEST0032-FKNUM,
        EBELN     LIKE ZLEST0032-EBELN,
        EBELP     LIKE ZLEST0032-EBELP,
        BELNR     LIKE ZLEST0032-BELNR,
        GJAHR     LIKE RBKP-GJAHR,
        WERKS     LIKE LIPS-WERKS,
        LGORT     LIKE LIPS-LGORT,
        CHARG     LIKE LIPS-CHARG,
        TCODE(20),
        WRBTR     LIKE RBCO-WRBTR,
        BUKRS     LIKE RBKP-BUKRS,
        BUDAT     LIKE SY-DATUM,
        XBLNR     LIKE BSID-XBLNR,
        ZUUMB(16),
        TPLST     LIKE VTTK-TPLST,
      END OF T_SAIDA.

DATA: T_SAIDA2 LIKE T_SAIDA OCCURS 0.

DATA: BEGIN OF T_TVTKT OCCURS 0.
        INCLUDE STRUCTURE TVTKT.
DATA: END OF T_TVTKT.

*&---------------------------------------------------------------------*
*& Tela de Seleção                                                     *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETER: S_BUKRS TYPE T001-BUKRS OBLIGATORY.

  SELECT-OPTIONS: S_WERKS FOR LIPS-WERKS.

  PARAMETERS:     P_TPOPER TYPE ZCOT0001-TP_OPER AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY.

  SELECT-OPTIONS: S_TDLNR FOR VTTK-TDLNR,
                  S_MATNR FOR LIPS-MATNR,
                  S_BUDAT FOR BKPF-BUDAT.
SELECTION-SCREEN END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& Início do Programa                                                  *
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  DATA: VG_CHECK TYPE C LENGTH 1.

  LOOP AT S_WERKS.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        BUKRS                = S_BUKRS
        CENTRO               = S_WERKS-LOW
      EXCEPTIONS
        INFORMAR_CENTRO      = 1
        NAO_CENTRO_R_VIRTUAL = 2
        INFORMAR_CENTRO_OUT  = 3
        INFORMAR_CENTRO_V    = 4
        OTHERS               = 5.

    IF SY-SUBRC <> 0.
      VG_CHECK = 'X'.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDLOOP.

  CHECK VG_CHECK IS INITIAL.

  PERFORM: SELECIONA_DADOS,
           ORGANIZA_DADOS.

  IF SY-BATCH IS INITIAL.
    PERFORM CHAMA_ALV.
  ELSE.

    LOOP AT T_SAIDA.
      T_SAIDA-SEL = 'X'.
      MODIFY T_SAIDA.
    ENDLOOP.
    PERFORM LANC_CONTABIL.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  DATA: IT_ZSDT_DEPARA_CEN TYPE TABLE OF ZSDT_DEPARA_CEN WITH HEADER LINE.

  RANGES: R_SHTYP FOR VTTK-SHTYP.
  RANGES: R_SAKNR FOR ZCOT0001-SAKNR.
  CLEAR: R_SHTYP, R_SAKNR. REFRESH: R_SHTYP, R_SAKNR.

* Seleção ZLEST0032
  SELECT TKNUM FKNUM EBELN EBELP LBLNI BELNR GJAHR
    FROM ZLEST0032 INTO TABLE T_032
    WHERE STATUS1 EQ SPACE AND
          BELNR   NE SPACE.

  CHECK SY-SUBRC IS INITIAL.

  SELECT * FROM TVTKT INTO TABLE T_TVTKT WHERE SPRAS = SY-LANGU.

  SELECT * FROM ZCOT0001 INTO TABLE T_001
    WHERE TP_OPER = P_TPOPER.

  CHECK SY-SUBRC IS INITIAL.
  LOOP AT T_001.

    R_SHTYP-OPTION = 'EQ'.
    R_SHTYP-SIGN = 'I'.
    R_SHTYP-LOW = T_001-SHTYP.

    R_SAKNR-OPTION = 'EQ'.
    R_SAKNR-SIGN = 'I'.
    R_SAKNR-LOW = T_001-SAKNR.

    APPEND: R_SHTYP, R_SAKNR.
    CLEAR: R_SHTYP, R_SAKNR.

    IF NOT T_001-HKONT IS INITIAL.
      R_SAKNR-OPTION = 'EQ'.
      R_SAKNR-SIGN = 'I'.
      R_SAKNR-LOW = T_001-HKONT.

      APPEND: R_SAKNR.
      CLEAR: R_SAKNR.
    ENDIF.

  ENDLOOP.

  IF NOT T_032[] IS INITIAL.
* Selecionar DOCUMENTO DE TRANSPORTE:
    SELECT TKNUM SHTYP EXTI1 EXTI2 TPLST DATBG
      FROM VTTK INTO TABLE T_VTTK
      FOR ALL ENTRIES IN T_032
      WHERE TKNUM EQ T_032-TKNUM AND
            TDLNR IN S_TDLNR     AND
            SHTYP IN R_SHTYP.
  ENDIF.

  CHECK ( SY-SUBRC IS INITIAL ) AND ( NOT T_032[] IS INITIAL ).

* Selecionar Remessa vinculada ao Doc.Transporte:
  SELECT TKNUM TPNUM VBELN FROM VTTP INTO TABLE T_VTTP
    FOR ALL ENTRIES IN T_VTTK
    WHERE TKNUM EQ T_VTTK-TKNUM.

  SORT T_VTTP BY TKNUM.
  DELETE ADJACENT DUPLICATES FROM T_VTTP COMPARING TKNUM.

  CHECK NOT T_VTTP[] IS INITIAL.

* Selecionar Dados da Remessa:
  SELECT VBELN VKORG ERDAT LFDAT FROM LIKP INTO TABLE T_LIKP
    FOR ALL ENTRIES IN T_VTTP
    WHERE VBELN = T_VTTP-VBELN.

*  if p_tpoper = '03'.
*    if sy-subrc is initial.
*      select vbelv posnv vbeln posnn vbtyp_n erdat vbtyp_v
*        from vbfa into table t_vbfa
*         for all entries in t_vttp
*       where vbelv   = t_vttp-vbeln
*         and vbtyp_n = 'i'
*         and vbtyp_v = 'J'.
*
*      select vbelv posnv vbeln posnn vbtyp_n erdat vbtyp_v
*        from vbfa appending table t_vbfa
*         for all entries in t_vttp
*       where vbelv   = t_vttp-vbeln
*         and vbtyp_n = 'R'
*         and vbtyp_v = '7'.
*    endif.
*  endif.

  IF ( NOT S_WERKS IS INITIAL )  AND ( NOT S_BUKRS IS INITIAL ).
    S_WERKS-OPTION = 'EQ'.
    S_WERKS-SIGN   = 'I'.
    SELECT * INTO TABLE IT_ZSDT_DEPARA_CEN FROM ZSDT_DEPARA_CEN
     WHERE VKORG EQ S_BUKRS
       AND CENTRO_REAL IN S_WERKS.

    LOOP AT IT_ZSDT_DEPARA_CEN.
      S_WERKS-OPTION = 'EQ'.
      S_WERKS-SIGN   = 'I'.
      S_WERKS-LOW    = IT_ZSDT_DEPARA_CEN-CENTROV_1.
      APPEND S_WERKS.
    ENDLOOP.
  ENDIF.

  IF ( S_WERKS IS INITIAL ) AND ( NOT S_BUKRS IS INITIAL ).

    S_WERKS-OPTION = 'EQ'.
    S_WERKS-SIGN   = 'I'.
    SELECT CENTROV_1 INTO S_WERKS-LOW FROM ZSDT_DEPARA_CEN
     WHERE VKORG EQ S_BUKRS.
      APPEND S_WERKS.
    ENDSELECT.

    SELECT BRANCH INTO S_WERKS-LOW FROM J_1BBRANCH
     WHERE BUKRS EQ S_BUKRS.
      APPEND S_WERKS.
    ENDSELECT.

  ENDIF.

  IF NOT T_LIKP[] IS INITIAL.

    SELECT VBELN POSNR MATNR WERKS VTWEG
           SPART BRGEW LGORT CHARG VGBEL VGPOS
      FROM LIPS
      INTO TABLE T_LIPS
      FOR ALL ENTRIES IN T_LIKP
      WHERE VBELN = T_LIKP-VBELN
        AND WERKS IN S_WERKS
        AND MATNR IN S_MATNR.

  ENDIF.

  IF NOT T_LIPS[] IS INITIAL.

    IF P_TPOPER = '04'.

      SELECT VBELN POSNR ERDAT KWMENG WERKS VSTEL
        FROM VBAP INTO TABLE T_VBAP
        FOR ALL ENTRIES IN T_LIPS
        WHERE VBELN = T_LIPS-VGBEL
          AND POSNR = T_LIPS-VGPOS.

      SELECT VBRP~VBELN VBRP~POSNR VBRP~VGBEL VBRP~VGPOS
             VBAP~WERKS VBAP~ERDAT VBAP~KWMENG
        FROM VBRP INNER JOIN VBAK ON
        ( VBAK~VGBEL = VBRP~VBELN )
                  INNER JOIN VBAP ON
        ( VBAK~VBELN = VBAP~VBELN )
        INTO TABLE T_VBRP
        FOR ALL ENTRIES IN T_LIPS
        WHERE VBRP~VGBEL = T_LIPS-VBELN AND
              VBRP~VGPOS = T_LIPS-POSNR AND VBRP~DRAFT = SPACE .

    ELSEIF P_TPOPER = '03'.

      SELECT EBELN EBELP MATNR WERKS
         FROM EKPO INTO TABLE T_EKPO
         FOR ALL ENTRIES IN T_LIPS
         WHERE EBELN = T_LIPS-VGBEL AND
               MATNR = T_LIPS-MATNR.
    ENDIF.
  ENDIF.
* Selecionar Dados MIRO
  SELECT BELNR GJAHR BUDAT TCODE RMWWR BUKRS
    FROM RBKP INTO TABLE T_RBKP
    FOR ALL ENTRIES IN T_032
    WHERE BELNR = T_032-BELNR AND
          GJAHR = T_032-GJAHR AND
          BUDAT IN S_BUDAT.

* MIRO - Conta Razão
  SELECT BELNR GJAHR BUZEI COBL_NR SAKNR WRBTR GSBER SGTXT
    FROM RBCO INTO TABLE T_RBCO
    FOR ALL ENTRIES IN T_032
    WHERE BELNR = T_032-BELNR AND
          GJAHR = T_032-GJAHR AND
*          buzei = 0           AND
          SHKZG = 'S'         AND
          SAKNR IN R_SAKNR.

* Selecionar Dados COKIPT
  SELECT TRANSPORTADOR POSTO LOTE CHVID CTAFRETE CONHECIMENTO VLR_CONFIRMADO
    FROM ZLEST0016 INTO TABLE T_016
     FOR ALL ENTRIES IN T_VTTK
   WHERE CTAFRETE = T_VTTK-EXTI2
     AND CONHECIMENTO = T_VTTK-EXTI1
     AND APR_CUSTO = SPACE.

  IF NOT T_016[] IS INITIAL.
    SELECT TRANSPORTADOR POSTO LOTE CHVID DIFER_TRANSP VLRPERDA
      FROM ZLEST0020 INTO TABLE T_020
      FOR ALL ENTRIES IN T_016
      WHERE TRANSPORTADOR = T_016-TRANSPORTADOR AND
            POSTO         = T_016-POSTO         AND
            LOTE          = T_016-LOTE          AND
            CHVID         = T_016-CHVID.

    SELECT TRANSPORTADOR POSTO LOTE
      FROM ZLEST0015 INTO TABLE T_015
      FOR ALL ENTRIES IN T_016
      WHERE TRANSPORTADOR = T_016-TRANSPORTADOR AND
            POSTO         = T_016-POSTO         AND
            LOTE          = T_016-LOTE          AND
            STATUS        = 'C'.

    IF SY-SUBRC = 0.
      SELECT TRANSPORTADOR POSTO LOTE CHVID
              TIPTRANSP CTLGLANCTO DOCSAP GJAHR BUKRS
        FROM ZLEST0022 INTO TABLE T_022
        FOR ALL ENTRIES IN T_016
        WHERE TRANSPORTADOR = T_016-TRANSPORTADOR AND
              POSTO         = T_016-POSTO         AND
              LOTE          = T_016-LOTE          AND
              CHVID         = T_016-CHVID.
      IF SY-SUBRC = 0.
        SELECT BUKRS BELNR GJAHR BUDAT
          FROM BKPF INTO TABLE T_BKPF
          FOR ALL ENTRIES IN T_022
          WHERE BUKRS = T_022-BUKRS AND
                BELNR = T_022-DOCSAP AND
                GJAHR = T_022-GJAHR.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM ORGANIZA_DADOS .

  SORT: T_VTTK  BY TKNUM,
        T_VTTP  BY TKNUM TPNUM,
        T_LIKP  BY VBELN,
*        t_vbfa  by vbelv,
        T_LIPS  BY VBELN POSNR,
        T_VBAP  BY VBELN POSNR,
        T_RBKP  BY BELNR GJAHR,
        T_TVTKT BY SHTYP.

  DATA: T_SADRVB  TYPE TABLE OF SADRVB INITIAL SIZE 0 WITH HEADER LINE,
        T_VBPAVB  TYPE TABLE OF VBPAVB INITIAL SIZE 0 WITH HEADER LINE,
        WA_DEPARA TYPE ZSDT_DEPARA_DEPO,
        WA_VBPAVB TYPE VBPAVB.

  DATA: WL_INDEX1 TYPE SY-TABIX,
        WL_INDEX2 TYPE SY-TABIX.
  REFRESH T_SAIDA.


  LOOP AT T_032.
    CLEAR: T_SAIDA, WL_INDEX1, WL_INDEX2.

    MOVE-CORRESPONDING T_032 TO T_SAIDA.

    READ TABLE T_RBKP WITH KEY BELNR = T_032-BELNR
                               GJAHR = T_032-GJAHR
                               BINARY SEARCH.

    LOOP AT T_RBCO WHERE BELNR = T_032-BELNR AND
                         GJAHR = T_032-GJAHR.

      IF SY-SUBRC = 0.
        IF T_RBKP-TCODE = ''.
          T_SAIDA-TCODE = 'COCKIPT'.
        ELSE.
          T_SAIDA-TCODE = T_RBKP-TCODE.
        ENDIF.
*        MOVE t_rbkp-rmwwr TO t_saida-wrbtr.
      ENDIF.
      MOVE T_RBCO-WRBTR TO T_SAIDA-WRBTR.
      READ TABLE T_VTTK WITH KEY TKNUM = T_032-TKNUM
                                 BINARY SEARCH.
      CHECK SY-SUBRC IS INITIAL.

      READ TABLE T_TVTKT WITH KEY SHTYP = T_VTTK-SHTYP BINARY SEARCH.
      MOVE T_TVTKT-BEZEI TO T_SAIDA-BEZEI.

      READ TABLE T_VTTP WITH KEY TKNUM = T_032-TKNUM
                                 BINARY SEARCH.
      CHECK SY-SUBRC IS INITIAL.
      WL_INDEX1 = SY-TABIX.

      LOOP AT T_VTTP FROM WL_INDEX1.
        IF T_VTTP-TKNUM NE T_VTTK-TKNUM.
          EXIT.
        ENDIF.
        MOVE-CORRESPONDING T_VTTK TO T_SAIDA.
        MOVE-CORRESPONDING T_VTTP TO T_SAIDA.

        READ TABLE T_LIKP WITH KEY VBELN = T_VTTP-VBELN
                                   BINARY SEARCH.

        READ TABLE T_LIPS WITH KEY VBELN = T_VTTP-VBELN
                                   BINARY SEARCH.
        CHECK SY-SUBRC IS INITIAL.
        WL_INDEX2 = SY-TABIX.
        LOOP AT T_LIPS FROM WL_INDEX2.

          IF T_LIPS-VBELN NE T_VTTP-VBELN.
            EXIT.
          ENDIF.
*RBCO-WRBTR, ZLEST0020-DIFER_TRANSP, ZLEST0020-VLRPERDA , ZLEST0016-VLR_CONFIRMADO
          MOVE-CORRESPONDING T_LIKP TO T_SAIDA.
          MOVE-CORRESPONDING T_LIPS TO T_SAIDA.


          CASE P_TPOPER.
            WHEN '03'.
*              read table t_vbfa with key vbelv = t_lips-vbeln
*                                         binary search.
*              if sy-subrc = 0.
*                case t_vbfa-vbtyp_v.
*                  when 'J'.
*                    move: t_vbfa-erdat  to t_saida-dtche.
*                  when '7'.
*                    move: t_vttk-datbg to t_saida-dtche.
*                endcase.
*              endif.

              MOVE: T_LIKP-LFDAT TO T_SAIDA-DTCHE.

              READ TABLE T_EKPO WITH KEY EBELN = T_LIPS-VGBEL
                                         MATNR = T_LIPS-MATNR.
              IF SY-SUBRC = 0.
                MOVE T_EKPO-WERKS  TO T_SAIDA-WERKS.
              ENDIF.


            WHEN '04'.
              MOVE: T_LIKP-ERDAT  TO T_SAIDA-DTCHE.

              READ TABLE T_VBRP WITH KEY VGBEL = T_LIPS-VBELN
                                         VGPOS = T_LIPS-POSNR.
              IF SY-SUBRC = 0.
                IF NOT T_VBRP-ERDAT IS INITIAL.
                  MOVE: T_VBRP-WERKS  TO T_SAIDA-WERKS.
                  MOVE: T_VBRP-KWMENG TO T_SAIDA-PESOC.
                ENDIF.
              ELSE.

                CLEAR: T_SAIDA-PESOC, T_SAIDA-WERKS.

                READ TABLE T_VBAP WITH KEY VBELN = T_LIPS-VGBEL
                                           POSNR = T_LIPS-VGPOS
                                           BINARY SEARCH.
                IF SY-SUBRC IS INITIAL.

                  CALL FUNCTION 'SD_PARTNER_READ'
                    EXPORTING
                      F_VBELN  = T_VBAP-VBELN
                      OBJECT   = 'VBPA'
                    TABLES
                      I_XVBADR = T_SADRVB
                      I_XVBPA  = T_VBPAVB.

                  DELETE T_VBPAVB WHERE PARVW NE 'Z1'.

                  IF NOT T_VBPAVB[] IS INITIAL.

                    READ TABLE T_VBPAVB INTO WA_VBPAVB INDEX 1.

*                    select single * into wa_depara
*                      from zsdt_depara_depo
*                     where werks eq t_vbap-vstel
*                       and lifnr eq wa_vbpavb-lifnr.

                    DATA(_OPERA) = 'RF'.
                    SELECT SINGLE *
                      FROM VBAK
                      INTO @DATA(W_VBAK)
                      WHERE VBELN = @T_LIPS-VGBEL.

                    IF  W_VBAK-AUART EQ 'ZIND'.
                      _OPERA = 'RI'.
                    ENDIF.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255 - INICIO
*                    CALL FUNCTION 'Z_BUSCA_DEPARA'
*                      EXPORTING
*                        I_WERKS          = T_VBAP-VSTEL
*                        I_LIFNR          = WA_VBPAVB-LIFNR
*                        I_OPERA          = _OPERA
*                      IMPORTING
*                        ZSDT_DEPARA_DEPO = WA_DEPARA.

                    ZCL_DEPARA_CENTRO_FIXO_VIRTUAL=>GET_DADOS_DEPARA(
                       EXPORTING
                         I_WERKS       = T_VBAP-VSTEL
                         I_LIFNR       =  WA_VBPAVB-LIFNR
                         I_OPERACAO    = _OPERA
                         "I_EUDR        =
                       IMPORTING
                        E_SINGLE_DEPARA          = WA_DEPARA ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255 - FIM

                    IF SY-SUBRC EQ 0.
                      MOVE: T_VBRP-KWMENG     TO T_SAIDA-PESOC.
                      MOVE: WA_DEPARA-WERKS_V TO T_SAIDA-WERKS.
                    ENDIF.

                    CLEAR: T_SAIDA-PESOC.

                  ENDIF.
                ELSE.
                  CLEAR: T_SAIDA-DTCHE, T_SAIDA-PESOC, T_SAIDA-WERKS.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.


          T_SAIDA-TPOPER = P_TPOPER.
          APPEND T_SAIDA.
        ENDLOOP. "T_LIPS.
      ENDLOOP. "T_VTTP

    ENDLOOP.                                                "T_032
  ENDLOOP.
ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*
FORM CHAMA_ALV .

  PERFORM: CRIA_CAMPOS,
           LAYOUT,
           SORT,
           ALV.

ENDFORM.                    " CHAMA_ALV
*&---------------------------------------------------------------------*
*&      Form  CRIA_CAMPOS
*&---------------------------------------------------------------------*
FORM CRIA_CAMPOS .

  PERFORM ADICIONA_CAMPO USING:
      'TPOPER' 'T_SAIDA'  'ZCOT0001'    'Tp.Oper.' ' ' ' ' 'X' ' ' ' ' '',
      'SHTYP'  'T_SAIDA'  'VTTK'        '' ' ' ' ' 'X' ' ' ' ' '',
      'BEZEI'  'T_SAIDA'  'TVTKT'       '' ' ' ' ' 'X' ' ' ' ' '',
      'DTMOV'  'T_SAIDA'  ''            'Data Movto Conh.' ' ' ' ' '' ' ' ' ' '',
      'DTCHE'  'T_SAIDA'  ''            'Data Chegada' ' ' ' ' ' ' ' ' ' ' '',
      'MATNR'  'T_SAIDA'  'LIPS'        '' ' ' ' ' ' ' ' ' ' ' '',
      'BRGEW'  'T_SAIDA'  'LIPS'        '' ' ' ' ' ' ' ' ' ' ' 'BRGEW',
      'PESOC'  'T_SAIDA'  ''            'Peso Chegada' ' ' ' ' ' ' ' ' ' ' 'BRGEW',
      'EXTI1'  'T_SAIDA'  'VTTK'        '' ' ' ' ' ' ' ' ' ' ' '',
      'TKNUM'  'T_SAIDA'  'VTTK'        '' ' ' ' ' ' ' 'X' ' ' '',
      'TPNUM'  'T_SAIDA'  'VTTP'        '' ' ' ' ' ' ' ' ' ' ' '',
      'VBELN'  'T_SAIDA'  'LIPS'        '' ' ' ' ' ' ' 'X' ' ' '',
      'POSNR'  'T_SAIDA'  'LIPS'        '' ' ' ' ' ' ' ' ' ' ' '',
      'FKNUM'  'T_SAIDA'  'ZLEST0032'   '' ' ' ' ' ' ' ' ' ' ' '',
      'EBELN'  'T_SAIDA'  'ZLEST0032'   '' ' ' ' ' ' ' 'X' ' ' '',
      'EBELP'  'T_SAIDA'  'ZLEST0032'   '' ' ' ' ' ' ' ' ' ' ' '',
      'BELNR'  'T_SAIDA'  'ZLEST0032'   '' ' ' ' ' ' ' 'X' ' ' '',
      'WERKS'  'T_SAIDA'  'LIPS'        '' ' ' ' ' ' ' ' ' ' ' '',
      'CHARG'  'T_SAIDA'  'LIPS'        '' ' ' ' ' ' ' ' ' ' ' '',
      'TCODE'  'T_SAIDA'  'RBKP'        '' ' ' ' ' ' ' ' ' ' ' '',
      'WRBTR'  'T_SAIDA'  'RBCO'        '' ' ' ' ' ' ' ' ' ' ' 'WRBTR'.

ENDFORM.                    " CRIA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  ADICIONA_CAMPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ADICIONA_CAMPO  USING
                 X_FIELD X_TAB X_REF X_TEXT X_SUM X_JUST X_KEY X_HOTSPOT X_OUTPUTLEN X_RFIELD.
  DATA: WL_TAM TYPE I.
  WL_TAM = STRLEN( X_TEXT ).
  WL_TAM = WL_TAM + 1.
*leoobsf
  CLEAR  FIELDCATALOG.


  FIELDCATALOG-FIELDNAME     = X_FIELD.
  FIELDCATALOG-TABNAME       = X_TAB.
  FIELDCATALOG-REF_TABNAME   = X_REF.
  FIELDCATALOG-REF_FIELDNAME = X_RFIELD.
  FIELDCATALOG-DO_SUM        = X_SUM.
  FIELDCATALOG-JUST          = X_JUST.
  FIELDCATALOG-KEY           = X_KEY.
  FIELDCATALOG-HOTSPOT       = X_HOTSPOT.
  FIELDCATALOG-SELTEXT_L     =
  FIELDCATALOG-SELTEXT_M     =
  FIELDCATALOG-SELTEXT_S     =
  FIELDCATALOG-REPTEXT_DDIC  = X_TEXT.
  IF WL_TAM > X_OUTPUTLEN.
    FIELDCATALOG-OUTPUTLEN     =   WL_TAM.
  ELSE.
    FIELDCATALOG-OUTPUTLEN     = X_OUTPUTLEN.
  ENDIF.

  APPEND FIELDCATALOG.
  CLEAR FIELDCATALOG.

ENDFORM.                    " ADICIONA_CAMPO
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
FORM LAYOUT .

  GD_LAYOUT-NO_INPUT          = 'X'.
  GD_LAYOUT-ZEBRA             = 'X'.
  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GD_LAYOUT-BOX_FIELDNAME     = 'SEL'.
  GD_LAYOUT-BOX_TABNAME       = 'T_SAIDA'.
  GD_LAYOUT-WINDOW_TITLEBAR   = 'Apropriação de Custo de Frete'.
  GD_LAYOUT-DETAIL_TITLEBAR   = 'Apropriação de Custo de Frete'.

ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
FORM ALV .

  GD_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = GD_REPID
      I_CALLBACK_USER_COMMAND  = 'COMANDO'
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS'
      IS_LAYOUT                = GD_LAYOUT
      IT_FIELDCAT              = FIELDCATALOG[]
      IT_SORT                  = SORT[]
      I_SAVE                   = 'A'
    TABLES
      T_OUTTAB                 = T_SAIDA
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

ENDFORM.                    " ALV
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM COMANDO USING L_UCOMM
                          L_SELFIELD TYPE SLIS_SELFIELD.
*  PERFORM f_retrieve_changed_data.
*  CLEAR: w_leave, w_exit.


  CASE L_UCOMM.
    WHEN 'CONT'.      PERFORM LANC_CONTABIL.
    WHEN 'VOLTA' OR 'CANC' OR 'EXIT'. L_SELFIELD-EXIT = 'X'.
    WHEN OTHERS.

      CASE L_SELFIELD-SEL_TAB_FIELD.
* Doc Transporte
        WHEN 'T_SAIDA-TKNUM'.
          SET PARAMETER ID 'TNR' FIELD L_SELFIELD-VALUE.
          CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
* Doc Compras
        WHEN 'T_SAIDA-EBELN'.
          SET PARAMETER ID 'BES' FIELD L_SELFIELD-VALUE.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
* Remassa
        WHEN 'T_SAIDA-VBELN'.
          SET PARAMETER ID 'VL' FIELD L_SELFIELD-VALUE.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
* Mir
        WHEN 'T_SAIDA-BELNR'.
          READ TABLE T_SAIDA INDEX L_SELFIELD-TABINDEX.

          SET PARAMETER ID 'RBN' FIELD L_SELFIELD-VALUE.
          SET PARAMETER ID 'GJR' FIELD T_SAIDA-DTMOV(4).
          CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.
  L_SELFIELD-COL_STABLE   = 'X'.
  L_SELFIELD-ROW_STABLE   = 'X'.
  L_SELFIELD-REFRESH      = 'X'.
  L_SELFIELD-AFTER_ACTION = 'X'.
*  l_selfield-exit         = w_exit.
ENDFORM.                                                    "COMANDO
*---------------------------------------------------------------------*
*       FORM SET_PF_STATUS                                            *
*---------------------------------------------------------------------*
FORM PF_STATUS USING EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'ALV' EXCLUDING EXTAB.

ENDFORM.                    "pf_status
*&---------------------------------------------------------------------*
*&      Form  SORT
*&---------------------------------------------------------------------*
FORM SORT .

  REFRESH SORT.

  PERFORM Z_ORGANIZA USING 1 'SHTYP'            'VTTK'  ' '.
  PERFORM Z_ORGANIZA USING 2 'BEZEI'            'TVTKT' ' '.
  PERFORM Z_ORGANIZA USING 3 'TKNUM'            'VTTK'  ' '.

ENDFORM.                    " SORT


*&---------------------------------------------------------------------*
*&      Form  z_organiza
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NUM      text
*      -->P_FIELD    text
*      -->P_TABLE    text
*      -->P_SUBTOT   text
*----------------------------------------------------------------------*
FORM Z_ORGANIZA  USING    P_NUM
                          P_FIELD
                          P_TABLE
                          P_SUBTOT.
  SORT-SPOS = P_NUM.
  SORT-FIELDNAME = P_FIELD.
  SORT-TABNAME = P_TABLE.
  SORT-GROUP = 'C'.
  SORT-SUBTOT = 'X'.
  APPEND SORT.

ENDFORM.                    "z_organiza
*&---------------------------------------------------------------------*
*&      Form  LANC_CONTABIL
*&---------------------------------------------------------------------*
FORM LANC_CONTABIL .


  T_SAIDA2[] = T_SAIDA[].

  DELETE T_SAIDA2 WHERE SEL IS INITIAL.


  SORT T_SAIDA2 BY TKNUM.
  DELETE ADJACENT DUPLICATES FROM T_SAIDA2 COMPARING TKNUM.


  LOOP AT T_SAIDA2 INTO T_SAIDA.

    IF P_TPOPER = '03' OR P_TPOPER = '04'.
*    DELETE t_saida2 WHERE dtche IS INITIAL.
      IF T_SAIDA-DTCHE IS INITIAL.
        MESSAGE W398(00) WITH 'TP 03/04 não executa MR22 sem DT chegada.'.
        CONTINUE.
      ENDIF.
    ENDIF.

    CASE P_TPOPER.
      WHEN '03' OR '04'.
        PERFORM BI_MR22.
      WHEN '01'.
        PERFORM BI_F02.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " LANC_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  BI_MR22
*&---------------------------------------------------------------------*
FORM BI_MR22 .

  DATA: WL_NUMERADOR TYPE ZCOT0003-NUMERADOR.
  DATA: VL_ID(3).
  DATA: P_DATA_ENT TYPE DATUM,
        P_DATA_VAL TYPE DATUM,
        DIA(2)     TYPE C,
        MES(2)     TYPE C,
        ANO(4)     TYPE C,
        DTA(10)    TYPE C.

  VL_ID = 'ZCO'.

  CLEAR: T_003, WL_NUMERADOR. REFRESH T_003.

  CASE P_TPOPER.
    WHEN '03'.
      READ TABLE T_LIPS WITH KEY VBELN = T_SAIDA-VBELN
                                  MATNR = T_SAIDA-MATNR.
      IF SY-SUBRC = 0.
        SELECT SINGLE WERKS FROM EKPO INTO T_SAIDA-WERKS
          WHERE EBELN = T_LIPS-VGBEL AND
                MATNR = T_LIPS-MATNR.
      ENDIF.

    WHEN '04'.
*      READ TABLE t_lips WITH KEY vbeln = t_saida-vbeln
*                                  matnr = t_saida-matnr.
*      IF sy-subrc = 0.
*        SELECT SINGLE werks FROM vbap INTO t_saida-werks
*          WHERE vbeln = t_lips-vgbel AND
*                posnr = t_lips-vgpos.
*      ENDIF.


    WHEN OTHERS.
  ENDCASE.

  DATA: BEGIN OF T_T001B OCCURS 0.
          INCLUDE STRUCTURE T001B.
  DATA: END OF T_T001B.

  IF S_BUKRS IS INITIAL.
    SELECT * FROM T001B INTO TABLE T_T001B.
  ELSE.
    SELECT * FROM T001B INTO TABLE T_T001B
      WHERE BUKRS EQ S_BUKRS.
  ENDIF.

  SORT T_T001B BY BUKRS TOYE1.
  DELETE ADJACENT DUPLICATES FROM T_T001B COMPARING BUKRS TOYE1.

  LOOP AT T_RBCO WHERE BELNR = T_SAIDA-BELNR AND
                       GJAHR = T_SAIDA-GJAHR.

    EXPORT VL_ID TO MEMORY ID 'ZCO'.

    CLEAR: BDCDATA, MESSTAB. REFRESH: BDCDATA, MESSTAB.

    DATA: WL_DATA(10).

    IF T_SAIDA-TCODE = 'MIRO'.
      T_SAIDA-XBLNR = T_SAIDA-BELNR.
      READ TABLE T_RBKP WITH KEY BELNR = T_SAIDA-BELNR
                                 GJAHR = T_SAIDA-GJAHR.
      "IF ( p_tpoper EQ '03' ) AND ( p_tpoper EQ '04' ).
      T_SAIDA-BUDAT = T_SAIDA-DTCHE.
      "ELSE.
      "  t_saida-budat = t_rbkp-budat.
      "ENDIF.
      T_SAIDA-BUKRS = T_RBKP-BUKRS.
    ENDIF.

    CLEAR: P_DATA_ENT,
           P_DATA_VAL.

    P_DATA_ENT = T_SAIDA-DTCHE .

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        P_DATA_ENT     = P_DATA_ENT
        P_BUKRS        = T_SAIDA-BUKRS
        P_VAL_FI       = 'X'
        P_VAL_MM       = 'X'
      IMPORTING
        P_DATA_VAL     = P_DATA_VAL
      EXCEPTIONS
        DATA_FI_MM_NAO = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR: ANO, MES, DIA, DTA.

    ANO = P_DATA_VAL(4).
    MES = P_DATA_VAL+4(2).
    DIA = P_DATA_VAL+6(2).

    CONCATENATE DIA '.' MES '.' ANO INTO DTA.

    WL_DATA = DTA.


*    READ TABLE t_t001b WITH KEY bukrs = t_saida-bukrs
*                                toye1 = t_saida-dtche(4) BINARY SEARCH.
*    IF sy-subrc = 0.
*     IF ( t_t001b-frpe1 LE t_saida-dtche+4(2) AND
*           t_t001b-tope1 GE t_saida-dtche+4(2) ) AND
*           t_t001b-toye1 EQ t_saida-dtche(4).
*        WRITE t_saida-dtche TO wl_data.
*      ELSE.
*        t_saida-dtche+4(2) = t_t001b-tope1.
*        t_saida-dtche+6(2) = '01'.
*        WRITE t_saida-dtche TO wl_data.
*      ENDIF.
*    ELSE.
*      SORT t_t001b BY bukrs mkoar.
*      READ TABLE t_t001b WITH KEY bukrs = t_saida-bukrs
*                                  mkoar = c_m BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        CONCATENATE '01' t_t001b-frpe1+1 t_t001b-frye1 INTO wl_data.
*      ELSE.
*        WRITE t_saida-budat TO wl_data.
*      ENDIF.
*    ENDIF.

    WRITE: T_RBCO-WRBTR TO T_SAIDA-ZUUMB.


    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'MR21HEAD-BKTXT'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM BDC_FIELD       USING 'MR21HEAD-BUDAT'
                                  WL_DATA.
    PERFORM BDC_FIELD       USING 'MR21HEAD-BUKRS'
                                  T_SAIDA-BUKRS.
    PERFORM BDC_FIELD       USING 'MR21HEAD-WERKS'
                                  T_SAIDA-WERKS.
    PERFORM BDC_FIELD       USING 'MR21HEAD-XBLNR'
                                  T_SAIDA-XBLNR.
    PERFORM BDC_FIELD       USING 'MR21HEAD-BKTXT'
                                  T_SAIDA-TKNUM.
*perform bdc_field       using 'MR21HEAD-SCREEN_VARIANT'
*                              t_saida-SCREEN_VARIANT_006.


    WRITE: T_RBCO-WRBTR TO T_SAIDA-ZUUMB.
    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'CKI_MR22_0250-ZUUMB(01)'.
    PERFORM BDC_FIELD       USING 'CKI_MR22_0250-MATNR(01)'
                                  T_SAIDA-MATNR.
    PERFORM BDC_FIELD       USING 'CKI_MR22_0250-ZUUMB(01)'
                                  T_SAIDA-ZUUMB.




    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=TAB2'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'CKI_MR22_0250-MATNR(02)'.
*perform bdc_field       using 'MR21HEAD-SCREEN_VARIANT'
*                              record-SCREEN_VARIANT_010.



    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=CALC'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'CKI_MR22_0250-MATNR(01)'.
*perform bdc_field       using 'MR21HEAD-SCREEN_VARIANT'
*                              record-SCREEN_VARIANT_011.
    PERFORM BDC_FIELD       USING 'CKI_MR22_0250-SELKZ(01)'
                                  'X'.


    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0400'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  '%#AUTOTEXT001'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM BDC_FIELD       USING 'DISPLAY-F_CURR1-SELKZ'
                                  'X'.



    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=TAB3'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'CKI_MR22_0250-MATNR(02)'.
*perform bdc_field       using 'MR21HEAD-SCREEN_VARIANT'
*                              record-SCREEN_VARIANT_014.


    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=CALC'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'CKI_MR22_0250-MATNR(01)'.
*perform bdc_field       using 'MR21HEAD-SCREEN_VARIANT'
*                              record-SCREEN_VARIANT_015.
    PERFORM BDC_FIELD       USING 'CKI_MR22_0250-SELKZ(01)'
                                  'X'.


    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0400'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  '%#AUTOTEXT001'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM BDC_FIELD       USING 'DISPLAY-F_CURR1-SELKZ'
                                  'X'.



    PERFORM BDC_DYNPRO      USING 'SAPRCKM_MR22' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=SAVE'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'CKI_MR22_0250-MATNR(02)'.


*    PERFORM bdc_dynpro      USING 'SAPRCKM_MR22' '0201'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=SAVE'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'CKI_MR22_0250-MATNR(02)'.



    CALL TRANSACTION 'MR22'
            USING  BDCDATA
            MODE   'N' "v_mode
            UPDATE 'S'
            MESSAGES INTO MESSTAB.

    IF SY-SUBRC = 0.

      ADD 1 TO WL_NUMERADOR.
      MOVE-CORRESPONDING T_SAIDA TO T_003.
      READ TABLE MESSTAB WITH KEY MSGTYP = 'S'
                                  MSGNR = '019'.
      IF SY-SUBRC = 0.
        T_003-DOC_APROPR = MESSTAB-MSGV1.
      ENDIF.
      T_003-NUMERADOR = WL_NUMERADOR.
      T_003-USNAM = SY-UNAME.
      T_003-DT_ATUAL = SY-DATUM.
      T_003-HORA_ATUL = SY-UZEIT.
      T_003-VLR_APROPR = T_RBCO-WRBTR.
      T_003-PESO_BRUTO = T_SAIDA-BRGEW.
      T_003-DT_MOV     = T_SAIDA-DTMOV.
      T_003-DT_CHEGADA = T_SAIDA-DTCHE.
      T_003-PESO_CHEGADA = T_SAIDA-PESOC.

      APPEND T_003. CLEAR T_003.
      DELETE T_SAIDA WHERE TKNUM = T_SAIDA-TKNUM.

      UPDATE ZLEST0032 SET STATUS1 = 'X'
                     WHERE TKNUM   = T_SAIDA-TKNUM.
    ENDIF.

  ENDLOOP.

  IF NOT T_003[] IS INITIAL.
    MODIFY ZCOT0003  FROM TABLE T_003.
    COMMIT WORK AND WAIT.
  ENDIF.


ENDFORM.                                                    " BI_MR22
*&---------------------------------------------------------------------*
*&      Form  BI_F02
*&---------------------------------------------------------------------*
FORM BI_F02 .

  CLEAR T_003. REFRESH T_003.

  DATA: WL_NUM(9) TYPE N.
  DATA: WL_CONT TYPE N.
  DATA: WL_SEQITEM TYPE ZIB_CONTABIL-SEQITEM.
  CLEAR: ZIB_CONTABIL, WL_SEQITEM.

  READ TABLE T_001 WITH KEY SHTYP = T_SAIDA-SHTYP.

  IF T_SAIDA-TCODE = 'MIRO'.
    T_SAIDA-XBLNR = T_SAIDA-BELNR.
    READ TABLE T_RBKP WITH KEY BELNR = T_SAIDA-BELNR
                               GJAHR = T_SAIDA-GJAHR.
    T_SAIDA-BUDAT = T_RBKP-BUDAT.
    T_SAIDA-BUKRS = T_RBKP-BUKRS.
  ENDIF.


  SELECT SINGLE MAX( OBJ_KEY ) FROM ZCOT0004 INTO ZCOT0004-OBJ_KEY.
*    WHERE obj_key LIKE 'CO'.
  IF NOT ZCOT0004-OBJ_KEY IS INITIAL.
    WL_NUM = ZCOT0004-OBJ_KEY+2(9).
    ADD 1 TO WL_NUM .
    ZCOT0004-OBJ_KEY+2(9) = WL_NUM.
    INSERT ZCOT0004 FROM ZCOT0004.
  ELSE.
    WL_NUM = 1.
    ZCOT0004-OBJ_KEY(2) = 'CO'.
    ZCOT0004-OBJ_KEY+2(9) = WL_NUM.
    ZCOT0004-OBJ_KEY+11(4) = T_SAIDA-BUDAT(4).
    INSERT ZCOT0004 FROM ZCOT0004.
  ENDIF.

  CLEAR WL_SEQITEM.
  LOOP AT T_RBCO WHERE BELNR = T_SAIDA-BELNR AND
                       GJAHR = T_SAIDA-GJAHR.
    CLEAR WL_CONT.


    DO 2 TIMES.
      ADD 1 TO WL_CONT.
      ADD 1 TO WL_SEQITEM.
** 1ª - Partida Débito
      ZIB_CONTABIL-MANDT   = SY-MANDT.
      ZIB_CONTABIL-OBJ_KEY = ZCOT0004-OBJ_KEY.
      ZIB_CONTABIL-SEQITEM = WL_SEQITEM.
      IF WL_CONT = 1.
        ZIB_CONTABIL-BSCHL =  '40'.
      ELSE.
        ZIB_CONTABIL-BSCHL =  '50'.
      ENDIF.
      ZIB_CONTABIL-GSBER =  T_SAIDA-WERKS.
      ZIB_CONTABIL-BUKRS =  T_SAIDA-BUKRS.
      ZIB_CONTABIL-INTERFACE =  '3'.
      ZIB_CONTABIL-BKTXT = T_SAIDA-TKNUM.

*      zib_contabil-bldat = t_saida-budat.
      WRITE T_SAIDA-BUDAT TO ZIB_CONTABIL-BLDAT.
*  ZIB_CONTABIL-BLDAT = SE rbkp-tcode=”miro”pegar O valor DO campo RBKP-BUDAT, se for cockipt pegar o valor campo bkpf-budat
*      zib_contabil-budat = t_saida-budat.
      WRITE T_SAIDA-BUDAT TO ZIB_CONTABIL-BUDAT .
*  zib_contabil-budat = se rbkp-tcode=”miro”pegar o valor do campo rbkp-budat, se for cockipt pegar o valor campo bkpf-budat

      ZIB_CONTABIL-GJAHR = T_SAIDA-BUDAT(4).
*  zib_contabil-gjahr = ano da data da criação (zib_contabil-budat)


      ZIB_CONTABIL-MONAT = T_SAIDA-BUDAT+4(2).
*  zib_contabil-monat = mês da data da criação (zib_contabil-budat)

      ZIB_CONTABIL-BLART = 'FA'.
      ZIB_CONTABIL-XBLNR = T_SAIDA-BELNR.
*  zib_contabil-xblnr =  se RBKP-TCODE=”MIRO”PEGAR o VALOR do CAMPO zlest0032-belnr , se for cockipt pegar o valor campo zlest0022-docsap

      IF WL_CONT = 1.
        ZIB_CONTABIL-HKONT =  T_001-SAKNR.
        CLEAR ZIB_CONTABIL-KOSTL.
        SELECT SINGLE KOSTL FROM ZCOT0005 INTO ZIB_CONTABIL-KOSTL
          WHERE MATNR = T_SAIDA-MATNR AND
                WERKS = T_SAIDA-WERKS.

*        zib_contabil-kostl  = '0010110184'.
      ELSE.
        ZIB_CONTABIL-HKONT =  T_RBCO-SAKNR.
        ZIB_CONTABIL-KOSTL  = ''.
      ENDIF.


      ZIB_CONTABIL-WRBTR = T_RBCO-WRBTR."t_saida-belnr.
*  zib_contabil-wrbtr=  se rbkp-tcode=”miro”pegar o valor do campo rbco-wrbtr, se for cockipt pegar o valor campo zlest0016-vlr_confirmado ou zlest0020-difer_transp ou zlest0020-vlrperda

      ZIB_CONTABIL-WAERS_I = ZIB_CONTABIL-WAERS =  'BRL'.
      ZIB_CONTABIL-ZFBDT = ''.
      ZIB_CONTABIL-ZLSPR  = ''.
      ZIB_CONTABIL-ZLSCH  = ''.
      ZIB_CONTABIL-KIDNO  = ''.

*      zib_contabil-sgtxt = t_saida-tknum.
      CONCATENATE 'Apropriação de frete' T_SAIDA-TKNUM  INTO ZIB_CONTABIL-SGTXT SEPARATED BY SPACE.
*  zib_contabil-sgtxt=  apropriação de custo de frete  + zlest0032-tknum

      ZIB_CONTABIL-XREF1  = ''.
      ZIB_CONTABIL-XREF2  = ''.
      ZIB_CONTABIL-XREF3  = ''.
      ZIB_CONTABIL-BUPLA = T_SAIDA-TPLST.
      ZIB_CONTABIL-ZUONR  = ''.
      ZIB_CONTABIL-UMSKZ  = ''.

      ZIB_CONTABIL-AUFNR  = ''.
      ZIB_CONTABIL-PRCTR  = ''.
*      zib_contabil-waers_i  = ''.

      ZIB_CONTABIL-DMBTR = T_RBCO-WRBTR.
*  zib_contabil-dmbtr=  se rbkp-tcode=”miro”pegar o valor do campo rbco-wrbtr, se for cockipt pegar o valor campo zlest0016-vlr_confirmado ou zlest0020-difer_transp ou zlest0020-vlrperda

      ZIB_CONTABIL-WAERS_F  = ''.
      ZIB_CONTABIL-DMBE2  = ''.
      ZIB_CONTABIL-BVTYP  = ''.
      ZIB_CONTABIL-HBKID  = ''.
      ZIB_CONTABIL-RG_ATUALIZADO = 'N'.
      ZIB_CONTABIL-BANKL  = ''.
      ZIB_CONTABIL-BANKN = ''.

      INSERT ZIB_CONTABIL.
      CLEAR ZIB_CONTABIL.
    ENDDO.

    MOVE-CORRESPONDING T_SAIDA TO T_003.
    T_003-NUMERADOR = WL_SEQITEM.
    T_003-USNAM = SY-UNAME.
    T_003-DT_ATUAL = SY-DATUM.
    T_003-HORA_ATUL = SY-UZEIT.
    T_003-VLR_APROPR = T_RBCO-WRBTR.
    T_003-PESO_BRUTO = T_SAIDA-BRGEW.
    T_003-DT_MOV     = T_SAIDA-DTMOV.
    T_003-DT_CHEGADA = T_SAIDA-DTCHE.
    T_003-PESO_CHEGADA = T_SAIDA-PESOC.

    APPEND T_003. CLEAR T_003.

  ENDLOOP.

  DATA WL_OBJ LIKE ZIB_CONTABIL-OBJ_KEY.


*  DO 32 TIMES.
*    WAIT UP TO 2 SECONDS.
*
*    SELECT SINGLE obj_key FROM zib_contabil INTO wl_obj
*      WHERE  obj_key = zcot0004-obj_key AND
*              rg_atualizado = 'S'.
*    IF sy-subrc = 0.
*
*      SELECT SINGLE obj_key FROM zib_contabil_err INTO wl_obj
*        WHERE obj_key = zcot0004-obj_key.
*      IF sy-subrc EQ 0.
*        CLEAR t_003. REFRESH t_003.
*      ELSE.
*
*        DATA: wl_awkey LIKE bkpf-awkey,
*              wl_belnr LIKE bkpf-belnr.
*
*        wl_awkey = zcot0004-obj_key.
*        SELECT SINGLE belnr INTO wl_belnr FROM bkpf WHERE
*          bukrs IN s_bukrs          AND
*          gjahr EQ t_saida-budat(4) AND
*          awkey = wl_awkey.
*
  LOOP AT T_003.
    T_003-NUMERADOR = SY-TABIX.
*    t_003-doc_apropr = wl_belnr.
    MODIFY T_003.
  ENDLOOP.

  DELETE T_SAIDA WHERE TKNUM = T_SAIDA-TKNUM.

  UPDATE ZLEST0032 SET STATUS1 = 'X'
                 WHERE TKNUM   = T_SAIDA-TKNUM.
*      ENDIF.
*      EXIT.
*    ENDIF.
*  ENDDO.
  IF NOT T_003[] IS INITIAL.
    MODIFY ZCOT0003  FROM TABLE T_003.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                                                    " BI_F02

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "bdc_dynpro

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "bdc_field
