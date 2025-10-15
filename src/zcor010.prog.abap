*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 20/08/2012                                              &*
*& Descrição: Ajuste Gerencial do Valor do Dólar Histórico – Centro   &*
*&            Fixo e A Fixar                                          &*
*& Transação: ZCO0015                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  ZCOR010.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.


TYPES: BEGIN OF TY_BKPF,
        BUKRS          TYPE BKPF-BUKRS,
        GJAHR          TYPE BKPF-GJAHR,
        BUDAT          TYPE BKPF-BUDAT,
        BLART          TYPE BKPF-BLART,
        TCODE          TYPE BKPF-TCODE,
        BKTXT          TYPE BKPF-BKTXT,
        BELNR          TYPE BKPF-BELNR,
        AWKEY          TYPE BKPF-AWKEY,
        CH_REFERENCIA  TYPE ZMMT0006-CH_REFERENCIA,
       END OF TY_BKPF,

       BEGIN OF TY_0006,
        CH_REFERENCIA   TYPE ZMMT0006-CH_REFERENCIA,
        TCODE           TYPE ZMMT0006-TCODE,
        BUKRS           TYPE ZMMT0006-BUKRS,
        MATNR           TYPE ZMMT0006-MATNR,
        BUDAT           TYPE ZMMT0006-BUDAT,
        VR_MATERIAL_USD TYPE ZMMT0006-VR_MATERIAL_USD,
        WERKS           TYPE ZMMT0006-WERKS,
        WERKS_D         TYPE ZMMT0006-WERKS_D,
        ERFMG           TYPE ZMMT0006-ERFMG,
        STATUS          TYPE ZMMT0006-STATUS,
       END OF TY_0006,

       BEGIN OF TY_MAKT,
        MATNR   TYPE MAKT-MATNR,
        MAKTX   TYPE MAKT-MAKTX,
       END OF TY_MAKT,

       BEGIN OF TY_BSIS,
         BELNR TYPE BSIS-BELNR,
         GJAHR TYPE BSIS-GJAHR,
         BUKRS TYPE BSIS-BUKRS,
         WERKS TYPE BSIS-WERKS,
         DMBE2 TYPE BSIS-DMBE2,
         SHKZG TYPE BSIS-SHKZG,
       END OF TY_BSIS,

       BEGIN OF TY_SAIDA,
         MARK,
*         belnr           TYPE BKPF-belnr,
         CH_REFERENCIA   TYPE ZMMT0006-CH_REFERENCIA,
         TCODE           TYPE ZMMT0006-TCODE,
         MATNR           TYPE ZMMT0006-MATNR,
         MAKTX           TYPE MAKT-MAKTX,
         WERKS           TYPE ZMMT0006-WERKS,
         WERKS_D         TYPE ZMMT0006-WERKS_D,
         BUDAT           TYPE ZMMT0006-BUDAT,
         ERFMG           TYPE ZMMT0006-ERFMG,
         VR_MATERIAL_USD TYPE ZMMT0006-VR_MATERIAL_USD,
         AWKEY           TYPE BKPF-AWKEY,
         BELNR           TYPE BKPF-BELNR,
         VLR_CONTABIL    TYPE BSIS-DMBE2,
         VLR_AJUSTE      TYPE BSIS-DMBE2,
         STATUS      TYPE ZMMT0006-STATUS,
       END OF TY_SAIDA,

       BEGIN OF TY_SAIDA_EXEC,
         WERKS     TYPE ZMMT0006-WERKS,
         MATNR     TYPE ZMMT0006-MATNR,
         VLR_AJUST TYPE BSIS-DMBE2,
         MSG(255),
       END OF TY_SAIDA_EXEC.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELA INTERNAS
*----------------------------------------------------------------------*
DATA: TG_BKPF        TYPE TABLE OF TY_BKPF,
      TG_0006        TYPE TABLE OF ZMMT0006,                "TY_0006,
      TG_MAKT        TYPE TABLE OF TY_MAKT,
      TG_BSIS        TYPE TABLE OF BSIS, "ty_bsis,
      TG_SAIDA       TYPE TABLE OF TY_SAIDA,
      TG_SAIDA_EXEC  TYPE TABLE OF TY_SAIDA_EXEC,
      TG_MSG         TYPE TABLE OF BDCMSGCOLL,
      TG_BDC         TYPE TABLE OF BDCDATA.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WG_BKPF        TYPE TY_BKPF,
      WG_0006        TYPE ZMMT0006,                         "TY_0006,
      WG_MAKT        TYPE TY_MAKT,
      WG_BSIS        TYPE BSIS, "ty_bsis,
      WG_SAIDA       TYPE TY_SAIDA,
      WG_SAIDA_EXEC  TYPE TY_SAIDA_EXEC,
      WG_MSG         TYPE BDCMSGCOLL,
      WA_BDC         TYPE BDCDATA.

*----------------------------------------------------------------------*
* VARIAVEIS
*----------------------------------------------------------------------*
DATA: WG_STATUS.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      T_SORT       TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.
*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR WG_BKPF-BUKRS NO-EXTENSION NO INTERVALS OBLIGATORY,
                S_BUDAT FOR WG_BKPF-BUDAT NO-EXTENSION OBLIGATORY.

PARAMETERS: P_DATA TYPE TY_BKPF-BUDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM INICIAR_VARIAVES.
  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZACAO_DADOS.
  PERFORM IMPRIMIR_DADOS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS.

  SELECT BUKRS GJAHR BUDAT BLART TCODE BKTXT BELNR AWKEY BKTXT
    FROM BKPF
    INTO TABLE TG_BKPF
     WHERE BUKRS IN S_BUKRS
       AND BUDAT IN S_BUDAT
       AND BLART EQ 'WA'
       AND TCODE EQ 'MB11'.

  LOOP AT S_BUDAT.
    IF S_BUDAT-HIGH IS NOT INITIAL.
      DELETE TG_BKPF WHERE GJAHR NE S_BUDAT-LOW(4)
                       AND GJAHR NE S_BUDAT-HIGH(4).
    ELSE.
      DELETE TG_BKPF WHERE GJAHR NE S_BUDAT-LOW(4).
    ENDIF.
  ENDLOOP.
  IF TG_BKPF[] IS NOT INITIAL.
    SELECT * "CH_REFERENCIA TCODE BUKRS MATNR BUDAT VR_MATERIAL_USD WERKS WERKS_D ERFMG
      FROM ZMMT0006
      INTO TABLE TG_0006
       FOR ALL ENTRIES IN TG_BKPF
        WHERE CH_REFERENCIA EQ TG_BKPF-CH_REFERENCIA
          AND TCODE         EQ 'MB11'
          AND STATUS        IN (SPACE, 1).

    IF SY-SUBRC IS INITIAL.
      LOOP AT TG_0006 INTO WG_0006.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT  = WG_0006-MATNR
          IMPORTING
            OUTPUT = WG_0006-MATNR.

        MODIFY TG_0006 FROM WG_0006.
      ENDLOOP.

      SELECT MATNR MAKTX
        FROM MAKT
        INTO TABLE TG_MAKT
         FOR ALL ENTRIES IN TG_0006
          WHERE MATNR EQ TG_0006-MATNR.

    ENDIF.

    SELECT * "belnr gjahr bukrs werks dmbe2 shkzg
      FROM BSIS
      INTO TABLE TG_BSIS
       FOR ALL ENTRIES IN TG_BKPF
        WHERE BELNR EQ TG_BKPF-BELNR
          AND GJAHR EQ TG_BKPF-GJAHR
          AND BUKRS EQ TG_BKPF-BUKRS.
*           AND WERKS EQ TG_BKPF-WERKS.


  ENDIF.


ENDFORM.                    " SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZACAO_DADOS.
  SORT: TG_0006 BY CH_REFERENCIA,
        TG_MAKT BY MATNR,
        TG_BSIS BY BELNR GJAHR BUKRS WERKS.

  LOOP AT TG_BKPF INTO WG_BKPF.
    READ TABLE TG_0006 INTO WG_0006
      WITH KEY CH_REFERENCIA = WG_BKPF-CH_REFERENCIA
                       BINARY SEARCH.

    IF SY-SUBRC IS INITIAL.
      READ TABLE TG_MAKT INTO WG_MAKT
        WITH KEY MATNR = WG_0006-MATNR
                 BINARY SEARCH.

      LOOP AT TG_BSIS INTO WG_BSIS
         WHERE BELNR EQ WG_BKPF-BELNR
           AND GJAHR EQ WG_BKPF-GJAHR
           AND BUKRS EQ WG_BKPF-BUKRS
           AND WERKS EQ WG_0006-WERKS.

        IF WG_BSIS-SHKZG EQ 'S'.
          ADD WG_BSIS-DMBE2 TO WG_SAIDA-VLR_CONTABIL.
        ELSEIF WG_BSIS-SHKZG EQ 'H'.
          SUBTRACT WG_BSIS-DMBE2 FROM WG_SAIDA-VLR_CONTABIL.
        ENDIF.
      ENDLOOP.

      IF WG_SAIDA-VLR_CONTABIL LT 0.
        MULTIPLY WG_SAIDA-VLR_CONTABIL BY -1.
      ENDIF.

*      if sy-subrc is initial.
*      WG_SAIDA-BELNR            = WG_BKPF-BELNR.
      WG_SAIDA-CH_REFERENCIA    = WG_0006-CH_REFERENCIA.
      WG_SAIDA-TCODE            = WG_0006-TCODE.
      WG_SAIDA-MATNR            = WG_0006-MATNR.
      WG_SAIDA-MAKTX            = WG_MAKT-MAKTX.
      WG_SAIDA-WERKS            = WG_0006-WERKS.
      WG_SAIDA-WERKS_D          = WG_0006-WERKS_D.
      WG_SAIDA-BUDAT            = WG_0006-BUDAT.
      WG_SAIDA-ERFMG            = WG_0006-ERFMG.
      WG_SAIDA-VR_MATERIAL_USD  = WG_0006-VR_MATERIAL_USD.
      WG_SAIDA-AWKEY            = WG_BKPF-AWKEY(10).
      WG_SAIDA-BELNR            = WG_BKPF-BELNR.
      WG_SAIDA-VLR_AJUSTE       = WG_SAIDA-VR_MATERIAL_USD - WG_SAIDA-VLR_CONTABIL.

      APPEND WG_SAIDA TO TG_SAIDA.
      CLEAR: WG_SAIDA.
*      endif.
    ENDIF.


  ENDLOOP.
ENDFORM.                    " ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS.
  DATA: LAYOUT          TYPE SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
*  PERFORM F_ALV_SORT.
  PERFORM MONTAR_LAYOUT USING 'TG_SAIDA'.
  LAYOUT-BOX_FIELDNAME = 'MARK'.
  LAYOUT-BOX_TABNAME  = 'TG_SAIDA'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM      = V_REPORT
            I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
            IT_FIELDCAT             = ESTRUTURA[]
*            IT_SORT                 = T_SORT[]
            I_SAVE                  = 'A'
            IT_EVENTS               = EVENTS
            IS_LAYOUT               = LAYOUT
            IS_PRINT                = T_PRINT
       TABLES
            T_OUTTAB                = TG_SAIDA.

ENDFORM.                    " IMPRIMIR_DADOS
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
                                 SLIS_EV_USER_COMMAND  'XUSER_COMMAND',
                                 SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                 SLIS_EV_TOP_OF_PAGE   'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT USING P_ALV.
  REFRESH: ESTRUTURA.
  IF P_ALV EQ 'TG_SAIDA'.
    PERFORM MONTAR_ESTRUTURA USING:
   1  'BKPF'       'BELNR'            'TG_SAIDA' 'BELNR'             ' '  ' ',
   1  'ZMMT0006'   'CH_REFERENCIA'    'TG_SAIDA' 'CH_REFERENCIA'     'Chave de referencia'  ' ',
   2  'ZMMT0006'   'TCODE'            'TG_SAIDA' 'TCODE'             'TCODE'  ' ',
   3  'ZMMT0006'   'MATNR'            'TG_SAIDA' 'MATNR'             ' '   ' ',
   4  'MAKT'       'MAKTX'            'TG_SAIDA' 'MAKTX'             ' '   ' ',
   5  'ZMMT0006'   'WERKS'            'TG_SAIDA' 'WERKS'             'Centro-a Fixar'   ' ',
   6  'ZMMT0006'   'WERKS_D'          'TG_SAIDA' 'WERKS_D'           'Centro Fixo'   ' ',
   7  'ZMMT0006'   'BUDAT'            'TG_SAIDA' 'BUDAT'             ' '   ' ',
   8  'ZMMT0006'   'ERFMG'            'TG_SAIDA' 'ERFMG'             ' '   ' ',
   9  'ZMMT0006'   'VR_MATERIAL_USD'  'TG_SAIDA' 'VR_MATERIAL_USD'   'Vlr.US$-SIGAM'   ' ',
  10  'BKPF'       'AWKEY'            'TG_SAIDA' 'AWKEY'             'Nro.material'   ' ',
  11  'BKPF'       'DMBE2'            'TG_SAIDA' 'VLR_CONTABIL'      'Vlr.US$-Contabil'   ' ',
  12  'BKPF'       'DMBE2'            'TG_SAIDA' 'VLR_AJUSTE'        'Vlr US$-Ajuste'   ' '.

  ELSE.
    PERFORM MONTAR_ESTRUTURA USING:
   1  'ZMMT0006'   'WERKS'            'TG_SAIDA_EXEC' 'WERKS'     ' '  ' ',
   2  'ZMMT0006'   'MATNR'            'TG_SAIDA_EXEC' 'MATNR'     ' '  ' ',
   3  'BSIS'       'DMBE2'            'TG_SAIDA_EXEC' 'VLR_AJUST' ' '   ' ',
   4  ' '       ' '                   'TG_SAIDA_EXEC' 'MSG'       'Msg de Batchinput'   '80'.
  ENDIF.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

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
  IF P_OUTPUTLEN IS INITIAL.
    WA_ESTRUTURA-OUTPUTLEN     = X_CONTADOR.
  ELSE.
    WA_ESTRUTURA-OUTPUTLEN     =  P_OUTPUTLEN.
  ENDIF.

  CASE P_FIELD.
    WHEN 'BELNR'
      OR 'AWKEY'.
      WA_ESTRUTURA-HOTSPOT = 'X'.
      WA_ESTRUTURA-KEY = 'X'.

  ENDCASE.


  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " montar_estrutura

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.
*            I_LOGO             = 'CLARO_50'.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVES.

  V_REPORT = SY-REPID.

  PERFORM F_CONSTRUIR_CABECALHO.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO.
  DATA: WL_DATA_LOW(30),
        WL_DATA_HIGHT(30).


  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  CLEAR: LS_LINE.
  SELECT SINGLE BUTXT
      FROM T001
      INTO LS_LINE-INFO
       WHERE BUKRS EQ S_BUKRS-LOW.
  LS_LINE-KEY = 'Empresa:'.
  CONCATENATE 'Empresa: ' LS_LINE-INFO INTO LS_LINE-INFO SEPARATED BY SPACE.
  LS_LINE-TYP =  'A'.
*  CONCATENATE 'EMPRESA:' S_BUKRS-LOW INTO LS_LINE-INFO SEPARATED BY SPACE.
  APPEND LS_LINE TO T_TOP.

  CLEAR LS_LINE.
  IF S_BUDAT-HIGH IS INITIAL.
    S_BUDAT-HIGH = S_BUDAT-LOW.
  ENDIF.
  CONCATENATE S_BUDAT-LOW+6(2) S_BUDAT-LOW+4(2) S_BUDAT-LOW(4) INTO WL_DATA_LOW SEPARATED BY '/'.
  CONCATENATE S_BUDAT-HIGH+6(2) S_BUDAT-HIGH+4(2) S_BUDAT-HIGH(4) INTO WL_DATA_HIGHT SEPARATED BY '/'.
  CONDENSE WL_DATA_LOW NO-GAPS.
  CONDENSE WL_DATA_HIGHT NO-GAPS.
  CONCATENATE 'Período:' WL_DATA_LOW 'á' WL_DATA_HIGHT INTO LS_LINE-INFO SEPARATED BY SPACE.
  LS_LINE-TYP  = 'A'.
  APPEND LS_LINE TO T_TOP.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'A'.
  LS_LINE-KEY = 'QUEBRA'.
  LS_LINE-INFO = ' '.
  APPEND LS_LINE TO T_TOP.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'A'.
  LS_LINE-KEY = 'QUEBRA'.
  LS_LINE-INFO = ' '.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD.
  DATA: BEGIN OF TL_MR22 OCCURS 0,
         MATNR      TYPE MARA-MATNR,
         WERKS      TYPE ZMMT0006-WERKS,
         WERKS_D    TYPE ZMMT0006-WERKS_D,
         VLR_AJUSTE TYPE BSIS-DMBE2,
        END OF TL_MR22.

  DATA: WL_VLR TYPE BSIS-DMBE2,
        WL_SAIDA LIKE WG_SAIDA,
        WL_ERRO(1).

  CLEAR: WL_VLR, WL_SAIDA,WL_ERRO.
  REFRESH: TL_MR22, TG_SAIDA_EXEC.
  SELFIELD = SELFIELD.                                      "#EC CALLED
  CASE UCOMM.
    WHEN 'MR22'.
*      BREAK-POINT.
      LOOP AT TG_SAIDA INTO WG_SAIDA
        WHERE MARK IS NOT INITIAL.
*        ADD WG_SAIDA-VLR_AJUSTE TO WL_VLR.
        MOVE: WG_SAIDA-WERKS      TO TL_MR22-WERKS,
              WG_SAIDA-WERKS_D    TO TL_MR22-WERKS_D,
              WG_SAIDA-MATNR      TO TL_MR22-MATNR,
              WG_SAIDA-VLR_AJUSTE TO TL_MR22-VLR_AJUSTE.

        COLLECT TL_MR22.
        CLEAR: TL_MR22.
      ENDLOOP.

      LOOP AT TL_MR22.
        READ TABLE TG_SAIDA INTO WL_SAIDA
          WITH KEY WERKS   = TL_MR22-WERKS
                   WERKS_D = TL_MR22-WERKS_D
                   MATNR   = TL_MR22-MATNR
                   MARK    = 'X'.

        IF WL_SAIDA-STATUS IS INITIAL.
          PERFORM GERA_MR22 USING P_DATA TL_MR22-WERKS_D S_BUKRS-LOW TL_MR22-MATNR TL_MR22-VLR_AJUSTE.
          READ TABLE TG_MSG TRANSPORTING NO FIELDS
            WITH KEY MSGTYP = 'E'.
          IF SY-SUBRC IS NOT INITIAL.
            LOOP AT TG_0006 INTO WG_0006
              WHERE WERKS_D EQ TL_MR22-WERKS_D
                AND BUKRS   EQ S_BUKRS-LOW
                AND MATNR   EQ TL_MR22-MATNR
                AND WERKS   EQ TL_MR22-WERKS.

              READ TABLE TG_SAIDA TRANSPORTING NO FIELDS
                WITH KEY CH_REFERENCIA = WG_0006-CH_REFERENCIA
                         MARK          = 'X'.
              IF SY-SUBRC IS INITIAL.

                MOVE : 1 TO WG_0006-STATUS.

                MODIFY ZMMT0006 FROM WG_0006.
                READ TABLE TG_SAIDA INTO WG_SAIDA
                  WITH KEY CH_REFERENCIA = WG_0006-CH_REFERENCIA
                           MARK          = 'X'.

                MOVE '1' TO WG_SAIDA-STATUS.
                MOVE '1' TO WL_SAIDA-STATUS.
                MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX.

              ENDIF.
              CLEAR: WG_0006.
            ENDLOOP.
          ENDIF.
        ENDIF.

        CLEAR WL_ERRO.
        IF WL_SAIDA-STATUS EQ '1'.
          MULTIPLY TL_MR22-VLR_AJUSTE BY -1.
          PERFORM GERA_MR22 USING P_DATA TL_MR22-WERKS S_BUKRS-LOW TL_MR22-MATNR TL_MR22-VLR_AJUSTE.
          READ TABLE TG_MSG TRANSPORTING NO FIELDS
              WITH KEY MSGTYP = 'E'.
          IF SY-SUBRC IS NOT INITIAL.
*            LOOP AT TG_0006 INTO WG_0006
*              WHERE WERKS_D EQ TL_MR22-WERKS_D
*                AND BUKRS   EQ S_BUKRS-LOW
*                AND MATNR   EQ TL_MR22-MATNR
*                AND WERKS   EQ TL_MR22-WERKS.
*
*              READ TABLE TG_SAIDA TRANSPORTING NO FIELDS
*                WITH KEY CH_REFERENCIA = WG_0006-CH_REFERENCIA
*                         MARK          = 'X'.
*              IF SY-SUBRC IS INITIAL.
*                MOVE : 2 TO WG_0006-STATUS.
*
*                MODIFY  ZMMT0006 FROM WG_0006.
*                READ TABLE TG_SAIDA INTO WG_SAIDA
*                  WITH KEY CH_REFERENCIA = WG_0006-CH_REFERENCIA
*                           MARK          = 'X'.
*
*                MOVE '2' TO WG_SAIDA-STATUS.
*                MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX .
*              ENDIF.
*              CLEAR: WG_0006.
*            ENDLOOP.
          ELSE.
            WL_ERRO = 'X'.
          ENDIF.
        ENDIF.
        " Executa FBB1
        IF TL_MR22-VLR_AJUSTE NE 0 AND WL_ERRO IS INITIAL.
          PERFORM EXECUTA_FBB1 USING P_DATA TL_MR22-WERKS_D S_BUKRS-LOW TL_MR22-MATNR TL_MR22-VLR_AJUSTE.
          READ TABLE TG_MSG TRANSPORTING NO FIELDS
              WITH KEY MSGTYP = 'E'.
          IF SY-SUBRC IS NOT INITIAL.
            LOOP AT TG_0006 INTO WG_0006
                WHERE WERKS_D EQ TL_MR22-WERKS_D
                  AND BUKRS   EQ S_BUKRS-LOW
                  AND MATNR   EQ TL_MR22-MATNR
                  AND WERKS   EQ TL_MR22-WERKS.

              READ TABLE TG_SAIDA TRANSPORTING NO FIELDS
                WITH KEY CH_REFERENCIA = WG_0006-CH_REFERENCIA
                         MARK          = 'X'.
              IF SY-SUBRC IS INITIAL.
                MOVE : 2 TO WG_0006-STATUS.

                MODIFY  ZMMT0006 FROM WG_0006.
                READ TABLE TG_SAIDA INTO WG_SAIDA
                  WITH KEY CH_REFERENCIA = WG_0006-CH_REFERENCIA
                           MARK          = 'X'.

                MOVE '2' TO WG_SAIDA-STATUS.
                MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX .
              ENDIF.
              CLEAR: WG_0006.
            ENDLOOP.
          ENDIF.
        ENDIF.

      ENDLOOP.


      DELETE TG_SAIDA WHERE STATUS EQ '2'.

      PERFORM MONTAR_LAYOUT USING 'TG_SAIDA_EXEC'.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
         EXPORTING
              I_CALLBACK_PROGRAM      = V_REPORT
*            I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
              IT_FIELDCAT             = ESTRUTURA[]
*            IT_SORT                 = T_SORT[]
              I_SAVE                  = 'A'
              I_SCREEN_START_COLUMN   = 3
              I_SCREEN_START_LINE     = 3
              I_SCREEN_END_COLUMN     = 100
               I_SCREEN_END_LINE      = 13
         TABLES
              T_OUTTAB                = TG_SAIDA_EXEC.

      SY-UCOMM = '&REFRESH'.
      UCOMM    = SY-UCOMM.
    WHEN '&IC1'.
* Lê na tabela de saída
      READ TABLE TG_SAIDA INTO WG_SAIDA INDEX SELFIELD-TABINDEX.

      IF SY-SUBRC EQ 0.
** Se foi clicado na coluna EBELN.
        IF SELFIELD-FIELDNAME = 'BELNR'.
** Passa o valor clicado na coluna como parâmetro para a transação que
**se quer chamar.
** Passa o id do campo Pedido na transação ME23N.
          SET PARAMETER ID 'BLN' FIELD WG_SAIDA-BELNR.
          SET PARAMETER ID 'BUK' FIELD S_BUKRS-LOW.
          SET PARAMETER ID 'GJR' FIELD WG_SAIDA-BUDAT(4).

** Chamo a transação
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF SELFIELD-FIELDNAME =  'AWKEY'.
* ---> S4 Migration - 19/07/2023 - LO
*          SET PARAMETER ID 'MBN' FIELD WG_SAIDA-AWKEY.
*          SET PARAMETER ID 'MJA' FIELD WG_SAIDA-BUDAT(4).
*** Chamo a transação
*          CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

          CALL FUNCTION 'MIGO_DIALOG'
           EXPORTING
             I_ACTION                  = 'A04'
             I_REFDOC                  = 'R02'
             I_NOTREE                  = 'X'
             I_NO_AUTH_CHECK           = ''
             I_SKIP_FIRST_SCREEN       = 'X'
             I_DEADEND                 = 'X'
             I_OKCODE                  = 'OK_GO'
             I_MBLNR                   = WG_SAIDA-AWKEY
             I_MJAHR                   = WG_SAIDA-BUDAT(4)
           EXCEPTIONS
             ILLEGAL_COMBINATION       = 1
             OTHERS                    = 2
                    .
* <--- S4 Migration - 19/07/2023 - LO
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM. "XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  Z_PF_STATUS_SET
*&---------------------------------------------------------------------*
* Especificar barra de status
*----------------------------------------------------------------------*
FORM XPF_STATUS_SET USING PT_EXTAB TYPE SLIS_T_EXTAB.
* Acionar barra de comandos
*  CLEAR IT_FCODE[].

*  IF NOT RA2 IS INITIAL.
*    WA_FCODE = C_GRAVAR.
*    APPEND WA_FCODE TO IT_FCODE.
*    WA_FCODE = C_EDITAR.
*    APPEND WA_FCODE TO IT_FCODE.
*    WA_FCODE = C_SIMULAR.
*    APPEND WA_FCODE TO IT_FCODE.
*    WA_FCODE = C_GERAR.
*    APPEND WA_FCODE TO IT_FCODE.
*  ENDIF.

  SET PF-STATUS 'STANDARD'. " EXCLUDING IT_FCODE.
ENDFORM.                    "z_pf_status_set
*&---------------------------------------------------------------------*
*&      Form  GERA_MR22
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GERA_MR22 USING P_DATA P_WERKS P_BUKRS P_MATERIAL P_VLR.
  DATA: WL_VLR(30),
        WL_DATA(15),
        WL_MODE VALUE 'N'.


  WRITE: P_VLR TO WL_VLR,
         P_DATA TO WL_DATA.

  CONDENSE WL_DATA NO-GAPS.
  CONDENSE WL_VLR NO-GAPS.
  REFRESH: TG_MSG.
  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPRCKM_MR22'         '0201',
      ' ' 'MR21HEAD-BUDAT'       WL_DATA,
      ' ' 'MR21HEAD-BUKRS'       P_BUKRS,
      ' ' 'MR21HEAD-WERKS'       P_WERKS,
      ' ' 'MR21HEAD-XBLNR'       'DOLAR-GERENCIAL',
      ' ' 'BDC_OKCODE'           '=ENTR'.

  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPRCKM_MR22'         '0201',
*      ' ' 'MR21HEAD-BUDAT'       p_data,
*      ' ' 'MR21HEAD-BUKRS'       p_bukrs,
*      ' ' 'MR21HEAD-WERKS'       p_werks,
      ' ' 'BDC_OKCODE'           '=TAB2'.

  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPRCKM_MR22'         '0201',
      ' ' 'CKI_MR22_0250-MATNR(01)'    P_MATERIAL,
      ' ' 'CKI_MR22_0250-ZUUMB(01)'    WL_VLR,
*      ' ' 'MR21HEAD-WERKS'       p_werks,
      ' ' 'BDC_OKCODE'           '=ENTR'.

  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPRCKM_MR22'         '0201',
*      ' ' 'CKI_MR22_0250-MATNR(01)'    p_material,
*      ' ' 'MR21HEAD-BUKRS'       p_bukrs,
*      ' ' 'MR21HEAD-WERKS'       p_werks,
      ' ' 'BDC_OKCODE'           '=SAVE'.

  CALL TRANSACTION 'MR22' USING TG_BDC
    MODE   WL_MODE
    UPDATE 'S'
    MESSAGES INTO TG_MSG.

  MOVE: P_MATERIAL TO WG_SAIDA_EXEC-MATNR,
        P_WERKS    TO WG_SAIDA_EXEC-WERKS,
        P_VLR      TO WG_SAIDA_EXEC-VLR_AJUST.
  PERFORM EXIBE_LOG.

*  APPEND WA_LOG TO T_LOG.

  REFRESH TG_BDC.

ENDFORM.                    " GERA_MR22
*&---------------------------------------------------------------------*
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM F_PREENCHER_DYNPRO USING L_START TYPE C L_NAME TYPE C L_VALUE.

  CLEAR : WA_BDC.
  MOVE L_START TO WA_BDC-DYNBEGIN.
  IF L_START = 'X'.
    MOVE:
  L_NAME  TO WA_BDC-PROGRAM,
  L_VALUE TO WA_BDC-DYNPRO.
  ELSE.
    MOVE:
      L_NAME  TO WA_BDC-FNAM,
      L_VALUE TO WA_BDC-FVAL.
  ENDIF.
  APPEND WA_BDC TO TG_BDC.

ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Form  exibe_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXIBE_LOG.

  DATA: X_MSGID          LIKE SY-MSGID,
        X_MSGNO          LIKE SY-MSGNO,
        X_MSGTY          LIKE SY-MSGTY,
        X_MSGV1          LIKE SY-MSGV1,
        X_MSGV2          LIKE SY-MSGV2,
        X_MSGV3          LIKE SY-MSGV3,
        X_MSGV4          LIKE SY-MSGV4,
        X_MESSAGE        LIKE MESSAGE.


  LOOP AT TG_MSG INTO WG_MSG.
    CLEAR:
      X_MSGID,
      X_MSGNO,
      X_MSGTY,
      X_MSGV1,
      X_MSGV2,
      X_MSGV3,
      X_MSGV4,
      X_MESSAGE.

    MOVE:
      WG_MSG-MSGID  TO X_MSGID,
      WG_MSG-MSGNR  TO X_MSGNO,
      WG_MSG-MSGTYP TO X_MSGTY,
      WG_MSG-MSGV1  TO X_MSGV1,
      WG_MSG-MSGV2  TO X_MSGV2,
      WG_MSG-MSGV3  TO X_MSGV3,
      WG_MSG-MSGV4  TO X_MSGV4.

    CALL FUNCTION 'WRITE_MESSAGE_NEW'
      EXPORTING
        MSGID = X_MSGID
        MSGNO = X_MSGNO
        MSGTY = X_MSGTY
        MSGV1 = X_MSGV1
        MSGV2 = X_MSGV2
        MSGV3 = X_MSGV3
        MSGV4 = X_MSGV4
        MSGV5 = X_MSGV4
      IMPORTING
        MESSG = X_MESSAGE.

    IF WG_MSG-MSGTYP = 'S'
    OR WG_MSG-MSGTYP = 'E'.
      MOVE: X_MESSAGE TO WG_SAIDA_EXEC-MSG.

      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
      CLEAR: WG_SAIDA_EXEC-MSG.

    ENDIF.

  ENDLOOP.


ENDFORM.                    " exibe_log
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_FBB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATA  text
*      -->P_TL_MR22_WERKS_D  text
*      -->P_S_BUKRS_LOW  text
*      -->P_TL_MR22_MATNR  text
*      -->P_TL_MR22_VLR_AJUSTE  text
*----------------------------------------------------------------------*
FORM EXECUTA_FBB1  USING P_DATA P_WERKS P_BUKRS P_MATERIAL P_VLR.
  DATA: WL_VLR(30),
        WL_DATA(15),
        WL_DATAL(15),
        WL_MODE VALUE 'N',
        V_NEWBS1 TYPE RF05A-NEWBS,
        V_NEWBS2 TYPE RF05A-NEWBS.

  IF P_VLR GT 0.
    V_NEWBS1 = '50'.
    V_NEWBS2 = '40'.
  ELSE.
    V_NEWBS2 = '50'.
    V_NEWBS1 = '40'.
    MULTIPLY P_VLR BY -1.
  ENDIF.


  WRITE: P_VLR TO WL_VLR,
         P_DATA TO WL_DATA,
         S_BUDAT-LOW TO WL_DATAL.

  CONDENSE WL_DATA NO-GAPS.
  CONDENSE WL_DATAL NO-GAPS.
  CONDENSE WL_VLR NO-GAPS.

  REFRESH: TG_MSG,
           TG_BDC.

  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPMF05A'         '0100',
      ' ' 'BDC_OKCODE'       '/00',
      ' ' 'BKPF-BLDAT'       WL_DATA,
      ' ' 'BKPF-BLART'       'SA',
      ' ' 'BKPF-BUKRS'       P_BUKRS,
      ' ' 'BKPF-BUDAT'       WL_DATA,
      ' ' 'BKPF-MONAT'       WL_DATA+3(2),
      ' ' 'BKPF-WAERS'       'USD',
      ' ' 'FS006-DOCID'      '*',
      ' ' 'RF05A-NEWBS'      V_NEWBS1,
      ' ' 'RF05A-NEWKO'      '511000'.

  PERFORM F_PREENCHER_DYNPRO USING:
     'X' 'SAPMF05A'         '0300',
     ' ' 'BDC_OKCODE'       '=ZK',
     ' ' 'BSEG-WRBTR'       WL_VLR,
     ' ' 'BSEG-BUPLA'       P_WERKS.

  PERFORM F_PREENCHER_DYNPRO USING:
     'X' 'SAPLKACB'         '0002',
     ' ' 'BDC_OKCODE'       '=ENTE',
     ' ' 'COBL-GSBER'       P_WERKS.

  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPMF05A'         '0330',
      ' ' 'BDC_OKCODE'       '/00',
      ' ' 'BSEG-DMBE2'       WL_VLR,
      ' ' 'RF05A-NEWBS'      V_NEWBS2,
      ' ' 'RF05A-NEWKO'      '511015'.

  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPMF05A'         '0300',
      ' ' 'BDC_OKCODE'       '=ZK',
      ' ' 'BSEG-WRBTR'       WL_VLR,
      ' ' 'BSEG-BUPLA'       P_WERKS.

  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPLKACB'         '0002',
      ' ' 'BDC_OKCODE'       '=ENTE',
      ' ' 'COBL-GSBER'       P_WERKS.

  PERFORM F_PREENCHER_DYNPRO USING:
     'X' 'SAPMF05A'         '0330',
     ' ' 'BDC_OKCODE'       '=AB',
     ' ' 'BSEG-DMBE2'       WL_VLR.

  PERFORM F_PREENCHER_DYNPRO USING:
      'X' 'SAPMF05A'         '0700',
      ' ' 'BDC_OKCODE'       '=BU',
      ' ' 'BKPF-BKTXT'       'DOLAR-GERENCIAL'.


  CALL TRANSACTION 'FBB1' USING TG_BDC
    MODE   WL_MODE
    UPDATE 'S'
    MESSAGES INTO TG_MSG.

  PERFORM EXIBE_LOG.

  REFRESH TG_BDC.
ENDFORM.                    " EXECUTA_FBB1
