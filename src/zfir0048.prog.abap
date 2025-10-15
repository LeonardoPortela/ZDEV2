*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 30/04/2014                                              &*
*& Descrição: Livro Diário - Paraguay                                 &*
*& Transação: ZFI0052                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  ZFIR0048.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.

TYPES: BEGIN OF TY_BKPF,
         BUKRS TYPE BKPF-BUKRS,
         GJAHR TYPE BKPF-GJAHR,
         BUDAT TYPE BKPF-BUDAT,
         BELNR TYPE BKPF-BELNR,
       END OF TY_BKPF,

       BEGIN OF TY_FAGLFLEXA,
         RYEAR  TYPE FAGLFLEXA-RYEAR,
         DOCNR  TYPE FAGLFLEXA-DOCNR,
         RBUKRS TYPE FAGLFLEXA-RBUKRS,
         RLDNR  TYPE FAGLFLEXA-RLDNR,
         RCNTR  TYPE FAGLFLEXA-RCNTR,
         RACCT  TYPE FAGLFLEXA-RACCT,
         HSL    TYPE FAGLFLEXA-HSL,
         DRCRK  TYPE FAGLFLEXA-DRCRK,
       END OF TY_FAGLFLEXA,

       BEGIN OF TY_SKAT,
         KTOPL TYPE SKAT-KTOPL,
         SAKNR TYPE SKAT-SAKNR,
         TXT50 TYPE SKAT-TXT50,
       END OF TY_SKAT,

       BEGIN OF TY_T001,
         BUKRS TYPE T001-BUKRS,
         BUTXT TYPE T001-BUTXT,
       END OF TY_T001,

       BEGIN OF TY_SAIDA,
         BUTXT    TYPE T001-BUTXT,
         DE       TYPE BKPF-BUDAT,
         ATE      TYPE BKPF-BUDAT,
         BUDAT    TYPE BKPF-BUDAT,
         ASIENTO(6),
         TC(2),
         N_COMPR(8),
         N_CTA    TYPE FAGLFLEXA-RACCT,
         NOMBRE   TYPE SKAT-TXT50,
         DEBITOS  TYPE FAGLFLEXA-HSL,
         CREDITOS TYPE FAGLFLEXA-HSL,
         C_COSTO  TYPE FAGLFLEXA-RCNTR,
         A_NEGOCIO,
         CONCEPTO,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: T_BKPF      TYPE TABLE OF TY_BKPF,
      T_FAGLFLEXA TYPE TABLE OF TY_FAGLFLEXA,
      T_SKAT      TYPE TABLE OF TY_SKAT,
      T_T001      TYPE TABLE OF TY_T001,
      T_SAIDA     TYPE TABLE OF TY_SAIDA.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_BKPF      TYPE TY_BKPF,
      WA_FAGLFLEXA TYPE TY_FAGLFLEXA,
      WA_SKAT      TYPE TY_SKAT,
      WA_T001      TYPE TY_T001,
      WA_SAIDA     TYPE TY_SAIDA.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: C_X               TYPE C VALUE 'X'.
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

*DATA: VARIANTE         LIKE DISVARIANT.
*DATA: GS_VARIANT_C     TYPE DISVARIANT.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR WA_BKPF-BUKRS OBLIGATORY,
                S_BUDAT FOR WA_BKPF-BUDAT NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: VL_FORMNAME           TYPE TDSFNAME,
        VL_NAME               TYPE RS38L_FNAM,
        LS_CONTROL            TYPE SSFCTRLOP,
        LS_OPTIONS            TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO       TYPE SSFCRESCL,
        JOB_OUTPUT_OPTIONS    TYPE SSFCRESOP.

  DATA: WG_BUDAT_LOW  TYPE BKPF-BUDAT,
        WG_BUDAT_HIGH TYPE BKPF-BUDAT,
        WG_DIALOG.

  LS_OPTIONS-TDDEST   = 'LOCL'.

  REFRESH T_BKPF.

  SELECT BUKRS GJAHR BUDAT BELNR
    FROM BKPF
      INTO TABLE T_BKPF
        WHERE BUKRS IN S_BUKRS
          AND BUDAT IN S_BUDAT.

  IF SY-SUBRC IS INITIAL.
    IF S_BUDAT IS INITIAL.
      SORT T_BKPF BY BUDAT ASCENDING.
      READ TABLE T_BKPF INTO WA_BKPF INDEX 1.
      WG_BUDAT_LOW = WA_BKPF-BUDAT.
      CLEAR WA_BKPF.

      SORT T_BKPF BY BUDAT DESCENDING.
      READ TABLE T_BKPF INTO WA_BKPF INDEX 1.
      WG_BUDAT_HIGH = WA_BKPF-BUDAT.
      CLEAR WA_BKPF.
    ELSE.
      WG_BUDAT_LOW = S_BUDAT-LOW.
      WG_BUDAT_HIGH = S_BUDAT-HIGH.
    ENDIF.

    SORT T_BKPF BY BUKRS.

    DELETE ADJACENT DUPLICATES FROM T_BKPF
                               COMPARING BUKRS.
    IF S_BUDAT-HIGH IS NOT INITIAL
      AND S_BUDAT-HIGH(6) NE S_BUDAT-LOW(6).
      MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'Preencher com o mesmo mês do período'.

    ELSE.

      VL_FORMNAME = 'ZLIBRO_DIARIO'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          FORMNAME           = VL_FORMNAME
        IMPORTING
          FM_NAME            = VL_NAME
        EXCEPTIONS
          NO_FORM            = 1
          NO_FUNCTION_MODULE = 2
          OTHERS             = 3.

      LOOP AT T_BKPF INTO WA_BKPF.

        IF WG_DIALOG EQ C_X.
          LS_CONTROL-NO_DIALOG = C_X.
*          LS_CONTROL-PREVIEW = C_X.
          MOVE-CORRESPONDING JOB_OUTPUT_OPTIONS TO LS_CONTROL.
          MOVE-CORRESPONDING JOB_OUTPUT_OPTIONS TO LS_OPTIONS.
          MOVE-CORRESPONDING JOB_OUTPUT_INFO TO LS_OPTIONS.
          MOVE JOB_OUTPUT_OPTIONS-TDPREVIEW TO LS_CONTROL-PREVIEW.

        ELSE.
          LS_CONTROL-NO_DIALOG = ' '.
*          LS_CONTROL-PREVIEW   = SPACE.
**  Impressora
*        LS_CONTROL-NO_DIALOG = ' '. "Evita la pantalla de opciones de salida del formulario
*          LS_OPTIONS-TDDEST   = ' '."'LOCL'.
*          LS_OPTIONS-TDIMMED  = ' '.
*          LS_OPTIONS-TDNEWID  = ' '.
*          LS_OPTIONS-TDNOARCH = ' '.
*
*          LS_CONTROL-DEVICE  = 'PRINTER'.
*          LS_CONTROL-GETOTF  = ' '.
        ENDIF.

        CLEAR:JOB_OUTPUT_INFO, JOB_OUTPUT_OPTIONS.

        CALL FUNCTION VL_NAME
          EXPORTING
            USER_SETTINGS      = ' '
            CONTROL_PARAMETERS = LS_CONTROL
            OUTPUT_OPTIONS     = LS_OPTIONS
            BUKRS              = WA_BKPF-BUKRS
            BUDAT_LOW          = WG_BUDAT_LOW  "S_BUDAT-LOW
            BUDAT_HIGH         = WG_BUDAT_HIGH "S_BUDAT-HIGH
          IMPORTING
            JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
            JOB_OUTPUT_OPTIONS = JOB_OUTPUT_OPTIONS
          EXCEPTIONS
            FORMATTING_ERROR   = 1
            INTERNAL_ERROR     = 2
            SEND_ERROR         = 3
            USER_CANCELED      = 4
            OTHERS             = 5.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        WG_DIALOG = C_X.
      ENDLOOP.
*    PERFORM SELECIONAR_DADOS.
*    PERFORM ORGANIZAR_DADOS.
*    PERFORM INICIAR_VARIAVEIS.
*    PERFORM IMPRIMIR_DADOS.
    ENDIF.
  ELSE.
    MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros'.
  ENDIF.
*
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SELECIONAR_DADOS .
*  DATA: WL_GJAHR(4).
*
*  WL_GJAHR = S_BUDAT-LOW(4).
*
*  SELECT BUKRS GJAHR BUDAT BELNR
*    FROM BKPF
*      INTO TABLE T_BKPF
*        WHERE BUKRS IN S_BUKRS
*          AND BUDAT IN S_BUDAT
*          AND GJAHR IN S_BUDAT.
*
*  IF T_BKPF[] IS NOT INITIAL.
*    SELECT RYEAR DOCNR RBUKRS RLDNR RCNTR RACCT HSL DRCRK
*      FROM FAGLFLEXA
*        INTO TABLE T_FAGLFLEXA
*        FOR ALL ENTRIES IN T_BKPF
*          WHERE RYEAR  EQ T_BKPF-GJAHR
*            AND DOCNR  EQ T_BKPF-BELNR
*            AND RBUKRS EQ T_BKPF-BUKRS
*            AND RLDNR  EQ '0L'.
*
*    IF T_FAGLFLEXA[] IS NOT INITIAL.
*      SELECT KTOPL SAKNR TXT50
*        FROM SKAT
*          INTO TABLE T_SKAT
*          FOR ALL ENTRIES IN T_FAGLFLEXA
*            WHERE SAKNR EQ T_FAGLFLEXA-RACCT
*              AND KTOPL EQ '0050'
*              AND SPRAS EQ 'ES'.
*
*    ENDIF.
*
*    SELECT BUKRS BUTXT
*      FROM T001
*        INTO TABLE T_T001
*        FOR ALL ENTRIES IN T_BKPF
*          WHERE BUKRS EQ T_BKPF-BUKRS.
*  ENDIF.
*
*ENDFORM.                    " SELECIONAR_DADOS
**&---------------------------------------------------------------------*
**&      Form  ORGANIZAR_DADOS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM ORGANIZAR_DADOS .
*
*  LOOP AT  T_BKPF INTO WA_BKPF.
*    READ TABLE T_FAGLFLEXA INTO WA_FAGLFLEXA
*      WITH KEY RYEAR = WA_BKPF-GJAHR
*               DOCNR = WA_BKPF-BELNR
*               RBUKRS = WA_BKPF-BUKRS
*               RLDNR  = '0L'.
*
*    IF SY-SUBRC IS INITIAL.
*      READ TABLE T_SKAT INTO WA_SKAT
*        WITH KEY SAKNR = WA_FAGLFLEXA-RACCT
*                 KTOPL = '0050'.
*    ENDIF.
*
*    READ TABLE T_T001 INTO WA_T001
*      WITH KEY BUKRS = WA_BKPF-BUKRS.
*
*    WA_SAIDA-BUTXT = WA_T001-BUTXT.
*    WA_SAIDA-DE    = S_BUDAT-LOW.
*    WA_SAIDA-ATE   = S_BUDAT-HIGH.
*    WA_SAIDA-BUDAT = WA_BKPF-BUDAT.
*    WA_SAIDA-ASIENTO = '000001'.
*    WA_SAIDA-TC      = 'AP'.
*    WA_SAIDA-N_COMPR = '00000000'.
*    WA_SAIDA-N_CTA   = WA_FAGLFLEXA-RACCT.
*    WA_SAIDA-NOMBRE  = WA_SKAT-TXT50.
*
*    IF WA_FAGLFLEXA-DRCRK EQ 'S'.
*      WA_SAIDA-DEBITOS =  WA_FAGLFLEXA-HSL.
*    ELSEIF WA_FAGLFLEXA-DRCRK EQ 'H'.
*      WA_SAIDA-CREDITOS = WA_FAGLFLEXA-HSL.
*    ENDIF.
*
*    WA_SAIDA-C_COSTO = WA_FAGLFLEXA-RCNTR.
*    WA_SAIDA-A_NEGOCIO = SPACE.
*    WA_SAIDA-CONCEPTO  = SPACE.
*
*    APPEND WA_SAIDA TO T_SAIDA.
*    CLEAR: WA_SAIDA, WA_FAGLFLEXA, WA_T001, WA_SKAT.
*  ENDLOOP.
*
*ENDFORM.                    " ORGANIZAR_DADOS
*
**&---------------------------------------------------------------------*
**&      Form  IMPRIMIR_DADOS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM IMPRIMIR_DADOS .
*  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
*  PERFORM DEFINIR_EVENTOS.
*  PERFORM MONTAR_LAYOUT." USING 'T_SAIDA'.
**  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
**  WL_LAYOUT-BOX_TABNAME  = 'T_SAIDA'.
*
*  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*     I_CALLBACK_PROGRAM                = V_REPORT
**     IS_VARIANT                        = GS_VARIANT_C
**    I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
*     IT_FIELDCAT                       = ESTRUTURA[]
*     IS_LAYOUT                         = WL_LAYOUT
*     I_SAVE                            = 'A'
*     IT_EVENTS                         = EVENTS
*     IS_PRINT                          = T_PRINT
*
*    TABLES
*      T_OUTTAB                          = T_SAIDA.
*
*
*
*ENDFORM.                    "imprimir_dados
**&---------------------------------------------------------------------*
**&      Form  DEFINIR_EVENTOS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM DEFINIR_EVENTOS.
*
*  PERFORM F_CARREGAR_EVENTOS USING:
*                                   SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
*                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET'.
**                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
*
*
*
*ENDFORM.                    " DEFINIR_EVENTOS
**&---------------------------------------------------------------------*
**&      Form  F_CARREGAR_EVENTOS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_SLIS_EV_USER_COMMAND  text
**      -->P_0290   text
**----------------------------------------------------------------------*
*FORM F_CARREGAR_EVENTOS USING    NAME FORM.
*  CLEAR XS_EVENTS.
*  XS_EVENTS-NAME = NAME.
*  XS_EVENTS-FORM = FORM.
*  APPEND XS_EVENTS TO EVENTS.
*
*ENDFORM.                    " F_CARREGAR_EVENTOS
**&---------------------------------------------------------------------*
**&      Form  MONTAR_LAYOUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM MONTAR_LAYOUT.
*
*  PERFORM MONTAR_ESTRUTURA USING:
*        1  'T001'       'BUTXT'            'T_SAIDA' 'BUTXT'            'Nombre de la Empresa'   ' ' ,      "   Empresa
*        2  'BKPF'       'BUDAT'            'T_SAIDA' 'BUDAT'            'Fecha'                  ' ' ,      "   Fecha
*        3  ' '          ' '                'T_SAIDA' 'ASIENTO'          'Asiento'                ' ' ,      "   Asiento
*        4  ' '          ' '                'T_SAIDA' 'TC'               'TC'                     ' ' ,      "   TC
*        5  ' '          ' '                'T_SAIDA' 'N_COMPR'          'N.Compr'                ' ' ,      "   N. Compr
*        6  'FAGLFLEXA'  'RACCT'            'T_SAIDA' 'RACCT'            'N.Cta'                  ' ' ,      "   N. Cta.
*        7  'SKAT'       'TXT50'            'T_SAIDA' 'NOMBRE'           'Nombre'                 ' ' ,      "   Nombre
*        8  'FAGLFLEXA'  'HSL'              'T_SAIDA' 'DEBITOS'          'Debitos'                ' ' ,      "   Debitos
*        9  'FAGLFLEXA'  'HSL'              'T_SAIDA' 'CREDITOS'         'Creditos'               ' ' ,      "   Creditos
*       10  'FAGLFLEXA'  'RCNTR'            'T_SAIDA' 'C_COSTO'          'C. Costo'               ' ' ,      "   C. Costo
*       11  ' '          ' '                'T_SAIDA' 'A_NEGOCIO'        'A.Negocio'              ' ' ,      "   A. negocio
*       12  ' '          ' '                'T_SAIDA' 'CONCEPTO'         'Concepto'               ' ' .      "   Concetpo
*ENDFORM.                    " MONTAR_LAYOUT
**&---------------------------------------------------------------------*
**&      Form  MONTAR_ESTRUTURA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_1      text
**      -->P_0321   text
**      -->P_0322   text
**      -->P_0323   text
**      -->P_0324   text
**      -->P_0325   text
**      -->P_0326   text
**----------------------------------------------------------------------*
*FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
*                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
*                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
*                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
*                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
*                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
*                            VALUE(P_OUTPUTLEN).
*
*  CLEAR: WA_ESTRUTURA.
*
*
*  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
*  WA_ESTRUTURA-TABNAME       = P_TABNAME.
*  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
*  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
*  WA_ESTRUTURA-KEY           = ' '.
*  WA_ESTRUTURA-KEY_SEL       = 'X'.
*  WA_ESTRUTURA-COL_POS       = P_COL_POS.
*  WA_ESTRUTURA-NO_OUT        = ' '.
*  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
*  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
*  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
*
*  IF P_SCRTEXT_L IS NOT INITIAL.
*    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
*  ENDIF.
*
**  IF P_FIELD EQ 'BELNR'
**    OR P_FIELD EQ 'DOC_VARIACAO'.
**    WA_ESTRUTURA-HOTSPOT = 'X'.
**  ENDIF.
*
*  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
*  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
*  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
*  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.
*
*  APPEND WA_ESTRUTURA TO ESTRUTURA.
*
*ENDFORM.                    " MONTAR_ESTRUTURA
**---------------------------------------------------------------------*
**       FORM XUSER_COMMAND                                            *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM XUSER_COMMAND USING UCOMM    LIKE SY-UCOMM
*                         SELFIELD TYPE KKBLO_SELFIELD..     "#EC CALLED
*
*  DATA: VL_FORMNAME           TYPE TDSFNAME,
*        VL_NAME               TYPE RS38L_FNAM,
*        LS_CONTROL            TYPE SSFCTRLOP,
*        LS_OPTIONS            TYPE SSFCOMPOP,
*        JOB_OUTPUT_INFO       TYPE SSFCRESCL,
*        JOB_OUTPUT_OPTIONS    TYPE SSFCRESOP,
*        WL_DIALOG.
**
**
**  CASE SY-UCOMM.
**    WHEN '&LIB_DIAR'.
**      VL_FORMNAME = 'ZLIBRO_DIARIO'.
**
**      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
**        EXPORTING
**          FORMNAME           = VL_FORMNAME
**        IMPORTING
**          FM_NAME            = VL_NAME
**        EXCEPTIONS
**          NO_FORM            = 1
**          NO_FUNCTION_MODULE = 2
**          OTHERS             = 3.
**
**        CALL FUNCTION VL_NAME
**          EXPORTING
**            USER_SETTINGS      = ' '
**            CONTROL_PARAMETERS = LS_CONTROL
**            OUTPUT_OPTIONS     = LS_OPTIONS
**          IMPORTING
**            JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
**            JOB_OUTPUT_OPTIONS = JOB_OUTPUT_OPTIONS
**          EXCEPTIONS
**            FORMATTING_ERROR   = 1
**            INTERNAL_ERROR     = 2
**            SEND_ERROR         = 3
**            USER_CANCELED      = 4
**            OTHERS             = 5.
**
**        IF SY-SUBRC <> 0.
**          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**        ENDIF.
****        WL_DIALOG = C_X.
**  ENDCASE.
**
*ENDFORM. "XUSER_COMMAND
**---------------------------------------------------------------------*
**       FORM x_top_of_page                                            *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM XTOP_OF_PAGE.                                          "#EC CALLED
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = T_TOP
*      I_LOGO             = ''.
*
*ENDFORM. "X_TOP_PAGE
**&---------------------------------------------------------------------*
**&      Form  F_CONSTRUIR_CABECALHO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_0181   text
**      -->P_TEXT_002  text
**----------------------------------------------------------------------*
*FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.
*
*  DATA: LS_LINE TYPE SLIS_LISTHEADER.
*  LS_LINE-TYP = TYP.
*  LS_LINE-INFO = TEXT.
*  APPEND LS_LINE TO T_TOP.
*
*ENDFORM.                    " F_CONSTRUIR_CABECALHO
**&---------------------------------------------------------------------*
**&      Form  INICIAR_VARIAVES
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM INICIAR_VARIAVEIS.
*
*  V_REPORT = SY-REPID.
*
*  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.
*
*ENDFORM.                    " INICIAR_VARIAVES
**---------------------------------------------------------------------*
**       FORM XPF_STATUS_SET                                            *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM XPF_STATUS_SET USING UCOMM TYPE KKBLO_T_EXTAB.         "#EC CALLED
*
*  SET PF-STATUS 'STANDARD_FULLSCREEN'.
*ENDFORM. "XPF_STATUS_SET
