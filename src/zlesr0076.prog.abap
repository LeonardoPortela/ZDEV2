*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
********************************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                                     *
* Data desenv ...: 07.12.2011                                                              *
* Objetivo    ...: Relatório de Comparativo de Saída e Chegada - MODAL FERROVIÁRIO         *
* Transação   ...: ZLES0085                                                                *
* Autor       ...: Victor Hugo                                                             *
********************************************************************************************
REPORT  ZLESR0076.

TYPE-POOLS: VRM, ICON.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZLEST0065, ZSDT0001, ZIB_NFE_DIST_TER.

**----------------------------------------------------------------------*
* Estrutura
**----------------------------------------------------------------------*

TYPES:
      BEGIN OF TY_SAIDA,
       LIFNR        TYPE LFA1-LIFNR,
       STCD1        TYPE LFA1-STCD1,
       NAME1        TYPE LFA1-NAME1,
       BRANCH       TYPE ZSDT0001-BRANCH,
       NFNUM        TYPE ZSDT0001-NFNUM,
       SERIES       TYPE ZSDT0001-SERIES,
       DOCDAT       TYPE ZSDT0001-DOCDAT,
       NR_ROMANEIO  TYPE ZSDT0001-NR_ROMANEIO,
       DT_MOVIMENTO TYPE ZSDT0001-DT_MOVIMENTO,
       STATUS       TYPE C LENGTH 4,
       STATUS_C     TYPE C LENGTH 1,
       END OF TY_SAIDA.

**----------------------------------------------------------------------*
* TABELAS INTERNA
**----------------------------------------------------------------------*
DATA: IT_ZSDT0001         TYPE TABLE OF ZSDT0001,
      "IT_ZLEST0065        TYPE TABLE OF ZLEST0065,
      IT_ZIB_NFE_DIST_TER TYPE TABLE OF ZIB_NFE_DIST_TER,
      IT_LFA1             TYPE TABLE OF LFA1,
      IT_SAIDA            TYPE TABLE OF TY_SAIDA,
      IT_FCAT             TYPE TABLE OF LVC_S_FCAT.

DATA: GW_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GW_GRID        TYPE REF TO  CL_GUI_ALV_GRID.


**----------------------------------------------------------------------*
** WORK AREA
**----------------------------------------------------------------------*
DATA: WA_ZSDT0001         TYPE ZSDT0001,
      "WA_ZLEST0065        TYPE ZLEST0065,
      WA_ZIB_NFE_DIST_TER TYPE ZIB_NFE_DIST_TER,
      WA_LFA1             TYPE LFA1,
      WA_SAIDA            TYPE TY_SAIDA.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
                P_CNPJ FOR ZLEST0065-STCD1 NO INTERVALS OBLIGATORY,
                "P_CNPJ FOR ZIB_NFE_DIST_TER-FORNE_CNPJ NO INTERVALS OBLIGATORY,
                P_DATA FOR ZSDT0001-DT_MOVIMENTO OBLIGATORY NO-EXTENSION,
                P_NOTA FOR ZSDT0001-NFNUM.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-002.
PARAMETERS:
             COM_XML    TYPE CHAR1  RADIOBUTTON GROUP RB02 MODIF ID LES,
             SEM_XML    TYPE CHAR1  RADIOBUTTON GROUP RB02 MODIF ID LES,
             TODOS_MM   TYPE CHAR1  RADIOBUTTON GROUP RB02 DEFAULT 'X' MODIF ID LES.
SELECTION-SCREEN: END OF BLOCK B4.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:
              F_SELECIONA_DADOS, " Form seleciona dados
              F_SAIDA            . "Saida

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR
         'CANC' OR
         'EXIT'  .
      LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
  ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT
**&---------------------------------------------------------------------*
**&      Form  F_SELECIONA_DADOS
**&---------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.

  SELECT * FROM LFA1
    INTO TABLE IT_LFA1
   WHERE STCD1 IN P_CNPJ.

  CHECK NOT IT_LFA1[] IS INITIAL.

  SELECT * FROM ZSDT0001
    INTO TABLE IT_ZSDT0001
    FOR ALL ENTRIES IN IT_LFA1
  WHERE PARID        EQ IT_LFA1-LIFNR
    AND DT_MOVIMENTO IN P_DATA.

  CHECK NOT IT_ZSDT0001[] IS INITIAL.


  REFRESH IT_ZIB_NFE_DIST_TER.
  CLEAR: WA_ZIB_NFE_DIST_TER, WA_ZSDT0001.

  LOOP AT IT_ZSDT0001 INTO WA_ZSDT0001.

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZSDT0001-PARID.

    CLEAR: WA_ZIB_NFE_DIST_TER.
    SELECT SINGLE *
      INTO WA_ZIB_NFE_DIST_TER
      FROM ZIB_NFE_DIST_TER
     WHERE FORNE_CNPJ = WA_LFA1-STCD1
       AND NUMERO     = WA_ZSDT0001-NFNUM
       AND SERIE      = WA_ZSDT0001-SERIES.

    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    APPEND WA_ZIB_NFE_DIST_TER TO IT_ZIB_NFE_DIST_TER.

    CLEAR: WA_ZSDT0001.

  ENDLOOP.


*  SELECT * FROM ZLEST0065
*    INTO TABLE IT_ZLEST0065
*    FOR ALL ENTRIES IN IT_ZSDT0001
*  WHERE CH_REFERENCIA EQ IT_ZSDT0001-CH_REFERENCIA.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
FORM F_SAIDA.

  LOOP AT IT_ZSDT0001 INTO WA_ZSDT0001.

    WA_SAIDA-NFNUM        = WA_ZSDT0001-NFNUM.
    WA_SAIDA-NR_ROMANEIO  = WA_ZSDT0001-NR_ROMANEIO.
    WA_SAIDA-SERIES       = WA_ZSDT0001-SERIES.
    WA_SAIDA-DOCDAT       = WA_ZSDT0001-DOCDAT.
    WA_SAIDA-BRANCH       = WA_ZSDT0001-BRANCH.
    WA_SAIDA-DT_MOVIMENTO = WA_ZSDT0001-DT_MOVIMENTO.

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZSDT0001-PARID.
    WA_SAIDA-LIFNR = WA_LFA1-LIFNR.
    WA_SAIDA-STCD1 = WA_LFA1-STCD1.
    WA_SAIDA-NAME1 = WA_LFA1-NAME1.

    "READ TABLE IT_ZLEST0065 INTO WA_ZLEST0065 WITH KEY CH_REFERENCIA = WA_ZSDT0001-CH_REFERENCIA.

    READ TABLE IT_ZIB_NFE_DIST_TER INTO WA_ZIB_NFE_DIST_TER WITH KEY FORNE_CNPJ = WA_SAIDA-STCD1
                                                                     NUMERO     = WA_ZSDT0001-NFNUM
                                                                     SERIE      = WA_ZSDT0001-SERIES.

    CASE SY-SUBRC.
      WHEN: '0'.
        WA_SAIDA-STATUS   = ICON_LED_GREEN.
        WA_SAIDA-STATUS_C = 1.
      WHEN OTHERS.
        WA_SAIDA-STATUS = ICON_LED_RED.
        WA_SAIDA-STATUS_C = 0.
    ENDCASE.

    APPEND WA_SAIDA TO IT_SAIDA.
  ENDLOOP.

  CHECK NOT IT_SAIDA[] IS INITIAL.

  IF ( COM_XML  EQ 'X' ).
    DELETE IT_SAIDA WHERE STATUS_C NE '1'.
  ELSEIF ( SEM_XML EQ 'X' ).
    DELETE IT_SAIDA WHERE STATUS_C NE '0'.
  ENDIF.

  PERFORM: F_ALV,
          Z_EXIBE_ALV.

  CALL SCREEN 0100.
ENDFORM.                    "F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM F_ALV .

  PERFORM ALV_PREENCHE_CAT USING:
      'LIFNR'           'Cod. Forn.'     '12'  '' 'X'  '', " Código do Fornecedor
      'NAME1'           'Desc.Forn.'     '20'  '' ''  '', " Desc. Forn.
      'STCD1'           'CNPJ'           '12'  '' ''  '', " CNPJ
      'BRANCH'          'Centro'         '6'   '' ''  '', " Centro
      'NFNUM'           'Nf-e'           '10'  '' ''  '', " Nota Fiscal
      'SERIES'          'Serie'          '5'   '' ''  '', " Serie
      'DOCDAT'          'Dt. Emissão'    '10'  '' ''  '', " Data de Emissão.
      'DT_MOVIMENTO'    'Dt. Movimento.' '10'  '' ''  '', " Data de Emissão.
      'NR_ROMANEIO'     'Romaneio'       '6'   '' ''  '', " Nr. Romaneio
      'STATUS'          'Status'         '5'   '' ''  ''. " Status




ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT USING    P_CAMPO TYPE C
                               P_DESC  TYPE C
                               P_TAM   TYPE C
                               P_HOT   TYPE C
                               P_ZERO  TYPE C
                               P_SUM   TYPE C.

  DATA: WL_FCAT TYPE LVC_S_FCAT.

  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-SCRTEXT_M = P_DESC.
  WL_FCAT-SCRTEXT_S = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-DO_SUM   =  P_SUM.
  WL_FCAT-OUTPUTLEN = P_TAM.

  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM Z_EXIBE_ALV .

  IF GW_CONTAINER IS INITIAL.

    CREATE OBJECT GW_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'ALV_CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT GW_GRID
      EXPORTING
        I_PARENT          = GW_CONTAINER
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    CALL METHOD GW_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*    EXPORTING
*      IS_LAYOUT                     = WA_LAYOUT
*      I_STRUCTURE_NAME              = 'IT_SAIDA'
      CHANGING
        IT_OUTTAB                     = IT_SAIDA
        IT_FIELDCATALOG               = IT_FCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " Z_EXIBE_ALV
