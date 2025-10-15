*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| Relatório de Cadência - Solicitação de Venda                              |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Paulo Quevedo ( paulo.quevedo@grupomaggi.com.br )                    |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*
REPORT  ZSDR0043.

TABLES: ZSDT0055.

*----------------
* Estrutura
*----------------
TYPES: BEGIN OF TY_SAIDA,
        NRO_SOL_OV   TYPE ZSDT0055-NRO_SOL_OV,
        DATA_PROGR   TYPE ZSDT0055-DATA_PROGR,
        CADENCIA_QTE TYPE ZSDT0055-CADENCIA_QTE,
        ZIEME        TYPE ZSDT0055-ZIEME,
        USNAM        TYPE ZSDT0055-USNAM,
        DATA_ATUAL   TYPE ZSDT0055-DATA_ATUAL,
       END OF TY_SAIDA.

*----------------
* Internal Table
*----------------
DATA: GT_ZSDT0055 TYPE TABLE OF ZSDT0055,
      GT_SAIDA    TYPE TABLE OF TY_SAIDA.

*----------------
* Work Area
*----------------
DATA: GW_ZSDT0055 TYPE ZSDT0055,
      GW_SAIDA    TYPE TY_SAIDA.

*-----------------------------------
* ALV
*-----------------------------------
DATA: OBJ_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV       TYPE REF TO CL_GUI_ALV_GRID.

DATA: GT_FIELDCATALOG TYPE LVC_T_FCAT,
      GW_FIELDCATALOG TYPE LVC_S_FCAT.


SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               P_NR_SOL FOR ZSDT0055-NRO_SOL_OV NO INTERVALS, " Número de Solicitação
               P_DT_PRO FOR ZSDT0055-DATA_PROGR OBLIGATORY NO-EXTENSION. " Data da Programação
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.

  PERFORM: SELECIONAR_DADOS,
           CRIAR_ALV.

  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONAR_DADOS.

  SELECT * FROM ZSDT0055
    INTO TABLE GT_ZSDT0055
  WHERE NRO_SOL_OV IN P_NR_SOL
    AND DATA_PROGR IN P_DT_PRO.

  CHECK NOT GT_ZSDT0055[] IS INITIAL.

  LOOP AT  GT_ZSDT0055 INTO GW_ZSDT0055.

    GW_SAIDA-NRO_SOL_OV   = GW_ZSDT0055-NRO_SOL_OV.
    GW_SAIDA-DATA_PROGR   = GW_ZSDT0055-DATA_PROGR.
    GW_SAIDA-CADENCIA_QTE = GW_ZSDT0055-CADENCIA_QTE.
    GW_SAIDA-ZIEME        = GW_ZSDT0055-ZIEME.
    GW_SAIDA-USNAM        = GW_ZSDT0055-USNAM.
    GW_SAIDA-DATA_ATUAL   = GW_ZSDT0055-DATA_ATUAL.

    APPEND GW_SAIDA TO GT_SAIDA.


    CLEAR: GW_SAIDA, GW_ZSDT0055.
  ENDLOOP.


ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM CRIAR_ALV.

  DATA: WA_LAYOUT  TYPE LVC_S_LAYO,
        GS_VARIANT TYPE DISVARIANT.

  WA_LAYOUT-ZEBRA      = 'X'.

  CREATE OBJECT OBJ_CONTAINER
    EXPORTING
      CONTAINER_NAME              = 'CONTAINER'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  PERFORM: CATALOG.

  CREATE OBJECT OBJ_ALV
    EXPORTING
      I_PARENT          = OBJ_CONTAINER
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

  CALL METHOD OBJ_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = GT_SAIDA[]
      IT_FIELDCATALOG               = GT_FIELDCATALOG[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CRIAR_ALV
*&---------------------------------------------------------------------*
*&      Form  CATALOG
*&---------------------------------------------------------------------*
FORM CATALOG .

  PERFORM FIELDCATALOG USING:
      'NRO_SOL_OV'      'Nr. Solicitação'         '10'   'X' '' ' ' '' ' ',
      'DATA_PROGR'      'Data Programação'        '10'   '' '' ' ' '' ' ',
      'CADENCIA_QTE'    'Cadência'                '10'   '' '' ' ' '' ' ',
      'ZIEME'           'UM'                      '5'   '' '' ' ' '' ' ',
      'USNAM'           'Usuário'                 '10'   '' '' ' ' '' ' ',
      'DATA_ATUAL'      'Data Atual'              '10'   '' '' ' ' '' ' '.

ENDFORM.                    " CATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FIELDCATALOG   USING   VALUE(P_FIELDNAME)
                            VALUE(P_DESC)
                            VALUE(P_TAM)
                            VALUE(P_NO_ZERO)
                            VALUE(P_HOTSPOT)
                            VALUE(P_COR)
                            VALUE(P_JUST)
                            VALUE(P_SUM).

  GW_FIELDCATALOG-FIELDNAME = P_FIELDNAME.
  GW_FIELDCATALOG-SCRTEXT_L = P_DESC.
  GW_FIELDCATALOG-SCRTEXT_M = P_DESC.
  GW_FIELDCATALOG-SCRTEXT_S = P_DESC.
  GW_FIELDCATALOG-OUTPUTLEN = P_TAM.
  GW_FIELDCATALOG-NO_ZERO   = P_NO_ZERO.
  GW_FIELDCATALOG-HOTSPOT   = P_HOTSPOT.
  GW_FIELDCATALOG-EMPHASIZE = P_COR.
  GW_FIELDCATALOG-JUST      = P_JUST.
  GW_FIELDCATALOG-DO_SUM    = P_SUM.

  APPEND GW_FIELDCATALOG TO GT_FIELDCATALOG.

  CLEAR: GW_FIELDCATALOG.

ENDFORM.                    " FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'PS0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT
