*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: SD
*& Data.....: 20.01.2014 16:07:05
*& Descrição:  Report de log de alterações nas solicitações de Vendas.
*&--------------------------------------------------------------------&*
REPORT  ZSDR0039.

**********************************************************************
* TABLES
**********************************************************************
TABLES: ZSDT0083, ZSDT0051, ZSDT0053.

**********************************************************************
* CONSTANTES
**********************************************************************
CONSTANTS: TELA_0100 TYPE SY-DYNNR VALUE '0100'. "Tela Principal

**********************************************************************
* ALV
**********************************************************************
DATA: OBJ_CUSTOM    TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "Custom do ALV - Principal
      OBJ_GRID      TYPE REF TO CL_GUI_ALV_GRID. "Grid do ALV Principal


**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF TY_SAIDA,
        NRO_SOL_OV  TYPE ZSDT0083-NRO_SOL_OV,
        AREA        TYPE ZSDT0083-AREA,
        CAMPO       TYPE ZSDT0083-CAMPO,
        OLD_VALUE   TYPE ZSDT0083-OLD_VALUE,
        NEW_VALUE   TYPE ZSDT0083-NEW_VALUE,
        USNAM       TYPE ZSDT0083-USNAM,
        DATA_ATUAL  TYPE ZSDT0083-DATA_ATUAL,
        HORA_ATUAL  TYPE ZSDT0083-HORA_ATUAL,
        DESCRICAO   TYPE STRING,
       END OF TY_SAIDA.

**********************************************************************
* INTERNAL TABLE/WORK AREA.
**********************************************************************
DATA: GT_SAIDA    TYPE TABLE OF TY_SAIDA,
      GW_SAIDA    TYPE TY_SAIDA,
      GT_FCAT     TYPE LVC_T_FCAT,
      GW_FCAT     TYPE LVC_S_FCAT,
      GT_ZSDT0085 TYPE TABLE OF ZSDT0085,
      GW_ZSDT0085 TYPE ZSDT0085,
      GT_ZSDT0051 TYPE TABLE OF ZSDT0051,
      GW_ZSDT0051 TYPE ZSDT0051,
      GT_ZSDT0053 TYPE TABLE OF ZSDT0053,
      GW_ZSDT0053 TYPE ZSDT0053.


**********************************************************************
* Parametros.
* P_NRO  = Numero de Solicitação de Ordem de Venda
* P_DATA = Atualidade dos dados (data)
**********************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_NRO   FOR ZSDT0083-NRO_SOL_OV, "Numero de Solicitação de Ordem de Venda
                P_DATA  FOR ZSDT0083-DATA_ATUAL OBLIGATORY, "Atualidade dos dados (data)
                P_VKORG FOR ZSDT0051-VKORG NO INTERVALS,
                P_MATNR FOR ZSDT0053-MATNR NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK B1.


**********************************************************************
* START-OF-SELECTON.
**********************************************************************

START-OF-SELECTION.

  PERFORM: SELECIONAR_DADOS,
           CRIAR_ALV.
  "Chamar Tela 0100.
  CALL SCREEN TELA_0100.
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.

  SET PF-STATUS 'PF0100'. "Status da Tela 0100.
  SET TITLEBAR  'TB0100'. "Title da Tela 0100


ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'. "Botão para sair do programa corrente.
  ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT



*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  DATA: TL_ZSDT0083 TYPE TABLE OF ZSDT0083,
        WL_ZSDT0083 TYPE ZSDT0083.

  DATA: WA_GOTSTATE TYPE DDGOTSTATE,
        WA_DD01V    TYPE DD01V,
        VL_NAME     TYPE DDOBJNAME.

  "Selecionar Log
  SELECT * FROM ZSDT0083
    INTO TABLE TL_ZSDT0083
  WHERE NRO_SOL_OV IN P_NRO
    AND DATA_ATUAL IN P_DATA.

  CHECK NOT TL_ZSDT0083[] IS INITIAL.

  SELECT * FROM ZSDT0085
    INTO TABLE GT_ZSDT0085
    FOR ALL ENTRIES IN TL_ZSDT0083
  WHERE CAMPO EQ TL_ZSDT0083-CAMPO
    AND SPRAS EQ 'PT'.

 SELECT * FROM ZSDT0051
   INTO TABLE GT_ZSDT0051
   FOR ALL ENTRIES IN TL_ZSDT0083
 WHERE NRO_SOL_OV EQ TL_ZSDT0083-NRO_SOL_OV
   AND VKORG      IN P_VKORG.

 SELECT * FROM ZSDT0053
   INTO TABLE GT_ZSDT0053
   FOR ALL ENTRIES IN TL_ZSDT0083
 WHERE NRO_SOL_OV EQ TL_ZSDT0083-NRO_SOL_OV
   AND MATNR      IN P_MATNR.

  LOOP AT TL_ZSDT0083 INTO WL_ZSDT0083.

    READ TABLE GT_ZSDT0051 INTO GW_ZSDT0051 WITH KEY NRO_SOL_OV = WL_ZSDT0083-NRO_SOL_OV.
    IF ( SY-SUBRC NE 0 ).
      CLEAR: WL_ZSDT0083, GW_ZSDT0051.
      CONTINUE.
    ENDIF.

    READ TABLE GT_ZSDT0053 INTO GW_ZSDT0053 WITH KEY NRO_SOL_OV = WL_ZSDT0083-NRO_SOL_OV.
    IF ( SY-SUBRC NE 0 ).
      CLEAR: WL_ZSDT0083, GW_ZSDT0051, GW_ZSDT0053.
      CONTINUE.
    ENDIF.

    GW_SAIDA-NRO_SOL_OV  = WL_ZSDT0083-NRO_SOL_OV.
    GW_SAIDA-AREA        = WL_ZSDT0083-AREA.
    GW_SAIDA-CAMPO       = WL_ZSDT0083-CAMPO.
    GW_SAIDA-OLD_VALUE   = WL_ZSDT0083-OLD_VALUE.
    GW_SAIDA-NEW_VALUE   = WL_ZSDT0083-NEW_VALUE.
    GW_SAIDA-USNAM       = WL_ZSDT0083-USNAM.
    GW_SAIDA-DATA_ATUAL  = WL_ZSDT0083-DATA_ATUAL.
    GW_SAIDA-HORA_ATUAL  = WL_ZSDT0083-HORA_ATUAL.

    READ TABLE GT_ZSDT0085 INTO GW_ZSDT0085 WITH KEY CAMPO = WL_ZSDT0083-CAMPO
                                                     SPRAS = 'PT'.

    IF ( SY-SUBRC EQ 0 ).
      GW_SAIDA-DESCRICAO = GW_ZSDT0085-DESCRICAO.
    ELSE.

      CLEAR: VL_NAME.
      VL_NAME = WL_ZSDT0083-CAMPO.

      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          NAME          = VL_NAME
          STATE         = 'A'
          LANGU         = 'P'
        IMPORTING
          GOTSTATE      = WA_GOTSTATE
          DD01V_WA      = WA_DD01V
        EXCEPTIONS
          ILLEGAL_INPUT = 1
          OTHERS        = 2.

      IF WA_DD01V-DDTEXT IS INITIAL.
        GW_SAIDA-DESCRICAO = 'Campo não encontrado.'.
      ELSE.
        GW_SAIDA-DESCRICAO = WA_DD01V-DDTEXT.
      ENDIF.

    ENDIF.

    APPEND GW_SAIDA TO GT_SAIDA.

    CLEAR: GW_SAIDA, WL_ZSDT0083, WA_GOTSTATE, WA_DD01V.

  ENDLOOP.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM CRIAR_ALV .

  DATA:   WL_LAYOUT  TYPE LVC_S_LAYO,
          WL_VARIANT TYPE DISVARIANT.


  CREATE OBJECT OBJ_CUSTOM
    EXPORTING
      CONTAINER_NAME              = 'CONTAINER_PRINCIPAL'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  PERFORM: CRIAR_CATALOG.



  CREATE OBJECT OBJ_GRID
    EXPORTING
      I_PARENT          = OBJ_CUSTOM
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  WL_VARIANT-REPORT   = SY-REPID.
  WL_VARIANT-USERNAME = SY-UNAME.

  CALL METHOD OBJ_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WL_LAYOUT
      I_SAVE                        = 'A'
      IS_VARIANT                    = WL_VARIANT
    CHANGING
      IT_OUTTAB                     = GT_SAIDA[]
      IT_FIELDCATALOG               = GT_FCAT[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.


ENDFORM.                    " CRIAR_ALV

*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG .


  REFRESH: GT_FCAT[].

  PERFORM MONTAR_CATALOG USING:


  'NRO_SOL_OV'  'Nr. Solic.'    '11'   'X' '' '' '' '' ''  '' '' '' '' ,
  'AREA'        'Área'          '10'   '' '' '' '' '' ''  '' '' '' '' ,
  'CAMPO'       'Campo'         '20'   '' '' '' '' '' ''  '' '' '' '' ,
  'DESCRICAO'   'Descrição'     '20'   '' '' '' '' '' ''  '' '' '' '' ,
  'OLD_VALUE'   'Vlr. Anterior' '20'   '' '' '' '' '' ''  '' '' '' '' ,
  'NEW_VALUE'   'Vlr. Atual'    '20'   '' '' '' '' '' ''  '' '' '' '' ,
  'USNAM'       'Usuário'       '10'   '' '' '' '' '' ''  '' '' '' '' ,
  'DATA_ATUAL'  'Data'          '10'   '' '' '' '' '' ''  '' '' '' '' ,
  'HORA_ATUAL'  'Hora'          '8'    '' '' '' '' '' ''  '' '' '' '' .



ENDFORM.                    " CRIAR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG
*&---------------------------------------------------------------------*
FORM MONTAR_CATALOG   USING  VALUE(P_FIELDNAME)
                                     VALUE(P_DESC)
                                     VALUE(P_TAM)
                                     VALUE(P_NO_ZERO)
                                     VALUE(P_HOTSPOT)
                                     VALUE(P_COR)
                                     VALUE(P_JUST)
                                     VALUE(P_SUM)
                                     VALUE(P_EDIT)
                                     VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                                     VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                                     VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                                     VALUE(P_CHECK).

  CLEAR: GW_FCAT.

  GW_FCAT-FIELDNAME = P_FIELDNAME.
  GW_FCAT-REF_TABLE = P_REF_TABNAME..
  GW_FCAT-REF_FIELD = P_REF_FIELDNAME.
  GW_FCAT-TABNAME   = P_TABNAME.
  GW_FCAT-SCRTEXT_L = P_DESC.
  GW_FCAT-SCRTEXT_M = P_DESC.
  GW_FCAT-SCRTEXT_S = P_DESC.
  GW_FCAT-OUTPUTLEN = P_TAM.
  GW_FCAT-NO_ZERO   = P_NO_ZERO.
  GW_FCAT-HOTSPOT   = P_HOTSPOT.
  GW_FCAT-EMPHASIZE = P_COR.
  GW_FCAT-JUST      = P_JUST.
  GW_FCAT-DO_SUM    = P_SUM.
  GW_FCAT-EDIT      = P_EDIT.
  GW_FCAT-CHECKBOX  = P_CHECK.

  APPEND GW_FCAT TO GT_FCAT.
ENDFORM.                    " MONTAR_CATALOG
