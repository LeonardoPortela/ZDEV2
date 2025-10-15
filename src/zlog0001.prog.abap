*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi
*&--------------------------------------------------------------------&*
*& Projeto..: Tipcard/Neus
*& Data.....: 29.01.2014 13:27:29
*& Descrição: Report de Comunicação da Tipcard/Neus
*&--------------------------------------------------------------------&*

REPORT  ZLOG0001.

**********************************************************************
* TABLES
**********************************************************************
TABLES: ZLOG0001.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF TY_ZLOG0001,
            HORA        TYPE ZLOG0001-HORA,
            TIPO        TYPE ZLOG0001-TIPO,
            USUARIO     TYPE ZLOG0001-USUARIO,
            PROGRAMA    TYPE ZLOG0001-PROGRAMA,
            DATA        TYPE ZLOG0001-DATA,
            MSGNR       TYPE ZLOG0001-MSGNR,
            IP_ADDRESS  TYPE ZLOG0001-IP_ADDRESS,
            DEPARTMENT  TYPE ZLOG0001-DEPARTMENT,
            URL         TYPE ZLOG0001-URL,
       END OF TY_ZLOG0001,

        BEGIN OF TY_T100,
          SPRSL TYPE T100-SPRSL,
          ARBGB TYPE T100-ARBGB,
          MSGNR TYPE T100-MSGNR,
          TEXT  TYPE T100-TEXT,
        END OF TY_T100,

        BEGIN OF TY_SAIDA,
             HORA        TYPE ZLOG0001-HORA,
             TIPO        TYPE ZLOG0001-TIPO,
             USUARIO     TYPE ZLOG0001-USUARIO,
             PROGRAMA    TYPE ZLOG0001-PROGRAMA,
             DATA        TYPE ZLOG0001-DATA,
             MSGNR       TYPE ZLOG0001-MSGNR,
             IP_ADDRESS  TYPE ZLOG0001-IP_ADDRESS,
             DEPARTMENT  TYPE ZLOG0001-DEPARTMENT,
             URL         TYPE ZLOG0001-URL,
             TEXT        TYPE T100-TEXT,
        END OF TY_SAIDA.

**********************************************************************
* Variaveis
**********************************************************************
DATA: QTD_ERRO TYPE SY-TABIX.

**********************************************************************
* ICONES
**********************************************************************
DATA: ICON_QTD TYPE C LENGTH 4.

**********************************************************************
* TABLES
**********************************************************************
DATA: GT_ZLOG0001 TYPE TABLE OF TY_ZLOG0001,
      GT_T100     TYPE TABLE OF TY_T100,
      GT_SAIDA    TYPE TABLE OF TY_SAIDA,
      GT_FCAT     TYPE LVC_T_FCAT.

**********************************************************************
* WORK AREA
**********************************************************************
DATA: GW_ZLOG0001 TYPE TY_ZLOG0001,
      GW_T100     TYPE TY_T100,
      GW_SAIDA    TYPE TY_SAIDA,
      GW_FCAT     TYPE LVC_S_FCAT.
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
* Seleção
**********************************************************************

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_DATA FOR ZLOG0001-DATA. "OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.
**********************************************************************
* Performs.
**********************************************************************

  PERFORM: SELECAO_DADOS,
           MONTA_ALV.

  "Chamar Tela 0100.

  CALL SCREEN TELA_0100.

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'PF0100'. "Status da Tela 0100.
  SET TITLEBAR  'TB0100'. "Title da Tela 0100
ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*

MODULE PAI INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'. "Botão para sair do programa corrente.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT

*&---------------------------------------------------------------------*
*&      Form  SELECAO_DADOS
*&---------------------------------------------------------------------*
FORM SELECAO_DADOS .

  ICON_QTD = ICON_SUM.

  REFRESH: GT_ZLOG0001[], GT_T100[], GT_SAIDA[].

  SELECT HORA TIPO USUARIO PROGRAMA DATA MSGNR IP_ADDRESS DEPARTMENT URL
       FROM ZLOG0001
    INTO TABLE GT_ZLOG0001
  WHERE DATA IN P_DATA.

  CHECK NOT GT_ZLOG0001[] IS INITIAL.

  SELECT SPRSL ARBGB MSGNR TEXT
    FROM T100
    INTO TABLE GT_T100
   FOR ALL ENTRIES IN GT_ZLOG0001
    WHERE SPRSL EQ 'PT'
      AND ARBGB EQ 'ZSIMETRYA'
      AND MSGNR EQ GT_ZLOG0001-MSGNR+1(3).

  CLEAR: QTD_ERRO.

  LOOP AT GT_ZLOG0001 INTO GW_ZLOG0001.

    GW_SAIDA-HORA        = GW_ZLOG0001-HORA.
    GW_SAIDA-TIPO        = GW_ZLOG0001-TIPO.
    GW_SAIDA-USUARIO     = GW_ZLOG0001-USUARIO.
    GW_SAIDA-PROGRAMA    = GW_ZLOG0001-PROGRAMA.
    GW_SAIDA-DATA        = GW_ZLOG0001-DATA.
    GW_SAIDA-MSGNR       = GW_ZLOG0001-MSGNR.
    GW_SAIDA-IP_ADDRESS  = GW_ZLOG0001-IP_ADDRESS.
    GW_SAIDA-DEPARTMENT  = GW_ZLOG0001-DEPARTMENT.
    GW_SAIDA-URL         = GW_ZLOG0001-URL.

    READ TABLE GT_T100 INTO GW_T100 WITH KEY SPRSL = 'PT'
                                             ARBGB = 'ZSIMETRYA'
                                             MSGNR = GW_ZLOG0001-MSGNR+1(3).

    GW_SAIDA-TEXT = GW_T100-TEXT.

    APPEND GW_SAIDA TO GT_SAIDA.
    CLEAR: GW_SAIDA, GW_ZLOG0001, GW_T100.

    QTD_ERRO = QTD_ERRO + 1.
  ENDLOOP.

ENDFORM.                    " SELECAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_ALV
*&---------------------------------------------------------------------*
FORM MONTA_ALV .

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

ENDFORM.                    " MONTA_ALV
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG .

  REFRESH: GT_FCAT[].

  PERFORM MONTAR_CATALOG USING:

    'HORA'               'Hora'                '8'    '' '' '' '' '' ''  '' '' '' '' ,
    'TIPO'               'Request/Response'    '12'   '' '' '' '' '' ''  '' '' '' '' ,
    'USUARIO'            'Usuário'             '10'   '' '' '' '' '' ''  '' '' '' '' ,
    'DEPARTMENT'         'Departamento'        '10'   '' '' '' '' '' ''  '' '' '' '' ,
    'PROGRAMA'           'Programa'            '10'   '' '' '' '' '' ''  '' '' '' '' ,
    'DATA'               'Data'                '10'   '' '' '' '' '' ''  '' '' '' '' ,
    'MSGNR'              'Cod. Mensagem'       '13'    '' '' '' '' '' ''  '' '' '' '' ,
    'TEXT'               'Mensagem'            '50'   '' '' '' '' '' ''  '' '' '' '' ,
    'IP_ADDRESS'         'IP Servidor'         '10'   '' '' '' '' '' ''  '' '' '' '' ,
    'URL'                'Endereço/URL'        '50'    '' '' '' '' '' ''  '' '' '' '' .


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
