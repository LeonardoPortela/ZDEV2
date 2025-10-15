*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: Aquaviário.
*& Data.....: 18.03.2014 16:07:27
*& Descrição: Acompanhamento do Arquivo de Descarga.
*&--------------------------------------------------------------------&*

REPORT  ZLESR0080.
**********************************************************************
*  TABLES
**********************************************************************
TABLES: ZLEST0080, ZLEST0082, ZLEST0039.

**********************************************************************
* Types
**********************************************************************
TYPES: BEGIN OF TY_SAIDA,
         CHAVE_NFE      TYPE ZLEST0080-CHAVE_NFE,
         ARQUIVO        TYPE ZLEST0080-ARQUIVO,
         DT_DESCARGA    TYPE ZLEST0080-DT_DESCARGA,
         HORA_DESCARGA  TYPE ZLEST0080-HORA_DESCARGA,

         PESO_DESCARGA  TYPE ZLEST0080-PESO_DESCARGA,
         CNPJ_DESTINO   TYPE ZLEST0080-CNPJ_DESTINO,
         NAME1_DEST     TYPE LFA1-NAME1,
         LIFNR          TYPE LFA1-LIFNR,
         ORT01_DEST     TYPE LFA1-ORT01,
         REGIO_DEST     TYPE LFA1-REGIO,

         LOCAL_DESCARGA TYPE ZLEST0080-LOCAL_DESCARGA,
         LOCAL_DESC     TYPE ZLEST0081-DESCRICAO,
         CNPJ_DESCARGA  TYPE ZLEST0080-CNPJ_DESTINO,
         NAME1_DESC     TYPE KNA1-NAME1,
         KUNNR          TYPE KNA1-KUNNR,
         ORT01_DESC     TYPE KNA1-ORT01,
         REGIO_DESC     TYPE KNA1-REGIO,
         STATUS         TYPE ZLEST0080-STATUS,
         STATUS_ICON    TYPE C LENGTH 4,
       END OF TY_SAIDA,

       BEGIN OF TY_SAIDA_ERRO,
         ARQUIVO   TYPE ZLEST0082-ARQUIVO,
         CHAVE_NFE TYPE ZLEST0082-CHAVE_NFE,
         MENSAGEM  TYPE ZLEST0082-MENSAGEM,
       END OF TY_SAIDA_ERRO.

**********************************************************************
* INTERNAL TABLE/WORK AREA
**********************************************************************
DATA: GT_ZLEST0080 TYPE TABLE OF ZLEST0080,
      GW_ZLEST0080 TYPE ZLEST0080,
      GT_ZLEST0082 TYPE TABLE OF ZLEST0082,
      GW_ZLEST0082 TYPE ZLEST0082,
      GT_ZLEST0081 TYPE TABLE OF ZLEST0081,
      GW_ZLEST0081 TYPE ZLEST0081,

      GT_KNA1      TYPE TABLE OF KNA1,
      GW_KNA1      TYPE KNA1,
      GT_LFA1      TYPE TABLE OF LFA1,
      GW_LFA1      TYPE LFA1.


DATA: GT_SAIDA      TYPE TABLE OF TY_SAIDA,
      GW_SAIDA      TYPE TY_SAIDA,
      GT_SAIDA_ERRO TYPE TABLE OF TY_SAIDA_ERRO,
      GW_SAIDA_ERRO TYPE TY_SAIDA_ERRO.

**********************************************************************
* ALV
**********************************************************************

DATA: GT_FIELDCATALOG      TYPE LVC_T_FCAT,
      GW_FIELDCATALOG      TYPE LVC_S_FCAT,

      GT_FIELDCATALOG_ERRO TYPE LVC_T_FCAT,
      GW_FIELDCATALOG_ERRO TYPE LVC_S_FCAT,

      GT_CUSTOM            TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GT_GRID              TYPE REF TO CL_GUI_ALV_GRID,

      GT_CUSTOM_ERRO       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GT_GRID_ERRO         TYPE REF TO CL_GUI_ALV_GRID.


**********************************************************************
* SELECTION SCREEN.
**********************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_ARQ    FOR ZLEST0080-ARQUIVO NO INTERVALS,
                P_CHAVE  FOR ZLEST0080-CHAVE_NFE NO INTERVALS,
                P_DATA   FOR ZLEST0080-DT_DESCARGA OBLIGATORY,
                P_LOCALD FOR ZLEST0080-LOCAL_DESCARGA NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
*       CLASS cl_handler DEFINITION
*----------------------------------------------------------------------*
CLASS CL_EVENT DEFINITION.
  PUBLIC SECTION.
    METHODS: HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "cl_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS CL_EVENT IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "cl_handler IMPLEMENTATION


START-OF-SELECTION.

**********************************************************************
* PERFORMS
**********************************************************************
  PERFORM: SELECIONAR_DADOS.
  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  "Seleciona todas as notas descarregadas.
  SELECT * FROM ZLEST0080
    INTO TABLE GT_ZLEST0080
  WHERE ARQUIVO        IN P_ARQ
    AND CHAVE_NFE      IN P_CHAVE
    AND DT_DESCARGA    IN P_DATA
    AND LOCAL_DESCARGA IN P_LOCALD.

  CHECK NOT GT_ZLEST0080[] IS INITIAL.

  SELECT * FROM ZLEST0082
    INTO TABLE GT_ZLEST0082
    FOR ALL ENTRIES IN GT_ZLEST0080
  WHERE ARQUIVO    EQ GT_ZLEST0080-ARQUIVO
    AND CHAVE_NFE  EQ GT_ZLEST0080-CHAVE_NFE.

  SELECT * FROM KNA1
    INTO TABLE GT_KNA1
    FOR ALL ENTRIES IN GT_ZLEST0080
  WHERE STCD1 EQ GT_ZLEST0080-CNPJ_DESCARGA.

  SELECT * FROM LFA1
    INTO TABLE GT_LFA1
    FOR ALL ENTRIES IN GT_ZLEST0080
  WHERE STCD1 EQ GT_ZLEST0080-CNPJ_DESTINO.

  SELECT * FROM ZLEST0081
    INTO TABLE GT_ZLEST0081
    FOR ALL ENTRIES IN GT_ZLEST0080
  WHERE LOCAL_DESCARGA EQ GT_ZLEST0080-LOCAL_DESCARGA.


  LOOP AT GT_ZLEST0080 INTO GW_ZLEST0080.

    GW_SAIDA-CHAVE_NFE      = GW_ZLEST0080-CHAVE_NFE.
    GW_SAIDA-ARQUIVO        = GW_ZLEST0080-ARQUIVO.
    GW_SAIDA-DT_DESCARGA    = GW_ZLEST0080-DT_DESCARGA.
    GW_SAIDA-HORA_DESCARGA  = GW_ZLEST0080-HORA_DESCARGA.
    GW_SAIDA-PESO_DESCARGA  = GW_ZLEST0080-PESO_DESCARGA.
    GW_SAIDA-CNPJ_DESTINO   = GW_ZLEST0080-CNPJ_DESTINO.

    READ TABLE GT_LFA1 INTO GW_LFA1 WITH KEY STCD1 = GW_SAIDA-CNPJ_DESTINO.
    GW_SAIDA-NAME1_DEST = GW_LFA1-NAME1.
    GW_SAIDA-LIFNR      = GW_LFA1-LIFNR.
    GW_SAIDA-ORT01_DEST = GW_LFA1-ORT01.
    GW_SAIDA-REGIO_DEST = GW_LFA1-REGIO.

    GW_SAIDA-LOCAL_DESCARGA = GW_ZLEST0080-LOCAL_DESCARGA.
    READ TABLE GT_ZLEST0081 INTO GW_ZLEST0081 WITH KEY LOCAL_DESCARGA = GW_SAIDA-LOCAL_DESCARGA.
    GW_SAIDA-LOCAL_DESC = GW_ZLEST0081-DESCRICAO.

    GW_SAIDA-CNPJ_DESCARGA  = GW_ZLEST0080-CNPJ_DESCARGA.

    READ TABLE GT_KNA1 INTO GW_KNA1 WITH KEY STCD1 = GW_SAIDA-CNPJ_DESCARGA.
    GW_SAIDA-NAME1_DESC = GW_KNA1-NAME1.
    GW_SAIDA-KUNNR      = GW_KNA1-KUNNR.
    GW_SAIDA-ORT01_DESC = GW_KNA1-ORT01.
    GW_SAIDA-REGIO_DESC = GW_KNA1-REGIO.

    GW_SAIDA-STATUS         = GW_ZLEST0080-STATUS.

    CASE GW_SAIDA-STATUS.
      WHEN: 'E'.
        GW_SAIDA-STATUS_ICON = ICON_OBJECT_LIST.
      WHEN  'L'.
        GW_SAIDA-STATUS_ICON = ICON_INSERT_RELATION.
      WHEN OTHERS.
        GW_SAIDA-STATUS_ICON = ICON_CHECKED.
    ENDCASE.

    APPEND GW_SAIDA TO GT_SAIDA.

    CLEAR: GW_SAIDA, GW_LFA1, GW_KNA1, GW_ZLEST0081.

  ENDLOOP.

  IF NOT GT_SAIDA[] IS INITIAL.
    PERFORM: MONTAR_CATALOG,
             CRIAR_ALV.
  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG
*&---------------------------------------------------------------------*
FORM MONTAR_CATALOG .

  PERFORM FIELD_CAT USING:

  'STATUS_ICON'     'Status'         '5'  '' 'X' '' 'C' '',
  'CHAVE_NFE'       'Chave Nf-e'     '45' '' '' '' '' '',
  'ARQUIVO'         'Arquivo'        '25' '' '' '' '' '',
  'DT_DESCARGA'     'Dt. Descarga'   '10' '' '' '' '' '',
  'HORA_DESCARGA'   'Hr. Descarga'   '8'  '' '' '' '' '',
  'PESO_DESCARGA'   'Peso Descarga'  '10' '' '' '' '' '',
  'CNPJ_DESTINO'    'CNPJ Destino'   '14' '' '' '' '' '',
  'NAME1_DEST'      'Destino'        '10' '' '' '' '' '',
  'ORT01_DEST'      'Municipio'      '8' '' '' '' '' '',
  'REGIO_DEST'      'UF'             '3' '' '' '' '' '',

  'LOCAL_DESCARGA'  'Local Descarga' '14' '' '' '' 'C' '',
  'LOCAL_DESC'      'Descrição'      '10' '' '' '' 'C' '',
  'CNPJ_DESCARGA'   'CNPJ Descarga'  '14' '' '' '' '' '',
  'NAME1_DESC'      'Descarga'       '10' '' '' '' '' '',
  'ORT01_DESC'      'Municipio'      '8' '' '' '' '' '',
  'REGIO_DESC'      'UF'             '3' '' '' '' '' ''.


ENDFORM.                    " MONTAR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM CRIAR_ALV.

  CONSTANTS: C_A  TYPE C VALUE 'A'.

  DATA: WA_LAYOUT TYPE LVC_S_LAYO.
  DATA: GR_EVENT TYPE REF TO CL_EVENT.

  CREATE OBJECT GT_CUSTOM
    EXPORTING
      CONTAINER_NAME              = 'CONTAINER'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  CREATE OBJECT GT_GRID
    EXPORTING
      I_PARENT          = GT_CUSTOM
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.


  CREATE OBJECT GR_EVENT.
  SET HANDLER GR_EVENT->HANDLE_HOTSPOT_CLICK FOR GT_GRID.

  CALL METHOD GT_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      I_SAVE                        = C_A
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
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
FORM FIELD_CAT  USING       VALUE(P_FIELDNAME)
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

ENDFORM.                    " FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING    I_ROW_ID     TYPE LVC_S_ROW
                                    I_COLUMN_ID  TYPE LVC_S_COL
                                    IS_ROW_NO    TYPE LVC_S_ROID.

  DATA: LT_ZLEST0039 TYPE TABLE OF ZLEST0039,
        LW_ZLEST0039 TYPE ZLEST0039.

  DATA: WA_STABLE TYPE LVC_S_STBL.

  CLEAR: GW_SAIDA.

  READ TABLE GT_SAIDA INTO GW_SAIDA INDEX I_ROW_ID.

  IF ( SY-SUBRC EQ 0 ).

    CASE GW_SAIDA-STATUS.
      WHEN: 'E'.
        REFRESH: GT_SAIDA_ERRO[].
        CLEAR: GW_SAIDA_ERRO.

        LOOP AT GT_ZLEST0082 INTO GW_ZLEST0082 WHERE ARQUIVO EQ GW_SAIDA-ARQUIVO.

          GW_SAIDA_ERRO-ARQUIVO   = GW_ZLEST0082-ARQUIVO.
          GW_SAIDA_ERRO-CHAVE_NFE = GW_ZLEST0082-CHAVE_NFE.
          GW_SAIDA_ERRO-MENSAGEM  = GW_ZLEST0082-MENSAGEM.

          APPEND GW_SAIDA_ERRO TO GT_SAIDA_ERRO.

          CLEAR: GW_SAIDA_ERRO, GW_ZLEST0082.

        ENDLOOP.

        CALL SCREEN 1200 STARTING AT 010 2 ENDING   AT 163 30.

      WHEN: 'L'.

        SELECT * FROM ZLEST0039
          INTO TABLE LT_ZLEST0039
       WHERE PONTOTRANSB  EQ GW_SAIDA-KUNNR
         AND PONTOENTREGA EQ GW_SAIDA-LIFNR
         AND CHAVE_NFE    EQ GW_SAIDA-CHAVE_NFE.

        IF ( SY-SUBRC EQ 0 ).

          READ TABLE LT_ZLEST0039 INTO DATA(WA_ZLEST0039) INDEX 1.

          DATA(LW_ZLEST0039_AJUSTE) = WA_ZLEST0039.
          LW_ZLEST0039_AJUSTE-DATATRANSB = GW_SAIDA-DT_DESCARGA.
          LW_ZLEST0039_AJUSTE-PESOTRANSB = GW_SAIDA-PESO_DESCARGA.
          PERFORM VERIFICAR_CARGUERO IN PROGRAM ZLESI0005 USING WA_ZLEST0039 LW_ZLEST0039_AJUSTE IF FOUND.
          CLEAR: LW_ZLEST0039_AJUSTE.

          UPDATE ZLEST0039 SET DATATRANSB = GW_SAIDA-DT_DESCARGA
                               PESOTRANSB = GW_SAIDA-PESO_DESCARGA
                               WHERE PONTOTRANSB EQ GW_SAIDA-KUNNR
                               AND PONTOENTREGA  EQ GW_SAIDA-LIFNR
                               AND CHAVE_NFE     EQ GW_SAIDA-CHAVE_NFE.


          IF ( SY-SUBRC EQ 0 ).

            COMMIT WORK.



            UPDATE ZLEST0080 SET STATUS      = 'P'
                                 USUARIO     = SY-UNAME
                                 DT_CADASTRO = SY-DATUM
                                 WHERE CHAVE_NFE EQ GW_SAIDA-CHAVE_NFE
                                   AND ARQUIVO   EQ GW_SAIDA-ARQUIVO.

            COMMIT WORK.

            GW_SAIDA-STATUS_ICON = ICON_CHECKED.
            GW_SAIDA-STATUS      = 'P'.

            MODIFY GT_SAIDA FROM GW_SAIDA INDEX I_ROW_ID TRANSPORTING STATUS_ICON STATUS.

            CALL METHOD GT_GRID->REFRESH_TABLE_DISPLAY
              EXPORTING
                IS_STABLE = WA_STABLE.

          ENDIF.

        ELSE.
          MESSAGE S000(ZQUAVIARIO) DISPLAY LIKE 'W' WITH 'Registro não encontrado na ZLEST0039'.
        ENDIF.
    ENDCASE.

  ELSE.
    MESSAGE S000(ZQUAVIARIO) DISPLAY LIKE 'W' WITH 'Selecionar uma linha valida.'.
  ENDIF.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Module  PBO_1200  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_1200 OUTPUT.
  SET PF-STATUS 'PF01200'.
  SET TITLEBAR  'TB01200'.

  PERFORM: CATALOG_LOG_ERRO,
           ALV_LOG_ERRO.
ENDMODULE.                 " PBO_1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_1200  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_1200 INPUT.
  CASE SY-UCOMM.
    WHEN: 'EXIT'.
      REFRESH: GT_SAIDA_ERRO[].
      CLEAR: GW_SAIDA_ERRO.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " PAI_1200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CATALOG_LOG_ERRO
*&---------------------------------------------------------------------*
FORM CATALOG_LOG_ERRO .


  PERFORM FIELD_CAT_ERRO USING:
    'ARQUIVO'     'Arquivo'         '25'  '' '' '' '' '',
    'CHAVE_NFE'   'Chave Nf-e'      '44'  '' '' '' '' '',
    'MENSAGEM'    'Arquivo'         '80'  '' '' '' '' ''.

ENDFORM.                    " CATALOG_LOG_ERRO

*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT_ERRO
*&---------------------------------------------------------------------*
FORM FIELD_CAT_ERRO  USING  VALUE(P_FIELDNAME)
                            VALUE(P_DESC)
                            VALUE(P_TAM)
                            VALUE(P_NO_ZERO)
                            VALUE(P_HOTSPOT)
                            VALUE(P_COR)
                            VALUE(P_JUST)
                            VALUE(P_SUM).

  GW_FIELDCATALOG_ERRO-FIELDNAME = P_FIELDNAME.
  GW_FIELDCATALOG_ERRO-SCRTEXT_L = P_DESC.
  GW_FIELDCATALOG_ERRO-SCRTEXT_M = P_DESC.
  GW_FIELDCATALOG_ERRO-SCRTEXT_S = P_DESC.
  GW_FIELDCATALOG_ERRO-OUTPUTLEN = P_TAM.
  GW_FIELDCATALOG_ERRO-NO_ZERO   = P_NO_ZERO.
  GW_FIELDCATALOG_ERRO-HOTSPOT   = P_HOTSPOT.
  GW_FIELDCATALOG_ERRO-EMPHASIZE = P_COR.
  GW_FIELDCATALOG_ERRO-JUST      = P_JUST.
  GW_FIELDCATALOG_ERRO-DO_SUM    = P_SUM.

  APPEND GW_FIELDCATALOG_ERRO TO GT_FIELDCATALOG_ERRO.

  CLEAR: GW_FIELDCATALOG_ERRO.

ENDFORM.                    " FIELD_CAT_ERRO

*&---------------------------------------------------------------------*
*&      Form  ALV_LOG_ERRO
*&---------------------------------------------------------------------*
FORM ALV_LOG_ERRO .


  CONSTANTS: C_A  TYPE C VALUE 'A'.

  DATA: WA_LAYOUT TYPE LVC_S_LAYO.
  DATA: WA_STABLE TYPE LVC_S_STBL.


  IF ( GT_CUSTOM_ERRO IS INITIAL ).
    CREATE OBJECT GT_CUSTOM_ERRO
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_ERRO'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT GT_GRID_ERRO
      EXPORTING
        I_PARENT          = GT_CUSTOM_ERRO
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    CALL METHOD GT_GRID_ERRO->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
        I_SAVE                        = C_A
      CHANGING
        IT_OUTTAB                     = GT_SAIDA_ERRO[]
        IT_FIELDCATALOG               = GT_FIELDCATALOG_ERRO[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.

    CALL METHOD GT_GRID_ERRO->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.

ENDFORM.                    " ALV_LOG_ERRO
