*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_PBO_0110
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_0110  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0110 OUTPUT.
  DATA OBJ_TOOLBAR_0110 TYPE REF TO LCL_EVENT_HANDLER.
  CLEAR: GT_FCAT_0110, WL_LAYOUT.

  PERFORM ALV_PREENCHE_CAT USING:
'CHECK'       ''                  ''  '' '' '' 'X' 'X' '' '' '' '' '' '' 'X',
'BUKRS'       'Empresa'           '7'  '' '' '' ''  ''  '' '' '' '' '' '' '',
'FILIAL'      'Filial'            '6'  '' '' '' ''  ''  '' '' '' '' '' '' '',
'ANLN1'       'Imobilizado'       '12' '' 'X' '' '' '' '' '' '' '' '' '' '',
'ANLN2'       'Sub Nr'            '6'  '' 'X' '' '' '' '' '' '' '' ''  '' '',
'DT_INCOR'    'Data Incor.'       '11'  '' '' '' '' '' '' '' '' '' '' '' '',
'DT_DESAT'    'Data Desat.'       '11'  '' '' '' '' '' '' '' '' '' '' '' '',
'KFZKZ'       'Placa Veículo'     '13'  '' '' '' '' '' '' '' '' '' '' '' '',
'CENTRO'      'Centro Custo'      '12' '' '' '' '' '' '' '' '' '' ''  '' '',
'NR_CHASSI'   'Nr. Chassi'        '10' '' '' '' '' '' '' '' '' '' '' ''  '',
'TXT_PRINC'   'Descrição'         '30'  '' '' '' '' '' '' '' '' '' '' '' '',
'PORTE_OBRIG' 'Porte Obrigatório' '4'  '' '' '' '' '' '' '' '' '' '' '' '',    "CS2019001264 28.05.2020
'PAIS'        'País'              '4'  '' '' '' '' '' '' '' '' '' '' '' '',
'REGIAO'      'Região'            '6'  '' '' '' '' '' '' '' '' '' '' '' '',
'ANO_FABR'    'Ano Fab.'          '8'  '' '' '' '' '' '' '' '' '' '' '' '',
'ANO_MOD'     'Ano Mod.'          '8'  '' '' '' '' '' '' '' '' '' '' '' '',
'POTENCIA'    'Potência'          '9'  '' '' '' '' '' '' '' '' '' '' '' '',
'COR'         'Cor'               '3'  '' '' '' '' '' '' '' '' '' '' '' '',
'PG_ARQ'      'Pg Arq.'           '7'  '' '' '' '' '' '' '' '' '' '' '' '',
'RESP_VEIC'   'Resp Veíc'         '9'  '' '' '' '' '' '' '' '' '' '' '' '',
'COD_RENAVAN' 'Cod Renavam'       '11'  '' '' '' '' '' '' '' '' '' '' '' '',
'MES_IPVA'    'Mês Ipva'          '8'  '' '' '' '' '' '' '' '' '' '' '' '',
'MES_LICENC'  'Mês Licenc'        '10'  '' '' '' '' '' '' '' '' '' '' '' '',
'MES_DPVAT'	  'Mês Dpvat'         '9'  '' '' '' '' '' '' '' '' '' '' '' '',
'USER_CRIAC'  'Usuário Criação'   '14'  '' '' '' '' '' '' '' '' '' '' '' '',
'DT_CRIACAO'  'Dt Criação'        '10'  '' '' '' '' '' '' '' '' '' '' '' '',
'HR_CRIACAO'  'Hr Criação'        '10'  '' '' '' '' '' '' '' '' '' '' '' '',
'USER_MODIF'  'Usuário Modificação' '17'  '' '' '' '' '' '' '' '' '' '' '' '',
'DT_MODIF'    'Dt Modific'        '10'  '' '' '' '' '' '' '' '' '' '' '' '',
'OBSERVACAO'  'Observação'        '20'  '' '' '' '' '' '' '' '' '' '' '' ''.

  IF ( OBJ_CUSTOM_0110 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0110
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_0110'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

*    wl_layout-no_rowmark = 'X'.
*    wl_layout-edit_mode = 'X'.

    CREATE OBJECT OBJ_ALV_0110
      EXPORTING
        I_PARENT          = OBJ_CUSTOM_0110
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    PERFORM EXCLUDE_TB_FUNCTIONS CHANGING WL_EXCLUDE.
  ENDIF.

*  create object obj_splitter
*    exporting
*      parent  = obj_custom_0110
*      rows    = 2
*      columns = 1.
*
*  call method obj_splitter->get_container
*    exporting
*      row       = 1
*      column    = 1
*    receiving
*      container = obj_custom_splitter.

  GT_VARIANT-REPORT = SY-REPID.

  SET HANDLER:
    LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR OBJ_ALV_0110,
    LCL_EVENT_HANDLER=>SET_TOOLBAR     FOR OBJ_ALV_0110,
    LCL_EVENT_HANDLER=>GET_UCOMM       FOR OBJ_ALV_0110.
*    lcl_event_handler=>handle_user_command for obj_alv_0140.

  CALL METHOD OBJ_ALV_0110->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WL_LAYOUT
      IS_VARIANT                    = GT_VARIANT
      I_SAVE                        = 'A'
      IT_TOOLBAR_EXCLUDING          = WL_EXCLUDE
    CHANGING
      IT_OUTTAB                     = GT_SAIDA_0110
      IT_FIELDCATALOG               = GT_FCAT_0110
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

*  call method obj_splitter->set_row_height
*    exporting
*      id     = 1
*      height = 100.

*  CALL METHOD OBJ_ALV_0110->REGISTER_EDIT_EVENT
*    EXPORTING
*      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CALL METHOD OBJ_ALV_0110->REGISTER_EDIT_EVENT
*    EXPORTING
*      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WL_STABLE.


ENDMODULE.                    "PBO_0110 OUTPUT
