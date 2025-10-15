*&---------------------------------------------------------------------*
*& FORM MONTA_ALV_TELA_0120                                            *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*
MODULE PBO_0120 OUTPUT.
  REFRESH IT_FCAT.
  PERFORM ALV_PREENCHE_CAT USING:
         'EQUNR' 'Nº Equip.'         '10' '' 'X' '' ''  ''  '' '' '',
         'EQKTX' 'Desc. Equipamento' '40' '' ''  '' ''  ''  '' '' '',
         'SWERK' 'Origem'            '6'  '' ''  '' ''  ''  '' '' '',
         'IWERK' 'Destino'           '6'  '' ''  '' ''  ''  '' '' '',
         'ERDAT' 'Data'             '10'  '' ''  '' ''  ''  '' '' '',
       'QT_DIAS' 'Dias'              '4'  '' ''  '' ''  ''  '' '' '',
         'UNAME' 'Responsável'       '10' '' ''  '' ''  ''  '' '' ''.
*  'DT_DEVOLUCAO' 'Data de devolução' '17' '' ''  '' 'X' ''  'ZEQUI_EMPRESTIMO' 'ERDAT'         'IT_SAIDA_EQUI_EMPRESTADOS',
*  'HR_DEVOLUCAO' 'Hora de devolução' '17' '' ''  '' 'X' ''  'ZLEST0056'        'HORA_REGISTRO' 'IT_SAIDA_EQUI_EMPRESTADOS'.

  IF ( OBJ_CUSTOM_0120 IS INITIAL ).
    CREATE OBJECT OBJ_CUSTOM_0120
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_EQUI_EMPRESTADOS'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT OBJ_ALV_0120
      EXPORTING
        I_PARENT          = OBJ_CUSTOM_0120
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
  ENDIF.

*  WA_LAYOUT-CWIDTH_OPT = 'X'.

  "Registra os eventos a serem utilizados
  SET HANDLER:
      LCL_EVENT_HANDLER=>CBX_DATA_CHANGED FOR OBJ_ALV_0120,
      LCL_EVENT_HANDLER=>SET_TOOLBAR              FOR OBJ_ALV_0120.
*      LCL_EVENT_HANDLER=>GET_UCOMM                FOR OBJ_ALV_0120.

  CALL METHOD OBJ_ALV_0120->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IT_TOOLBAR_EXCLUDING          = GT_EXC_BUTTON
    CHANGING
      IT_OUTTAB                     = IT_SAIDA_EQUI_EMPRESTADOS
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  CALL METHOD OBJ_ALV_0120->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.
ENDMODULE.
                  "PBO_0120 OUTPUT
