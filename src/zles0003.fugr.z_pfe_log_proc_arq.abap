FUNCTION Z_PFE_LOG_PROC_ARQ.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NR_LOTE_ADM) TYPE  LXHME_RANGE_C10_T OPTIONAL
*"     REFERENCE(I_NR_LOTE) TYPE  LXHME_RANGE_C10_T OPTIONAL
*"     REFERENCE(I_CHVID) TYPE  LXHME_RANGE_C2_T OPTIONAL
*"     REFERENCE(I_NUCONTRATO) TYPE  LXHME_RANGE_C12_T OPTIONAL
*"     REFERENCE(I_ID_TIPO) TYPE  LXHME_RANGE_C1_T OPTIONAL
*"     REFERENCE(I_POPUP) TYPE  CHAR1 DEFAULT '1'
*"     REFERENCE(I_SEARCH) TYPE  CHAR1 DEFAULT 'X'
*"  TABLES
*"      IT_LOG STRUCTURE  ZLEST0062 OPTIONAL
*"----------------------------------------------------------------------
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  REFRESH: TG_SAIDA.
  IF I_SEARCH IS NOT INITIAL.
    SELECT *
      FROM ZLEST0062
        INTO CORRESPONDING FIELDS OF TABLE TG_SAIDA
          WHERE NR_LOTE_ADM IN I_NR_LOTE_ADM
            AND NR_LOTE     IN I_NR_LOTE
            AND CHVID       IN I_CHVID
            AND NUCONTRATO  IN I_NUCONTRATO
            AND ID_TIPO     IN I_ID_TIPO.

  ENDIF.

  APPEND LINES OF IT_LOG TO TG_SAIDA.

  IF TG_SAIDA[] IS NOT INITIAL.

    LOOP AT TG_SAIDA.
      IF TG_SAIDA-LINHA IS INITIAL.
        TG_SAIDA-ICON = ICON_GREEN_LIGHT.
      ELSE.
        TG_SAIDA-ICON = ICON_RED_LIGHT.
      ENDIF.
      MODIFY TG_SAIDA.
    ENDLOOP.
    PERFORM DEFINIR_EVENTOS.
    PERFORM MONTAR_LAYOUT USING 'LOG'.

    WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
    IF I_POPUP = '1'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     I_CALLBACK_PROGRAM                = V_REPORT
*    I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
     IT_FIELDCAT                       = ESTRUTURA[]
     IS_LAYOUT                         = WL_LAYOUT
     I_SAVE                            = 'A'
     IT_EVENTS                         = EVENTS
     IS_PRINT                          = T_PRINT
     I_SCREEN_START_COLUMN             = 2
     I_SCREEN_START_LINE               = 1
     I_SCREEN_END_COLUMN               = 185
     I_SCREEN_END_LINE                 = 15
    TABLES
      T_OUTTAB                         = TG_SAIDA.

    ELSE.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
         I_CALLBACK_PROGRAM                = V_REPORT
*    I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
         IT_FIELDCAT                       = ESTRUTURA[]
         IS_LAYOUT                         = WL_LAYOUT
         I_SAVE                            = 'A'
         IT_EVENTS                         = EVENTS
         IS_PRINT                          = T_PRINT
        TABLES
          T_OUTTAB                         = TG_SAIDA.

    ENDIF.
  ENDIF.



ENDFUNCTION.
