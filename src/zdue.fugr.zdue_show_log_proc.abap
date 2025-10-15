FUNCTION ZDUE_SHOW_LOG_PROC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LOGS_PROC) TYPE  ZDE_DUE_LOG_PROC_T
*"----------------------------------------------------------------------

  DATA: LIT_LOGS_PROC TYPE TABLE OF ZDE_DUE_LOG_PROC.

  LIT_LOGS_PROC[] = I_LOGS_PROC[].

  REFRESH ESTRUTURA.
  PERFORM F_MONTAR_ESTRUTURA USING:

      01   ''  ''              'I_LOGS_PROC' 'MENSAGEM' 'Mensagem'        '255' '' '' '' ''.



   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
       IT_FIELDCAT           = ESTRUTURA[]
       I_SAVE                = 'A'
       I_SCREEN_START_COLUMN = 3
       I_SCREEN_START_LINE   = 3
       I_SCREEN_END_COLUMN   = 150
       I_SCREEN_END_LINE     = 20
     TABLES
        t_outtab             = LIT_LOGS_PROC.



ENDFUNCTION.
