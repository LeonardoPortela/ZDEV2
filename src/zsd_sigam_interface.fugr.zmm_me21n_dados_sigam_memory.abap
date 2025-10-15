FUNCTION zmm_me21n_dados_sigam_memory.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_WRITE) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(I_CLEAR) TYPE  FLAG OPTIONAL
*"  CHANGING
*"     REFERENCE(C_PROCESS) TYPE  ZMME_ME21N_PROC OPTIONAL
*"     REFERENCE(CS_LOTE) TYPE  ZSDE0005 OPTIONAL
*"     REFERENCE(CS_SCREEN) TYPE  ZSDE0004 OPTIONAL
*"----------------------------------------------------------------------

  IF i_write IS INITIAL.

    IMPORT c_process  FROM MEMORY ID 'ZSIGAM_PROCESS'.
    IMPORT cs_lote  FROM MEMORY ID 'ZSIGAM_LOTE'.
    IMPORT cs_screen  FROM MEMORY ID 'ZSIGAM_SCREEN'.

  ELSE.

    EXPORT c_process TO MEMORY ID 'ZSIGAM_PROCESS'.
    EXPORT cs_lote TO MEMORY ID 'ZSIGAM_LOTE'.
    EXPORT cs_screen TO MEMORY ID 'ZSIGAM_SCREEN'.

  ENDIF.

  IF i_clear = 'X'.

    CLEAR: c_process, cs_lote, cs_screen.

    EXPORT c_process TO MEMORY ID 'ZSIGAM_PROCESS'.
    EXPORT cs_lote TO MEMORY ID 'ZSIGAM_LOTE'.
    EXPORT cs_screen TO MEMORY ID 'ZSIGAM_SCREEN'.

  ENDIF.

ENDFUNCTION.
