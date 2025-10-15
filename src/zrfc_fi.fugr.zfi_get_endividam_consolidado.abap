FUNCTION zfi_get_endividam_consolidado.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DT_PRIMEIRO_DIA_MES_ATUAL) TYPE  ERDAT OPTIONAL
*"     VALUE(I_AUGDT_INI) TYPE  AUGDT OPTIONAL
*"     VALUE(I_AUGDT_FIM) TYPE  AUGDT OPTIONAL
*"     VALUE(I_BUDAT_INI) TYPE  BUDAT OPTIONAL
*"     VALUE(I_BUDAT_FIM) TYPE  BUDAT OPTIONAL
*"     VALUE(I_TP_PROCESSAMENTO) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      T_BUKRS TYPE  ZFIT_BUKRS
*"      T_SAIDA_001 STRUCTURE  ZFIE_SAIDA1
*"      T_SAIDA_002 STRUCTURE  ZFIE_SAIDA1
*"      T_SAIDA_003 STRUCTURE  ZFIE_SAIDA3
*"      T_SAIDA_004 STRUCTURE  ZFIE_SAIDA3
*"----------------------------------------------------------------------

  IF i_tp_processamento EQ '1' OR i_tp_processamento IS INITIAL AND
     ( i_budat_ini IS NOT INITIAL OR i_budat_fim IS NOT INITIAL ).

    PERFORM f_process_saida_001 USING i_dt_primeiro_dia_mes_atual
                                      i_budat_ini
                                      i_budat_fim
                                      i_tp_processamento
                                      t_bukrs[]
                             CHANGING t_saida_001[].

  ENDIF.

  IF ( i_tp_processamento EQ '2' OR i_tp_processamento IS INITIAL ) AND
     ( i_augdt_ini IS NOT INITIAL OR i_augdt_fim IS NOT INITIAL ) AND
     ( i_budat_ini IS NOT INITIAL OR i_budat_fim IS NOT INITIAL ).

    PERFORM f_process_saida_002 USING i_dt_primeiro_dia_mes_atual
                                      i_augdt_ini
                                      i_augdt_fim
                                      i_budat_ini
                                      i_budat_fim
                                      i_tp_processamento
                                      t_bukrs[]
                             CHANGING t_saida_002[].


  ENDIF.

  IF i_tp_processamento EQ '3' OR i_tp_processamento IS INITIAL AND
   ( i_budat_ini IS NOT INITIAL OR i_budat_fim IS NOT INITIAL ).

    PERFORM f_process_saida_003 USING i_dt_primeiro_dia_mes_atual
                                      i_budat_ini
                                      i_budat_fim
                                      i_tp_processamento
                                      t_bukrs[]
                             CHANGING t_saida_003[].

  ENDIF.

  IF ( i_tp_processamento EQ '4' OR i_tp_processamento IS INITIAL ) AND
     ( i_augdt_ini IS NOT INITIAL OR i_augdt_fim IS NOT INITIAL ) AND
     ( i_budat_ini IS NOT INITIAL OR i_budat_fim IS NOT INITIAL ).

    PERFORM f_process_saida_004 USING i_dt_primeiro_dia_mes_atual
                                      i_augdt_ini
                                      i_augdt_fim
                                      i_budat_ini
                                      i_budat_fim
                                      i_tp_processamento
                                      t_bukrs[]
                             CHANGING t_saida_004[].


  ENDIF.

ENDFUNCTION.
