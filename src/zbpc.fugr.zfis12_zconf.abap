FUNCTION zfis12_zconf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_ATIVAS) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_NATIVA) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_AUTOR) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_REJEIT) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_RECUS) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_CANCEL) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_AGRES) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_NENV) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_GRAVR) TYPE  J_1BNFDOC-CANCEL OPTIONAL
*"     REFERENCE(P_FREPRO) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      T_MOVIMENTO_MENSAL_ZCONF STRUCTURE  ZMOVIMENTO_MENSAL_BPC
*"      T_RET_MOVIMENTO_MENSAL_ZCONF STRUCTURE  ZDADOS_SAIDA
*"----------------------------------------------------------------------

  it_movimento_mensal_zconf[] = t_movimento_mensal_zconf[].

  IF sy-tcode = 'ZCONF_PRD'.
    PERFORM : zseleciona_dados_zconf_PRD USING p_ativas
                                           p_nativa
                                           p_autor
                                           p_rejeit
                                           p_recus
                                           p_cancel
                                           p_agres
                                           p_nenv
                                           p_gravr .

  ELSE.
    PERFORM : zseleciona_dados_zconf USING p_ativas
                                           p_nativa
                                           p_autor
                                           p_rejeit
                                           p_recus
                                           p_cancel
                                           p_agres
                                           p_nenv
                                           p_gravr
                                           p_frepro.
  ENDIF.                                  .

  t_ret_movimento_mensal_zconf[] = it_ret_movimento_mensal_zconf[].


ENDFUNCTION.
