FUNCTION zsd_eventos_distribuicao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ZSDS093) TYPE  ZSDS093 OPTIONAL
*"     REFERENCE(I_REPROCESSAR) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_VISUALIZAR) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_ABANDONA_TELA) TYPE  CHAR01
*"  TABLES
*"      T_ZSDS094 TYPE  ZSDS094_TT OPTIONAL
*"  EXCEPTIONS
*"      ERRO_VALIDACAO
*"----------------------------------------------------------------------

  e_abandona_tela = abap_false.

  FREE: t_zsdt0415,
        t_zsdt0415_pend.

  lv_start        = abap_false.
  lv_visualizar   = i_visualizar.

  CREATE OBJECT lc_distribuicao_insumos.

*-US191316-22.09.2025-#191316-JT-inicio
  lc_distribuicao_insumos->set_salvar_copia_distrib( t_zsds094[] ).

  DELETE t_zsds094 WHERE kwmeng IS INITIAL.
*-US191316-22.09.2025-#191316-JT-fim

  lc_zsds093   = i_zsds093.
  lc_zsds094[] = t_zsds094[].

  w_zsdt0082   = lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = lc_zsds093-nro_sol i_seq   = lc_zsds093-seq
                                                        i_vbeln   = lc_zsds093-vbeln   i_posnr = lc_zsds093-posnr ).

  IF lv_visualizar = abap_true.
    PERFORM f_selecao_total.
    PERFORM f_exibir_dados.

    IF t_saida[] IS INITIAL.
      MESSAGE s024(sd) WITH 'Não há informações a Exibir!'.
      RETURN.
    ENDIF.

    CALL SCREEN 0100 STARTING AT  20 5
                       ENDING AT 170 19.
    RETURN.
  ENDIF.
*----------------------------------
*- validar informacoes
*----------------------------------
  TRY.
      lc_distribuicao_insumos->set_executar_distribuicao( EXPORTING i_zsds093                = lc_zsds093
                                                                    i_zsds094                = lc_zsds094
                                                                    i_somente_validar        = abap_true
                                                                    i_somente_montar_eventos = abap_true
                                                                    i_reprocessar            = i_reprocessar ).
    CATCH zcx_error INTO DATA(ex_error).
      MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                      ex_error->msgv3 ex_error->msgv4
                                                              RAISING erro_validacao.
  ENDTRY.

*----------------------------------
*- efetuar processamento
*----------------------------------
  PERFORM f_selecao_inicial.
  PERFORM f_selecao_dados.
  PERFORM f_exibir_dados.

  IF t_saida[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não há informações a Exibir!'.
    RETURN.
  ENDIF.

  CALL SCREEN 0100 STARTING AT  20 5
                     ENDING AT 170 19.

  e_abandona_tela = abap_true.

ENDFUNCTION.
