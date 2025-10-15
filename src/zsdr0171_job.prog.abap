
REPORT zsdr0171_job MESSAGE-ID zjob.


START-OF-SELECTION.

  IF sy-batch EQ abap_true.
    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  PERFORM f_baixa_fardo_trace_cotton.

FORM f_baixa_fardo_trace_cotton.

  DATA: lwa_fardos_envio   TYPE zpps0014,
        lit_zsdt0340_envio TYPE TABLE OF zsdt0340.

  SELECT *
    FROM zsdt0340 INTO TABLE @DATA(lit_zsdt0340)
   WHERE sincronizado EQ @abap_false.

  CHECK lit_zsdt0340[] IS NOT INITIAL.

  SORT lit_zsdt0340 BY timestamp.

  DATA(lit_zsdt0340_aux) = lit_zsdt0340[].

  CLEAR: lit_zsdt0340_envio[].

  LOOP AT lit_zsdt0340 INTO DATA(lwa_zsdt0340).

    DATA(_registro_anterior) = abap_false.
    LOOP AT lit_zsdt0340_aux INTO DATA(lwa_zsdt0340_aux) WHERE timestamp < lwa_zsdt0340-timestamp
                                                           AND werks     = lwa_zsdt0340-werks
                                                           AND lgort     = lwa_zsdt0340-lgort
                                                           AND charg     = lwa_zsdt0340-charg
                                                           AND safra     = lwa_zsdt0340-safra.
      _registro_anterior = abap_true.
      EXIT.
    ENDLOOP.

    CHECK _registro_anterior = abap_false.

    APPEND INITIAL LINE TO lwa_fardos_envio-fardos ASSIGNING FIELD-SYMBOL(<fs_fardo>).

    <fs_fardo>-nro_fardo_completo           = lwa_zsdt0340-charg.
    <fs_fardo>-codigo_sai                   = lwa_zsdt0340-cd_sai.
    <fs_fardo>-filial_codigo                = lwa_zsdt0340-werks.
    <fs_fardo>-safra_ano                    = lwa_zsdt0340-safra.
    <fs_fardo>-bloco_numero                 = lwa_zsdt0340-lgort.
    <fs_fardo>-disponivel_comercializacao   = lwa_zsdt0340-disponibilizado_comerc.
    <fs_fardo>-data_disponibilizacao        = |{ lwa_zsdt0340-dt_registro(4) }-{ lwa_zsdt0340-dt_registro+4(2) }-{ lwa_zsdt0340-dt_registro+6(2) } { lwa_zsdt0340-hr_registro(2) }:{ lwa_zsdt0340-hr_registro+2(2) }|.
    <fs_fardo>-carregamento_automatico      = lwa_zsdt0340-carregamento_auto.
    <fs_fardo>-timestamp                    = lwa_zsdt0340-timestamp.

    APPEND lwa_zsdt0340 TO lit_zsdt0340_envio[].

    IF lines( lit_zsdt0340_envio ) >= 2000.
      EXIT.
    ENDIF.
  ENDLOOP.

  TRY.
      zcl_int_ob_baixa_fardo_trace=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_fardos_envio ).
    CATCH zcx_integracao. " Classe de Erro de Integração
      PERFORM f_envia_email_alerta TABLES lit_zsdt0340_envio.
    CATCH zcx_error.      " Classe de Erro Genérica
      PERFORM f_envia_email_alerta TABLES lit_zsdt0340_envio.
  ENDTRY.

ENDFORM.

FORM f_envia_email_alerta  TABLES  p_fardos_envio STRUCTURE zsdt0340.

  DATA: lva_dt_ultimo_email TYPE char14.

  LOOP AT p_fardos_envio INTO DATA(lwa_fardo_envio).

    DATA(_ultimo_email_registro) = lwa_fardo_envio-dt_ultimo_email && lwa_fardo_envio-hr_ultimo_email.

    IF lva_dt_ultimo_email IS INITIAL.
      lva_dt_ultimo_email = _ultimo_email_registro.
    ELSEIF  _ultimo_email_registro > lva_dt_ultimo_email.
      lva_dt_ultimo_email = _ultimo_email_registro.
    ENDIF.

  ENDLOOP.

  DATA(_email_enviado) =
    zcl_trace_cotton_utils=>disparar_email_alerta_generico(
    i_titulo_email      = | SAP { sy-sysid } - INTEGRACAO BAIXA FARDO SAP X TRACE  |
    i_texto_corpo_email = | Ocorreu falhas na integração de baixas de fardos entre SAP x Trace! Programa JOB: ZSDR0171_JOB! |
    i_ultimo_email      = lva_dt_ultimo_email
  ).

  CHECK _email_enviado EQ abap_true.

  LOOP AT p_fardos_envio INTO lwa_fardo_envio.

    UPDATE zsdt0340 SET dt_ultimo_email   = sy-datum
                        hr_ultimo_email   = sy-uzeit
    WHERE timestamp = lwa_fardo_envio-timestamp.

    COMMIT WORK.

  ENDLOOP.



ENDFORM.
