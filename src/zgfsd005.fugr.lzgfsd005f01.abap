*----------------------------------------------------------------------*
***INCLUDE LZGFSD005F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_VALIDA_EXIBICAO
*&---------------------------------------------------------------------*
FORM f_valida_exibicao USING ut_0090 TYPE zsdt0090_t
                    CHANGING cs_alv TYPE zsde0171_saida.


  " condições de pagamento PM e BN não dispara Hedge
  IF cs_alv-tpsim = 'PM' OR cs_alv-tpsim = 'BN'.

    cs_alv-desconsiderar = abap_true.
    EXIT.

  ENDIF.

  " Se não tiver status, não exibe
  IF cs_alv-status IS INITIAL.

    cs_alv-desconsiderar = abap_true.
    EXIT.

  ENDIF.

  " desconsiderar se status <> 'A' e sem disparo
  IF cs_alv-status <> 'A' AND cs_alv-vl_t0094_brl IS INITIAL.
    cs_alv-desconsiderar = abap_true.
    EXIT.
  ENDIF.

  " se não tem diferença, não exibe
  IF cs_alv-diferenca = 0.

    cs_alv-desconsiderar = abap_true.
    EXIT.

  ENDIF.

  " verifica se é dolar e se não tem trava de cambio
  IF cs_alv-waerk = 'USD' AND NOT ( line_exists( ut_0090[ doc_simulacao = cs_alv-doc_simulacao categoria = 'C' ] ) ).

    cs_alv-desconsiderar = abap_true.
    EXIT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valor_simulador
*&---------------------------------------------------------------------*
FORM f_valor_simulador USING ut_0090 TYPE zsdt0090_t
                             ut_vbak TYPE zsdc0171_vbak
                             ut_vbap TYPE zsdc0171_vbap
                             uv_doc_sim TYPE zsded003
                    CHANGING cv_vlr_sim  TYPE zdmbtr.

  DATA lv_brgew TYPE brgew_ap.
  DATA lv_netwr TYPE netwr_ap.
  DATA lv_mwsbp TYPE mwsbp.

  CLEAR cv_vlr_sim.

  " Verificar se existe trava de cambio nova
  PERFORM f_trava_cambio_nova
    USING ut_0090
          uv_doc_sim
 CHANGING cv_vlr_sim.

  CHECK cv_vlr_sim IS INITIAL.

  LOOP AT ut_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>) WHERE doc_sim = uv_doc_sim.

    LOOP AT ut_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>)
      WHERE vbeln = <fs_vbak>-vbeln.

      CHECK <fs_vbap>-lifsp <> '12'.

      IF <fs_vbak>-vbtyp = 'C'. "<-- se for ov soma

        ADD <fs_vbap>-brgew TO lv_brgew.
        ADD <fs_vbap>-netwr TO lv_netwr.
        ADD <fs_vbap>-mwsbp TO lv_mwsbp.

        " se for devolução deduz
      ELSE.

        SUBTRACT <fs_vbap>-brgew FROM lv_brgew.
        SUBTRACT <fs_vbap>-netwr FROM lv_netwr.
        SUBTRACT <fs_vbap>-mwsbp FROM lv_mwsbp.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

  " Valor do simulador, é a soma das OVS + IMPOSTOS
  cv_vlr_sim = lv_netwr + lv_mwsbp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valor_simulador
*&---------------------------------------------------------------------*
FORM f_valor_frete USING uv_doc_sim TYPE zsded003
                         uv_spart   TYPE spart
                CHANGING cv_vlr_sim TYPE zdmbtr.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_trava_cambio_nova
*&---------------------------------------------------------------------*
FORM f_trava_cambio_nova USING ut_0090 TYPE zsdt0090_t
                               uv_doc_sim TYPE zsded003
                      CHANGING cv_vlr_sim  TYPE zdmbtr.

  CLEAR cv_vlr_sim.

  LOOP AT ut_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>)
      WHERE doc_simulacao = uv_doc_sim
        AND prev_pgto_usd IS NOT INITIAL.

    cv_vlr_sim = cv_vlr_sim + <fs_0090>-prev_pgto_usd.

  ENDLOOP.

ENDFORM.
