FUNCTION ZLES_SET_STATUS_OC_SEM_OPUS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TKNUM) TYPE  TKNUM
*"     REFERENCE(I_STATUS) TYPE  ZDE_TP_STATUS_ORDEM
*"----------------------------------------------------------------------

  DATA: lwa_oc_opus_change TYPE zlese0159,
        lit_vttp TYPE TABLE OF vttp,
        lit_lips TYPE TABLE OF lips,
        lva_msg  TYPE string,
        e_data TYPE string.

  CLEAR: lwa_oc_opus_change, lit_vttp[].

  CHECK i_tknum is NOT INITIAL.

  SELECT tknum vbeln
    FROM VTTP INTO CORRESPONDING FIELDS OF TABLE lit_vttp
   WHERE tknum EQ i_tknum.

  CHECK lit_vttp[] IS NOT INITIAL.

  SELECT vbeln matkl
    from lips INTO CORRESPONDING FIELDS OF TABLE lit_lips
     FOR ALL ENTRIES IN lit_vttp
   WHERE vbeln eq lit_vttp-vbeln.

  CHECK lit_lips[] is NOT INITIAL.

  READ TABLE lit_lips INTO DATA(lwa_lips) INDEX 1.
  CHECK sy-subrc eq 0 AND lwa_lips-vbeln is NOT INITIAL.

  SELECT SINGLE *
    from zlest0108 INTO @DATA(lwa_zlest0108)
    WHERE vbeln eq @lwa_lips-vbeln.

  IF sy-subrc eq 0. "Frete de Entrada sem Pesagem OPUS

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarvc)
     WHERE name = 'MAGGI_GR_FERTILIZANTES'
       AND LOW  = @lwa_lips-matkl.

    CHECK sy-subrc eq 0.

    lwa_oc_opus_change-id_ordem = lwa_zlest0108-id_ordem.
    lwa_oc_opus_change-nr_ordem = lwa_zlest0108-nro_ordem_car.
    lwa_oc_opus_change-nr_safra = lwa_zlest0108-safra_ordem_car.

  ELSE.

    SELECT SINGLE *
      from zlest0211 INTO @DATA(lwa_zlest0211)
     WHERE vbeln eq @lwa_lips-vbeln.

    CHECK sy-subrc eq 0.

    lwa_oc_opus_change-id_ordem = lwa_zlest0211-id_ordem.
    lwa_oc_opus_change-nr_ordem = lwa_zlest0211-nro_ordem_car.
    lwa_oc_opus_change-nr_safra = lwa_zlest0211-safra_ordem_car.

  ENDIF.

  CHECK ( lwa_oc_opus_change-id_ordem is NOT INITIAL ) OR
        ( lwa_oc_opus_change-nr_ordem is NOT INITIAL AND
          lwa_oc_opus_change-nr_safra is NOT INITIAL ).

  lwa_oc_opus_change-tp_status = i_status.

  TRY.
      "Executa a API.
      CLEAR: e_data.
      zcl_int_ordem_carrega_opus=>zif_int_ordem_carrega_opus~get_instance(
            )->set_dados_ordem_carregamento( i_data = lwa_oc_opus_change
            )->post_ordem_car_opus( CHANGING e_data = e_data ).

  CATCH zcx_integracao INTO DATA(ex_integra).    "

    MESSAGE ID ex_integra->ZIF_ERROR~msgid
       TYPE 'I'
     NUMBER ex_integra->ZIF_ERROR~msgno
       WITH ex_integra->ZIF_ERROR~MSGV1
            ex_integra->ZIF_ERROR~MSGV2
            ex_integra->ZIF_ERROR~MSGV3
            ex_integra->ZIF_ERROR~MSGV4 INTO LVA_MSG.

    CONCATENATE 'Comunicação OPUS:' LVA_MSG INTO LVA_MSG SEPARATED BY SPACE.
    MESSAGE LVA_MSG TYPE 'I' DISPLAY LIKE 'E'.

    LVA_MSG = 'Operação cancelada!'.
    MESSAGE LVA_MSG TYPE 'E'.

*    if sy-batch eq abap_false.
*      ex_integra->zif_error~published_erro( i_msgty = 'I' i_msgty_display = 'W' ).
*    ENDIF.

  CATCH zcx_error INTO DATA(ex_error).

    MESSAGE ID ex_error->ZIF_ERROR~msgid
       TYPE 'I'
     NUMBER ex_error->ZIF_ERROR~msgno
       WITH ex_error->ZIF_ERROR~MSGV1
            ex_error->ZIF_ERROR~MSGV2
            ex_error->ZIF_ERROR~MSGV3
            ex_error->ZIF_ERROR~MSGV4 INTO LVA_MSG.

    CONCATENATE 'Comunicação OPUS:' LVA_MSG INTO LVA_MSG SEPARATED BY SPACE.
    MESSAGE LVA_MSG TYPE 'I' DISPLAY LIKE 'E'.

    LVA_MSG = 'Operação cancelada!'.
    MESSAGE LVA_MSG TYPE 'E'.

  ENDTRY.


ENDFUNCTION.
