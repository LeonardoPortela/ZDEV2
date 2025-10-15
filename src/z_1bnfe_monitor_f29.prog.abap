*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F29
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  RESET_REJECTED_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_rejected_nfe .

  DATA: wa_zcte_ciot   TYPE zcte_ciot,
        vg_ajustado    TYPE char01,
        wa_nfe_alv_aux LIKE wa_nfe_alv,
        vg_tabix       TYPE sy-tabix,
        vl_ok          TYPE char01,
        wl_zlest0061   TYPE zlest0061.

* Check authorization

  "FF #170994 - inicio
  IF vg_faturamento_autom = abap_true.                      "FF #170994
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.

  IF gf_authorization_nfe_35 IS INITIAL.

    IF vg_faturamento_autom = abap_off.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.

    ELSE.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056' INTO DATA(l_mesg).
      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
      EXIT.
    ENDIF.

  ENDIF.

* Check if an NF-e selection was made

  IF it_selected_rows IS INITIAL.
    IF vg_faturamento_autom = abap_off.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
      RETURN.

    ELSE.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030' INTO l_mesg.
      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
      EXIT.

    ENDIF.
  ENDIF.
  "FF #170994 - fim

* Undo contingency mode for selected NF-e documents
    CLEAR: subrc, vg_ajustado.

    IF sy-tcode EQ 'ZCTE' OR vg_faturamento_autom = abap_true. "FF #170994

      REFRESH it_active_mod.
      LOOP AT it_selected_rows INTO wa_selected_rows.

        READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

        SELECT SINGLE * INTO wa_zcte_ciot FROM zcte_ciot
          WHERE docnum EQ wa_nfe_alv-docnum.

        CLEAR: wl_zlest0061.
        SELECT SINGLE *
          FROM zlest0061 INTO wl_zlest0061
         WHERE docnum EQ wa_nfe_alv-docnum.

        IF ( ( wa_zcte_ciot-st_ciot EQ '3' ) OR ( wa_zcte_ciot-st_ciot EQ space ) OR ( wa_zcte_ciot-st_ciot EQ '0' )  ) AND ( wl_zlest0061 IS INITIAL ).

          CALL FUNCTION 'Z_SD_INFO_CTE_AVULSO'
            EXPORTING
              p_cte_avulso   = wa_nfe_alv-docnum
              p_chamar_tela  = 'X'
              p_gravar_dados = 'X'
              p_apagar_dados = 'X'
            EXCEPTIONS
              OTHERS         = 1.

          IF NOT sy-subrc IS INITIAL.

            IF vg_faturamento_autom = abap_off.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ELSE.

              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
              EXIT.

            ENDIF.


          ELSE.
            vg_ajustado = c_x.
            READ TABLE it_nfe_alv INTO wa_nfe_alv_aux WITH KEY docnum = wa_nfe_alv-docnum.
            IF sy-subrc IS INITIAL.
              vg_tabix = sy-tabix.
              PERFORM set_status_ciot CHANGING wa_nfe_alv.
              MODIFY it_nfe_alv INDEX vg_tabix FROM wa_nfe_alv TRANSPORTING status_ciot.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSEIF sy-tcode EQ 'ZNFE'.

      REFRESH it_active_mod.

      LOOP AT it_selected_rows INTO wa_selected_rows.

        CLEAR vl_ok.

        PERFORM z_verifica_fatura_cancelada USING wa_nfe_alv-docnum CHANGING vl_ok.

        IF vl_ok EQ 'N'.

          IF vg_faturamento_autom = abap_off.               "FF #170994
            MESSAGE 'a fatura desta nf esta cancelada !' TYPE 'E' .
            RETURN.

          ELSE.

            MOVE 'a fatura desta nf esta cancelada !' TO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).

          ENDIF.


        ENDIF.

      ENDLOOP.
    ENDIF.

    IF vg_ajustado EQ c_x.
      PERFORM grid_update_viagem USING 0.
      "FF #170994 - inicio
      IF vg_faturamento_autom = abap_off.                   "FF #170994
        MESSAGE s037(zsimetrya).
      ELSE.
        MESSAGE s037(zsimetrya) INTO DATA(lc_mesg).
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'S' i_msg = l_mesg i_status = 'SVIA' ).
      ENDIF.
      "FF #170994 - fim
    ENDIF.

    CHECK NOT vg_ajustado EQ c_x.

    REFRESH: it_active_mod.

    LOOP AT it_selected_rows INTO wa_selected_rows.

      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
*
      CALL FUNCTION 'J_1B_NFE_RESET_REJECT_STATUS'
        EXPORTING
          i_docnum           = wa_nfe_alv-docnum
        IMPORTING
          es_active_mod      = wa_active_mod
        EXCEPTIONS
          document_not_found = 1
          enqueue_error      = 2
          invalid_status     = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
        subrc = c_x.
* Unlock J_1BNFDOC
        CALL FUNCTION 'DEQUEUE_EJ_1BNFS'
          EXPORTING
            mode_j_1bnfdoc = 'E'
            mandt          = sy-mandt
            docnum         = wa_nfe_alv-docnum.
* Unlock J_1BNFE_ACTIVE
        CALL FUNCTION 'DEQUEUE_E_J1BNFE'
          EXPORTING
            mode_j_1bnfe_active = 'E'
            mandt               = sy-mandt
            docnum              = wa_nfe_alv-docnum.
* Unlock J_1BNFDE_INVALID
        CALL FUNCTION 'DEQUEUE_E_J1B_INVALID'
          EXPORTING
            mode_j_1bnfe_invalid = 'E'
            mandt                = sy-mandt
            docnum               = wa_nfe_alv-docnum.
      ELSE.
        APPEND wa_active_mod TO it_active_mod.
      ENDIF.
    ENDLOOP.

* Update ALV display
    PERFORM grid_update USING 'RESET'.

    IF NOT subrc IS INITIAL.

      "FF #170994 - inicio
      IF vg_faturamento_autom = abap_off.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.

      ELSE.

        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024' INTO lc_mesg.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).

      ENDIF.
    ELSE.

      IF vg_faturamento_autom = abap_off.                   "FF #170994
        MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.

      ELSE.

        MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).

      ENDIF.
    ENDIF.
    "FF #170994 - fim

ENDFORM.                    " RESET_REJECTED_NFE

*&---------------------------------------------------------------------*
*&      Form  GRID_UPDATE_VIAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grid_update_viagem USING vg_docnum TYPE j_1bdocnum.

  DATA: lv_mode  TYPE c,
        vg_tabix TYPE sy-tabix.

*-#133089-21.02.2024-JT-inicio
  CHECK vg_faturamento_autom = abap_off.
*-#133089-21.02.2024-JT-inicio

  IF vg_docnum IS INITIAL.
    LOOP AT it_nfe_alv INTO wa_nfe_alv.
      vg_tabix = sy-tabix.
      PERFORM set_status_ciot CHANGING wa_nfe_alv.
      MODIFY it_nfe_alv FROM wa_nfe_alv INDEX vg_tabix TRANSPORTING status_ciot.
    ENDLOOP.
  ELSE.
    LOOP AT it_nfe_alv INTO wa_nfe_alv WHERE docnum EQ vg_docnum.
      vg_tabix = sy-tabix.
      PERFORM set_status_ciot CHANGING wa_nfe_alv.
      MODIFY it_nfe_alv FROM wa_nfe_alv INDEX vg_tabix TRANSPORTING status_ciot.
    ENDLOOP.
  ENDIF.

  CALL METHOD ctl_alv_nfe->refresh_table_display
    EXPORTING
      is_stable      = gs_alv_refres_cond
      i_soft_refresh = c_x.

  CALL METHOD cl_gui_cfw=>flush.
  PERFORM set_scroll_info_via_id USING c_100.

ENDFORM.                    " GRID_UPDATE_VIAGEM
