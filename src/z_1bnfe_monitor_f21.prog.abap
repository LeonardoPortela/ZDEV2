*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F21
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  send_nfe
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_nfe USING p_confirma.

* Check authorization
  IF gf_authorization_nfe_35 IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
  ENDIF.

* Check if an NF-e selection was made
  IF it_selected_rows IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
    RETURN.
  ENDIF.

* Send NF-e that was posted under contingency
  CLEAR subrc.
  REFRESH it_active_mod.

  DATA(l_sem_conf) = COND #( WHEN p_confirma = abap_true THEN abap_false
                                                         ELSE abap_true ).

                                                            "1090279
  LOOP AT it_selected_rows INTO wa_selected_rows.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.

    IF sy-tcode EQ 'ZCTE'.
      CALL METHOD zcl_repom_viagem_vpr=>verifica_custo_vi
        EXPORTING
          i_docnum_cte = wa_nfe_alv-docnum
        EXCEPTIONS
          custo_vi     = 1
          OTHERS       = 2.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

* Send NF-e only if form is not initial                    "1396498
    IF wa_nfe_alv-form IS INITIAL.                          "1396498
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '103'.           "1396498
    ENDIF.                                                  "1396498
*
* check if NF-e is already numbered - ohterwise process    "1265172
* is not allowed                                           "1265172
    PERFORM check_numbered_nfe USING wa_nfe_alv-docnum      "1265172
                                     wa_nfe_alv-nfnum9      "1265172
                                     c_2                    "1265172
                               CHANGING subrc.              "1265172

    IF subrc IS INITIAL.                                    "1265172

      CASE wa_nfe_alv-model.
        WHEN zif_doc_eletronico=>at_st_model_mdfe.

          TRY .
              DATA(obj_mdfe) = NEW zcl_mdfe( i_docnum = wa_nfe_alv-docnum i_nmdfe = wa_nfe_alv-nfnum9 ).
              obj_mdfe->enviar_mdfe( i_sem_confirmacao = l_sem_conf ).
              CLEAR: obj_mdfe.

              SELECT SINGLE * INTO wa_active_mod
                FROM j_1bnfe_active
               WHERE docnum EQ wa_nfe_alv-docnum.

              APPEND wa_active_mod TO it_active_mod.        "1090279
              PERFORM registra_envio_data USING wa_nfe_alv space.

              gf_docnum = wa_nfe_alv-docnum.

            CATCH cx_root INTO DATA(ex_root).

              SELECT SINGLE * INTO wa_active_mod
                FROM j_1bnfe_active
               WHERE docnum EQ wa_nfe_alv-docnum.

              DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
              APPEND wa_active_mod TO it_active_mod.        "1090279
              PERFORM registra_envio_data USING wa_nfe_alv space.

              gf_docnum = wa_nfe_alv-docnum.

              CLEAR: obj_mdfe.
          ENDTRY.

        WHEN OTHERS.
          CALL FUNCTION 'J_1B_NFE_SEND_C_NFE'
            EXPORTING
              iv_docnum           = wa_nfe_alv-docnum
            IMPORTING
              es_active_mod       = wa_active_mod
            EXCEPTIONS
              not_sent            = 1
              not_allowed_to_send = 2
              OTHERS              = 3.

          IF sy-subrc <> 0.
            subrc = c_x.
          ELSE.
            DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
            APPEND wa_active_mod TO it_active_mod.
            PERFORM registra_envio_data USING wa_nfe_alv space.
          ENDIF.

      ENDCASE.

    ENDIF.
  ENDLOOP.

* Update ALV display                                          "1090279
  PERFORM grid_update USING space.                          "1090279

  IF NOT subrc IS INITIAL.
    CASE sy-tcode.
      WHEN 'ZNFE'.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.
      WHEN 'ZCTE'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER '004'.
    ENDCASE.
  ELSE.
    CASE sy-tcode.
      WHEN 'ZNFE'.
        MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.
      WHEN 'ZCTE'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER '003'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " send_nfe
