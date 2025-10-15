*&---------------------------------------------------------------------*
*& Include          ZSDR0195_COPY_CLS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.


ENDCLASS.

CLASS lcl_listener DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_salv_gui_om_edit_strct_lstr.
ENDCLASS.

CLASS lcl_listener IMPLEMENTATION.

  METHOD if_salv_gui_om_edit_strct_lstr~on_check_changed_data.

    DATA: lv_docnum TYPE zdocnum_flote,
          lr_docnum TYPE RANGE OF j_1bdocnum,
          lv_valor  TYPE zqtd_vinc,
          lo_util   TYPE REF TO zcl_util,
          lv_lines  TYPE sy-tabix.


    CREATE OBJECT lo_util.

    o_ui_data_modify->get_ui_changes( IMPORTING t_modified_cells = DATA(lt_modified) ).

    DATA(lt_modified_aux) = lt_modified.

    SORT lt_modified_aux BY fieldname row_id.

    LOOP AT lt_modified ASSIGNING FIELD-SYMBOL(<fs_modif>).
      FREE: lr_docnum.

      CASE <fs_modif>-fieldname.
**<<<------"167326 - NMS - INI------>>>
        WHEN 'DOCNUM_FLOTE'.
* Verifica se é Criação de Vínculo.
          CHECK NOT ckb_vinc IS INITIAL.

          READ TABLE gt_vinc ASSIGNING FIELD-SYMBOL(<fs_vinc>) INDEX <fs_modif>-row_id.

          IF sy-subrc IS INITIAL.

            DATA(vl_error) = abap_off.

            lv_docnum = <fs_modif>-value(10).
            <fs_vinc>-docnum_flote = lv_docnum.
*            DATA(vl_result) = zcl_im_cl_fluxo_exportacao=>check_processo( EXPORTING i_docnum = lv_docnum ).
*            IF vl_result(6) NS abap_on.
*              MESSAGE s000(z01) WITH 'O Doc Formação de Lote informado não faz parte do processo 1x1.' DISPLAY LIKE 'E'.
*              vl_error = abap_on.
*              EXIT.
*
*            ENDIF.

            DATA(tl_notas) = zcl_im_cl_fluxo_exportacao=>get_nfe_e_f( i_docnum = lv_docnum ).
            READ TABLE tl_notas INTO DATA(el_notas) WITH KEY docnum = lv_docnum.
            o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                 fieldname  = 'QTD_VINC_ORIG'
                                                 cell_value = el_notas-menge ).
            APPEND VALUE #( sign    = zcl_les_utils=>if_stab_constants~mc_sign_include
                            option  = zcl_les_utils=>if_stab_constants~mc_option_equal
                            low     = el_notas-docnum
                          ) TO r_docnum.

            READ TABLE tg_docs ASSIGNING FIELD-SYMBOL(<fs_docs>) WITH KEY row_id = <fs_modif>-row_id.

            IF sy-subrc IS INITIAL.
              <fs_docs>-docnum_flote = lv_docnum.

            ELSE.
              APPEND INITIAL LINE TO tg_docs ASSIGNING <fs_docs>.
              <fs_docs>-docnum_flote = lv_docnum.
              <fs_docs>-row_id       = <fs_modif>-row_id.

            ENDIF.

          ENDIF.
**<<<------"167326 - NMS - FIM------>>>
        WHEN 'DOCNUM_EPROD'.
**<<<------"167326 - NMS - INI------>>>
*          READ TABLE gt_vinc ASSIGNING FIELD-SYMBOL(<fs_vinc>) INDEX <fs_modif>-row_id.
          READ TABLE gt_vinc ASSIGNING <fs_vinc> INDEX <fs_modif>-row_id.
**<<<------"167326 - NMS - FIM------>>>
          IF sy-subrc IS INITIAL.
            APPEND INITIAL LINE TO lr_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>).
            <fs_docnum>-sign = 'I'.
            <fs_docnum>-option = 'EQ'.
            <fs_docnum>-low    = <fs_modif>-value.

            DATA(lt_vinculo) = zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo( EXPORTING i_docnum = lr_docnum  ).
**<<<------"167326 - NMS - INI------>>>
* Verifica se é Criação de Vínculo.
            IF NOT ckb_vinc IS INITIAL.
*** Verifica se está na Lista de Manutenção da Fila.
              IF lt_vinculo-zsdtprod_flote[] IS INITIAL.
                MESSAGE s000(z01) WITH 'Doc. Nota Entrada Produtor não está na lista de Manutenção da Fila.' DISPLAY LIKE 'E'.
                vl_error = abap_on.
                EXIT.

              ENDIF.
*** Verifica se está marcado como Saldo Indisponível.
              READ TABLE lt_vinculo-zsdtprod_flote INTO DATA(le_prod_flote) WITH KEY docnum = <fs_docnum>-low.
              IF le_prod_flote-saldo_nao_disponivel EQ abap_on.
                MESSAGE s000(z01) WITH 'Não há saldo disponível para Doc. Nota Entrada Produtor informada.' DISPLAY LIKE 'E'.
                EXIT.

              ENDIF.
*** Verifica se o rewgistro não está bloqueado.
              READ TABLE lt_vinculo-zsdtprod_flote INTO le_prod_flote WITH KEY docnum    = <fs_docnum>-low
                                                                               cancel    = abap_true
                                                                               us_cancel = 'BLOQUEADO'.

              IF NOT sy-subrc IS INITIAL.
                MESSAGE s000(z01) WITH 'NFe precisa estar bloqueada para manutenção.' DISPLAY LIKE 'E'.
                vl_error = abap_on.
                EXIT.

              ENDIF.

            ENDIF.
**<<<------"167326 - NMS - FIM------>>>
            DELETE lt_vinculo-zsdtprod_flote WHERE cancel EQ abap_true AND us_cancel NE 'BLOQUEADO'.
            READ TABLE lt_vinculo-zsdtprod_flote ASSIGNING FIELD-SYMBOL(<fs_vinculo>) INDEX 1.
            IF sy-subrc IS INITIAL.

              IF <fs_vinculo>-saldo_disponivel <= 0.
                MESSAGE s000(z01) WITH 'Docnum não possui saldo disponível para vincular' DISPLAY LIKE 'E'.
                vl_error = abap_on. "<<<------"167326 - NMS------>>>
                EXIT.
*              ELSEIF <fs_vinculo>-saldo_disponivel < <fs_vinc>-qtd_vinc.
*
*                READ TABLE lt_modified_aux TRANSPORTING NO FIELDS
*                WITH KEY fieldname = 'QTD_VINC'
*                         row_id    = <fs_modif>-row_id
*                BINARY SEARCH.
*                IF sy-subrc IS NOT INITIAL.
*                  MESSAGE s000(z01) WITH 'Qtd vinculada atual maior que saldo disponível!' DISPLAY LIKE 'E'.
*                  EXIT.
*                ELSE.

*                  READ TABLE gt_novos_doc ASSIGNING FIELD-SYMBOL(<fs_novos_doc>)
*                  WITH KEY docnum_flote = <fs_vinc>-docnum_flote
*                           docnum_eprod = <fs_vinc>-docnum_eprod.
*                  IF sy-subrc IS INITIAL.
*
*                    <fs_novos_doc>-docnum_eprod = <fs_modif>-value.
*
*                  ELSE.
*                    APPEND INITIAL LINE TO gt_novos_doc ASSIGNING <fs_novos_doc>.
*                    MOVE-CORRESPONDING <fs_vinc> TO <fs_novos_doc>.
*
*                    <fs_novos_doc>-doc_eprod_ant = <fs_vinc>-docnum_eprod.
*                    <fs_vinc>-docnum_eprod       = <fs_modif>-value.
*
*                    o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
*                                                fieldname  = 'US_CRIACAO'
*                                                cell_value = sy-uname ).
*                    o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
*                                                fieldname  = 'DT_CRIACAO'
*                                                cell_value = sy-datum ).
*                    o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
*                                                fieldname  = 'HR_CRIACAO'
*                                                cell_value = sy-uzeit ).
*                    DATA(lv_chave_nfe) = lo_util->get_chave_nfe( <fs_vinc>-docnum_eprod ).
*                    o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
*                                                fieldname  = 'CHAVE_NFE'
*                                                cell_value = lv_chave_nfe ).
*
*                    <fs_novos_doc>-docnum_eprod  = <fs_modif>-value.
*                    <fs_novos_doc>-qtd_vinc_ant = <fs_novos_doc>-qtd_vinc.
*
*                  ENDIF.
*
*                ENDIF.

              ELSEIF <fs_vinculo>-us_cancel <> 'BLOQUEADO'.
                MESSAGE s000(z01) WITH 'Docnum Não está bloqueado, realizar' 'bloqueio pela manutenção de fila' DISPLAY LIKE 'E'.
                vl_error = abap_on. "<<<------"167326 - NMS------>>>
                EXIT.
              ELSE.
                READ TABLE gt_novos_doc ASSIGNING FIELD-SYMBOL(<fs_novos_doc>)
                WITH KEY docnum_flote = <fs_vinc>-docnum_flote
                         docnum_eprod = <fs_vinc>-docnum_eprod.
                IF sy-subrc IS INITIAL.
                  <fs_novos_doc>-docnum_flote = lv_docnum. "<<<------"167326 - NMS------>>>
                  <fs_novos_doc>-docnum_eprod = <fs_modif>-value.

                ELSE.

                  APPEND INITIAL LINE TO gt_novos_doc ASSIGNING <fs_novos_doc>.
                  MOVE-CORRESPONDING <fs_vinc> TO <fs_novos_doc>.

                  <fs_novos_doc>-doc_eprod_ant = <fs_vinc>-docnum_eprod.
                  <fs_vinc>-docnum_eprod       = <fs_modif>-value.

                  READ TABLE lt_modified_aux ASSIGNING FIELD-SYMBOL(<fs_modified_aux>)
                  WITH KEY fieldname = 'DOCNUM_FLOTE'
                           row_id    = <fs_modif>-row_id
                  BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    <fs_novos_doc>-docnum_flote = <fs_modified_aux>-value.
                  ENDIF.
**<<<------"167326 - NMS - INI------>>>
                  IF ckb_vinc IS INITIAL.
                    IF <fs_novos_doc>-docnum_flote IS INITIAL.
                      <fs_novos_doc>-docnum_flote = lv_docnum.

                    ENDIF.
**<<<------"167326 - NMS - FIM------>>>
                    o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                         fieldname  = 'QTD_VINC'
                                                         cell_value = <fs_vinculo>-saldo_disponivel ).
**<<<------"167326 - NMS - INI------>>>
                  ENDIF.
**<<<------"167326 - NMS - FIM------>>>
                  o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                       fieldname  = 'US_CRIACAO'
                                                       cell_value = sy-uname ).
                  o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                       fieldname  = 'DT_CRIACAO'
                                                       cell_value = sy-datum ).
                  o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                       fieldname  = 'HR_CRIACAO'
                                                       cell_value = sy-uzeit ).
                  DATA(lv_chave_nfe) = lo_util->get_chave_nfe( <fs_vinc>-docnum_eprod ).
                  o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                       fieldname  = 'CHAVE_NFE'
                                                       cell_value = lv_chave_nfe ).
                  <fs_novos_doc>-docnum_eprod  = <fs_modif>-value.

                  <fs_novos_doc>-qtd_vinc = <fs_vinculo>-saldo_disponivel.
*
                  <fs_novos_doc>-chave_nfe = lv_chave_nfe.

                  IF <fs_novos_doc>-qtd_vinc_ant IS INITIAL.
                    <fs_novos_doc>-qtd_vinc_ant = <fs_novos_doc>-qtd_vinc.
                  ENDIF.

                ENDIF.

              ENDIF.
**<<<------"167326 - NMS - INI------>>>
              READ TABLE tg_docs ASSIGNING <fs_docs> WITH KEY row_id = <fs_modif>-row_id.

              IF sy-subrc IS INITIAL.
                <fs_docs>-docnum_eprod = <fs_docnum>-low.

              ELSE.
                APPEND INITIAL LINE TO tg_docs ASSIGNING <fs_docs>.
                <fs_docs>-docnum_eprod = <fs_docnum>-low.
                <fs_docs>-row_id       = <fs_modif>-row_id.

              ENDIF.
**<<<------"167326 - NMS - FIM------>
            ELSE.

              MESSAGE s000(z01) WITH 'Docnum inválido ou não existente!' DISPLAY LIKE 'E'.
              vl_error = abap_on. "<<<------"167326 - NMS------>>>
              EXIT.

            ENDIF.

          ENDIF.

        WHEN 'QTD_VINC'.
**<<<------"167326 - NMS - INI------>>>
* Verifica se é Criação de Vínculo.
          IF NOT ckb_vinc IS INITIAL.
* Converte texto em valor.
            CALL FUNCTION 'MOVE_CHAR_TO_NUM'
              EXPORTING
                chr             = <fs_modif>-value
              IMPORTING
                num             = lv_valor
              EXCEPTIONS
                convt_no_number = 1
                convt_overflow  = 2
                OTHERS          = 3.

            IF lt_vinculo IS INITIAL.
              IF lr_docnum[] IS INITIAL.
                APPEND INITIAL LINE TO lr_docnum ASSIGNING <fs_docnum>.
                <fs_docnum>-sign   = 'I'.
                <fs_docnum>-option = 'EQ'.

                IF lv_docnum IS INITIAL.
                  READ TABLE gt_vinc ASSIGNING <fs_vinc> INDEX <fs_modif>-row_id.

                  IF <fs_vinc>-docnum_eprod IS INITIAL.
                    EXIT.

                  ELSE.
                    <fs_docnum>-low = <fs_vinc>-docnum_eprod.

                  ENDIF.

                ELSE.
                  <fs_docnum>-low = lv_docnum.

                ENDIF.

              ENDIF.

              lt_vinculo = zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo( EXPORTING i_docnum = lr_docnum  ).
              SORT lt_vinculo-zsdtprod_flote BY docnum itmnum werks seq.
*** Verifica se está marcado como Saldo Indisponível.
              READ TABLE lt_vinculo-zsdtprod_flote INTO le_prod_flote WITH KEY docnum = <fs_docnum>-low.

            ENDIF.
*** Verifica se O Saldo Vinculado não excede o Saldo Disponível.
            IF lv_valor GT le_prod_flote-saldo_disponivel.
              MESSAGE s000(z01) WITH 'Valor Quantidade Vinculada informada maior que Saldo Disponível.' DISPLAY LIKE 'E'.
              vl_error = abap_on.
              EXIT.

            ENDIF.

          ENDIF.
**<<<------"167326 - NMS - FIM------>>>
          READ TABLE gt_vinc ASSIGNING <fs_vinc> INDEX <fs_modif>-row_id.
          IF sy-subrc IS INITIAL.

            APPEND INITIAL LINE TO lr_docnum ASSIGNING <fs_docnum>.
            <fs_docnum>-sign   = 'I'.
            <fs_docnum>-option = 'EQ'.

            READ TABLE lt_modified_aux ASSIGNING <fs_modified_aux>
            WITH KEY fieldname = 'DOCNUM_EPROD'
                     row_id    = <fs_modif>-row_id
            BINARY SEARCH.
            IF sy-subrc IS INITIAL .
              <fs_docnum>-low    = <fs_modified_aux>-value.
            ELSE.
              DATA(lv_mesmo_docnum) = abap_true.
              <fs_docnum>-low    = <fs_vinc>-docnum_eprod.
            ENDIF.

            lt_vinculo = zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo( EXPORTING i_docnum = lr_docnum  ).

            CALL FUNCTION 'MOVE_CHAR_TO_NUM'
              EXPORTING
                chr             = <fs_modif>-value
              IMPORTING
                num             = lv_valor
              EXCEPTIONS
                convt_no_number = 1
                convt_overflow  = 2
                OTHERS          = 3.
**<<<------"167326 - NMS - INI------>>>
            SORT lt_vinculo-zsdtprod_flote BY docnum itmnum werks seq.
**<<<------"167326 - NMS - FIM------>>>
            READ TABLE lt_vinculo-zsdtprod_flote ASSIGNING <fs_vinculo> INDEX 1.
            IF sy-subrc IS INITIAL.
              IF lv_mesmo_docnum IS INITIAL AND
                ( <fs_vinculo>-saldo_disponivel <= 0 OR <fs_vinculo>-saldo_disponivel < lv_valor ).
                MESSAGE s000(z01) WITH 'Saldo indisponível ou insulficiente!' DISPLAY LIKE 'E'.
                vl_error = abap_on. "<<<------"167326 - NMS------>>>
                EXIT.
**<<<------"167326 - NMS - INI------>>>
*              ELSEIF lv_valor > <fs_vinc>-qtd_vinc.
              ELSEIF lv_valor GT <fs_vinc>-qtd_vinc AND
                     ckb_vinc IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
                MESSAGE s000(z01) WITH 'Quantidade não pode ser maior que a original!' DISPLAY LIKE 'E'.
                vl_error = abap_on. "<<<------"167326 - NMS------>>>
                EXIT.
              ELSE.

                READ TABLE gt_novos_doc ASSIGNING <fs_novos_doc>
                WITH KEY docnum_flote = <fs_vinc>-docnum_flote
                         docnum_eprod = <fs_vinc>-docnum_eprod.
                IF sy-subrc IS INITIAL.
                  <fs_novos_doc>-qtd_vinc_ant = <fs_novos_doc>-qtd_vinc.
                  <fs_novos_doc>-qtd_vinc = lv_valor.

                ELSE.
**<<<------"167326 - NMS - INI------>>>
                  IF ckb_vinc IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
                    APPEND INITIAL LINE TO gt_novos_doc ASSIGNING <fs_novos_doc>.
                    MOVE-CORRESPONDING <fs_vinc> TO <fs_novos_doc>.

                    READ TABLE gt_vinc_aux ASSIGNING FIELD-SYMBOL(<fs_vinc_aux>) INDEX <fs_modif>-row_id.
                    IF sy-subrc IS INITIAL.
                      <fs_novos_doc>-doc_eprod_ant = <fs_vinc_aux>-docnum_eprod.
                    ENDIF.

                    <fs_novos_doc>-qtd_vinc_ant = <fs_novos_doc>-qtd_vinc.
**<<<------"167326 - NMS - INI------>>>
                  ELSE.
                    IF <fs_novos_doc>-docnum_flote EQ lv_docnum      AND
                       <fs_novos_doc>-docnum_eprod EQ <fs_docnum>-low.
                      <fs_novos_doc>-qtd_vinc_ant = <fs_novos_doc>-qtd_vinc.
                      <fs_novos_doc>-qtd_vinc     = lv_valor.

                    ENDIF.

                    o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                         fieldname  = 'QTD_VINC'
                                                         cell_value = lv_valor ).

                  ENDIF.
**<<<------"167326 - NMS - FIM------>>>
                  o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                       fieldname  = 'US_CRIACAO'
                                                       cell_value = sy-uname ).
                  o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                       fieldname  = 'DT_CRIACAO'
                                                       cell_value = sy-datum ).
                  o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                       fieldname  = 'HR_CRIACAO'
                                                       cell_value = sy-uzeit ).
                  lv_chave_nfe = lo_util->get_chave_nfe( <fs_vinc>-docnum_eprod ).
                  o_ui_data_modify->modify_cell_value( row_id     = <fs_modif>-row_id
                                                       fieldname  = 'CHAVE_NFE'
                                                       cell_value = lv_chave_nfe ).
**<<<------"167326 - NMS - INI------>>>
                  IF ckb_vinc IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
                    <fs_vinc>-qtd_vinc      = lv_valor.
                    <fs_novos_doc>-qtd_vinc = lv_valor.
**<<<------"167326 - NMS - INI------>>>
                  ENDIF.
**<<<------"167326 - NMS - FIM------>>>
                ENDIF.

              ENDIF.

            ELSE.

              MESSAGE s000(z01) WITH 'Docnum inválido ou não existente!' DISPLAY LIKE 'E'.
              vl_error = abap_on. "<<<------"167326 - NMS------>>>
              EXIT.

            ENDIF.

          ENDIF.

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    lo_alv->refresh( ).
    cl_gui_cfw=>flush( ).
**<<<------"167326 - NMS - INI------>>>
* Verifica se "Criar Vinculo" esta acionado.
    IF NOT ckb_vinc IS INITIAL  AND
           vl_error EQ abap_off.

      IF sy-ucomm EQ 'SAVE'.
        DATA(vl_tpmsg) = sy-abcde+4(1). "E - Erro

      ELSE.
        vl_tpmsg = sy-abcde+18(1). "S - Sucess

      ENDIF.
* Verifica se há linhas com Documento de Formação de Lote e do Produtor duplicados em "Criar Vínculo".
* Verifica linhas duplicada em "Criar Vínculo".
      PERFORM zf_check_linha_duplicada USING vl_tpmsg.
* Verifica se há linhas com Documento de Formação de Lote e do Produtor criadas em "Criar Vínculo".
* Verifica linhas já criadas em "Criar Vínculo".
      PERFORM zf_check_linha_criada USING vl_tpmsg.

    ENDIF.
* Verifica se houve algum erro durante o processamento no momento de salvar as alterações.
    IF sy-ucomm EQ 'SAVE' AND
       vl_error EQ abap_on.

      READ TABLE gt_novos_doc TRANSPORTING NO FIELDS WITH KEY docnum_flote = <fs_vinc>-docnum_flote
                                                              docnum_eprod = <fs_vinc>-docnum_eprod.

      IF sy-subrc IS INITIAL.
        DELETE gt_novos_doc INDEX sy-tabix.

      ENDIF.

      CLEAR vl_error.
      vg_err_save = abap_on.

    ENDIF.
**<<<------"167326 - NMS - FIM------>>>
  ENDMETHOD.
ENDCLASS.
