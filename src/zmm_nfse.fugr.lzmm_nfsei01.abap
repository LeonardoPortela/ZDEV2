*----------------------------------------------------------------------*
***INCLUDE LZMM_NFSEI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  CASE sy-ucomm.

      " 31.03.2023 - 107971 - RBL -->
    WHEN 'CHK'.
      PERFORM f_processa_retem USING 'X'.
      " 31.03.2023 - 107971 - RBL --<

    WHEN 'SAVE'.
      PERFORM f_save_2000 USING gv_erro.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'DOWN_PDF'.
      PERFORM f_download_pdf USING zibs_nfse_001-guid_header.

    WHEN 'DOWN_XML'.
      PERFORM f_download_xml USING zibs_nfse_001-guid_header.

    WHEN 'ML81N'.
      PERFORM f_valida_duplic_doc_fiscal. " Rubenilson - 30.08.2024 - US147735
      PERFORM f_save_2000 USING gv_erro.
      PERFORM f_execute_ml81n.

    WHEN 'MIGO'.
      PERFORM f_save_2000 USING gv_erro.
      PERFORM f_execute_migo.

    WHEN 'MIRO'.
      PERFORM f_execute_miro.

    WHEN 'EVENT'.
    WHEN 'ESTORNO'.
      PERFORM f_estornar.

    WHEN 'ASSOCIARPO'.
      PERFORM f_associar_po.

    WHEN 'ASSOCIARVT'.
      PERFORM f_associar_vt_tela.

    WHEN 'MIRO_FRETE'.

      PERFORM f_save_2000 USING gv_erro.

      IF gv_erro IS INITIAL.
        PERFORM f_miro_frete.
      ENDIF.

    WHEN 'MIRO_ESTOR'.
      PERFORM miro_estorno.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_3000 INPUT.

  CASE ok_code_9050.

    WHEN 'SEARCH'.

      PERFORM f_search_po.

      REFRESH CONTROL: 'TC_POPUP_SEARCH' FROM SCREEN sy-dynnr.

    WHEN 'OK'.

      PERFORM f_po_asssociation_commit.

      CLEAR: ok_code_9050.
      LEAVE TO SCREEN 0 .

    WHEN 'CANCEL'.
      LOOP AT gt_scr_popup_list INTO DATA(lw_desa).

        SELECT SINGLE * FROM zibt_nfse_002 WHERE guid_header = zibs_nfse_001-guid_header
                                            AND ebeln = lw_desa-ebeln
                                            AND ebelp = lw_desa-ebelp.
        IF sy-subrc NE 0. "inseriu mas nao associou
          DELETE FROM zibt_nfse_003 WHERE guid_header = zibs_nfse_001-guid_header
                                AND ebeln = lw_desa-ebeln
                                AND ebelp = lw_desa-ebelp.
          UPDATE zibt_nfse_001 SET ebeln = ''
          WHERE guid_header = zibs_nfse_001-guid_header.
          COMMIT WORK.
        ENDIF.
      ENDLOOP.

      CLEAR: ok_code_9050.
      LEAVE TO SCREEN 0 .

    WHEN 'ASSIGN'.

      PERFORM f_po_associate_po.

      "REFRESH: gt_scr_popup_list.
      REFRESH CONTROL: 'TC_POPUP_LIST' FROM SCREEN sy-dynnr.

    WHEN 'UNASSIGN'.

      PERFORM f_po_unassign.

    WHEN 'SEARCH_SAL'.

      PERFORM f_po_search_sal.

    WHEN 'SEARCH_DAL'.

      PERFORM f_po_search_dal.

    WHEN 'ASSIGN_SAL'.

      PERFORM f_po_assign_sal.

    WHEN 'ASSIGN_DAL'.

      PERFORM f_po_assign_dal.

    WHEN 'PICK'.

      PERFORM f_call_link.

    WHEN 'ENTER'.

    WHEN 'SERV'.

      PERFORM f_call_serv.


    WHEN 'CHECKBOX'.


  ENDCASE.

  CLEAR: ok_code_9050.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECKBOX_SEARCH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE checkbox_search INPUT.

  IF gw_scr_popup_search-checkbox IS INITIAL.

    CLEAR: gw_scr_popup_search-menge_iv,  "Invoice Verification Qty
           gw_scr_popup_search-dmbtr_iv.  "Calculate Value

  ELSE.
    "Set same Qty of PO
    gw_scr_popup_search-menge_iv = gw_scr_popup_search-menge.
  ENDIF.

  MODIFY gt_scr_popup_search FROM gw_scr_popup_search
                           INDEX tc_popup_search-current_line
                           TRANSPORTING checkbox
                                        menge_iv
                                        dmbtr_iv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECKBOX  INPUT
*&---------------------------------------------------------------------*
MODULE checkbox INPUT.

  MODIFY gt_scr_popup_list FROM gw_scr_popup_list
                           INDEX tc_popup_list-current_line.

*  IF gw_scr_popup_list-menge_iv > gw_scr_popup_list-menge.
*    MESSAGE 'Quantidade preenchida é maior que a disponível no Pedido de Compras' TYPE 'E'.
*  ENDIF.

  gv_popup_xml_value = zsheader_data_nfse_inbound-nfse_value.
  "gv_total_tolerance = 0.

  CLEAR gv_popup_sap_value.

  LOOP AT gt_scr_popup_list ASSIGNING FIELD-SYMBOL(<fs_associ>).
    PERFORM r_imposto_item USING  <fs_associ>-ebelp
                                  <fs_associ>-ebeln
                                  <fs_associ>-menge_iv
                      CHANGING   w_valor
                                 w_wmwst.
    SELECT SINGLE bsart
      FROM ekko
      INTO @DATA(_bsart)
      WHERE ebeln = @<fs_associ>-ebeln.
    IF _bsart+0(1) = 'Y'.
      w_wmwst = 0.
    ENDIF.
    gv_popup_sap_value = gv_popup_sap_value + w_wmwst + ( <fs_associ>-menge_iv * <fs_associ>-netwr ).
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3010  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_3010 INPUT.

  CASE sy-ucomm.

    WHEN 'OK'.

      PERFORM f_save_serv.

      PERFORM f_atualiza_totais_3000.

      CLEAR: ok_code_9050.
      LEAVE TO SCREEN 0 .

    WHEN 'CANCEL'.

      CLEAR: ok_code_9050.
      LEAVE TO SCREEN 0 .

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CHANGE_3010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_change_3010 INPUT.

  IF zibs_nfse_po_serv-menge > zibs_nfse_po_serv-menge_ori.

    MESSAGE 'Quantidade preenchida é maior que a disponível no Pedido de Compras' TYPE 'E'.
    EXIT.

  ENDIF.

  MODIFY gt_service FROM zibs_nfse_po_serv
    INDEX tc_service-current_line
      TRANSPORTING menge.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_4000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_4000 INPUT.

  CASE ok_code_4000.

    WHEN 'CHCK'.

      PERFORM f_habilitar_botoes.

      PERFORM f_fill_4000_alv_02.

      PERFORM f_preenche_alv_globais.

      PERFORM f_check_box_event.

      "PERFORM f_atualiza_alv_4000.

    WHEN 'SEARCH'.

      PERFORM f_search_4000 USING space.

      PERFORM f_preenche_alv_globais.

      PERFORM f_check_box_event.

      "PERFORM f_atualiza_alv_4000.

      "PERFORM f_atualiza_totais_4000.

    WHEN 'OK'.

      PERFORM f_save_4000.

      PERFORM f_free_alv_4000.

      LEAVE TO SCREEN 0 .

    WHEN 'CANCEL'.

      PERFORM f_free_alv_4000.

      LEAVE TO SCREEN 0 .

      " ------------------------ ALV 01
    WHEN 'DESMARK_01'.

      "PERFORM f_mark_4001 USING go_alv_4000_01 '01' space.

    WHEN 'MARK_ALL_01'.

      "PERFORM f_mark_4001 USING go_alv_4000_01 '01' 'X'.

    WHEN 'ASSIGN'.

      " 2.5 -  Botões da tela de recuperar VT/VI  ]
      PERFORM f_associar_vt.

      PERFORM f_preenche_alv_globais.

      PERFORM f_atualiza_alv_4000.

      " ------------------------ ALV 02
    WHEN 'DESMARK_02'.

      "PERFORM f_mark_4001 USING go_alv_4000_01 '02' space.

    WHEN 'MARK_ALL_02'.

      "PERFORM f_mark_4001 USING go_alv_4000_01 '02' 'X'.

    WHEN 'DESASSIGN'.

      PERFORM f_deassociar_vt.

      PERFORM f_preenche_alv_globais.

      PERFORM f_atualiza_alv_4000.

    WHEN 'EDIT'.

      PERFORM f_edita_4000_alv USING 'X'.

    WHEN 'SAVE'.
      PERFORM f_save_4000.

  ENDCASE.

  PERFORM f_atualiza_totais_4000.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_9070  INPUT
*&---------------------------------------------------------------------*
MODULE exit_command_9070 INPUT.

  CASE sy-ucomm.
    WHEN 'BT_CANCEL'.
      CALL METHOD go_container_popup->free.
      CLEAR: go_container_popup.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9070  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9070 INPUT.

  CASE sy-ucomm.
    WHEN 'BT_CONFIRM'.

      CHECK gv_9070_readonly IS INITIAL.

      IF /tcsr/t_cancrt-cancreason IS NOT INITIAL.

        PERFORM f_preenche_desc_canc
          USING /tcsr/t_cancrt-cancreason
       CHANGING /tcsr/t_cancrt-cancrdescr.

      ENDIF.

      CALL METHOD go_editor_popup->get_text_as_r3table
        IMPORTING
          table = gt_cancr_text[].

      IF gt_cancr_text[] IS INITIAL.
        MESSAGE TEXT-e20 "Obrigatório preencher o Motivo de Cancelamento do Processo
           TYPE 'S' DISPLAY LIKE 'E'..
      ELSE.
        CALL METHOD go_container_popup->free.
        CLEAR: go_container_popup.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'BT_CANCEL'.

      CALL METHOD go_container_popup->free.
      CALL METHOD go_editor_popup->free.

      CLEAR: go_container_popup,go_editor_popup.

      gv_9070_canc = 'X'.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      PERFORM f_preenche_canc_reason.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_SH_PAGAM  INPUT
*&---------------------------------------------------------------------*
MODULE zm_sh_pagam INPUT.

  PERFORM f_preenche_form_pag.

ENDMODULE.

* -----> US #170323 - MMSILVA - 02.05.2025 - Inicio <-----
*&---------------------------------------------------------------------*
*&      Module  VALIDA_DATA  INPUT
*&---------------------------------------------------------------------*
MODULE valida_data INPUT.

  DATA: lv_erro TYPE c.

  PERFORM f_data_vencimento CHANGING zspayment_data_nfse_inbound lv_erro.

ENDMODULE.
* -----> US #170323 - MMSILVA - 02.05.2025 - Fim <-----

*** Inicio - ALX
*&---------------------------------------------------------------------*
*&      Module  INICIA_ISS  INPUT
*&---------------------------------------------------------------------*
MODULE inicia_iss INPUT.

  PERFORM f_inicia_iss CHANGING gv_iss_2000.

ENDMODULE.
*** Fim - ALX
