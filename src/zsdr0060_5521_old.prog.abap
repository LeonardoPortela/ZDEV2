*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5521
*&---------------------------------------------------------------------*

DATA: vg_vbeln                     TYPE vbak-vbeln,
      it_fieldcatalog_return_final TYPE slis_t_fieldcat_alv,
      wa_fieldcatalog_return_final TYPE slis_fieldcat_alv.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5621_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5521_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5621  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5521 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5521'.

  CLEAR wa_header_ovs-meins.
  wa_header_ovs-meins = vg_sol_5520-meins.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5131  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5521 INPUT.

  DATA: vl_check_5521 TYPE char1,
        it_zsdt0138   TYPE STANDARD TABLE OF zsdt0138,
        wa_zsdt0138   TYPE zsdt0138,
        wa_sol_5520   TYPE ty_sol_5520,
        chave         TYPE char29,
        vl_qte_ov     TYPE zsdt0138-qtd_embarq.

  CLEAR: vl_qte_ov, vl_check_5521.

  PERFORM completa_dados_locais.

  CASE sy-ucomm.
    WHEN 'SALVAR'.

      SELECT SUM( kwmeng )
        FROM vbap
        INTO vl_qte_ov
        WHERE EXISTS ( SELECT *
                         FROM zsdt0144
                        WHERE nro_sol     EQ vg_sol_5520-nro_sol
                          AND seq         EQ vg_sol_5520-seq
                          AND filial_resp EQ 'TPGA'
                          AND status      NE 'X'
                          AND vbeln       EQ vbap~vbeln ).

      IF wa_header_ovs-pto_col IS INITIAL OR
         wa_header_ovs-pto_ent IS INITIAL OR
         wa_header_ovs-prc_frt IS INITIAL.
        MESSAGE TEXT-075 TYPE 'S' DISPLAY LIKE 'E'.
      ELSEIF wa_header_ovs-qte_ov GT vg_sol_5520-qtd_a_eb - vl_qte_ov.
        MESSAGE TEXT-106 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.

        SELECT *
          FROM zsdt0138
          INTO TABLE it_zsdt0138
          WHERE nro_sol EQ vg_sol_5520-nro_sol
            AND seq     EQ vg_sol_5520-seq
            AND filial_resp EQ 'TPGA'
            AND status NE 'X'.

        LOOP AT it_zsdt0138 INTO wa_zsdt0138.
          IF wa_zsdt0138-aviso_receb IS NOT INITIAL.
            MESSAGE s000(z_fi) WITH wa_zsdt0138-placa_cav TEXT-077 DISPLAY LIKE 'E'.
            vl_check_5521 = abap_true.
          ENDIF.
        ENDLOOP.

        IF vl_check_5521 IS INITIAL.
          PERFORM gera_ov_servico_5521.
          LEAVE TO SCREEN 0.
        ENDIF.

        CLEAR: wa_header_ovs.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GERA_OV_SERVICO_5520
*&---------------------------------------------------------------------*
FORM gera_ov_servico_5521.

  DATA: it_zsdt0082      TYPE STANDARD TABLE OF zsdt0082,
        it_vbap          TYPE STANDARD TABLE OF vbap,
        wa_vbap          TYPE vbap,
        wa_header_in     TYPE bapisdhd1,
        wa_header_inx2   TYPE  bapisdh1x,
        it_items_in      TYPE STANDARD TABLE OF bapisditm,
        wa_items_in      TYPE bapisditm,
        it_schedules_in  TYPE STANDARD TABLE OF bapischdl,
        wa_schedules_in  TYPE bapischdl,
        it_conditions_in TYPE STANDARD TABLE OF bapicond,
        wa_conditions_in TYPE bapicond,
        it_partners      TYPE STANDARD TABLE OF bapiparnr,
        wa_partners      TYPE bapiparnr,
        it_return        TYPE STANDARD TABLE OF bapiret2,
        it_return_aux    TYPE STANDARD TABLE OF bapiret2,
        it_return_final  TYPE STANDARD TABLE OF bapiret2,
        wa_return        TYPE bapiret2,
        wa_return_aux    TYPE bapiret2,
        wa_zsdt0082      TYPE zsdt0082,
        wa_zsdt0144      TYPE zsdt0144,
        it_vbuv          TYPE STANDARD TABLE OF vbuv,
        wa_vbuv          TYPE vbuv,
        vl_fieldname     TYPE rmdi_name,
        wa_text          TYPE rmdi_ddtxt.


  CLEAR: vg_vbeln, it_return_final, it_fieldcatalog_return_final.

  SELECT *
    FROM zsdt0082
    INTO TABLE it_zsdt0082
    WHERE nro_sol EQ vg_sol_5520-nro_sol
      AND vbeln EQ vg_sol_5520-vbeln
      AND posnr EQ vg_sol_5520-posnr
      AND seq   EQ vg_sol_5520-seq.

  IF it_zsdt0082 IS NOT INITIAL.

    SELECT *
      FROM vbap
      INTO TABLE it_vbap
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln
        AND posnr EQ it_zsdt0082-posnr.

  ENDIF.

  READ TABLE it_zsdt0082 INTO wa_zsdt0082 WITH KEY vbeln = vg_sol_5520-vbeln
                                                   posnr = vg_sol_5520-posnr
                                                   seq   = vg_sol_5520-seq.
  IF sy-subrc IS INITIAL.
    wa_header_in-sales_org  = wa_zsdt0082-vkorg.
    CONCATENATE 'SOL_ENTREGA ' wa_zsdt0082-nro_sol '/' wa_zsdt0082-seq INTO wa_header_in-purch_no_c.
    wa_items_in-target_qty  = wa_header_ovs-qte_ov.
    wa_schedules_in-req_qty = wa_header_ovs-qte_ov.
    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_zsdt0082-vbeln
                                             posnr = wa_zsdt0082-posnr.
    IF sy-subrc IS INITIAL.
      wa_items_in-batch    = wa_vbap-charg.
      wa_items_in-material = wa_vbap-matnr.
    ENDIF.
  ENDIF.

  wa_header_in-distr_chan =  '10'.
  wa_header_in-purch_date = sy-datum.
  wa_header_in-currency   = 'BRL'.
  wa_header_in-pymt_meth  = 'P'.
  wa_header_in-pmnttrms   = 'Z150'.
  wa_header_in-incoterms1 = 'CIF'.
  wa_header_in-incoterms2 = 'CIF'.
  wa_header_in-division   = '02'.
  wa_header_in-doc_type   = 'ZTER'.


  wa_items_in-target_qu    = 'KG'.
  wa_items_in-sales_unit   = 'KG'.
  wa_items_in-usage_ind    = 'T'.
  wa_items_in-plant        = '0120'.
  wa_items_in-ship_point   = '0120'.
  wa_items_in-store_loc    = 'IN01'.
  wa_items_in-matfrgtgrp   = '00000001'.
  wa_items_in-matl_group   = '700150'.
  wa_items_in-itm_number   = '10'.
  wa_items_in-cust_group   = '0'.

  wa_schedules_in-itm_number = '10'.

  wa_conditions_in-itm_number  = '10'.
  wa_conditions_in-currency    = 'BRL'.
  wa_conditions_in-cond_value  = wa_header_ovs-prc_frt.
  wa_conditions_in-cond_unit  = 'TO'.
  wa_conditions_in-cond_type   = 'PR00'.

  APPEND wa_items_in      TO it_items_in.
  APPEND wa_schedules_in  TO it_schedules_in.
  APPEND wa_conditions_in TO it_conditions_in.

  wa_partners-partn_role = 'AG'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_header_ovs-pto_ent
    IMPORTING
      output = wa_partners-partn_numb.

  APPEND wa_partners TO it_partners.

  wa_partners-partn_role = 'LR'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_header_ovs-pto_ent
    IMPORTING
      output = wa_partners-partn_numb.

  APPEND wa_partners TO it_partners.

  wa_partners-partn_role = 'PC'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_header_ovs-pto_col
    IMPORTING
      output = wa_partners-partn_numb.

  APPEND wa_partners TO it_partners.

  wa_partners-partn_role = 'RE'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_header_ovs-pto_ent
    IMPORTING
      output = wa_partners-partn_numb.

  APPEND wa_partners TO it_partners.

  wa_partners-partn_role = 'RG'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_header_ovs-pto_ent
    IMPORTING
      output = wa_partners-partn_numb.

  APPEND wa_partners TO it_partners.

  wa_partners-partn_role = 'RM'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_header_ovs-pto_col
    IMPORTING
      output = wa_partners-partn_numb.

  APPEND wa_partners TO it_partners.

  wa_partners-partn_role = 'WE'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_header_ovs-pto_ent
    IMPORTING
      output = wa_partners-partn_numb.

  APPEND wa_partners TO it_partners.

  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      sales_header_in     = wa_header_in
    IMPORTING
      salesdocument_ex    = vg_vbeln
    TABLES
      return              = it_return
      sales_items_in      = it_items_in
      sales_partners      = it_partners
      sales_schedules_in  = it_schedules_in
      sales_conditions_in = it_conditions_in.

  IF NOT vg_vbeln IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR: it_vbuv.

    SELECT *
      FROM vbuv
      INTO TABLE it_vbuv
      WHERE vbeln EQ vg_vbeln.

    IF sy-subrc IS INITIAL.

      LOOP AT it_vbuv INTO wa_vbuv.

        CLEAR: it_return, wa_return, vl_fieldname, wa_text.

        vl_fieldname = wa_vbuv-fdnam.

        CALL FUNCTION 'RM_DDIC_TEXTS_GET'
          EXPORTING
            i_name                = vl_fieldname
            i_type                = 'DTEL'
            i_langu               = sy-langu
          IMPORTING
            e_ddtxt               = wa_text
          EXCEPTIONS
            objtype_not_supported = 1
            illegal_input         = 2
            OTHERS                = 3.

        IF sy-subrc <> 0.
          CONCATENATE 'Existem campos incompletos na OV:' wa_vbuv-fdnam INTO wa_return-message SEPARATED BY space.
        ELSE.
          CONCATENATE 'Existem campos incompletos na OV:' wa_text INTO wa_return-message SEPARATED BY space.
        ENDIF.

        wa_return-type = 'E'.
        APPEND wa_return TO it_return_final.

      ENDLOOP.

      CLEAR: it_return_aux.

      wa_header_inx2-updateflag = 'D'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          salesdocument    = vg_vbeln
          order_header_inx = wa_header_inx2
        TABLES
          return           = it_return_aux.

      READ TABLE it_return_aux INTO wa_return_aux WITH KEY type = 'E'.

      IF sy-subrc NE 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

      ENDIF.

      CLEAR: vg_vbeln.

    ELSE.
      APPEND LINES OF it_return TO it_return_final.

      CLEAR: wa_zsdt0144.

      wa_zsdt0144-nro_sol     = vg_sol_5520-nro_sol.
      wa_zsdt0144-seq         = vg_sol_5520-seq.
      wa_zsdt0144-filial_resp = 'TPGA'.
      wa_zsdt0144-vbeln       = vg_vbeln.
      wa_zsdt0144-status      = '1'.
      MODIFY zsdt0144 FROM wa_zsdt0144.

    ENDIF.
  ELSE.
    APPEND LINES OF it_return TO it_return_final.
  ENDIF.

  wa_fieldcatalog_return_final-fieldname     = 'TYPE'.
  wa_fieldcatalog_return_final-tabname       = 'IT_RETURN_FINAL'.
  wa_fieldcatalog_return_final-seltext_s     = 'Msg Type'.
  wa_fieldcatalog_return_final-seltext_m     = 'Msg Type'.
  wa_fieldcatalog_return_final-seltext_l     = 'Msg Type'.
  wa_fieldcatalog_return_final-outputlen = 10.
  APPEND wa_fieldcatalog_return_final TO it_fieldcatalog_return_final.

  wa_fieldcatalog_return_final-fieldname     = 'MESSAGE'.
  wa_fieldcatalog_return_final-tabname       = 'IT_RETURN_FINAL'.
  wa_fieldcatalog_return_final-seltext_s     = 'Mensagem'.
  wa_fieldcatalog_return_final-seltext_m     = 'Mensagem'.
  wa_fieldcatalog_return_final-seltext_l     = 'Mensagem'.
  wa_fieldcatalog_return_final-outputlen = 150.
  APPEND wa_fieldcatalog_return_final TO it_fieldcatalog_return_final.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = it_fieldcatalog_return_final
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = it_return_final.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_DADOS_LOCAIS
*&---------------------------------------------------------------------*
FORM completa_dados_locais .

  DATA: wa_kna1 TYPE kna1,
        wa_lfa1 TYPE lfa1.

  SELECT SINGLE *
    FROM kna1
    INTO wa_kna1
    WHERE kunnr EQ wa_header_ovs-pto_ent.

  SELECT SINGLE *
    FROM lfa1
    INTO wa_lfa1
    WHERE lifnr EQ wa_header_ovs-pto_col.

  CLEAR: wa_header_ovs-pto_ent_desc, wa_header_ovs-pto_col_desc.

  wa_header_ovs-pto_ent_desc = wa_kna1-name1.
  wa_header_ovs-pto_col_desc = wa_lfa1-name1.

ENDFORM.
