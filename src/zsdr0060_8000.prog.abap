*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_8000
*&---------------------------------------------------------------------*
*TYPES:
*  BEGIN OF ty_popup_nr_fornec,
*    nr_forn TYPE zsdt0062-nr_forn,
*  END OF ty_popup_nr_fornec.
*
*DATA popup_forn TYPE ty_popup_nr_fornec.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8000 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCELARR'.
      "FREE t_0213.
      LEAVE TO SCREEN 0.
    WHEN 'OKAY'.
      if vg_subt_lote eq '5620'.
      PERFORM insert_nr_int_fornecedor_5620.
      elseif vg_subt_lote eq ''.
        PERFORM insert_nr_int_fornecedor_5520.
        endif.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_8000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_8000 OUTPUT.
  SET PF-STATUS '8000'.
  SET TITLEBAR '8000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INSERT_NR_INT_FORNECCEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_nr_int_fornecedor_5620 .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_index_rows2 TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no2     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        "t_saida_fg_aux TYPE TABLE OF ty_faturar_fg,
        w_row_no2     LIKE LINE OF t_row_no2,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD ctl_alv1_5620->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows2
      et_row_no     = t_row_no2.

  CALL METHOD ctl_alv2_5620->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.


  IF popup-nr_forn IS NOT INITIAL.
    DATA(_nr_forn) =  popup-nr_forn.

    LOOP AT t_row_no INTO w_row_no.
      READ TABLE  it_caminhao_5620 INTO DATA(wa_caminhao)  INDEX w_row_no-row_id.
      IF sy-subrc IS INITIAL.
        READ TABLE it_sol_5620 INTO DATA(wa_sol)  WITH KEY nro_sol = wa_caminhao-nro_sol.
        IF sy-subrc IS INITIAL.
          UPDATE zsdt0062 SET nr_forn = _nr_forn
          WHERE vbeln = wa_sol-vbeln AND
                posnr = wa_sol-posnr AND
                nro_sol = wa_sol-nro_sol AND
                seq = wa_sol-seq AND
                ebeln =  wa_caminhao-ebeln AND
                ebelp =  wa_caminhao-ebelp.


          PERFORM seleciona_sol_5620.
          PERFORM click_em_solic_5620 USING index_click.
          PERFORM bloqueia_linhas_caminhao_5620.
          CALL METHOD ctl_alv2_5620->refresh_table_display
            EXPORTING
              is_stable = _stable.

          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.

FORM insert_nr_int_fornecedor_5520 .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_index_rows2 TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no2     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        "t_saida_fg_aux TYPE TABLE OF ty_faturar_fg,
        w_row_no2     LIKE LINE OF t_row_no2,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c.

  CALL METHOD ctl_alv1_5520->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows2
      et_row_no     = t_row_no2.

  CALL METHOD ctl_alv2_5520->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.


  IF popup-nr_forn IS NOT INITIAL.
    DATA(_nr_forn) =  popup-nr_forn.

    LOOP AT t_row_no INTO w_row_no.
      READ TABLE  it_caminhao_5520 INTO DATA(wa_caminhao)  INDEX w_row_no-row_id.
      IF sy-subrc IS INITIAL.
        READ TABLE it_sol_5520 INTO DATA(wa_sol)  WITH KEY nro_sol = wa_caminhao-nro_sol.
        IF sy-subrc IS INITIAL.
          UPDATE zsdt0062 SET nr_forn = _nr_forn
          WHERE vbeln = wa_sol-vbeln AND
                posnr = wa_sol-posnr AND
                nro_sol = wa_sol-nro_sol AND
                seq = wa_sol-seq AND
                ebeln =  wa_caminhao-ebeln AND
                ebelp =  wa_caminhao-ebelp.


          PERFORM seleciona_sol_5520.
          PERFORM click_em_solic_5520 USING index_click.
          PERFORM bloqueia_linhas_caminhao_5520.
          CALL METHOD ctl_alv2_5520->refresh_table_display
            EXPORTING
              is_stable = _stable.

          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
