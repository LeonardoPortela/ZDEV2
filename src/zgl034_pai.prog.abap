*&---------------------------------------------------------------------*
*&  Include           ZGL034_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'SEA'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING '0100'.
    WHEN 'ANEXO'.
      PERFORM f_importar_anexos.
    WHEN 'DOC_ANEXO'.

      IF manager IS NOT INITIAL.
        CALL METHOD manager->unpublish.
        FREE manager.
      ENDIF.

      IF lines( it_sel_rows ) = 1.
        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
        CHECK sy-subrc = 0.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
        CHECK sy-subrc = 0.
        MOVE-CORRESPONDING wa_saida_0100 TO wl_0100_sel.
      ENDIF.
    WHEN 'RET_LEGADO'.
      PERFORM proc_retorno_legado.
    WHEN 'FIS_DT_ATU'.
      PERFORM f_lanca_fiscal_dt_atual.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  DATA: vl_valid TYPE c.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF vg_opr_lcto EQ c_display.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM f_gravar_lcto USING ''.
    WHEN 'CANCEL'.
      PERFORM f_exit_lcto.
    WHEN 'NEW_LOTE'.
      PERFORM f_create_lote.
    WHEN 'VALID'.
      CHECK vg_opr_lcto NE c_display.
      PERFORM f_gravar_lcto USING 'X'.
    WHEN 'CLEAR_DADOS'.
      PERFORM f_clear_dados_lcto USING 'X' .
    WHEN 'REPLY_NF'.
      PERFORM f_reply_dados_nf_item.
    WHEN 'NF_AGRP'.
      CALL METHOD obj_alv_0110->check_changed_data.
      PERFORM f_dados_nf_agrp.
  ENDCASE.

  IF zglt080-zlspr IS INITIAL.
    CLEAR lv_zlspr.
  ENDIF.


ENDMODULE.

MODULE help_zlsch INPUT.

  DATA: gt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        gt_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: gt_t042z TYPE TABLE OF t042z.

  CLEAR: gt_t042z, gt_t042z[].

  SELECT DISTINCT zlsch text1
    FROM t042z INTO CORRESPONDING FIELDS OF TABLE gt_t042z
   WHERE land1 = 'BR'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZLSCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT080-ZLSCH'
      value_org       = 'S'
    TABLES
      value_tab       = gt_t042z
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

ENDMODULE.

MODULE help_hbkid INPUT.

  DATA: gt_t012t TYPE TABLE OF t012t.

  CLEAR: gt_t012t, gt_t012t[].

  SELECT *
    FROM t012t INTO TABLE gt_t012t
   WHERE bukrs = zglt080-bukrs
     AND spras = sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'HBKID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT080-HBKID'
      value_org       = 'S'
    TABLES
      value_tab       = gt_t012t
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  IF gt_return_tab[] IS INITIAL.
    CLEAR: zglt080-hbkid.
  ELSE.
    READ TABLE gt_return_tab WITH KEY retfield = 'ZGLT080-HBKID'.
    IF sy-subrc = 0.
      zglt080-hbkid = gt_return_tab-fieldval.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 0110.

ENDMODULE.

MODULE help_bvtyp INPUT.

  DATA: gt_lfbk TYPE TABLE OF lfbk.

  CLEAR: gt_lfbk, gt_lfbk[].

  IF zglt080-lifnr IS INITIAL.
    MESSAGE s836(sd) WITH TEXT-e02 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = zglt080-lifnr
    IMPORTING
      output = zglt080-lifnr.

  SELECT *
    FROM lfbk INTO TABLE gt_lfbk
   WHERE lifnr = zglt080-lifnr
     AND bvtyp NE ''.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BVTYP'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT080-BVTYP'
      value_org       = 'S'
    TABLES
      value_tab       = gt_lfbk
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  IF gt_return_tab[] IS NOT INITIAL.
    READ TABLE gt_return_tab WITH KEY retfield = 'ZGLT080-BVTYP'.
    IF sy-subrc = 0.
      zglt080-bvtyp = gt_return_tab-fieldval.
      PERFORM f_atrib_forma_pagto.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 0110.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110_exit INPUT.
  CLEAR sy-ucomm.
  LEAVE TO SCREEN 0.
ENDMODULE.

MODULE help_lote INPUT.

  DATA: gt_zglt034 TYPE TABLE OF zglt034 WITH HEADER LINE.

  CLEAR: zglt080-lote, gt_zglt034, gt_zglt034[], gt_return_tab, gt_return_tab[].

  SELECT *
    FROM zglt034 INTO TABLE gt_zglt034.

  SORT gt_zglt034 BY lote DESCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LOTE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT080-LOTE'
      value_org       = 'S'
    TABLES
      value_tab       = gt_zglt034
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  READ TABLE gt_return_tab WITH KEY retfield = 'ZGLT080-LOTE'.
  IF sy-subrc = 0.
    READ TABLE gt_zglt034 WITH KEY lote = gt_return_tab-fieldval.
    IF sy-subrc = 0.
      vg_sel_lote = 'X'.
      zglt080-lote  = gt_zglt034-lote.
      zglt080-bukrs = gt_zglt034-bukrs.
      LEAVE TO SCREEN 0110.
    ENDIF.
  ENDIF.



ENDMODULE.

MODULE user_command_0111 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      PERFORM f_import_itens.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE carrega_arquivo INPUT.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_file
      mask             = ',*.xlsx.'
      mode             = 'O'
      title            = 'Arquivo a importar'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

ENDMODULE.

MODULE user_command_0112 INPUT.

  DATA: vl_idx_0112 TYPE sy-tabix,
        vl_idx_0110 TYPE sy-tabix.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      CALL METHOD obj_alv_0112->check_changed_data.

      "Modificação Código Barras.
      IF vg_modify_cbar IS NOT INITIAL.

        LOOP AT it_saida_0112 INTO wa_saida_0112.

          LOOP AT it_saida_0110 INTO wa_saida_0110 WHERE nfenum = wa_saida_0112-nfenum
                                                     AND series = wa_saida_0112-series.

            wa_saida_0110-cod_barras = wa_saida_0112-cod_barras.
            MODIFY it_saida_0110 FROM wa_saida_0110 INDEX sy-tabix.

          ENDLOOP.

        ENDLOOP.

      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'VIEW_IRF'.

      CHECK it_sel_rows[] IS NOT INITIAL.

      IF lines( it_sel_rows ) > 1.
        MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

      READ TABLE it_saida_0112 INTO wa_saida_0112 INDEX wa_sel_rows-index.

      PERFORM f_saida_0113 USING wa_saida_0112.

      CALL SCREEN 0113 STARTING AT 06 06 ENDING AT 110 13.

    WHEN 'IMPORT_BAS'.

      IF ( vg_opr_lcto NE c_edit ) AND
         ( vg_opr_lcto NE c_new  ).
        MESSAGE 'Operação só permitida em modo de edição!' TYPE 'S'.
        EXIT.
      ENDIF.

      CALL SCREEN 0114 STARTING AT 02 02 ENDING AT 83 04.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE selected_rows_0100 INPUT.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

ENDMODULE.

MODULE user_command_0113 INPUT.

  DATA: vl_idx_0113 TYPE sy-tabix,
        vl_error    TYPE c,
        vl_lcto_irf TYPE c.

  CASE sy-ucomm.
    WHEN 'RFS_IRF_MN'.

      CALL METHOD obj_alv_0113->check_changed_data.

      IF ( vg_opr_lcto EQ c_new ) OR ( vg_opr_lcto EQ c_edit ).

        PERFORM f_atualiza_irf_manual.

        "Gerar Dados IRF para as Notas Fiscais.
        PERFORM f_gerar_dados_irf TABLES it_saida_0112
                                   USING vl_error
                                         vl_lcto_irf.
      ENDIF.

      PERFORM f_saida_0113 USING wa_saida_0112.

      LEAVE TO SCREEN 0113.

    WHEN 'CONFIRM'.

      CALL METHOD obj_alv_0113->check_changed_data.

      IF ( vg_opr_lcto EQ c_new ) OR ( vg_opr_lcto EQ c_edit ).

        PERFORM f_atualiza_irf_manual.

        "Gerar Dados IRF para as Notas Fiscais.
        PERFORM f_gerar_dados_irf TABLES it_saida_0112
                                   USING vl_error
                                         vl_lcto_irf.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

MODULE selected_rows_0112 INPUT.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0112->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

ENDMODULE.

MODULE user_command_0114 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      PERFORM f_import_base_irf.

      "Gerar Dados IRF para os Lançamentos.
      PERFORM f_gerar_dados_irf TABLES it_saida_0112
                                 USING vl_error
                                       vl_lcto_irf.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HELP_ZLSPR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_zlspr INPUT.

  DATA: gt_t008t TYPE TABLE OF t008t WITH HEADER LINE.

  CLEAR: zglt080-zlspr, lv_zlspr.

  CLEAR: gt_t008t, gt_t008t[].

  SELECT *
    FROM t008t INTO TABLE gt_t008t
   WHERE spras = sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'HBKID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT080-HBKID'
      value_org       = 'S'
    TABLES
      value_tab       = gt_t008t
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  IF gt_return_tab[] IS INITIAL.
    CLEAR: zglt080-zlspr.
  ELSE.
    READ TABLE gt_return_tab WITH KEY retfield = 'ZGLT080-ZLSPR'.
    IF sy-subrc = 0.
      READ TABLE gt_t008t WITH KEY textl = gt_return_tab-fieldval.
      IF sy-subrc = 0.
        zglt080-zlspr = gt_t008t-zahls. "GT_RETURN_TAB-FIELDVAL.
        lv_zlspr      = gt_t008t-textl.
      ELSE.
        CLEAR gt_t008t-textl.
      ENDIF.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 0110.

ENDMODULE.
