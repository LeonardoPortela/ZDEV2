*&---------------------------------------------------------------------*
*&  Include           ZPMR0047_PERFOM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0070   text
*      -->P_0071   text
*      -->P_0072   text
*      -->P_0073   text
*      -->P_0074   text
*      -->P_0075   text
*      -->P_0076   text
*      -->P_0077   text
*      -->P_0078   text
*      -->P_0079   text
*      -->P_0080   text
*      -->P_0081   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING    VALUE(p_colnum)
                                    VALUE(p_fieldname)
                                    VALUE(p_tabname)
                                    VALUE(p_len)
                                    VALUE(p_edit)
                                    VALUE(p_icon)
                                    VALUE(p_do_sum)
                                    VALUE(p_header)
                                    VALUE(p_emphasize)
                                    VALUE(p_hotspot)
                                    VALUE(p_zero).


  DATA:  wl_fcat  TYPE lvc_s_fcat.

  wl_fcat-col_pos     = p_colnum.
  wl_fcat-fieldname   = p_fieldname.
  wl_fcat-tabname     = p_tabname.
  wl_fcat-outputlen   = p_len.
  wl_fcat-coltext     = p_header.
  wl_fcat-edit        = p_edit.
  wl_fcat-icon        = p_icon.
  wl_fcat-ref_table   = p_tabname.
  wl_fcat-checktable  = p_tabname.
  wl_fcat-do_sum      = p_do_sum.
  wl_fcat-emphasize   = p_emphasize.
  wl_fcat-hotspot     = p_hotspot.
  wl_fcat-no_zero     = p_zero.

  wa_layout-ctab_fname    = 'CELL_COLOR'.
  wa_layout-excp_conds    = 'X'.
  wa_layout-zebra         = 'X'.
  wa_layout-sel_mode      = 'A'.
  wa_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  wa_layout-totals_bef    = ''.

  APPEND wl_fcat TO it_fcat.

*WL_FCAT.
*IT_FCAT.
*LVC_S_FCAT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'ST002'.
  SET TITLEBAR 'SET002'.

  DATA: it_values TYPE vrm_values,
        li_vaues  LIKE LINE OF it_values.

  SELECT * INTO TABLE it_zpmt0021
    FROM zpmt0021
    WHERE centro IN p_werks
      AND inativo EQ space.


  CLEAR: it_values, li_vaues, wa_zpmt0021.
  LOOP AT it_zpmt0021 INTO wa_zpmt0021.
    li_vaues-key  = wa_zpmt0021-idequipe.
    li_vaues-text = wa_zpmt0021-dsequipe.
    APPEND li_vaues TO it_values.
  ENDLOOP.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'ZPMT0021-IDEQUIPE'
      values          = it_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_SAVE'.

      DATA: zcl_obj TYPE REF TO dados_ordem.
      CREATE OBJECT zcl_obj.

      CALL METHOD zcl_obj->selec_equip.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SEL_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM sel_dados  USING    p_row
                         p_column_fieldname.




  TRY .
      DATA(wa_ordem) = gt_ordem[ p_row ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  CASE p_column_fieldname.
    WHEN 'AUFNR'.
      SET PARAMETER ID 'ANR' FIELD wa_ordem-aufnr.
      CALL TRANSACTION 'IW32' AND SKIP FIRST SCREEN .

    WHEN 'EQUNR'.
      SET PARAMETER ID 'EQN' FIELD wa_ordem-equnr.
      CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.
