*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_PAI
*&---------------------------------------------------------------------*


MODULE pai INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING ''.
  ENDCASE.

ENDMODULE.

MODULE pai_manter INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      DATA(_error) = abap_false.

      <fs_wa_registro_manter>-vkorg = p_vkorg.

      PERFORM f_exit_0002 USING <fs_wa_registro_manter> CHANGING _error.

      CHECK _error IS INITIAL.

      IF vg_operacao EQ c_novo.

        CLEAR: <fs_wa_saida_tmp>.

        MOVE-CORRESPONDING  <fs_wa_registro_manter> TO <fs_wa_saida_tmp>.

        "Criar condição dinamica
        PERFORM f_get_cond_chave USING '<FS_WA_SAIDA_TMP>'
                              CHANGING vg_cond.

        CHECK vg_cond-where_tab[] IS NOT INITIAL.

        SELECT SINGLE *
          FROM (p_db_tab) INTO <fs_wa_registro_manter_tmp>
         WHERE "(vg_cond-where_tab)
               vkorg     = <fs_wa_saida_tmp>-vkorg
           AND vkbur     = <fs_wa_saida_tmp>-vkbur
           AND cancelado = abap_false.

        IF sy-subrc = 0.
          MESSAGE 'Registro já cadastrado!' TYPE 'E'.
          EXIT.
        ENDIF.

      ELSEIF vg_operacao EQ c_change.
        SELECT SINGLE *
          FROM (p_db_tab) INTO <fs_wa_registro_manter_tmp>
         WHERE "(vg_cond-where_tab)
               vkorg     = <fs_wa_registro_manter>-vkorg
           AND vkbur     = <fs_wa_registro_manter>-vkbur
           AND cancelado = abap_false.

        IF sy-subrc = 0.
          IF wa_saida_chg-vkbur = <fs_wa_registro_manter>-vkbur.
            MESSAGE 'Não houve alterações.'  TYPE 'W'.
            EXIT.
          ELSE.
            MESSAGE 'Registro já cadastrado!' TYPE 'E'.
            EXIT.
          ENDIF.
        ENDIF.

        IF wa_saida_chg-vkbur <> <fs_wa_registro_manter>-vkbur.
          UPDATE (p_db_tab) SET cancelado  = abap_true
                                usnam_canc = sy-uname
                                data_canc  = sy-datum
                                hora_canc  = sy-uzeit
                          WHERE vkorg      = wa_saida_chg-vkorg
                            AND vkbur      = wa_saida_chg-vkbur
                            AND cancelado  = abap_off.
        ENDIF.
      ENDIF.

      PERFORM f_exit_0003 CHANGING <fs_wa_registro_manter>.

      PERFORM f_exit_0011 CHANGING <fs_wa_registro_manter> <fs_wa_saida>.

      MODIFY (p_db_tab) FROM <fs_wa_registro_manter>.

      IF sy-subrc = 0.
        MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'E'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      PERFORM f_exit_0016 USING sy-ucomm CHANGING <fs_wa_registro_manter> <fs_wa_saida>.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_MANTER_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_manter_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

MODULE pai_manter_0138 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      _error = abap_false.
      PERFORM f_exit_0002 USING <fs_wa_registro_manter> CHANGING _error.

      CHECK _error IS INITIAL.

      IF vg_operacao EQ c_novo.

        CLEAR: <fs_wa_saida_tmp>.

        MOVE-CORRESPONDING  <fs_wa_registro_manter> TO <fs_wa_saida_tmp>.

        "Criar condição dinamica
        PERFORM f_get_cond_chave USING '<FS_WA_SAIDA_TMP>'
                              CHANGING vg_cond.

        CHECK vg_cond-where_tab[] IS NOT INITIAL.

        SELECT SINGLE *
          FROM (p_db_tab) INTO <fs_wa_registro_manter_tmp>
         WHERE (vg_cond-where_tab).

        IF sy-subrc = 0.
          MESSAGE 'Registro já cadastrado!' TYPE 'E'.
          EXIT.
        ENDIF.

      ENDIF.

      PERFORM f_exit_0003 CHANGING <fs_wa_registro_manter>.

      PERFORM f_exit_0011 CHANGING <fs_wa_registro_manter> <fs_wa_saida>.

      MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      PERFORM f_exit_0016 USING sy-ucomm CHANGING <fs_wa_registro_manter> <fs_wa_saida>.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_HELP_FIELD_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_help_field_0001 INPUT.
  PERFORM f_exit_0017 USING '0001'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_HELP_FIELD_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_help_field_0002 INPUT.
  PERFORM f_exit_0017 USING '0002'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_HELP_FIELD_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_help_field_0003 INPUT.
  PERFORM f_exit_0017 USING '0003'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_FOR_VKBUR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_for_vkbur INPUT.

  DATA: it_fieldtab TYPE TABLE OF dfies,
        wa_fieldtab TYPE dfies.

  TYPES: BEGIN OF ty_f4,
           vkbur TYPE tvkbt-vkbur,
           bezei TYPE tvkbt-bezei,
         END OF ty_f4.

  DATA: t_f4          TYPE TABLE OF ty_f4,
        tl_return_tab TYPE TABLE OF ddshretval,
        wl_return_tab TYPE ddshretval,
        tl_dselc      TYPE TABLE OF dselc.

  SELECT tvkbt~vkbur tvkbt~bezei
    FROM tvkbz
   INNER JOIN tvkbt ON tvkbt~spras = sy-langu
                   AND tvkbt~vkbur = tvkbz~vkbur
    INTO TABLE t_f4
   WHERE tvkbz~vkorg = p_vkorg.

  SORT t_f4 BY vkbur.
  DELETE ADJACENT DUPLICATES FROM t_f4 COMPARING vkbur.

  FREE: it_fieldtab.
  PERFORM f_get_info_field USING: 'TVKBT' 'VKBUR' wa_fieldtab.
  wa_fieldtab-position  = 1.
  APPEND wa_fieldtab TO it_fieldtab.
  PERFORM f_get_info_field USING: 'TVKBT' 'BEZEI' wa_fieldtab.
  wa_fieldtab-position  = 2.
  APPEND wa_fieldtab TO it_fieldtab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'VKBUR'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
*     dynprofield     = '<FS_WA_REGISTRO_MANTER>-VKBUR'
      value_org  = 'S'
    TABLES
      value_tab  = t_f4
*     field_tab  = it_fieldtab
      return_tab = tl_return_tab.
*     dynpfld_mapping = tl_dselc.

  READ TABLE tl_return_tab INTO wl_return_tab INDEX 1.

  IF sy-subrc = 0.
    <fs_wa_registro_manter>-vkbur = wl_return_tab-fieldval.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_FIELD
*&---------------------------------------------------------------------*
FORM f_get_info_field USING p_tabname   TYPE ddobjname
                            p_fieldname TYPE dfies-fieldname
                            p_info      TYPE dfies.

  DATA: dfies_tab TYPE STANDARD TABLE OF dfies.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = p_tabname
      fieldname = p_fieldname
    TABLES
      dfies_tab = dfies_tab.

  READ TABLE dfies_tab INTO p_info INDEX 1.

  CASE p_fieldname.

    WHEN 'VKBUR'.
      p_info-reptext   = 'Escr.Vendas'.
      p_info-scrtext_s = 'Escr.Vendas'.
      p_info-scrtext_m = 'Escr.Vendas'.
      p_info-scrtext_l = 'Escr.Vendas'.

    WHEN 'BEZEI'.
      p_info-reptext   = 'Denominação'.
      p_info-scrtext_s = 'Denominação'.
      p_info-scrtext_m = 'Denominação'.
      p_info-scrtext_l = 'Denominação'.
  ENDCASE.

ENDFORM.

*MODULE value_for_vkbur INPUT.
*
*  DATA: l_shlp          TYPE shlp_descr,
*        l_wa            TYPE ddshiface,
*        l_rc            LIKE sy-subrc,
*        l_vtweg         TYPE vtweg,
*        l_spart         TYPE spart,
*        t_return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.
*
*  CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
*    EXPORTING
*      shlpname = 'H_TVKBZ'
*      shlptype = 'SH'
*    IMPORTING
*      shlp     = l_shlp.
*
*  SELECT SINGLE *
*    FROM tvkbz
*    INTO @DATA(w_tvkbz)
*   WHERE vkorg = @p_vkorg.
*
*  IF sy-subrc = 0.
*    l_vtweg = w_tvkbz-vtweg.
*    l_spart = w_tvkbz-spart.
*  ELSE.
*    l_vtweg = '*'.
*    l_spart = '*'.
*  ENDIF.
*
*  LOOP AT l_shlp-interface INTO l_wa.
*    IF     l_wa-shlpfield  EQ 'VKORG'.
*      l_wa-value = p_vkorg.
*    ELSEIF l_wa-shlpfield  EQ 'VTWEG'.
*      l_wa-value = l_vtweg.
*    ELSEIF l_wa-shlpfield  EQ 'SPART'.
*      l_wa-value = l_spart.
*    ELSEIF l_wa-shlpfield  EQ 'VKBUR'.
*      l_wa-valfield = 'X'.
*    ENDIF.
*    MODIFY l_shlp-interface FROM l_wa INDEX sy-tabix.
*  ENDLOOP.
*
*  CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
*    EXPORTING
*      shlp          = l_shlp
*    IMPORTING
*      rc            = l_rc
*    TABLES
*      return_values = t_return_values.
*
*  READ TABLE t_return_values INDEX 1.
*
*  IF sy-subrc = 0.
*    <fs_wa_registro_manter>-vkbur = t_return_values-fieldval.
*  ENDIF.
*
*ENDMODULE.
