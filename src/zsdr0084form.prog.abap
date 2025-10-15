*&---------------------------------------------------------------------*
*&  Include           ZSDR0084FORM
*&---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*     MODULE MODIFY_FORMA_LOTE INPUT.
*---------------------------------------------------------------------*
MODULE modify_forma_lote INPUT.


  CLEAR ok_code.
  ok_code = sy-ucomm.

  CASE ok_code.

    WHEN 'BUSCAR'.

      CLEAR: is_filial_participante_eudr,  is_filial_classificacao_eudr, is_material_eudr. "wbarbosa US 152850 10/10/2024

      PERFORM busca_filial USING wa_t001w-werks.

      PERFORM busca_material.

*      PERFORM BUSCA_FORNECEDOR.

      PERFORM busca_pc USING wa_lfa1_pc-lifnr.

      PERFORM busca_lr USING wa_kna1-kunnr.

      PERFORM busca_itinerario.

      PERFORM busca_terminal USING wa_lfa1_z1-lifnr.

      PERFORM preenche_z1.

      PERFORM valida_ag_frete.

      PERFORM busca_preco_pauta.

      PERFORM atualiza_controles_eudr.

    WHEN 'INCO1'.

      PERFORM busca_preco_pauta.

  ENDCASE.

ENDMODULE.

*---------------------------------------------------------------------*
*     FORM BUSCA_FILIAL USING P_WA_T001W_WERKS.
*---------------------------------------------------------------------*
FORM busca_filial USING p_wa_t001w_werks.

  CHECK ( wa_t001w-werks IS NOT INITIAL ).

  DATA: wl_branch_detail TYPE bapibranch.

  IF ( wa_t001w-werks NE wa_t001w_aux-werks ).

    CLEAR: wa_t001w-cnpj, wa_t001w-j_1bbranch, wa_t001w-name1, wa_t001w-regio, wa_t001w-vkorg.

    SELECT SINGLE werks, name1, j_1bbranch, vkorg, regio
      FROM t001w INTO @DATA(w_t001w)
      WHERE werks EQ @p_wa_t001w_werks.

    PERFORM pf_check_filial USING wa_t001w-werks vl_subrc.
    CHECK vl_subrc IS INITIAL.

    wa_t001w_aux  = CORRESPONDING #( w_t001w ).
    wa_t001w      = CORRESPONDING #( w_t001w ).

    SELECT SINGLE bukrs, branch, state_insc FROM j_1bbranch
      INTO @DATA(w_j_1bbranch) WHERE branch = @wa_t001w-j_1bbranch. "BUKRS = @WA_T001W-VKORG AND.

    IF wa_t001w-vkorg IS INITIAL AND w_j_1bbranch-bukrs IS NOT INITIAL.
      MOVE w_j_1bbranch-bukrs TO wa_t001w-vkorg.
    ENDIF.

    IF ( w_t001w-name1 IS NOT INITIAL ).

      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
        EXPORTING
          company       = wa_t001w-vkorg
          branch        = wa_t001w-j_1bbranch
        IMPORTING
          branch_detail = wl_branch_detail.

      MOVE wl_branch_detail-cgc_number TO wa_t001w-cnpj.  "PREENCHE CAMPO CNPJ-FILIAL

      wa_j_1bbranch = CORRESPONDING #( w_j_1bbranch ).

    ENDIF.

  ENDIF.

*   "// wbarbosa US 152850 10/10/2024
  "API que verifica se a Filial é participante EUDR
  DATA(ls_param_eudr_filial) = zcl_eudr_utils=>get_filial_eudr( i_werks = wa_t001w-werks ).
  is_filial_participante_eudr  = ls_param_eudr_filial-participanteeudr.
  is_filial_classificacao_eudr = ls_param_eudr_filial-atendeeudr.
*   "// wbarbosa US 152850 10/10/2024

  IF ( wa_t001w-name1 IS INITIAL ).
    MESSAGE TEXT-005 TYPE 'I'.
    vg_focus = 'WA_T001W-WERKS'.
  ENDIF.

  "CLEAR: W_T001W.

ENDFORM.

*---------------------------------------------------------------------*
*     FORM BUSCA_PC USING P_WA_LFA1_PC_LIFNR.
*---------------------------------------------------------------------*
FORM busca_pc USING p_wa_lfa1_pc_lifnr.

  CHECK ( wa_lfa1_pc-lifnr IS NOT INITIAL ).

  p_wa_lfa1_pc_lifnr = |{ p_wa_lfa1_pc_lifnr ALPHA = IN }|.

  IF ( wa_lfa1_pc-lifnr NE wa_lfa1_pc_aux-lifnr ).

    SELECT SINGLE lifnr, land1, name1, stras, ort01, regio, stcd1, stcd3, lzone
      FROM lfa1 INTO @DATA(w_lfa1) WHERE lifnr = @p_wa_lfa1_pc_lifnr AND land1 EQ 'BR'.

    IF ( sy-subrc = 0 ).

      wa_lfa1_pc      = CORRESPONDING #( w_lfa1 ).
      wa_lfa1_pc_aux  = CORRESPONDING #( w_lfa1 ).

      IF ( wa_lfa1_pc-stcd1 IS INITIAL ).
        SELECT SINGLE stcd2 FROM lfa1 INTO wa_lfa1_pc-stcd1
          WHERE lifnr EQ p_wa_lfa1_pc_lifnr AND land1 EQ 'BR'.
      ENDIF.

      IF ( wa_lfa1_pc-lzone IS NOT INITIAL ).
        SELECT SINGLE vtext FROM tzont INTO wa_lfa1_pc-zone_desc
          WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_lfa1_pc-lzone.
      ENDIF.

    ELSE. "Se não encontrar PC
      MESSAGE TEXT-006 TYPE 'I'.
    ENDIF.

  ENDIF.

  IF w_lfa1-lifnr IS NOT INITIAL.

    SELECT SINGLE 'X'
      INTO @DATA(lv_exists)
      FROM lfa1
      WHERE lifnr EQ @w_lfa1-lifnr
       AND loevm EQ  'X'.
    "OR SPERR EQ 'X'.

    IF sy-subrc EQ 4.

      SELECT SINGLE 'X'
        INTO @lv_exists
        FROM lfa1
        WHERE lifnr EQ @w_lfa1-lifnr
        AND sperr EQ  'X'.
      "OR SPERR EQ 'X'.

    ENDIF.
    IF lv_exists = abap_true .
      MESSAGE TEXT-103 TYPE 'I'.
      CLEAR: wa_lfa1_pc.
      vg_focus = 'WA_LFA1_PC-LIFNR'.
    ELSE.
      vg_focus = 'WA_KNA1-KUNNR'.
      wa_lfa1_pc-lifnr = |{ wa_lfa1_pc-lifnr ALPHA = OUT }|.
    ENDIF.

  ENDIF.




ENDFORM.

*---------------------------------------------------------------------*
*     FORM BUSCA_LR USING P_WA_KNA1_KUNNR.
*---------------------------------------------------------------------*
FORM busca_lr USING p_wa_kna1_kunnr.

  CHECK ( p_wa_kna1_kunnr IS NOT INITIAL ).

  p_wa_kna1_kunnr = |{ p_wa_kna1_kunnr ALPHA = IN }|.

  IF ( wa_kna1-kunnr NE wa_kna1_aux-kunnr ).
    CLEAR wa_kna1-name1.

    SELECT SINGLE kunnr, land1, name1, stras, ort01, regio, stcd1, stcd3, lzone
      FROM kna1 INTO @DATA(w_kna1) WHERE kunnr = @p_wa_kna1_kunnr AND land1 EQ 'BR'.

    IF ( sy-subrc = 0 ).

      wa_kna1     = CORRESPONDING #( w_kna1 ).
      wa_kna1_aux = CORRESPONDING #( w_kna1 ).

      wa_filial_dest-werks = wa_kna1-kunnr.

      IF ( wa_kna1-lzone IS NOT INITIAL ).

        SELECT SINGLE vtext FROM tzont INTO wa_kna1-zone_desc
          WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_kna1-lzone.

      ENDIF.

    ELSE.

      MESSAGE TEXT-007 TYPE 'I'.
      CLEAR wa_kna1.

    ENDIF.

  ENDIF.

  IF wa_kna1-kunnr IS NOT INITIAL.

    SELECT SINGLE @abap_true
      INTO @DATA(lv_exists)
      FROM kna1
      WHERE kunnr EQ @wa_kna1-kunnr
       AND loevm EQ  'X'.
    " OR sperr EQ 'X'.

    IF sy-subrc EQ 4.
      SELECT SINGLE @abap_true
        INTO @lv_exists
        FROM kna1
        WHERE kunnr EQ @wa_kna1-kunnr
         AND sperr EQ  'X'.
      " OR sperr EQ 'X'.
    ENDIF.
    IF lv_exists = abap_true .
      CLEAR: wa_kna1.
      vg_focus = 'WA_KNA1-KUNNR'.
      MESSAGE TEXT-103 TYPE 'I'.

    ELSE.
      wa_kna1-kunnr = |{ wa_kna1-kunnr ALPHA = OUT }|.
      vg_focus = 'WA_LFA1_Z1-LIFNR'.
    ENDIF.

  ENDIF.




ENDFORM.


*---------------------------------------------------------------------*
*     FORM BUSCA_ITINERARIO .
*---------------------------------------------------------------------*
FORM busca_itinerario .

  DATA: lv_answer TYPE c,
        lv_route  TYPE tvro-route.

  CHECK ( wa_lfa1_pc-lzone IS NOT INITIAL AND wa_kna1-lzone IS NOT INITIAL ).

  IF tinct-inco1 EQ 'FOB'.

    CLEAR wa_trolz-route.
    CLEAR: wa_saida-status_itinerario.

    wa_trolz-route = 'FOB'.

    SELECT SINGLE bezei FROM tvrot INTO wa_saida-itinerario_desc
      WHERE spras = 'P' AND route = wa_trolz-route.

    CLEAR: wa_saida-status_itinerario.

  ELSE.

    CLEAR wa_trolz-route.
    CLEAR: wa_saida-status_itinerario.

    SELECT SINGLE aland, azone, lland, lzone, route
      FROM trolz INTO @DATA(w_trolz)
      WHERE aland EQ 'BR'
        AND azone EQ @wa_lfa1_pc-lzone
        AND lland EQ 'BR'
        AND lzone EQ @wa_kna1-lzone.

    IF ( sy-subrc = 0 ).

      wa_trolz = CORRESPONDING #( w_trolz ).

      SELECT SINGLE bezei FROM tvrot INTO wa_saida-itinerario_desc
        WHERE spras = 'P' AND route = wa_trolz-route.

      CLEAR: wa_saida-status_itinerario.

    ELSE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question  = TEXT-008
          text_button_1  = 'Sim'
          text_button_2  = 'Não'
        IMPORTING
          answer         = lv_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc = 0.

        IF lv_answer EQ '1'.

          FREE MEMORY ID 'ZLESR0162_ROUTE'.

          SET PARAMETER ID 'ZONA_DES' FIELD wa_kna1-lzone.
          SET PARAMETER ID 'ZONA_ORI' FIELD wa_lfa1_pc-lzone.
          SET PARAMETER ID 'COD_CLI' FIELD wa_kna1-kunnr.
          SET PARAMETER ID 'COD_PC' FIELD wa_lfa1_pc-lifnr.

          CALL TRANSACTION 'ZLES0214'.

          IMPORT lv_route TO lv_route FROM MEMORY ID 'ZLESR0162_ROUTE'.
          wa_trolz-route = lv_route.
          SELECT SINGLE bezei FROM tvrot INTO wa_saida-itinerario_desc
           WHERE spras = 'P' AND route = wa_trolz-route.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_saida-itinerario_desc.
          ENDIF.

        ELSE.
          CLEAR: wa_trolz, wa_saida-itinerario_desc.
*      MESSAGE text-008 TYPE 'I'.
          vg_focus = 'WA_KNA1-KUNNR'.
        ENDIF.
* Implement suitable error handling here
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*     FORM ALTERA_ZONA_PC  USING    P_WA_LFA1_PC_LIFNR.
*---------------------------------------------------------------------*
FORM altera_zona_pc  USING    p_wa_lfa1_pc_lifnr.

  DATA: ls_selfield TYPE slis_selfield.

  DATA(vl_lifnr) = |{ p_wa_lfa1_pc_lifnr ALPHA = IN }|.

  REFRESH it_z0153.

  SELECT * FROM zlest0153 INTO TABLE it_zlest0153[]
    WHERE land1 NE ' ' AND lifnr = vl_lifnr.

  SELECT * FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE it_zlest0153[]
    WHERE lifnr = vl_lifnr.

  SORT it_zlest0153[] BY lzone ASCENDING.

  LOOP AT it_zlest0153 INTO wa_zlest0153.

    SELECT SINGLE *
      FROM tzont INTO @DATA(wa_tzont)
     WHERE zone1 EQ @wa_zlest0153-lzone
      AND  spras EQ 'P'
      AND land1  EQ 'BR'.

    IF sy-subrc = 0.

      wa_z0153-land1        = wa_zlest0153-land1.
      wa_z0153-vtext        = wa_tzont-vtext.
      wa_z0153-lzone        = wa_zlest0153-lzone.
      wa_z0153-lifnr        = wa_zlest0153-lifnr.
      wa_z0153-kunnr        = wa_zlest0153-kunnr.
      wa_z0153-us_registro  = wa_zlest0153-us_registro.
      wa_z0153-dt_registro  = wa_zlest0153-dt_registro.
      wa_z0153-hr_registro  = wa_zlest0153-hr_registro.

      APPEND wa_z0153 TO it_z0153.
      CLEAR :  wa_z0153, wa_zlest0153.
    ENDIF.
  ENDLOOP.


  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title              = 'Selecione uma Zona de Transporte'
      i_selection          = 'X'
      i_zebra              = 'X'
      i_tabname            = 1
      i_structure_name     = 'ZLESE0153'
      i_allow_no_selection = 'X'
    IMPORTING
      es_selfield          = ls_selfield
    TABLES
      t_outtab             = it_z0153[]
    EXCEPTIONS
      program_error        = 1
      OTHERS               = 2.

  IF ( ls_selfield IS NOT INITIAL ).

    wa_lfa1_pc-lzone = it_zlest0153[ ls_selfield-tabindex ]-lzone.

    SELECT SINGLE vtext FROM tzont INTO wa_lfa1_pc-zone_desc
        WHERE spras = 'P' AND land1 = 'BR' AND zone1 = wa_lfa1_pc-lzone.

  ENDIF.

  IF ( wa_lfa1_pc-lzone IS NOT INITIAL ) AND ( wa_kna1-lzone IS NOT INITIAL ).
    PERFORM busca_itinerario.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*     FORM ALTERA_ZONA_Z1
*---------------------------------------------------------------------*
FORM altera_zona_z1 USING p_porto200 TYPE c CHANGING p_porto TYPE lifnr.

  DATA lt_split TYPE TABLE OF char200.
  DATA lr_lifnr TYPE RANGE OF lifnr.
  DATA: ls_selfield TYPE slis_selfield.

  "DATA(vl_lifnr) = |{ p_wa_lfa1_pc_lifnr ALPHA = IN }|.

  REFRESH it_z0153.

  SPLIT p_porto200 AT ',' INTO TABLE lt_split.

  LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<fs_split>).

    APPEND INITIAL LINE TO lr_lifnr ASSIGNING FIELD-SYMBOL(<fs_lifnr>).

    <fs_lifnr>-sign = 'I'.
    <fs_lifnr>-option = 'EQ'.
    UNPACK <fs_split> TO <fs_lifnr>-low.

  ENDLOOP.

  SELECT * FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE it_zlest0153[]
    WHERE lifnr IN lr_lifnr.

  SORT it_zlest0153[] BY lzone ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_zlest0153[] COMPARING lifnr kunnr.

  LOOP AT it_zlest0153 INTO wa_zlest0153.

    SELECT SINGLE *
      FROM tzont INTO @DATA(wa_tzont)
     WHERE zone1 EQ @wa_zlest0153-lzone
      AND  spras EQ 'P'
      AND land1  EQ 'BR'.

    IF sy-subrc = 0.

      wa_z0153-land1        = wa_zlest0153-land1.
      wa_z0153-vtext        = wa_tzont-vtext.
      wa_z0153-lzone        = wa_zlest0153-lzone.
      wa_z0153-lifnr        = wa_zlest0153-lifnr.
      wa_z0153-kunnr        = wa_zlest0153-kunnr.
      wa_z0153-us_registro  = wa_zlest0153-us_registro.
      wa_z0153-dt_registro  = wa_zlest0153-dt_registro.
      wa_z0153-hr_registro  = wa_zlest0153-hr_registro.

      APPEND wa_z0153 TO it_z0153.
      CLEAR :  wa_z0153, wa_zlest0153.
    ENDIF.
  ENDLOOP.


  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title              = 'Selecione um Terminal - Porto (Z1)' "xxxx
      i_selection          = 'X'
      i_zebra              = 'X'
      i_tabname            = 1
      i_structure_name     = 'ZLESE0153'
      i_allow_no_selection = 'X'
    IMPORTING
      es_selfield          = ls_selfield
    TABLES
      t_outtab             = it_z0153[]
    EXCEPTIONS
      program_error        = 1
      OTHERS               = 2.

  IF ( ls_selfield IS NOT INITIAL ).

    p_porto = it_zlest0153[ ls_selfield-tabindex ]-lifnr.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_Z1
*&---------------------------------------------------------------------*
FORM preenche_z1 .

  CHECK ( check_z1 EQ abap_true ).

  IF ( check_z1 IS NOT INITIAL ).

    SELECT SINGLE lifnr, land1, name1, stras, ort01, regio, stcd1, stcd3, lzone
        FROM lfa1 INTO @DATA(w_lfa1) WHERE land1 EQ 'BR' AND stcd1 EQ @wa_kna1-stcd1.

    IF ( sy-subrc = 0 ).
      wa_lfa1_z1 = CORRESPONDING #( w_lfa1 ).
    ELSE.
      MESSAGE TEXT-009 TYPE 'I'.
      vg_focus = 'WA_LFA1_Z1-LIFNR'.
    ENDIF.

  ELSE.

    CLEAR: wa_lfa1_z1, wa_lfa1_z1-lifnr.

  ENDIF.

*  IF wa_lfa1_z1-lifnr IS NOT INITIAL.
*
*    SELECT SINGLE @abap_true
*      INTO @DATA(lv_exists)
*      FROM lfa1
*      WHERE lifnr EQ @wa_lfa1_z1-lifnr
*       AND loevm EQ  'X'.
*    "  OR sperr EQ 'X'.
*    IF sy-subrc EQ 4.
*      SELECT SINGLE @abap_true
*       INTO @lv_exists
*       FROM lfa1
*       WHERE lifnr EQ @wa_lfa1_z1-lifnr
*        AND sperr EQ  'X'.
*    ENDIF.
*
*    IF lv_exists = abap_true .
*      CLEAR: wa_lfa1_z1.
*      vg_focus = 'WA_LFA1_Z1-LIFNR'.
*      MESSAGE text-103 TYPE 'I'.
*    ELSE.
*      " vg_focus = 'WA_KNA1-KUNNR'.
*      wa_lfa1_z1-lifnr = |{ wa_lfa1_z1-lifnr ALPHA = OUT }|.
*    ENDIF.
*
*  ENDIF.


  wa_lfa1_z1-lifnr = |{ wa_lfa1_z1-lifnr ALPHA = OUT }|.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_TERMINAL
*&---------------------------------------------------------------------*
FORM busca_terminal USING p_wa_lfa1_z1_lifnr.

  CHECK ( p_wa_lfa1_z1_lifnr IS NOT INITIAL ).

  p_wa_lfa1_z1_lifnr = |{ p_wa_lfa1_z1_lifnr ALPHA = IN }|.

  IF ( wa_lfa1_z1-lifnr NE wa_lfa1_z1_aux-lifnr ).

    CLEAR: wa_lfa1_z1-name1.

    SELECT SINGLE lifnr, land1, name1, stras, ort01, regio, stcd1, stcd3, lzone
        FROM lfa1 INTO @DATA(w_lfa1) WHERE lifnr = @p_wa_lfa1_z1_lifnr AND land1 EQ 'BR'.

    IF ( sy-subrc = 0 ).

      wa_lfa1_z1      = CORRESPONDING #( w_lfa1 ).
      wa_lfa1_z1_aux  = CORRESPONDING #( w_lfa1 ).

    ELSE.
      MESSAGE TEXT-006 TYPE 'I'.
    ENDIF.
    vg_focus = 'WA_LFA1_Z1-LIFNR'.

  ENDIF.

  IF wa_lfa1_z1-lifnr IS NOT INITIAL.

    SELECT SINGLE @abap_true
      INTO @DATA(lv_exists)
      FROM lfa1
      WHERE lifnr EQ @wa_lfa1_z1-lifnr
       AND loevm EQ  'X'.
    "  OR sperr EQ 'X'.
    IF sy-subrc EQ 4.
      SELECT SINGLE @abap_true
       INTO @lv_exists
       FROM lfa1
       WHERE lifnr EQ @wa_lfa1_z1-lifnr
        AND sperr EQ  'X'.
    ENDIF.

    IF lv_exists = abap_true .
      CLEAR: wa_lfa1_z1.
      vg_focus = 'WA_LFA1_Z1-LIFNR'.
      MESSAGE TEXT-103 TYPE 'I'.
    ELSE.
      " vg_focus = 'WA_KNA1-KUNNR'.
      wa_lfa1_z1-lifnr = |{ wa_lfa1_z1-lifnr ALPHA = OUT }|.
    ENDIF.

  ENDIF.



  "wa_lfa1_z1-lifnr = |{ wa_lfa1_z1-lifnr ALPHA = OUT }|.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_MATERIAL
*&---------------------------------------------------------------------*
FORM busca_material .

  DATA _matnr TYPE char18.
  DATA _matnr_long TYPE matnr.


  CHECK ( wa_makt-matnr IS NOT INITIAL ).

  _matnr = |{ wa_makt-matnr ALPHA = IN }|.

  IF ( _matnr NE wa_makt_aux-matnr ).

    _matnr_long = _matnr.

    SELECT SINGLE maktx FROM makt INTO wa_makt-maktx
        WHERE matnr EQ _matnr_long AND spras EQ 'PT'.

    CHECK ( sy-subrc = 0 ).

    SELECT SINGLE matnr meins FROM mara INTO wa_mara
        WHERE matnr EQ _matnr_long.

    wa_makt_aux-matnr =  _matnr.
    wa_makt_aux-maktx = wa_makt-maktx.
    vg_focus = 'WA_FORNECEDOR-LIFNR'.

  ENDIF.

  wa_makt-maktx =  wa_makt_aux-maktx.
  wa_makt-matnr = |{ wa_makt_aux-matnr ALPHA = OUT }|.

  IF wa_zsdt0158_saida-kvgr5 IS NOT INITIAL.

    SELECT SINGLE bezei
      FROM tvv5t
      INTO wa_tvv5t-bezei
     WHERE spras EQ sy-langu
      AND  kvgr5 EQ wa_zsdt0158_saida-kvgr5.

  ENDIF.

*   "// wbarbosa US 152850 10/10/2024
  "//  API que verifica se o Material esta participando do EUDR
  zcl_eudr_utils=>check_material_eudr(
    EXPORTING
      i_matnr = wa_makt-matnr          " Nº do material
    RECEIVING
      r_eudr  = is_material_eudr      " Código de um caractere
  ).
*   "// wbarbosa US 152850 10/10/2024

  CHECK ( wa_zsdt0158_saida-kvgr4 IS NOT INITIAL ).

  SELECT SINGLE t~bezei
    FROM tvv4t AS t
    INTO wa_tvv4t-bezei
    WHERE t~spras = 'PT'
      AND t~kvgr4 = wa_zsdt0158_saida-kvgr4.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_DOC
*&---------------------------------------------------------------------*
FORM preenche_doc.

  DATA: ls_selfield   TYPE slis_selfield.
  DATA vmatnr18       TYPE matnr18.
  DATA vl_matnr       TYPE matnr.



  IF ( check_dco IS NOT INITIAL ).

    vmatnr18 = |{ wa_makt-matnr     ALPHA = IN }|.
    vl_matnr  =  vmatnr18.
    DATA(vl_lifnr) = |{ wa_lfa1_pc-lifnr  ALPHA = IN }|.
    DATA(vl_werks) = |{ wa_t001w-werks    ALPHA = IN }|.

    SELECT * FROM zdco_produtor
      INTO TABLE @DATA(tl_zdco_prod)
      WHERE id_fornecedor EQ @vl_lifnr AND
            cd_material   EQ @vl_matnr AND
            cd_centro     EQ @vl_werks AND
            vbeln         EQ @space
      ORDER BY nr_dco ASCENDING
      %_HINTS ORACLE 'INDEX(ZDCO_PRODUTOR "Z1")'.

    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        i_title              = 'Selecione uma Zona de Transporte'
        i_selection          = 'X'
        i_zebra              = 'X'
        i_tabname            = 1
        i_structure_name     = 'ZDCO_PRODUTOR'
        i_allow_no_selection = 'X'
      IMPORTING
        es_selfield          = ls_selfield
      TABLES
        t_outtab             = tl_zdco_prod[]
      EXCEPTIONS
        program_error        = 1
        OTHERS               = 2.

    CLEAR wa_zdco_prod-nr_dco.
    CHECK ( ls_selfield IS NOT INITIAL ).
    wa_zdco_prod = tl_zdco_prod[ ls_selfield-tabindex ].

  ENDIF.

  vg_focus = 'WA_ZDCO_PROD-NR_DCO'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_TP_FRETE
*&---------------------------------------------------------------------*
FORM valida_tp_frete .

  IF ( tinct-inco1 NE 'CIF' OR tinct-inco1 NE 'CPT' ) AND ( it_id_compra[] IS NOT INITIAL ) .
    CLEAR it_id_compra[].
    obj_alv->refresh_table_display( ).
  ENDIF.

  wa_frete-exige_ag_frete = COND string( WHEN ( tinct-inco1 = 'CIF' OR tinct-inco1 = 'CPT' ) THEN 'S'
                                         WHEN ( tinct-inco1 = 'FOB' OR tinct-inco1 = 'CFR' ) THEN 'N' ).

  IF ( wa_frete-exige_ag_frete IS INITIAL ).
    CLEAR: wa_frete-ag_frete, wa_frete-exige_ag_frete.
  ENDIF.

  vg_focus = 'WA_ID_COMPRA-QUANTIDADE' .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_AG_FRETE
*&---------------------------------------------------------------------*
FORM valida_ag_frete.

  CHECK ( wa_frete-ag_frete IS NOT INITIAL ).

  DATA(ag_frete_nr) = |{ wa_frete-ag_frete ALPHA = IN }|.

  SELECT SINGLE * FROM lfa1 INTO @DATA(wl_valida_ag) WHERE lifnr = @ag_frete_nr.

  IF ( wl_valida_ag IS NOT INITIAL ) AND ( wl_valida_ag-dlgrp EQ '0001' ).
    wa_frete-ag_frete_desc = wl_valida_ag-name1.
  ELSE.
    CLEAR wa_frete-ag_frete_desc.
    MESSAGE TEXT-013 TYPE 'I'.
  ENDIF.

  vg_focus = 'WA_FRETE-AG_FRETE'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALTERA_ZONA_Z1
*&---------------------------------------------------------------------*
FORM altera_zona_lr.

  DATA: ls_selfield        TYPE slis_selfield.

  DATA(vl_kunnr) = |{ wa_kna1-kunnr ALPHA = IN }|.

  SELECT * FROM zlest0153   INTO TABLE it_zlest0153     WHERE land1 NE ' ' AND kunnr = vl_kunnr.
  SELECT * FROM kna1        INTO TABLE @DATA(tl_kna1)   WHERE kunnr = @vl_kunnr.
  IF ( sy-subrc = 0 ).
    it_zlest0153[] = CORRESPONDING #( tl_kna1[] ).
  ENDIF.

  CHECK ( it_zlest0153[] IS NOT INITIAL ).
  SORT it_zlest0153[] BY lzone ASCENDING.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title              = 'Selecione uma Zona de Transporte'
      i_selection          = 'X'
      i_zebra              = 'X'
      i_allow_no_selection = 'X'
      i_tabname            = 1
      i_structure_name     = 'ZLEST0153'
    IMPORTING
      es_selfield          = ls_selfield
    TABLES
      t_outtab             = it_zlest0153[]
    EXCEPTIONS
      program_error        = 1
      OTHERS               = 2.

  CHECK ( ls_selfield IS NOT INITIAL ).

  wa_kna1-lzone = it_zlest0153[ ls_selfield-tabindex ]-lzone.

  SELECT SINGLE vtext FROM tzont INTO wa_kna1-zone_desc
    WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_kna1-lzone.

  "AO ALTERAR A ZONA, REFAZER A BUSCA DE ITINERÁRIO.
  IF ( wa_lfa1_pc-lzone IS NOT INITIAL ) AND ( wa_kna1-lzone IS NOT INITIAL ).
    PERFORM busca_itinerario.
  ENDIF.

  vg_focus = 'WA_KNA1-ZONE_DESC'.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  BUSCA_PRECO_PAUTA
*&---------------------------------------------------------------------*
FORM busca_preco_pauta.

  DATA: lv_kbetr     TYPE konp-kbetr,
        wa_konp_full TYPE konp.

  CHECK ( wa_t001w-regio IS NOT INITIAL ) AND ( tinct-inco1 IS NOT INITIAL ) AND
        ( wa_makt-maktx  IS NOT INITIAL ).

  TRY.

      SELECT * FROM tvarvc
        INTO TABLE @DATA(t_uf_pauta)
        WHERE name = 'UF_PRECO_PAUTA'.

      READ TABLE t_uf_pauta WITH KEY low = wa_t001w-regio TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        DATA(lv_last) = abap_true.

      ELSE.
        lv_last = abap_false.
      ENDIF.

      CLEAR wa_konp-kbetr.

      DATA vl_matnr TYPE char18.
      vl_matnr = |{ wa_makt-matnr ALPHA = IN }|.

      zcl_preco=>zif_preco~get_preco_pauta(
        EXPORTING
          i_regio = CONV #( wa_t001w-regio )
          i_matnr = CONV #( vl_matnr )
          i_inco1 = CONV #( tinct-inco1 )
          i_last  = lv_last
        IMPORTING
          e_konp  = wa_konp_full
        RECEIVING
          r_kbetr = lv_kbetr
      ).

      MOVE-CORRESPONDING wa_konp_full TO wa_konp.
      wa_konp-kbetr = lv_kbetr.
*      wa_konp-kbetr  = zcl_preco=>zif_preco~get_preco_pauta( i_regio = CONV #( wa_t001w-regio )
*                                                                   i_matnr = CONV #( vl_matnr )
*                                                                   i_inco1 = CONV #( tinct-inco1 )
*                                                                   i_last  = lv_last ).
      IF wa_konp-kbetr IS INITIAL.
        MESSAGE TEXT-014 TYPE 'I'.
      ENDIF.

    CATCH zcx_preco.
      MESSAGE TEXT-014 TYPE 'I'.
  ENDTRY.

*  DATA(vl_matnr) = |{ wa_makt-matnr ALPHA = IN }|.
*  DATA(vl_vakey) = |BR MT { tinct-inco1 }{ vl_matnr }|.
*
*  SELECT knumh, kotabnr, kschl, vakey, datab, datbi
*    FROM konh INTO TABLE @DATA(t_konh)
*    WHERE kotabnr EQ '924'
*      AND kschl   EQ 'ZIVP'
*      AND vakey   EQ @vl_vakey
*    ORDER BY datab DESCENDING
*    %_HINTS ORACLE 'INDEX(KONH "Z1")'.
*
*  IF ( t_konh[] IS NOT INITIAL ).
*
*    wa_konh = t_konh[ 1 ].
*
*    IF ( wa_konh-datbi GE sy-datum ). "Verifica data de validade da condição
*
*      SELECT SINGLE knumh kbetr kmein konwa
*        FROM konp INTO wa_konp WHERE knumh = wa_konh-knumh.
*
*      IF ( wa_konp IS INITIAL ).
*        MESSAGE text-014 TYPE 'I'.
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_SOLICITACAO
*&---------------------------------------------------------------------*
FORM valida_solicitacao_ov.

  valida = abap_false.

  PERFORM check_material_centro USING e_lgort.

  DATA(_msg_erro) = COND string(
      WHEN ( wa_zsdt0158-tipo     IS INITIAL )    THEN TEXT-015
      WHEN ( wa_t001w-name1       IS INITIAL )    THEN TEXT-005
      WHEN ( wa_lfa1_pc-lifnr     IS INITIAL OR wa_lfa1_pc-name1    IS INITIAL )    THEN TEXT-006
"      WHEN ( WA_LFA1_PC-ZONE_DESC IS INITIAL OR WA_KNA1-ZONE_DESC   IS INITIAL  OR  TINCT-INCO1 <> 'FOB')    THEN TEXT-016
      WHEN ( wa_kna1-kunnr        IS INITIAL OR wa_kna1-name1       IS INITIAL )    THEN TEXT-007
      WHEN ( wa_makt-maktx        IS INITIAL OR tvv3t-kvgr3         IS INITIAL      OR wa_zsdt0158_saida-safra IS INITIAL ) THEN TEXT-010
      WHEN ( wa_lfa1_z1-lifnr     IS INITIAL OR wa_lfa1_z1-name1    IS INITIAL )    THEN TEXT-018
      WHEN ( tinct-inco1          IS INITIAL )    THEN TEXT-012
      WHEN ( wa_konp-kbetr        IS INITIAL )    THEN TEXT-014
      "WHEN ( WA_TROLZ-ROUTE       IS INITIAL )    THEN TEXT-008
      WHEN ( wa_id_compra-unidade NE 'KG'    )    THEN TEXT-039
      WHEN ( wa_zsdt0158_saida-kvgr5 IS INITIAL ) THEN TEXT-041 ).

  " 26.09.2022 - RAMON - 19450 -->
  IF wa_saida-ped_trans = 'X'.

    IF wa_lfa1_z1-lifnr IS INITIAL.
      CLEAR _msg_erro.
    ENDIF.

    " 12.10.2022 - RAMON --- 93610 -->
    TRY .

        IF wa_kna1-kunnr IS NOT INITIAL.
          "BREAK-POINT.

          DATA lv_kunnr TYPE kunnr.

          lv_kunnr = |{ wa_kna1-kunnr ALPHA = IN  }|.

          " 1 - Local de Entrega deve ser uma filial(Usar a Classe ZCL_CLIENTES->CK_PARCEIRO_LOCAL_NEGOCIO para verificação )
          zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = lv_kunnr
          )->ck_parceiro_local_negocio(  ).

          DATA lv_xwerks TYPE werks_d.

          lv_xwerks = |{ wa_kna1-kunnr ALPHA = IN  }|.

          SELECT SINGLE bukrs FROM j_1bbranch
            INTO @DATA(lv_bukrs)
              WHERE branch = @lv_xwerks.

          SELECT SINGLE bukrs FROM j_1bbranch
            INTO @DATA(lv_bukrs2)
              WHERE branch = @wa_t001w-werks.

          IF lv_bukrs NE lv_bukrs2.

            _msg_erro = `Cód. Cliente Destino e cód. Filial não pertence a mesma empresa`.

            erro_validacao = 'X'.

            MESSAGE _msg_erro TYPE 'I'.

            EXIT.
          ENDIF.

          "WPP US 152850 10/10/2024 - Ini
          if is_material_eudr is NOT INITIAL.
            DATA(ls_param_eudr_filial) = zcl_eudr_utils=>get_filial_eudr( i_werks = conv #( lv_xwerks ) ).

            "Não pode transferir produto para filiais 100% EUDR. Será tratado numa v2
            IF sy-subrc eq 0 AND
               ls_param_eudr_filial-participanteeudr = 'S' AND
               ls_param_eudr_filial-atendeeudr       = 'S'.
              _msg_erro = 'Não é possivel realizar transferencias para uma filial 100% EUDR!'.
              erro_validacao = 'X'.
              MESSAGE _msg_erro TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.
          "WPP US 152850 10/10/2024 - Fim

        ENDIF.

      CATCH zcx_parceiros.

        _msg_erro = `O Local de entrega ` && wa_kna1-kunnr && ` da solicitação não é uma filial`.

        erro_validacao = 'X'.

        MESSAGE _msg_erro TYPE 'I'.

        EXIT.

    ENDTRY.
    " 12.10.2022 - RAMON --- 93610 --<

    " 17.10.2022 - RAMON --- 93893 -->
  ELSEIF ( wa_saida-form_lote = 'X' OR wa_saida-indust = 'X' ).

    lv_kunnr = |{ wa_kna1-kunnr ALPHA = IN  }|.

    TRY .
        " 1 - Local de Entrega deve ser uma filial(Usar a Classe ZCL_CLIENTES->CK_PARCEIRO_LOCAL_NEGOCIO para verificação )
        zcl_clientes=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = lv_kunnr
        )->ck_parceiro_local_negocio(  ).

        lv_xwerks = |{ wa_kna1-kunnr ALPHA = IN  }|.

        SELECT SINGLE *
          FROM tvarvc INTO @DATA(lwa_tvarvc)
         WHERE name EQ 'ZSDT0132_EXC_FILIAL_LR'
           AND low  EQ @lv_xwerks.

        IF sy-subrc NE 0.
          SELECT SINGLE bukrs FROM j_1bbranch
            INTO lv_bukrs
              WHERE branch = lv_xwerks.

          SELECT SINGLE bukrs FROM j_1bbranch
            INTO lv_bukrs2
              WHERE branch = wa_t001w-werks.

          IF lv_bukrs = lv_bukrs2.
            _msg_erro = `Cód. Cliente Destino e cód. Filial pertence a mesma empresa - Tipo deve ser Pedido de Transferência`.
            erro_validacao = 'X'.
            MESSAGE _msg_erro TYPE 'I'.
            EXIT.
          ENDIF.
        ENDIF.

      CATCH zcx_parceiros.

    ENDTRY.

    " 17.10.2022 - RAMON --- 93893 --<

  ENDIF.
  " 26.09.2022 - RAMON - 19450 --<

  IF tinct-inco1 <> 'FOB'.
    IF wa_lfa1_pc-zone_desc IS INITIAL OR wa_kna1-zone_desc   IS INITIAL.
      _msg_erro =  TEXT-016.
    ENDIF.

    IF  wa_trolz-route       IS INITIAL .
      _msg_erro = TEXT-008.
    ENDIF.
  ENDIF.

*---------------------------------------------------------------------
* Verifica tamanho do campo
*---------------------------------------------------------------------
  DATA: l_vlrtotf TYPE c LENGTH 25,
        tam       TYPE i.

  "Verifica se a quantidade x Preço Pauta excede o tamanho do
*Elemento de dados DMBTR

  l_vlrtotf =  ( wa_id_compra-quantidade * wa_konp-kbetr ).
  CONDENSE l_vlrtotf.

  SPLIT  l_vlrtotf AT '.' INTO DATA(l_inteiro) DATA(l_decimal).

  tam        = strlen( l_inteiro ).

  IF tam > 11.
    MESSAGE s000(z_les) WITH 'Quantidade ' wa_id_compra-quantidade ' invalida!' INTO _msg_erro.
  ENDIF.

  IF wa_id_compra-quantidade EQ 0.
    MESSAGE s000(z_les) WITH 'Quantidade não informada!' INTO _msg_erro.
  ENDIF.

  "WHEN ( WA_ZSDT0158_SAIDA-KVGR4    IS INITIAL )    THEN TEXT-040 ).

*  IF ( ( TINCT-INCO1 = 'CIF' ) OR ( TINCT-INCO1 = 'CPT' ) ) AND ( PR_PRODUTO_F = ABAP_TRUE ).
*    IF IT_ID_COMPRA[] IS INITIAL.
*      _MSG_ERRO = 'Informar ID Compra!'.
*    ENDIF.
*  ENDIF.

  IF ( _msg_erro IS NOT INITIAL ).
    erro_validacao = 'X'.
    valida =  abap_true.
    MESSAGE _msg_erro TYPE 'I'. EXIT.
  ELSE.
    CLEAR: erro_validacao.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  salvar_dados_solicitacao
*&---------------------------------------------------------------------*
FORM salvar_solicitacao.
  DATA: vl_num_fl_pt     TYPE char10,
        vfilial          TYPE t001w-werks,
        vid_produto      TYPE matnr18,
        vid_produto_long TYPE mara-matnr.



  vfilial       =  |{ wa_t001w-werks ALPHA = IN }|.
  vid_produto   =  |{ wa_makt-matnr ALPHA = IN }|.

  vid_produto_long = vid_produto.


  IF tvv3t-kvgr3 EQ 'C'.
    vtp_produto = 'CO'.
  ELSEIF  tvv3t-kvgr3 EQ 'R'.
    vtp_produto = 'RR'.
  ELSE.
    vtp_produto = ' '.
  ENDIF.

  "wbarbosa US 152850 11/10/2024
*  SELECT *
*    FROM zmmt0017 INTO TABLE it_zmmt0017
*      WHERE centro_fixo EQ vfilial
*      AND   matnr       EQ vid_produto_long.


  CLEAR e_lgort.
  IF is_processo_eudr = abap_false.
    TRY.
        zcl_deposito=>zif_deposito~get_instance(
                 )->get_deposito_material_filial(
                 EXPORTING
                   i_matnr          = vid_produto_long  " Nº do material
                   i_tp_produto     = vtp_produto       " Tipo de Produto
                   i_bukrs          = wa_t001w-vkorg    "// Incluido uma variavel que esteja com a empresa preenchida 14102024 wbarbosa
                   i_branch         = vfilial           " Local de negócios
                 IMPORTING
                   e_lgort          = e_lgort  ).
      CATCH zcx_deposito.
    ENDTRY.
  ENDIF.
  "// wbarbosa US 152850 11/10/2024

  IF e_lgort IS INITIAL AND is_processo_eudr = abap_false.

    valida =  abap_true.
    MESSAGE 'Não existe parametrização para a Filial e Material informado! Favor realizar o cadastro na transação ZMM0017!' TYPE 'S'.
    EXIT.

  ENDIF.

  " wbarbosa US 152850 11/10/2024
*    LOOP AT it_zmmt0017 INTO wa_zmmt0017.
*
*      IF wa_zmmt0017-tp_produto = 'CO' AND vtp_produto = 'CO'.
*        MOVE wa_zmmt0017  TO  wa_t0017.
*
*      ELSEIF    wa_zmmt0017-tp_produto = 'RR' AND vtp_produto = 'RR'.
*        MOVE wa_zmmt0017  TO  wa_t0017.
*      ELSE.
*        MOVE wa_zmmt0017  TO  wa_t0017.
*
*      ENDIF.
*      CLEAR: wa_zmmt0017.
*    ENDLOOP.
  " wbarbosa US 152850 11/10/2024


  PERFORM valida_solicitacao_ov.

  CHECK valida = abap_false.

  " 26.09.2022 - RAMON -->
  IF wa_saida-ped_trans IS INITIAL.
    " 26.09.2022 - RAMON -->

    TRY .

        DATA(at_kna1) = CAST zcl_clientes(
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = |{ wa_kna1-kunnr ALPHA = IN }| ) )->at_kna1.

        DATA(at_lfa1) = CAST zcl_fornecedores(
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = |{ wa_lfa1_z1-lifnr ALPHA = IN }| ) )->at_lfa1.

        IF at_kna1-stcd1 EQ at_lfa1-stcd1.

          DATA: lc_titlebar      TYPE string,
                lc_text_question TYPE string,
                answer           TYPE c.

          lc_titlebar = 'Frete sem Transbordo (Direto)'.
          lc_text_question = |Parceiro LR e Z1 são iguais!. Vai existir transbordo?|.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = lc_titlebar
              text_question         = lc_text_question
              text_button_1         = TEXT-101
              icon_button_1         = 'ICON_CHECKED'
              text_button_2         = TEXT-102
              icon_button_2         = 'ICON_INCOMPLETE'
              default_button        = '1'
              display_cancel_button = ' '
            IMPORTING
              answer                = answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF answer EQ '1'.
            erro_validacao = 'X'.
            valida =  abap_true.
            MESSAGE |Parceiros LR e Z1 não devem possuir o mesmo CNPJ { at_lfa1-stcd1 }| TYPE 'I'. EXIT.
          ENDIF.

        ENDIF.

      CATCH zcx_parceiros INTO DATA(ex_parceiros).
        erro_validacao = 'X'.
        valida =  abap_true.
        MESSAGE ex_parceiros->zif_error~get_msg_erro( ) TYPE 'I'. EXIT.
    ENDTRY.

  ENDIF. " 26.09.2022 - RAMON - 19450

  IF ( wa_saida-sequencial EQ '$00000001' ).

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSD_FL_PT'
      IMPORTING
        number                  = vl_num_fl_pt
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    MOVE vl_num_fl_pt TO wa_saida-sequencial.

  ENDIF.

  IF ( erro_validacao IS INITIAL ).

    IF ( it_id_compra[] IS NOT INITIAL ).
      wa_id_compra-quantidade = it_id_compra[ 1 ]-quantidade.
      wa_id_compra-unidade    = it_id_compra[ 1 ]-unidade.
    ENDIF.

    wa_zsdt0158_saida-sequencial          = wa_saida-sequencial.
    wa_zsdt0158_saida-nro_sol_ov          = wa_saida-nro_sol_ov.
    wa_zsdt0158_saida-status              = 'G'.
    wa_zsdt0158_saida-filial              = |{ wa_t001w-werks ALPHA = IN }|.
    wa_zsdt0158_saida-id_ponto_coleta     = |{ wa_lfa1_pc-lifnr ALPHA = IN }|.
    wa_zsdt0158_saida-id_local_destino    = |{ wa_kna1-kunnr ALPHA = IN }|.

*        wa_zsdt0158_saida-id_produto          = |{ wa_makt-matnr ALPHA = IN }|.
    wa_zsdt0158_saida-id_produto          = vid_produto_long.
    wa_zsdt0158_saida-quantidade          = wa_zsdt0158_saida-quantidade.
    wa_zsdt0158_saida-unidade             = wa_mara-meins.
    wa_zsdt0158_saida-tp_producao         = tvv3t-kvgr3.
    wa_zsdt0158_saida-safra               = wa_zsdt0158_saida-safra.
    wa_zsdt0158_saida-tp_frete            = tinct-inco1.
    wa_zsdt0158_saida-ag_frete            = wa_frete-exige_ag_frete.
    wa_zsdt0158_saida-id_ag_frete         = wa_frete-ag_frete.

    CALL FUNCTION 'J_1B_COMPANY_DETERMINE'
      EXPORTING
        plant                    = wa_t001w-werks
      IMPORTING
        company                  = wa_zsdt0158_saida-bukrs
      EXCEPTIONS
        plant_not_found          = 1
        valuation_area_not_found = 2
        company_not_found        = 3
        OTHERS                   = 4.
    IF sy-subrc <> 0.
      MESSAGE |Empresa não encontrada para a filial { wa_t001w-werks }!| TYPE 'E'.
      RETURN.
    ENDIF.

    IF wa_saida-ped_trans IS NOT INITIAL.
      wa_zsdt0158_saida-tp_solicitacao      = 'P'. "Pedido
      wa_zsdt0158_saida-lgort               = e_lgort.
    ELSE.
      wa_zsdt0158_saida-tp_solicitacao      = 'O'. "Ordem Venda
      wa_zsdt0158_saida-id_terminal         = |{ wa_lfa1_z1-lifnr ALPHA = IN }|.
      wa_zsdt0158_saida-vlr_pauta           = wa_konp-kbetr.
      wa_zsdt0158_saida-dco = COND string(
          WHEN ( check_dco IS NOT INITIAL ) AND ( wa_zdco_prod-nr_dco IS NOT INITIAL ) THEN 'S' ELSE 'N').

    ENDIF.
    "// wbarbosa US 152850 10/10/2024

    wa_zsdt0158_saida-nr_dco              = wa_zdco_prod-nr_dco.
    wa_zsdt0158_saida-waers               = wa_konp-konwa.

    IF wa_zsdt0158_saida-waers IS INITIAL.
      wa_zsdt0158_saida-waers = 'BRL'.
    ENDIF.

    wa_zsdt0158_saida-zona_pc             = wa_lfa1_pc-lzone.
    wa_zsdt0158_saida-zona_lr             = wa_kna1-lzone.
    wa_zsdt0158_saida-rg_atualizado       = 0.
    wa_zsdt0158_saida-usnam               = sy-uname.
    wa_zsdt0158_saida-data_atual          = sy-datum.
    wa_zsdt0158_saida-hora_atual          = sy-uzeit.
*    WA_ZSDT0158_SAIDA-ID_FORNECEDOR       = |{ WA_FORNECEDOR-LIFNR ALPHA = IN }|.
    wa_zsdt0158_saida-quantidade = COND string( WHEN wa_id_compra-quantidade IS NOT INITIAL THEN wa_id_compra-quantidade ).
    wa_zsdt0158_saida-unidade    = COND string( WHEN wa_id_compra-unidade    IS NOT INITIAL THEN 'KG' ).
*    WA_ZSDT0158_SAIDA-PR_PRODUTO = COND STRING( WHEN PR_PRODUTO_A EQ ABAP_TRUE THEN 'A'
*                                                WHEN PR_PRODUTO_F EQ ABAP_TRUE THEN 'F' ).

    " 05.07.2022 - RAMON - 76636 ->
    IF wa_saida-indust IS NOT INITIAL.
      wa_zsdt0158_saida-industrializacao = 'S'.
    ELSE.
      wa_zsdt0158_saida-industrializacao = 'N'.
    ENDIF.
    " 05.07.2022 - RAMON - 76636 -<

    "wbarbosa US 152850 11/10/2024
    "Verifica as regras de validações da UEDR
    IF is_processo_eudr = abap_true.

      wa_zsdt0158_saida-eudr  = wa_saida-eudr.

      zcl_eudr_utils=>check_regra_solicitacao_eudr(
        EXPORTING
           i_tipo   = COND #(  WHEN wa_saida-form_lote EQ abap_true THEN 'F'   "// Formação de Lote
                               WHEN wa_saida-indust    EQ abap_true THEN 'I'   "// Industrialização
                               WHEN wa_saida-ped_trans EQ abap_true THEN 'P'  "// Pedido
                            )
        CHANGING
           c_zsdt0158 = wa_zsdt0158_saida " Tabela Solicitação Formação Lote e Pedido Transferência
      ).

      IF wa_zsdt0158_saida-lgort IS NOT INITIAL.
        PERFORM check_material_centro USING wa_zsdt0158_saida-lgort.
        IF valida EQ abap_true.
          RETURN.
        ENDIF.
      ENDIF.

    ELSE.
      CLEAR: wa_zsdt0158_saida-eudr.
    ENDIF.
    "wbarbosa US 152850 11/10/2024

    CHECK ( wa_zsdt0158_saida IS NOT INITIAL ).

    PERFORM grava_dados_ov.

  ENDIF.




ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS_OV
*&---------------------------------------------------------------------*
FORM grava_dados_ov.

  DATA: _quantidade_total TYPE zde_qtd_total.

  " 05.05.2022 - RAMON LIMA - RECLIKE - 76054 --->
  PERFORM f_recupera_zsdt0187 USING wa_zsdt0158_saida.
  " 05.05.2022 - RAMON LIMA - RECLIKE - 76054 ---<

  LOOP AT it_id_compra[] INTO DATA(_id_compra).

    " 03.05.2022 - RAMON LIMA - RECLIKE -->
*    UPDATE zsdt0187   SET ov_ped = abap_true
*     WHERE id_compra  EQ _id_compra-id_compra
*       AND data_atual EQ _id_compra-data_atual.
*
*    COMMIT WORK.
    " 03.05.2022 - RAMON LIMA - RECLIKE --<

    DATA(_zsdt0158_id) = VALUE zsdt0158_id( nro_sol_ov      = ''
                                            id_compra       = _id_compra
                                            quantidade      = _id_compra-quantidade
                                            unidade         = _id_compra-unidade
                                            tp_frete_compra = _id_compra-tp_frete_compra
                                            usnam           = sy-uname
                                            data_atual      = sy-datum
                                            hora_atual      = sy-uzeit
                                            sequencial      = wa_zsdt0158_saida-sequencial ).
    MODIFY zsdt0158_id FROM _zsdt0158_id.

    _quantidade_total = _quantidade_total + _id_compra-quantidade.

  ENDLOOP.

  IF ( _quantidade_total IS NOT INITIAL ).
    wa_zsdt0158_saida-quantidade = _quantidade_total.
  ENDIF.

  " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 -->
  PERFORM f_insert_zsdt0187 USING wa_zsdt0158_saida.
  PERFORM f_modify_zsdt0187 USING wa_zsdt0158_saida.
  " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 --<

  MODIFY zsdt0158 FROM wa_zsdt0158_saida.
  COMMIT WORK.

  IF ( sy-subrc EQ 0 ).
    CLEAR: wa_zsdt0158-tipo.
    MESSAGE TEXT-021 TYPE 'S'.
  ENDIF.

  DATA(vl_sequencial) = wa_zsdt0158_saida-sequencial.
  DATA(vl_tipo)       = wa_zsdt0158_saida-tp_solicitacao.

  PERFORM limpa_dados.

  wa_saida-sequencial = vl_sequencial.

*  IF ( vl_tipo EQ 'O' ).
  PERFORM busca_dados_ov.
*  ELSE.
*    PERFORM busca_dados_pt.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_OV
*&---------------------------------------------------------------------*
FORM busca_dados_ov.

  PERFORM limpa_dados.

  wa_saida-sequencial = |{ wa_saida-sequencial ALPHA = IN }|.

  SELECT SINGLE * FROM zsdt0158 INTO wa_zsdt0158_saida WHERE sequencial EQ wa_saida-sequencial.

  PERFORM pf_check_filial USING wa_zsdt0158_saida-filial vl_subrc.

  CHECK vl_subrc IS INITIAL.

  IF wa_saida-sequencial IS NOT INITIAL.

    SELECT SINGLE * FROM zsdt0187 INTO @DATA(lw_187)
      WHERE sequencial = @wa_saida-sequencial.

    IF sy-subrc EQ 0.

      PERFORM f_dados_lote_tela
        USING lw_187-nu_lote
              lw_187-id_forn
              ''
              lw_187-quantidade.
    ENDIF.

  ENDIF.

  MOVE wa_zsdt0158_saida-sequencial       TO wa_saida-sequencial.
  MOVE wa_zsdt0158_saida-nro_sol_ov       TO wa_saida-nro_sol_ov.
  MOVE wa_zsdt0158_saida-filial           TO wa_t001w-werks.
  MOVE wa_zsdt0158_saida-id_ponto_coleta  TO wa_lfa1_pc-lifnr.
  MOVE wa_zsdt0158_saida-id_local_destino TO wa_kna1-kunnr.
  MOVE wa_zsdt0158_saida-id_terminal      TO wa_lfa1_z1-lifnr.
  MOVE wa_zsdt0158_saida-id_produto       TO wa_makt-matnr.
  MOVE wa_zsdt0158_saida-quantidade       TO wa_zsdt0158_saida-quantidade.
  MOVE wa_zsdt0158_saida-unidade          TO wa_mara-meins.
  MOVE wa_zsdt0158_saida-tp_producao      TO tvv3t-kvgr3.
  MOVE wa_zsdt0158_saida-safra            TO wa_zsdt0158_saida-safra.
  MOVE wa_zsdt0158_saida-tp_frete         TO tinct-inco1.
  MOVE wa_zsdt0158_saida-ag_frete         TO wa_frete-exige_ag_frete.
  MOVE wa_zsdt0158_saida-id_ag_frete      TO wa_frete-ag_frete.
  MOVE wa_zsdt0158_saida-vlr_pauta        TO wa_konp-kbetr.
  MOVE wa_zsdt0158_saida-id_fornecedor    TO wa_fornecedor-lifnr.
  MOVE wa_zsdt0158_saida-eudr             TO wa_saida-eudr. "WPP US 152850 10/10/2024

*  PR_PRODUTO_A = COND STRING( WHEN WA_ZSDT0158_SAIDA-PR_PRODUTO EQ 'A' THEN ABAP_TRUE ).
*  PR_PRODUTO_F = COND STRING( WHEN WA_ZSDT0158_SAIDA-PR_PRODUTO EQ 'F' THEN ABAP_TRUE ).

  wa_saida-status_solicitacao = COND string( WHEN ( wa_zsdt0158_saida-status EQ 'G' ) THEN icon_initial
                                             WHEN ( wa_zsdt0158_saida-status EQ 'L' ) THEN icon_release ).

*  IF ( PR_PRODUTO_A EQ ABAP_TRUE ) OR ( TINCT-INCO1 EQ 'FOB' ).
  wa_id_compra-quantidade = wa_zsdt0158_saida-quantidade.
  wa_id_compra-unidade    = wa_zsdt0158_saida-unidade.
*  ENDIF.

  IF ( wa_zsdt0158_saida-dco EQ 'S' ).
    check_dco = 'X'.
    wa_zdco_prod-nr_dco = wa_zsdt0158_saida-nr_dco.
  ENDIF.

  IF ( wa_zsdt0158_saida-tp_frete IS INITIAL ) OR ( wa_kna1-kunnr IS INITIAL ).
    MESSAGE TEXT-025 TYPE 'I'.
  ELSE.

    PERFORM busca_filial USING wa_t001w-werks.

    PERFORM busca_pc USING wa_lfa1_pc-lifnr.
    PERFORM busca_lr USING wa_kna1-kunnr.

    PERFORM busca_material.
    PERFORM valida_ag_frete.
*    PERFORM BUSCA_FORNECEDOR.

    wa_lfa1_z1-lifnr =  |{ wa_lfa1_z1-lifnr ALPHA = OUT  }|.

    IF ( wa_kna1-kunnr EQ wa_lfa1_z1-lifnr ).
      check_z1 = 'X'.
      PERFORM preenche_z1.
    ELSE.
      IF ( wa_lfa1_z1-lifnr IS NOT INITIAL ).
        PERFORM busca_terminal USING wa_lfa1_z1-lifnr.
      ENDIF.
    ENDIF.


    IF ( wa_zsdt0158_saida-tp_solicitacao EQ 'O' ) OR
       ( wa_zsdt0158_saida-tp_solicitacao EQ 'P' ).

      PERFORM busca_preco_pauta.

      MOVE wa_zsdt0158_saida-zona_pc    TO wa_lfa1_pc-lzone.
      MOVE wa_zsdt0158_saida-zona_lr    TO wa_kna1-lzone.

      PERFORM busca_itinerario.

      SELECT * FROM zsdt0158_id INTO TABLE @DATA(tl_zsdt0158_id)
        WHERE nro_sol_ov EQ @wa_saida-nro_sol_ov
          AND sequencial EQ @wa_saida-sequencial.

      IF ( sy-subrc = 0 ).
        it_id_compra[] = CORRESPONDING #( tl_zsdt0158_id ).
        obj_alv->refresh_table_display( ).
      ENDIF.

    ENDIF.

  ENDIF.

  " 22.09.2022 - RAMON 19450 -->
*  CASE wa_zsdt0158_saida-tp_solicitacao.
*    WHEN 'O' OR space.

  i_main_tab-pressed_tab = c_main_tab-tab1.
  i_main_tab-subscreen = '0102'.

*    WHEN OTHERS.
*
*      i_main_tab-pressed_tab = c_main_tab-tab2.
*      i_main_tab-subscreen = '0103'.
*
*  ENDCASE.
  " 22.09.2022 - RAMON 19450 --<

*  IF ( wa_zsdt0158_saida-tp_solicitacao EQ 'O' ) OR (  wa_zsdt0158_saida-tp_solicitacao IS INITIAL ).
*    i_main_tab-pressed_tab = c_main_tab-tab1.
*    i_main_tab-subscreen = '0102'.
*  ELSE.
*    i_main_tab-pressed_tab = c_main_tab-tab2.
*    i_main_tab-subscreen = '0103'.
*  ENDIF.

  CLEAR: wa_saida-indust, wa_saida-form_lote, wa_saida-indefinido, wa_saida-ped_trans.

  " 05.07.2022 - RAMON - 83138 ->
  IF wa_zsdt0158_saida-industrializacao = 'S'.
    wa_saida-indust = 'X'.
    wa_saida-form_lote = ''.
  ELSE.
    wa_saida-indust = ''.
    wa_saida-form_lote = 'X'.
  ENDIF.
  " 05.07.2022 - RAMON - 83138 -<

  " 22.09.2022 - RAMON 19450 -->
  IF wa_zsdt0158_saida-tp_solicitacao EQ 'P'.

    CLEAR: wa_saida-indust, wa_saida-form_lote, wa_saida-indefinido.

    wa_saida-ped_trans = 'X'.

    wa_saida-ebeln = wa_zsdt0158_saida-ebeln.
    "wa_saida-tp_solicitacao = wa_zsdt0158_saida-tp_solicitacao.

  ENDIF.
  " 22.09.2022 - RAMON 19450 --<

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_PT
*&---------------------------------------------------------------------*
FORM busca_dados_pt.

  PERFORM limpa_dados.

  wa_saida-nro_sol_ov = |{ wa_saida-nro_sol_ov ALPHA = IN }|.

  SELECT SINGLE * FROM zsdt0158 INTO wa_zsdt0158_saida
    WHERE sequencial     NE ' '
      AND nro_sol_ov     EQ wa_saida-nro_sol_ov
      AND tp_solicitacao EQ 'P'.

  MOVE wa_zsdt0158_saida-nro_sol_ov       TO wa_zsdt0158_saida-nro_sol_ov.
  MOVE wa_zsdt0158_saida-filial           TO wa_t001w-werks.
  MOVE wa_zsdt0158_saida-id_ponto_coleta  TO wa_lfa1_pc-lifnr.
  MOVE wa_zsdt0158_saida-id_local_destino TO wa_kna1-kunnr.
  MOVE wa_zsdt0158_saida-id_terminal      TO wa_lfa1_z1-lifnr.
  MOVE wa_zsdt0158_saida-id_produto       TO wa_makt-matnr.
  MOVE wa_zsdt0158_saida-quantidade       TO wa_zsdt0158_saida-quantidade.
  MOVE wa_zsdt0158_saida-unidade          TO wa_mara-meins.
  MOVE wa_zsdt0158_saida-tp_producao      TO tvv3t-kvgr3.
  MOVE wa_zsdt0158_saida-safra            TO wa_zsdt0158_saida-safra.
  MOVE wa_zsdt0158_saida-tp_frete         TO tinct-inco1.
  MOVE wa_zsdt0158_saida-ag_frete         TO wa_frete-exige_ag_frete.
  MOVE wa_zsdt0158_saida-id_ag_frete      TO wa_frete-ag_frete.
  MOVE wa_zsdt0158_saida-vlr_pauta        TO wa_konp-kbetr.
  MOVE wa_zsdt0158_saida-zona_pc          TO wa_kna1-lzone.
  MOVE wa_zsdt0158_saida-zona_lr          TO wa_lfa1_z1-lzone.

  IF ( wa_zsdt0158_saida-status EQ 'G' ).
    wa_saida-status_solicitacao = icon_initial.
  ELSEIF ( wa_zsdt0158_saida-status EQ 'L' ).
    wa_saida-status_solicitacao = icon_release.
  ENDIF.


  IF ( wa_zsdt0158_saida-tp_frete IS INITIAL ) OR ( wa_kna1-kunnr IS INITIAL ).
    MESSAGE TEXT-025 TYPE 'I'.
  ELSE.

    PERFORM busca_filial  USING wa_t001w-werks.
    PERFORM busca_pc      USING wa_lfa1_pc-lifnr.
    PERFORM busca_lr      USING wa_kna1-kunnr.
    PERFORM busca_itinerario.
    PERFORM busca_material.
    PERFORM valida_ag_frete.

  ENDIF.

  i_main_tab-pressed_tab = c_main_tab-tab2.
  i_main_tab-subscreen = '0102'.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_SOLICITACOES
*&---------------------------------------------------------------------*
FORM f4_busca_solicitacoes.

  SELECT * FROM zsdt0158 INTO TABLE @DATA(tl_zsdt0158) ORDER BY sequencial DESCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'SEQUENCIAL'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-SEQUENCIAL'
      value_org   = 'S'
    TABLES
      value_tab   = tl_zsdt0158[].

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ENVIA_SOLICITACAO
*&---------------------------------------------------------------------*
FORM envia_solicitacao .

  wa_saida-sequencial = |{ wa_saida-sequencial ALPHA = IN }|.

  SELECT SINGLE * FROM zsdt0158 INTO @DATA(w_zsdt0158) WHERE sequencial = @wa_saida-sequencial.

  PERFORM pf_check_filial USING w_zsdt0158-filial vl_subrc.
  CHECK vl_subrc IS INITIAL.

* CS2020000143
* Limpar o campo mensagem quando clicar no botão enviar.
*
  UPDATE zsdt0158 SET status = 'L' rg_atualizado = '0' mensagem = ''
     WHERE sequencial EQ wa_saida-sequencial.

  IF ( sy-subrc EQ 0 ).
    MESSAGE TEXT-031 TYPE 'S'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  MODIFY_PED_TRANSF  INPUT
*&---------------------------------------------------------------------*
MODULE modify_ped_transf INPUT.

  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN 'BUSCAR'.

      PERFORM busca_filial      USING wa_t001w-werks.
      PERFORM busca_pc          USING wa_lfa1_pc-lifnr.
      PERFORM busca_filial_dest USING wa_filial_dest-werks.
      PERFORM busca_itinerario.
      PERFORM busca_material.

  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  BUSCA_FILIAL_DEST
*&---------------------------------------------------------------------*
FORM busca_filial_dest USING p_wa_filial_dest_werks.

  DATA: wl_branch_detail TYPE bapibranch.

  IF ( wa_filial_dest-werks NE wa_filial_dest2-werks ).

    CLEAR:  wa_filial_dest-cnpj,  wa_filial_dest-j_1bbranch,
            wa_filial_dest-name1, wa_filial_dest-regio,
            wa_filial_dest-vkorg, wa_kna1,  wa_trolz-route.

    MOVE wa_filial_dest-werks TO wa_filial_dest2-werks.

    SELECT SINGLE werks, name1, j_1bbranch, vkorg, regio
      FROM t001w INTO @DATA(w_t0001w)
      WHERE werks = @p_wa_filial_dest_werks.

    IF ( sy-subrc = 0 ).

      wa_filial_dest = CORRESPONDING #( w_t0001w ).

      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
        EXPORTING
          company       = wa_filial_dest-vkorg
          branch        = wa_filial_dest-j_1bbranch
        IMPORTING
          branch_detail = wl_branch_detail.

      DATA(vl_cnpj_destino) = wl_branch_detail-cgc_number.

    ENDIF.

    IF ( vl_cnpj_destino IS NOT INITIAL ).

      SELECT SINGLE kunnr, land1, name1, stras, ort01, regio, stcd1, stcd3, lzone
        FROM kna1 INTO @DATA(w_kna1) WHERE stcd1 = @vl_cnpj_destino AND land1 EQ 'BR'.

      IF ( sy-subrc = 0 ).
        wa_kna1 = CORRESPONDING #( w_kna1 ).
      ENDIF.

      IF ( wa_kna1-lzone IS NOT INITIAL ).

        SELECT SINGLE vtext FROM tzont INTO wa_kna1-zone_desc
          WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_kna1-lzone.

      ELSE.
        MESSAGE 'Zona de transporte não encontrada para destino' TYPE 'I'.
      ENDIF.

    ELSE.
      MESSAGE TEXT-033 TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  VALIDA_SOLICITACAO_PT
*&---------------------------------------------------------------------*
*FORM valida_solicitacao_pt .
*
*  DATA(_msg_erro) = COND string(
*      WHEN ( wa_zsdt0158-tipo     IS INITIAL )    THEN text-015
*      WHEN ( wa_t001w-name1       IS INITIAL )    THEN text-005
*      WHEN ( wa_lfa1_pc-name1     IS INITIAL )    THEN text-006
*      WHEN ( wa_lfa1_pc-zone_desc IS INITIAL  OR wa_kna1-zone_desc IS INITIAL ) THEN text-016
*      WHEN ( wa_kna1-name1        IS INITIAL )    THEN text-032
*      WHEN ( wa_makt-maktx        IS INITIAL  OR tvv3t-kvgr3 IS INITIAL OR wa_zsdt0158_saida-safra IS INITIAL ) THEN text-010
*      WHEN ( tinct-inco1          IS INITIAL )    THEN text-012
*      WHEN ( wa_trolz-route       IS INITIAL )    THEN text-008 ).
*
*  IF ( _msg_erro IS NOT INITIAL ).
*    erro_validacao = 'X'.
*    MESSAGE _msg_erro TYPE 'I'. EXIT.
*  ENDIF.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS_PT
*&---------------------------------------------------------------------*
*FORM organiza_dados_pt .
*
*  PERFORM valida_solicitacao_pt.
*
*  IF ( erro_validacao IS INITIAL ).
*
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr             = '01'
*        object                  = 'ZSD_FL_PT'
*      IMPORTING
*        number                  = wa_zsdt0158_saida-sequencial
*      EXCEPTIONS
*        interval_not_found      = 1
*        number_range_not_intern = 2
*        object_not_found        = 3
*        quantity_is_0           = 4
*        quantity_is_not_1       = 5
*        interval_overflow       = 6
*        buffer_overflow         = 7
*        OTHERS                  = 8.
*
*    wa_zsdt0158_saida-sequencial          = wa_saida-sequencial.
*    wa_zsdt0158_saida-nro_sol_ov          = wa_saida-nro_sol_ov.
*    wa_zsdt0158_saida-tp_solicitacao      = 'P'.
*    wa_zsdt0158_saida-status              = 'G'.
*    wa_zsdt0158_saida-filial              = wa_t001w-werks.
*    wa_zsdt0158_saida-id_ponto_coleta     = wa_lfa1_pc-lifnr.
*    wa_zsdt0158_saida-id_local_destino    = wa_kna1-kunnr.
*    wa_zsdt0158_saida-id_produto          = wa_makt-matnr.
*    wa_zsdt0158_saida-quantidade          = wa_zsdt0158_saida-quantidade.
*    wa_zsdt0158_saida-unidade             = wa_mara-meins.
*    wa_zsdt0158_saida-tp_producao         = tvv3t-kvgr3.
*    wa_zsdt0158_saida-safra               = wa_zsdt0158_saida-safra.
*    wa_zsdt0158_saida-tp_frete            = tinct-inco1.
*    wa_zsdt0158_saida-zona_pc             = wa_lfa1_pc-lzone.
*    wa_zsdt0158_saida-zona_lr             = wa_kna1-lzone.
*    wa_zsdt0158_saida-usnam               = sy-uname.
*    wa_zsdt0158_saida-data_atual          = sy-datum.
*    wa_zsdt0158_saida-hora_atual          = sy-uzeit.
*
*    " 05.07.2022 - RAMON - 76636 ->
*    IF wa_zsdt0158_saida-industrializacao IS NOT INITIAL.
*      wa_zsdt0158_saida-industrializacao = 'S'.
*    ELSE.
*      wa_zsdt0158_saida-industrializacao = 'N'.
*    ENDIF.
*    " 05.07.2022 - RAMON - 76636 -<
*
*    CHECK ( wa_zsdt0158_saida IS NOT INITIAL ).
*
*    PERFORM grava_dados_ov.
*
*  ENDIF.
*
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_SOLICITACOES
*&---------------------------------------------------------------------*
FORM busca_solicitacoes . "Minhas solicitações / histórico

  DATA: ls_selfield  TYPE slis_selfield.

  SELECT    z~sequencial,
            z~nro_sol_ov,
            e~vbeln,
            z~tp_solicitacao,
            z~status,
            z~filial,
            t~name1,
            z~id_ponto_coleta,
            z~id_local_destino,
            z~id_terminal,
            z~id_filial_destino,
            z~id_produto,
            z~quantidade,
            z~tp_producao,
            z~safra,
            z~data_atual,
            z~usnam
    FROM zsdt0158 AS z
    INNER JOIN t001w AS t ON z~filial = t~werks
    LEFT JOIN zsdt0066 AS e ON e~nro_sol_ov EQ z~nro_sol_ov
    INTO TABLE @DATA(tl_resumo)
    WHERE z~usnam EQ @sy-uname
    ORDER BY z~sequencial DESCENDING.

  CHECK ( tl_resumo[] IS NOT INITIAL ).

  LOOP AT tl_resumo[] ASSIGNING FIELD-SYMBOL(<wl_resumo>).

    <wl_resumo>-filial = COND string( WHEN ( <wl_resumo>-status = 'G' ) THEN icon_initial
                                      WHEN ( <wl_resumo>-status = 'L' ) THEN icon_release
                                      ELSE '').
  ENDLOOP.

  DATA(tl_fieldcat) = VALUE slis_t_fieldcat_alv(
      ( fieldname = 'SEQUENCIAL'        seltext_m = 'Sequencial'      just = 'X ')
      ( fieldname = 'NRO_SOL_OV'        seltext_m = 'Nro.Solicitacao' just = 'X' )
      ( fieldname = 'VBELN     '        seltext_m = 'Nro.Ordem Venda' just = 'X' )
      ( fieldname = 'TP_SOLICITACAO'    seltext_m = 'Tipo'            just = 'X' )
      ( fieldname = 'STATUS'            seltext_m = 'Status'          just = 'X' no_out = 'X')
      ( fieldname = 'FILIAL'            seltext_m = 'Status'          just = 'X' )
      ( fieldname = 'NAME1'             seltext_m = 'Filial'          just = 'X' )
      ( fieldname = 'ID_PONTO_COLETA'   seltext_m = 'PC'              just = 'X' )
      ( fieldname = 'ID_LOCAL_DESTINO'  seltext_m = 'LD'              just = 'X' )
      ( fieldname = 'ID_TERMINAL'       seltext_m = 'Terminal'        just = 'X' )
      ( fieldname = 'ID_FILIAL_DESTINO' seltext_m = 'Filial Dest'     just = 'X' )
      ( fieldname = 'ID_PRODUTO'        seltext_m = 'Material'        just = 'X' )
      ( fieldname = 'QUANTIDADE'        seltext_m = 'Quantidade'      just = 'X' )
      ( fieldname = 'TP_PRODUCAO'       seltext_m = 'Tp. Producao'    just = 'X' )
      ( fieldname = 'SAFRA'             seltext_m = 'Safra'           just = 'X' )
      ( fieldname = 'DATA_ATUAL'        seltext_m = 'Data'            just = 'X' )
      ( fieldname = 'USNAM'             seltext_m = 'Usuário'         just = 'X' ) ).

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'Solicitações feitas pelo usuário'
      i_selection           = 'X'
      i_zebra               = 'X'
*     I_ALLOW_NO_SELECTION  = 'X'
      i_tabname             = 'TL_RESUMO'
      it_fieldcat           = tl_fieldcat
      i_screen_start_column = 1
      i_screen_end_column   = 120
      i_screen_start_line   = 1
      i_screen_end_line     = 15
    IMPORTING
      es_selfield           = ls_selfield
    TABLES
      t_outtab              = tl_resumo[]
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

  CHECK ( ls_selfield IS NOT INITIAL ).

  CLEAR: wa_zsdt0158-tipo.
  DATA(wl_resumo) = tl_resumo[ ls_selfield-tabindex ].

  IF ( wl_resumo-tp_solicitacao EQ 'O' ).

    wa_saida-sequencial     = wl_resumo-sequencial.
    wa_saida-nro_sol_ov     = wl_resumo-nro_sol_ov.
    i_main_tab-pressed_tab  = c_main_tab-tab1.
    i_main_tab-subscreen    = '0102'.

    PERFORM busca_dados_ov.

  ELSE.

    wa_saida-sequencial     = wl_resumo-sequencial.
    wa_saida-nro_sol_ov     = wl_resumo-nro_sol_ov.
    i_main_tab-pressed_tab  = c_main_tab-tab2.
    i_main_tab-subscreen    = '0103'.

    PERFORM busca_dados_pt.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  LIMPA_DADOS
*&---------------------------------------------------------------------*
FORM limpa_dados.

  CLEAR:     wa_zsdt0158-include, wa_zsdt0158-status_icon, wa_t001w, wa_j_1bbranch, wa_lfa1_pc, wa_kna1, wa_lfa1_z1, wa_zsdt0158_saida, wa_makt,
             wa_mara, wa_frete, wa_konp, wa_zdco_prod, check_dco, check_z1, tinct-inco1, tvv3t-kvgr3, wa_zsdt0158_aux,  wa_saida-filial_desc, wa_saida-nro_sol_ov, wa_saida-indust,wa_saida-form_lote,
             wa_saida-pc_desc,  wa_saida-propriedade_desc, wa_saida-produto_desc, wa_saida-destino_desc, wa_lfa1_pc_aux,  wa_lfa1_z1_aux, wa_kna1_aux, wa_saida-ag_frete_desc,
             wa_saida-status_itinerario, wa_saida-status_solicitacao, wa_t001w_aux, wa_fornecedor, wa_trolz, wa_saida-itinerario_desc, wa_id_compra,
             it_t001w[], it_j_1bbranch[], it_lfa1_pc[], it_lfa1_z1[], it_makt[], it_mara[], it_kna1[], it_trolz[], it_saida[], it_konh[], it_konp[], it_frete[], it_zlest0153[],
             it_zsdt0158[], it_zsdt0158_aux[], it_zsdt0158_saida[], it_id_compra[], pr_produto_a, wa_tvv4t,
             wa_saida-eudr."WPP US 152850 10/10/2024

*  OBJ_ALV->REFRESH_TABLE_DISPLAY( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_ID_COMPRA
*&---------------------------------------------------------------------*
FORM cria_alv_id_compra.

  TYPES: t_fieldcat TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  IF ( obj_container IS INITIAL ).

    CREATE OBJECT obj_container EXPORTING container_name = 'CONTAINER_ID'.
    CREATE OBJECT obj_alv EXPORTING i_parent = obj_container.

    DATA(wl_layout) = VALUE lvc_s_layo( zebra = 'X' no_rowmark = 'X' info_fname = 'X' no_toolbar = 'X' ).

    DATA(tl_fieldcat) = VALUE t_fieldcat(
      ( fieldname = 'ID_COMPRA'       coltext =  'ID Compra'  outputlen = '15'  just = 'C' ref_field = 'ID_COMPRA'   ref_table = 'ZSDT0187' )
      ( fieldname = 'QUANTIDADE'      coltext =  'Quantidade' outputlen = '20'  just = 'C' ref_field = 'QUANTIDADE'  ref_table = 'ZSDT0187' )
      ( fieldname = 'UNIDADE'         coltext =  'UN'         outputlen = '04'  just = 'C' ref_field = 'UN'          ref_table = 'ZSDT0187' )
      ( fieldname = 'TP_FRETE_COMPRA' coltext =  'Frete'      outputlen = '05'  just = 'C' ref_field = 'FRETE'       ref_table = 'ZSDT0187' ) ).

    CALL METHOD obj_alv->set_table_for_first_display
      EXPORTING
        is_layout       = wl_layout
      CHANGING
        it_outtab       = it_id_compra[]
        it_fieldcatalog = tl_fieldcat[].

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  BUSCA_ID_COMPRA
*&---------------------------------------------------------------------*
FORM busca_id_compra.

  DATA: linha_selecionada TYPE slis_selfield,
        _exit             TYPE c.
  DATA vmatnr18           TYPE matnr18.
  DATA _id_produto        TYPE matnr.

  IF ( pr_produto_a EQ abap_true ).
    MESSAGE 'Tipo de Produto à Fixar. Impossível definir ID de Compra' TYPE 'I'. EXIT.
  ENDIF.

  DATA(msg_ero) = COND string(  WHEN ( wa_t001w-werks IS INITIAL )          THEN 'Informar Filial'
                                WHEN ( wa_zsdt0158_saida-safra IS INITIAL ) THEN 'Informar Safra'
                                WHEN ( wa_makt-matnr IS INITIAL )           THEN 'Informar Material'
                                WHEN ( tvv3t-kvgr3 IS INITIAL )             THEN 'Informar Tipo Produção'
                                WHEN ( wa_fornecedor-lifnr IS INITIAL )     THEN 'Informar Fornecedor'
                                ELSE 'OK').

  IF ( msg_ero = 'OK' ).

    CASE tinct-inco1.

      WHEN 'CIF' OR 'CPT'.

        wa_fornecedor-lifnr = |{ wa_fornecedor-lifnr ALPHA = IN }|.
        vmatnr18  = |{ wa_makt-matnr ALPHA = IN }|.
        _id_produto   = vmatnr18.
        DATA(_tp_producao)  = COND string(  WHEN ( tvv3t-kvgr3 = 'R' ) THEN 'RR'
                                            WHEN ( tvv3t-kvgr3 = 'C' ) THEN 'CO'
                                            ELSE ' ' ).

        SELECT * FROM zsdt0187 INTO TABLE @DATA(tl_zsdt0187)
          WHERE safra           EQ @wa_zsdt0158_saida-safra
            AND werks           EQ @wa_t001w-werks
            AND id_produto      EQ @_id_produto
            AND tp_producao     EQ @_tp_producao
            AND tp_frete_compra EQ 'FOB'
*            AND ID_FORN         EQ @WA_FORNECEDOR-LIFNR
          ORDER BY nu_compra
          %_HINTS ORACLE 'INDEX(ZSDT0187 "Z1")'.

        " 03.05.2022 - RAMON LIMA - RECLIKE -- >
        "DELETE tl_zsdt0187[] WHERE ov_ped IS NOT INITIAL.
        " 03.05.2022 - RAMON LIMA - RECLIKE <--

        "IF ( tl_zsdt0187[] IS INITIAL ).
        "MESSAGE 'ID de Compra não encontrado para os dados informados!' TYPE 'E'.
        "vg_focus  = 'WA_ZSDT0158_SAIDA-SAFRA'.
        "EXIT.
        "ELSE.

        " 03.05.2022 - RAMON LIMA - RECLIKE -- >
*        LOOP AT tl_zsdt0187[] ASSIGNING FIELD-SYMBOL(<w_zsdt0187>).
*          IF ( <w_zsdt0187>-qtd_estorno IS NOT INITIAL ).
*            DATA(_quantidade) = ( <w_zsdt0187>-quantidade - <w_zsdt0187>-qtd_estorno ).
*            IF ( _quantidade IS NOT INITIAL ).
*              <w_zsdt0187>-quantidade = _quantidade.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
        " 03.05.2022 - RAMON LIMA - RECLIKE -- <

        DATA(tl_fieldcat) = VALUE slis_t_fieldcat_alv(
          ( fieldname = 'ID_COMPRA'       seltext_m = 'ID_COMPRA'   outputlen = '11' )
          ( fieldname = 'ID_FORN'         seltext_m = 'FORNECEDOR'  outputlen = '10' )
          ( fieldname = 'SAFRA'           seltext_m = 'SAFRA'       outputlen = '05' )
          ( fieldname = 'BUKRS'           seltext_m = 'EMPRESA'     outputlen = '04' )
          ( fieldname = 'WERKS'           seltext_m = 'FILIAL'      outputlen = '05' )
          ( fieldname = 'ID_PRODUTO'      seltext_m = 'MATERIAL'    outputlen = '08' )
          ( fieldname = 'QUANTIDADE'      seltext_m = 'QUANTIDADE'  outputlen = '18' )
          ( fieldname = 'UNIDADE'         seltext_m = 'UN'          outputlen = '03' )
          ( fieldname = 'TP_FRETE_COMPRA' seltext_m = 'FRETE'       outputlen = '05' )
          ( fieldname = 'ID_TERMINAL'     seltext_m = 'Terminal'    outputlen = '08' ) ).

        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            i_title     = 'Selecionar ID Compra'
            i_selection = 'X'
            i_tabname   = 'TL_ZSDT0187'
            i_zebra     = 'X'
            it_fieldcat = tl_fieldcat
          IMPORTING
            es_selfield = linha_selecionada
            e_exit      = _exit
          TABLES
            t_outtab    = tl_zsdt0187[].

        IF ( linha_selecionada IS NOT INITIAL ).

          CLEAR it_id_compra[].

          DATA(wl_0187) = tl_zsdt0187[ linha_selecionada-tabindex ].


          DATA(wl_id_compra) = VALUE zsdt0187(  nu_compra       = wl_0187-nu_compra
                                                data_atual      = wl_0187-data_atual
                                                quantidade      = wl_0187-quantidade
                                                unidade         = wl_0187-unidade
                                                tp_frete_compra = wl_0187-tp_frete_compra ).

          wa_lfa1_pc-lifnr  = COND string( WHEN wl_0187-id_ponto_coleta   IS NOT INITIAL THEN wl_0187-id_ponto_coleta ).
*            WA_KNA1-KUNNR     = COND STRING( WHEN WL_0187-ID_LOCAL_DESTINO  IS NOT INITIAL THEN WL_0187-ID_LOCAL_DESTINO ).
          wa_lfa1_z1-lifnr  = COND string( WHEN wl_0187-id_terminal       IS NOT INITIAL THEN wl_0187-id_terminal ).

          IF  ( wl_0187-id_ponto_coleta IS NOT INITIAL ).   PERFORM busca_pc USING wa_lfa1_pc-lifnr.       ENDIF.
*            IF  ( WL_0187-ID_LOCAL_DESTINO IS NOT INITIAL ).  PERFORM BUSCA_LR USING WA_KNA1-KUNNR.          ENDIF.
          IF  ( wl_0187-id_terminal IS NOT INITIAL ).       PERFORM busca_terminal USING wa_lfa1_z1-lifnr. ENDIF.

          APPEND wl_id_compra TO it_id_compra[].

          CLEAR: wl_id_compra, wl_0187, tl_zsdt0187[].
          CALL METHOD obj_alv->refresh_table_display( ).

        ENDIF.

        "ENDIF.

      WHEN OTHERS.

        MESSAGE 'Tipo de Frete não permite ID de Compra' TYPE 'I'. EXIT.

    ENDCASE.

  ELSE.
    MESSAGE msg_ero TYPE 'I'. EXIT.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  BUSCA_FORNECEDOR
*&---------------------------------------------------------------------*
FORM busca_fornecedor.

  CHECK ( wa_fornecedor-lifnr IS NOT INITIAL ).

  wa_fornecedor-lifnr = |{ wa_fornecedor-lifnr ALPHA = IN }|.

  SELECT SINGLE * FROM lfa1 INTO @DATA(w_lfa1) WHERE lifnr = @wa_fornecedor-lifnr.

  IF ( sy-subrc <> 0 ).
    MESSAGE 'Fornecedor não encontrado' TYPE 'I'.
    vg_focus = 'WA_FORNECEDOR-LIFNR'.
    EXIT.
  ELSE.
    wa_fornecedor = CORRESPONDING #( w_lfa1 ).
    wa_fornecedor-lifnr = |{ wa_fornecedor-lifnr ALPHA = OUT }|.
  ENDIF.

  CLEAR: w_lfa1.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  DELETA_SOLICITACAO
*&---------------------------------------------------------------------*
FORM deleta_solicitacao.

  DATA: answ_del TYPE c.

  IF ( wa_zsdt0158_saida-status EQ 'L' ).
    MESSAGE TEXT-035 TYPE 'I'.
    EXIT.
  ELSEIF ( wa_zsdt0158_saida-status EQ 'G' ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja eliminar a solicitação?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'Nao'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = 'X'
        popup_type            = 'ICON_MESSAGE_ERROR'
      IMPORTING
        answer                = answ_del.

    CHECK ( answ_del EQ 1 ).

    DELETE FROM zsdt0158 WHERE sequencial EQ wa_saida-sequencial
        AND tp_solicitacao = 'P'.

    MESSAGE TEXT-037 TYPE 'S'.

    PERFORM limpa_dados.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  ALTERA_QUANTIDADE_PED
*&---------------------------------------------------------------------*
FORM altera_quantidade_ped .

*---> 20/07/2023 - Migração S4 - DG
*  DATA lw_header TYPE bapiekkol.
*  DATA lt_item TYPE TABLE OF bapiekpo.
*  DATA lt_bapiret TYPE TABLE OF bapiret2.

  DATA: lw_header  TYPE bapimepoheader,
        lt_item    TYPE TABLE OF bapimepoitem,
        lt_bapiret TYPE TABLE OF bapiret2.
*---> 20/07/2023 - Migração S4 - DG



  DATA tl_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  IF wa_zsdt0158_saida-ebeln IS NOT INITIAL.




*---> 20/07/2023 - Migração S4 - DG
*    CALL FUNCTION 'BAPI_PO_GETDETAIL'
*      EXPORTING
*        purchaseorder = wa_zsdt0158_saida-ebeln
*        items         = 'X'
*      IMPORTING
*        po_header     = lw_header
*      TABLES
*        po_items      = lt_item
*        return        = lt_bapiret.








    CALL FUNCTION 'BAPI_PO_GETDETAIL1' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        purchaseorder = wa_zsdt0158_saida-ebeln
        services      = 'X'
      IMPORTING
        poheader      = lw_header
      TABLES
        return        = lt_bapiret
        poitem        = lt_item.
*---> 20/07/2023 - Migração S4 - DG



    IF ( wa_zsdt0158_qt-qtdade_nova < lt_item[ 1 ]-quantity ).
      DATA(msg) = |Quantidade Pedido: { lt_item[ 1 ]-quantity  } maior que quantidade informada!|.
      MESSAGE msg TYPE 'S'.

    ELSE.

      CLEAR lt_bapiret.

      DATA lw_po_header TYPE bapimepoheader.
      DATA lw_po_headerx TYPE bapimepoheaderx.

      DATA lt_poitem TYPE TABLE OF bapimepoitem.
      DATA lt_poitemx TYPE TABLE OF bapimepoitemx.

      "DATA lt_poschedule TYPE TABLE OF bapimeposchedule.
      "DATA lt_poschedulex TYPE TABLE OF bapimeposchedulx.

      PERFORM f_preenche_po_item
        USING lt_item
     CHANGING lt_poitem
              lt_poitemx.

*    PERFORM f_preenche_po_sched
*      USING lt_sche
*   CHANGING lt_poschedule
*            lt_poschedulex.

      MOVE-CORRESPONDING lw_header TO lw_po_header.

      "lw_po_header-po_number = wa_zsdt0158_saida-ebeln.
      lw_po_headerx-po_number = 'X'.

      "BREAK-POINT.

      CALL FUNCTION 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          purchaseorder = wa_zsdt0158_saida-ebeln
          poheader      = lw_po_header
          poheaderx     = lw_po_headerx
        TABLES
          return        = lt_bapiret
          poitem        = lt_poitem
          poitemx       = lt_poitemx.

      READ TABLE lt_bapiret INTO DATA(wl_return) WITH KEY type = 'E'.

      IF sy-subrc NE 0.

*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.

        DATA(_0158_qt) = VALUE zsdt0158_qt(
             nro_sol_ov   = wa_saida-nro_sol_ov
             ebeln = wa_zsdt0158_saida-ebeln
             data_atual   = sy-datum
             hora_atual   = sy-uzeit
             qtdade_atual = wa_zsdt0158_saida-quantidade
             qtdade_nova  = wa_zsdt0158_qt-qtdade_nova
             unidade      = wa_zsdt0158_qt-unidade
             usnam        = sy-uname
             vlr_pauta    = wa_zsdt0158_qt-vlr_pauta ).

        MODIFY zsdt0158_qt FROM _0158_qt.

        DATA(_vlrtot) = wa_zsdt0158_qt-vlr_pauta.

        _vlrtot = ( _vlrtot * wa_zsdt0158_qt-qtdade_nova ).

        " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 -->

        UPDATE zsdt0158
          SET quantidade = wa_zsdt0158_qt-qtdade_nova
        WHERE sequencial = wa_saida-sequencial.

*        UPDATE zsdt0187
*           SET quantidade  = wa_zsdt0158_qt-qtdade_nova
*         WHERE nro_sol_ov = wa_saida-nro_sol_ov.
*        " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 --<

        wa_zsdt0158_saida-quantidade = _0158_qt-qtdade_nova.
        wa_id_compra-quantidade = _0158_qt-qtdade_nova.

        CLEAR: wa_zsdt0158_qt.

        COMMIT WORK AND WAIT.

        MESSAGE 'Quantidade alterada!' TYPE 'S'.

      ELSE.

        LOOP AT lt_bapiret INTO wl_return WHERE type = 'E'.
          MOVE: tl_return-message TO wa_saida_exec-msg.
          APPEND wa_saida_exec TO it_saida_exec.
          CLEAR:  wl_return, wa_saida_exec.
        ENDLOOP.

        PERFORM montar_layout_err.

        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            it_fieldcat           = estrutura[]
            i_save                = 'A'
            i_screen_start_column = 3
            i_screen_start_line   = 3
            i_screen_end_column   = 100
            i_screen_end_line     = 13
          TABLES
            t_outtab              = it_saida_exec.
      ENDIF.
    ENDIF.

  ELSE.

    _vlrtot = ( _vlrtot * wa_zsdt0158_qt-qtdade_nova ).

    UPDATE zsdt0158
      SET quantidade = wa_zsdt0158_qt-qtdade_nova
      WHERE sequencial = wa_saida-sequencial.

*    " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 -->
*    UPDATE zsdt0187
*       SET quantidade  = wa_zsdt0158_qt-qtdade_nova
*     WHERE nro_sol_ov = wa_saida-nro_sol_ov.
*    " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 --<

    wa_id_compra-quantidade = _0158_qt-qtdade_nova.
    wa_zsdt0158_saida-quantidade = _0158_qt-qtdade_nova.

    MESSAGE 'Quantidade alterada!' TYPE 'S'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALTERA_QUANTIDADE_OV
*&---------------------------------------------------------------------*
FORM altera_quantidade_ov .
  DATA: tl_vbap            TYPE TABLE OF vbap WITH HEADER LINE,
        tl_bapisditm       TYPE TABLE OF bapisditm WITH HEADER LINE,
        tl_bapisditmx      TYPE TABLE OF bapisditmx WITH HEADER LINE,
        tl_return          TYPE TABLE OF bapiret2 WITH HEADER LINE,
        wl_return          TYPE bapiret2,
        wl_orderheaderin   TYPE bapisdh1,
        wl_orderheaderinx  TYPE bapisdh1x,
        wl_logic_switch    TYPE bapisdls,
        tl_conditions_in   TYPE TABLE OF bapicond WITH HEADER LINE,
        tl_conditions_inx  TYPE TABLE OF bapicondx WITH HEADER LINE,
        tl_schedule_lines  TYPE TABLE OF bapischdl WITH HEADER LINE,
        tl_schedule_linesx TYPE TABLE OF bapischdlx WITH HEADER LINE.




  SELECT SINGLE nro_sol_ov, zmeng, zieme, vbeln
       FROM zsdt0066
       INTO @DATA(w_0066)
       WHERE nro_sol_ov = @wa_saida-nro_sol_ov.

  CHECK ( sy-subrc = 0 ).

  IF ( w_0066-vbeln IS NOT INITIAL ).

    SELECT vbelv, SUM( rfmng ) AS rfmng
      FROM vbfa
      INTO TABLE @DATA(t_vbfa)
      WHERE vbelv    = @w_0066-vbeln
        AND posnv    = '000010'
        AND vbtyp_n  = 'J'
        AND vbtyp_v  = 'C'
      GROUP BY vbelv.

    TRY.
        DATA(w_vbfa) = t_vbfa[ 1 ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.


    IF (  wa_zsdt0158_qt-qtdade_nova  < w_vbfa-rfmng   ).
      DATA(msg) = |Quantidade Faturada: { w_vbfa-rfmng } maior que quantidade informada!|.
      MESSAGE msg TYPE 'S'.
    ELSE.

      MOVE:  'U'                              TO tl_bapisditmx-updateflag,
             '000010'                         TO tl_bapisditmx-itm_number,
             'X'                              TO tl_bapisditmx-target_qty,
             '000010'                         TO tl_bapisditm-itm_number,
             wa_zsdt0158_qt-qtdade_nova       TO tl_bapisditm-target_qty,
             w_0066-vbeln TO tl_vbap-vbeln.

      APPEND tl_bapisditmx.
      APPEND tl_bapisditm.

      MOVE '0001'                      TO tl_schedule_lines-sched_line.
      MOVE wa_zsdt0158_qt-qtdade_nova  TO tl_schedule_lines-req_qty.
      MOVE '000010'                    TO tl_schedule_lines-itm_number.
      APPEND tl_schedule_lines.

      MOVE 'U'                         TO tl_schedule_linesx-updateflag.
      MOVE '0001'                      TO tl_schedule_linesx-sched_line.
      MOVE abap_true                   TO tl_schedule_linesx-req_qty.
      MOVE '000010'                    TO tl_schedule_linesx-itm_number.
      APPEND tl_schedule_linesx.

      wl_orderheaderinx-updateflag = 'U'.
      wl_logic_switch-cond_handl   = 'X'.


      CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          salesdocument    = tl_vbap-vbeln
          order_header_in  = wl_orderheaderin
          order_header_inx = wl_orderheaderinx
          logic_switch     = wl_logic_switch
        TABLES
          order_item_in    = tl_bapisditm
          order_item_inx   = tl_bapisditmx
          conditions_in    = tl_conditions_in
          conditions_inx   = tl_conditions_inx
          schedule_lines   = tl_schedule_lines
          schedule_linesx  = tl_schedule_linesx
          return           = tl_return.

      CLEAR:wl_return..

      READ TABLE tl_return INTO wl_return WITH KEY type = 'E'.
      IF sy-subrc NE 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        DATA(_0158_qt) = VALUE zsdt0158_qt(
             nro_sol_ov   = wa_saida-nro_sol_ov
             data_atual   = sy-datum
             hora_atual   = sy-uzeit
             qtdade_atual = wa_zsdt0158_saida-quantidade
             qtdade_nova  = wa_zsdt0158_qt-qtdade_nova
             unidade      = wa_zsdt0158_qt-unidade
             usnam        = sy-uname
             vlr_pauta    = wa_zsdt0158_qt-vlr_pauta ).
        MODIFY zsdt0158_qt FROM _0158_qt.

        DATA(_vlrtot) = wa_zsdt0158_qt-vlr_pauta.

        _vlrtot = ( _vlrtot * wa_zsdt0158_qt-qtdade_nova ).

        UPDATE zsdt0066
          SET  zmeng  = wa_zsdt0158_qt-qtdade_nova
               zieme  = wa_zsdt0158_qt-unidade
               dmbtr  = wa_zsdt0158_qt-vlr_pauta
               vlrtot = _vlrtot
          WHERE nro_sol_ov = wa_saida-nro_sol_ov.

        " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 -->

        UPDATE zsdt0158
          SET quantidade = wa_zsdt0158_qt-qtdade_nova
        WHERE sequencial = wa_saida-sequencial.

        UPDATE zsdt0187
           SET quantidade  = wa_zsdt0158_qt-qtdade_nova
         WHERE nro_sol_ov = wa_saida-nro_sol_ov.
        " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 --<


        CLEAR: wa_zsdt0158_qt, w_0066, w_vbfa.
        MESSAGE 'Quantidade alterada!' TYPE 'S'.
      ELSE.

        LOOP AT tl_return INTO wl_return WHERE type = 'E'.
          MOVE: w_0066-vbeln      TO wa_saida_exec-vbeln.
          MOVE: tl_return-message TO wa_saida_exec-msg.
          APPEND wa_saida_exec TO it_saida_exec.
          CLEAR:  wl_return, wa_saida_exec.
        ENDLOOP.

        PERFORM montar_layout_err.

        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            it_fieldcat           = estrutura[]
            i_save                = 'A'
            i_screen_start_column = 3
            i_screen_start_line   = 3
            i_screen_end_column   = 100
            i_screen_end_line     = 13
          TABLES
            t_outtab              = it_saida_exec.
      ENDIF.
    ENDIF.

  ELSE.

    _vlrtot = ( _vlrtot * wa_zsdt0158_qt-qtdade_nova ).

    UPDATE zsdt0066
      SET  zmeng  = wa_zsdt0158_qt-qtdade_nova
           zieme  = wa_zsdt0158_qt-unidade
           dmbtr  = wa_zsdt0158_qt-vlr_pauta
           vlrtot = _vlrtot
      WHERE nro_sol_ov = wa_saida-nro_sol_ov.

    UPDATE zsdt0158
      SET quantidade = wa_zsdt0158_qt-qtdade_nova
      WHERE sequencial = wa_saida-sequencial.

    " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 -->
    UPDATE zsdt0187
       SET quantidade  = wa_zsdt0158_qt-qtdade_nova
     WHERE nro_sol_ov = wa_saida-nro_sol_ov.
    " 06.05.2022 - RAMON LIMA - RECKLIKE 76054 --<

    MESSAGE 'Quantidade alterada!' TYPE 'S'.

  ENDIF.
ENDFORM.



FORM montar_layout_err .

  REFRESH estrutura.
  PERFORM montar_estrutura2 USING:
   1  'ZSDT0066'   'VBELN'            'IT_SAIDA_EXEC' 'VBELN'     ' '  ' ',
   2  'ZSDT0066'   'MSG'              'IT_SAIDA_EXEC' 'MSG'       'Msg de bapi'   '80'.
ENDFORM.                    " MONTAR_LAYOUT


FORM montar_estrutura2 USING VALUE(p_col_pos)       TYPE i
                             VALUE(p_ref_tabname)   LIKE dd02d-tabname
                             VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                             VALUE(p_tabname)       LIKE dd02d-tabname
                             VALUE(p_field)         LIKE dd03d-fieldname
                             VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                             VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_CENTRO
*&---------------------------------------------------------------------*
FORM check_material_centro USING p_lgort TYPE mard-lgort.

  DATA _matnr TYPE char18.

  _matnr = |{ wa_makt-matnr ALPHA = IN }|.
  wa_makt-matnr = _matnr.



*  SELECT SINGLE t~vkorg
*    FROM t001w AS t
*    INTO @DATA(v_bukrs)
*    WHERE t~werks = @wa_t001w-werks.
*
*  DATA(v_dco) = COND #( WHEN check_dco IS INITIAL THEN 'N' ELSE 'S' ).
*
*  " 05.07.2022 - RAMON - 76636 ->
*  DATA(v_ind) = COND #( WHEN wa_saida-indust IS INITIAL THEN 'N' ELSE 'S' ).
*  " 05.07.2022 - RAMON - 76636 -<
*
*  SELECT SINGLE zp~bukrs,
*                zp~werks,
*                zp~tp_producao,
*                zp~dco,
*                zp~auart
*    FROM zparam_cont_fret AS zp
*    INTO @DATA(w_zparam)
*    WHERE zp~bukrs        = @v_bukrs
*      AND zp~werks        = @wa_t001w-werks
*      AND zp~tp_producao  = @tvv3t-kvgr3
*      AND zp~dco          = @v_dco
*     " 05.07.2022 - RAMON - 76636 ->
*     AND zp~industrializacao = @v_ind.
*  " 05.07.2022 - RAMON - 76636 -<
*
*  IF ( sy-subrc <> 0 ).
*
*    SELECT SINGLE zp~bukrs
*                  zp~werks
*                  zp~tp_producao
*                  zp~dco
*                  zp~auart
*      FROM zparam_cont_fret AS zp
*      INTO w_zparam
*      WHERE zp~bukrs        = v_bukrs
*        AND zp~tp_producao  = tvv3t-kvgr3
*        AND zp~dco          = check_dco
*           " 05.07.2022 - RAMON - 76636 ->
*     AND zp~industrializacao = v_ind.
*    " 05.07.2022 - RAMON - 76636 -<
*
*  ENDIF.

  SELECT SINGLE mc~matnr,
                mc~werks
    FROM marc AS mc
    INTO @DATA(w_marc)
    WHERE mc~matnr = @wa_makt-matnr
      AND mc~werks = @wa_t001w-werks.

  IF ( sy-subrc <> 0 ).

    wa_makt-matnr = |{ wa_makt-matnr ALPHA = OUT }|.

    DATA(v_msg_material) = |Material: { wa_makt-matnr } não expandido para o centro: { wa_t001w-werks }. Solicite cadastro. |.
    valida =  abap_true.

    MESSAGE v_msg_material TYPE 'I'.
    vg_focus = 'WA_MAKT-MATNR'.
    EXIT.

  ELSE.

    IF p_lgort IS NOT INITIAL.

      SELECT SINGLE md~matnr,
                    md~werks,
                    md~lgort
        FROM mard AS md
        INTO @DATA(w_mard)
        WHERE md~matnr = @wa_makt-matnr
          AND md~werks = @wa_t001w-werks
        "AND md~lgort = @wa_t0017-lgort.
          AND md~lgort = @p_lgort. "wbarbosa US 152850 11/10/2024


      IF ( sy-subrc <> 0 ).

        wa_makt-matnr = |{ wa_makt-matnr ALPHA = OUT }|.

        v_msg_material = |Material { wa_makt-matnr } não expandido para o centro: { wa_t001w-werks } e déposito: { p_lgort }. Solicite cadastro. |.
        valida =  abap_true.

        MESSAGE v_msg_material TYPE 'I'.
        vg_focus = 'WA_MAKT-MATNR'.
        EXIT.

      ELSE.

        SELECT SINGLE mh~matnr,
                      mh~werks,
                      mh~charg,
                      mh~lvorm
          FROM mcha AS mh
          INTO @DATA(w_mcha)
          WHERE mh~matnr = @wa_makt-matnr
            AND mh~werks = @wa_t001w-werks
            AND mh~charg = @wa_zsdt0158_saida-safra.


        IF ( sy-subrc <> 0 ).

          wa_makt-matnr = |{ wa_makt-matnr ALPHA = OUT }|.

          v_msg_material = |Material { wa_makt-matnr } não expandido para o centro: { wa_t001w-werks }, lote: { wa_zsdt0158_saida-safra }. Solicite cadastro. |.
          valida =  abap_true.

          MESSAGE v_msg_material TYPE 'I'.
          vg_focus = 'WA_MAKT-MATNR'.
          EXIT.


        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  CLEAR: v_msg_material, w_marc, w_mard, w_mcha.

  wa_makt-matnr = |{ wa_makt-matnr ALPHA = OUT }|.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ZM_CHECK_KVGR4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_check_kvgr4 INPUT.

  PERFORM f_check_kvgr4.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ZM_CHECK_KVGR5  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_check_kvgr5 INPUT.

  PERFORM f_check_kvgr5.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ZM_CHECK_KVGR3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_check_kvgr3 INPUT.

  DATA vmatnr18 TYPE matnr18.


  CHECK wa_makt-matnr IS NOT INITIAL.
  CHECK tvv3t-kvgr3 IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_makt-matnr
    IMPORTING
      output = vmatnr18.
  wa_makt-matnr = vmatnr18.

  SELECT SINGLE * INTO @DATA(wa_mara_rr)
    FROM mara
   WHERE matnr EQ @wa_makt-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_makt-matnr
    IMPORTING
      output = wa_makt-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_mara_rr-matkl
    IMPORTING
      output = wa_mara_rr-matkl.

  IF wa_mara_rr-matkl EQ '700170' AND tvv3t-kvgr3 NE 'R'. "Milho
    CLEAR: wa_mara_rr.
    MESSAGE 'Milho deve ser informado RR' TYPE 'E'.
  ENDIF.

  CLEAR: wa_mara_rr.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      FORM F_CHECK_KVGR4
*&---------------------------------------------------------------------*
FORM f_check_kvgr4.

  CHECK wa_zsdt0158_saida-kvgr4 IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_tvv4)
    FROM tvv4
   WHERE kvgr4 EQ @wa_zsdt0158_saida-kvgr4.

  IF sy-subrc IS NOT INITIAL.
    gv_msg = |Corredor { wa_zsdt0158_saida-kvgr4 } não encontrado!|.
    MESSAGE gv_msg TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM F_CHECK_KVGR5  INPUT
*&---------------------------------------------------------------------*
FORM f_check_kvgr5.

  CHECK wa_zsdt0158_saida-kvgr5 IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_tvv5)
    FROM tvv5
   WHERE kvgr5 EQ @wa_zsdt0158_saida-kvgr5.

  IF sy-subrc IS NOT INITIAL.
    gv_msg = |Emite frete de entrada com valor { wa_zsdt0158_saida-kvgr5 } não encontrado!|.
    MESSAGE gv_msg TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM F_CHECK_KVGR3
*&---------------------------------------------------------------------*
FORM f_check_kvgr3.

  CHECK wa_makt-matnr IS NOT INITIAL.
  CHECK tvv3t-kvgr3 IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_makt-matnr
    IMPORTING
      output = wa_makt-matnr.

  SELECT SINGLE * INTO @DATA(wa_mara_rr)
    FROM mara
   WHERE matnr EQ @wa_makt-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_makt-matnr
    IMPORTING
      output = wa_makt-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_mara_rr-matkl
    IMPORTING
      output = wa_mara_rr-matkl.

  IF wa_mara_rr-matkl EQ '700170' AND tvv3t-kvgr3 NE 'R'. "Milho
    CLEAR: wa_mara_rr.
    MESSAGE 'Milho deve ser informado RR' TYPE 'E'.
  ENDIF.

  CLEAR: wa_mara_rr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_CHECK_FILIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_T001W_WERKS  text
*----------------------------------------------------------------------*
FORM pf_check_filial USING p_werks p_subrc.

  CLEAR: vl_block, p_subrc.

  SELECT SINGLE *
    FROM zhcmt0007
    INTO @DATA(wa_zhcmt0007)
    WHERE bname = @sy-uname.

  IF sy-subrc IS NOT INITIAL.

    "// WBARBOSA US152850 10/10/2024
    CASE sy-uname.
      WHEN 'RBLIMA' OR 'WBARBOSA'.
      WHEN OTHERS.

        MESSAGE 'Usuario não não encontrado na Tabela "ZHCMT0007"' TYPE 'I'.
        p_subrc = 4.
        EXIT.

    ENDCASE."#DEBUG - RETIRAR PÓS TESTE UNITARIO - RAMON LIMA ABAP RECLIKE "// WBARBOSA 10/10/2024
    "// WBARBOSA US152850 10/10/2024

  ENDIF.

  SELECT SINGLE valfrom
    FROM setleaf
    INTO @DATA(vl_empresa)
    WHERE setname = 'ZSDT0132_BLOCK_EMPRESA'
    AND valfrom = @wa_zhcmt0007-bukrs.

  CHECK sy-subrc IS INITIAL.

  vl_block  = vl_empresa.

  SELECT SINGLE *
    FROM t001w
    INTO @DATA(vl_t001w)
    WHERE werks EQ @p_werks.

  IF vl_block NE vl_t001w-vkorg.
    MESSAGE TEXT-104 TYPE 'I'.
    p_subrc = 4.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_INICIAL
*&---------------------------------------------------------------------*
FORM f_popup_inicial USING p_renew TYPE c
                  CHANGING p_erro TYPE c.

  DATA lv_ret TYPE c.

  CALL FUNCTION 'ZSD_TELA_INICIAL_ZSDT0132'
    EXPORTING
      i_renew           = p_renew
    IMPORTING
      e_bukrs           = gv_bukrs
      e_matnr           = gv_matnr
    TABLES
      et_dados_lote     = gt_lotes
      et_dados_ordens   = gt_ordens
    EXCEPTIONS
      erro_encontrado   = 1
      erro_autorizacao  = 2
      tela_cancelada    = 3
      sem_novo_processo = 4
      OTHERS            = 5.

*  CALL FUNCTION 'ZSD_BEFORE_ZSDT0132'
*    EXPORTING
*      i_reiniciar = p_renew
*    IMPORTING
*      e_bukrs     = gv_bukrs
*      e_matnr     = gv_matnr
*      e_ret       = lv_ret.
*
*  CHECK lv_ret NE 'A'.
*
*  CALL FUNCTION 'ZSD_PROCESSAR_GO_FLUX'
*    EXPORTING
*      i_bukrs           = gv_bukrs
*      i_matnr           = gv_matnr
*    TABLES
*      et_dados_lote     = gt_lotes
*      et_dados_ordens   = gt_ordens
*    EXCEPTIONS
*      erro_encontrado   = 1
*      erro_autorizacao  = 2
*      tela_cancelada    = 3
*      sem_novo_processo = 4
*      OTHERS            = 5.

  CASE sy-subrc.
    WHEN 1.
      p_erro = 'X'.
    WHEN 2.
      p_erro = 'X'.
    WHEN 3.
      p_erro = 'X'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_DADOS_POPUP - #RAMON
*&---------------------------------------------------------------------*
FORM f_carrega_dados_popup .

  PERFORM limpa_dados.

  LOOP AT gt_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>) WHERE selec = 'X'.

    CLEAR: wa_saida-indust, wa_saida-form_lote.

*    IF ( <fs_lote>-lcl_entr_bukrs <> <fs_lote>-bukrs ) AND <fs_lote>-produtoderivado = 'S'.
*      wa_saida-indust = 'X'.
*    ELSE.
*      wa_saida-form_lote = 'X'.
*    ENDIF.

    " 23.09.2022 - RAMON ->
    IF gv_bukrs IS INITIAL AND gv_matnr IS INITIAL.

      gv_bukrs = <fs_lote>-bukrs.
      gv_matnr = <fs_lote>-matnr.

    ENDIF.
    " 23.09.2022 - RAMON -<

    wa_saida-indefinido = 'X'.

    wa_makt-matnr	= <fs_lote>-matnr. "gv_matnr.
    wa_saida-sequencial = '$00000001'.
    wa_zsdt0158-tipo = 'I'.

    wa_t001w-werks = <fs_lote>-filial_ped.

    wa_zsdt0158_saida-safra = <fs_lote>-nr_safra.

    IF <fs_lote>-transgenia IS NOT INITIAL.

      SELECT SINGLE kvgr3 FROM tvv3t
          INTO tvv3t-kvgr3
        WHERE spras = sy-langu
          AND bezei = <fs_lote>-transgenia.

    ENDIF.

    PERFORM f_dados_lote_tela
      USING <fs_lote>-lote
            <fs_lote>-produtor
            <fs_lote>-desc_produtor
            <fs_lote>-qtde.

    wa_lfa1_pc-lifnr = <fs_lote>-ponto_coleta.
    wa_lfa1_pc-name1 = <fs_lote>-desc_coleta.

    wa_kna1-kunnr  = <fs_lote>-local_entrega.

**    " 01.06.2022 - correção porto -->
**    IF <fs_lote>-porto CA ','.
**      PERFORM altera_zona_z1 USING <fs_lote>-porto CHANGING wa_lfa1_z1-lifnr.
**    ELSE.
**      UNPACK <fs_lote>-porto TO wa_lfa1_z1-lifnr.
**    ENDIF.
**    " 01.06.2022 - correção porto --<

    IF wa_lfa1_z1-lifnr IS NOT INITIAL.
      "check_z1 = 'X'.
    ENDIF.

    wa_zsdt0158_saida-unidade = <fs_lote>-un_lote.
    wa_zsdt0158_saida-quantidade = <fs_lote>-saldo.
    wa_id_compra-quantidade = <fs_lote>-saldo.

    wa_id_compra-unidade = wa_zsdt0158_saida-unidade.
    "wa_id_compra-tp_frete_compra = tinct-inco1.

    "wa_lfa1_z1-lifnr = <fs_lote>-local_entrega.

    PERFORM f_check_kvgr4.
    PERFORM f_check_kvgr5.
    PERFORM f_check_kvgr3.
    PERFORM busca_filial USING wa_t001w-werks.

    PERFORM busca_material.

    PERFORM busca_fornecedor.

    PERFORM busca_pc USING wa_lfa1_pc-lifnr.

    PERFORM busca_lr USING wa_kna1-kunnr.

    PERFORM busca_itinerario.

    PERFORM busca_terminal USING wa_lfa1_z1-lifnr.

    PERFORM preenche_z1.

    PERFORM valida_ag_frete.

    PERFORM busca_preco_pauta.

    " PERFORM altera_zona_pc USING wa_lfa1_pc-lifnr.
    " PERFORM altera_zona_lr.
    PERFORM preenche_doc.
    "PERFORM valida_tp_frete.

  ENDLOOP.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TELA_GO_FLUX
*&---------------------------------------------------------------------*
FORM f_tela_go_flux USING p_renew TYPE c
                 CHANGING p_erro.

  CLEAR p_erro.

  "CHECK sy-uname = 'RBLIMA'.

  PERFORM f_popup_inicial USING p_renew CHANGING p_erro.

  CHECK p_erro IS INITIAL.

  PERFORM f_carrega_dados_popup.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_ZSDT0187
*&---------------------------------------------------------------------*
FORM f_insert_zsdt0187 USING p_zsdt0158 TYPE zsdt0158.

  DATA lw_zsdt0187 TYPE zsdt0187.

  "CHECK SY-UCOMM = 'SAVE'.

  " SÓ GRAVA SE TIVER DADOS NESSA TABELA,
  " PQ PARTE DOS DADOS DA 187 VEM DO JSON DO SIGAM
  READ TABLE gt_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>)
    WITH KEY selec = 'X'.

  CHECK sy-subrc EQ 0.

  IF lw_zsdt0187-id_vinculo IS INITIAL.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'Z_ID_VINC'
      IMPORTING
        number                  = lw_zsdt0187-id_vinculo
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      PERFORM f_mensagem_sistema.
      EXIT.
    ENDIF.

  ENDIF.

  " parei aqui em criar o snro

  lw_zsdt0187-sequencial = p_zsdt0158-sequencial.
  lw_zsdt0187-nu_compra = <fs_lote>-nu_compra.
  lw_zsdt0187-nu_lote = <fs_lote>-lote.
  "lw_zsdt0187-nro_sol_ov = wa_zsdt0158_saida-nro_sol_ov.
  lw_zsdt0187-id_forn = <fs_lote>-produtor.
  lw_zsdt0187-safra = <fs_lote>-nr_safra.
  lw_zsdt0187-bukrs = <fs_lote>-bukrs.
  lw_zsdt0187-werks = <fs_lote>-filial.
  lw_zsdt0187-id_ponto_coleta = <fs_lote>-ponto_coleta.
  lw_zsdt0187-id_local_destino = <fs_lote>-local_entrega. "<fs_lote>-porto.
  "lw_zsdt0187-id_terminal = <fs_lote>-local_entrega.
  lw_zsdt0187-id_terminal = p_zsdt0158-id_terminal.
  lw_zsdt0187-compra = <fs_lote>-id_compra.
  "lw_zsdt0187-id_compra = p_zsdt0158-id_compra.
  lw_zsdt0187-id_produto = <fs_lote>-matnr.
  lw_zsdt0187-quantidade = <fs_lote>-qtde.
  lw_zsdt0187-saldo_entregar = <fs_lote>-saldo.
  lw_zsdt0187-unidade = <fs_lote>-un_lote.
  lw_zsdt0187-tp_operacao = <fs_lote>-tipooperacao.
  lw_zsdt0187-tp_producao = <fs_lote>-transgenia.
  lw_zsdt0187-tp_frete_compra = <fs_lote>-tp_frete.
  lw_zsdt0187-ano_liquidez = <fs_lote>-ano.
  lw_zsdt0187-mes_liquidez = <fs_lote>-mes.
  lw_zsdt0187-periodo_liquidez = <fs_lote>-periodo.
  lw_zsdt0187-produto_derivado = <fs_lote>-produtoderivado.
  lw_zsdt0187-data_atual = sy-datum.
  lw_zsdt0187-hora_atual = sy-uzeit.

  MODIFY zsdt0187 FROM lw_zsdt0187.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_ZSDT0187
*&---------------------------------------------------------------------*
FORM f_modify_zsdt0187 USING p_zsdt0158 TYPE zsdt0158.

  DATA lw_zsdt0187_new TYPE zsdt0187.

  CHECK gw_zsdt0187_edit IS NOT INITIAL.

  MOVE-CORRESPONDING gw_zsdt0187_edit TO lw_zsdt0187_new.
  MOVE-CORRESPONDING wa_zsdt0158_saida TO lw_zsdt0187_new.

  lw_zsdt0187_new-werks = wa_zsdt0158_saida-filial.


*  lw_zsdt0187_new-id_terminal = wa_zsdt0158_saida-id_local_destino.
*  lw_zsdt0187_new-id_local_destino = wa_zsdt0158_saida-id_terminal.


*lw_zsdt0187_NEW-ID_PONTO_COLETA = WA_ZSDT0158_SAIDA-ID_PONTO_COLETA.
*lw_zsdt0187_NEW-ID_LOCAL_DESTINO = WA_ZSDT0158_SAIDA-ID_LOCAL_DESTINO."                                      0000001003
*lw_zsdt0187_NEW-ID_TERMINAL   = WA_ZSDT0158_SAIDA-ID_TERMINAL."                                0000001002
*
*lw_zsdt0187_NEW-ID_PRODUTO = WA_ZSDT0158_SAIDA-ID_PRODUTO."                                      000000000000119892
*
*lw_zsdt0187_NEW-QUANTIDADE = WA_ZSDT0158_SAIDA-QUANTIDADE."                                    770750.000
*lw_zsdt0187_NEW-UNIDADE                                      KG
*lw_zsdt0187_NEW-TP_PRODUCAO                                      R
*lw_zsdt0187_NEW-SAFRA                                      2021
*lw_zsdt0187_NEW-TP_FRETE	                                   	FOB
*lw_zsdt0187_NEW-AG_FRETE	                                   	N
*lw_zsdt0187_NEW-ID_AG_FRETE
*lw_zsdt0187_NEW-VLR_PAUTA                                      2.99
*lw_zsdt0187_NEW-DCO                                      N
*lw_zsdt0187_NEW-NR_DCO
*lw_zsdt0187_NEW-WAERS                                      BRL
*lw_zsdt0187_NEW-ZONA_PC                                      PPJACYRFMA
*lw_zsdt0187_NEW-ZONA_LR                                      HPVH
*lw_zsdt0187_NEW-RG_ATUALIZADO                                      0
*lw_zsdt0187_NEW-USNAM                                      RBLIMA
*lw_zsdt0187_NEW-DATA_ATUAL	                                   	20220513
*lw_zsdt0187_NEW-HORA_ATUAL	                                   	123216
*lw_zsdt0187_NEW-ID_FORNECEDOR
*lw_zsdt0187_NEW-OBS_PR_PRODUTO
*lw_zsdt0187_NEW-KVGR4
*lw_zsdt0187_NEW-KVGR5                                      001
*lw_zsdt0187_NEW-CK_TROCA_NOTA
*lw_zsdt0187_NEW-MENSAGEM

  PERFORM f_check_change_email
    USING gw_zsdt0187_edit
          lw_zsdt0187_new.

  MODIFY zsdt0187 FROM lw_zsdt0187_new.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema.

  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_RECUPERA_ZSDT0187
*&---------------------------------------------------------------------*
FORM f_recupera_zsdt0187 USING p_line TYPE zsdt0158.

  SELECT SINGLE * FROM zsdt0187
    INTO gw_zsdt0187_edit
      WHERE sequencial = p_line-sequencial.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_check_change_email
*&---------------------------------------------------------------------*
FORM f_check_change_email USING p_line_old TYPE zsdt0187
                               p_line_new TYPE zsdt0187.

  DATA lv_diferente TYPE c.
  DATA lt_html TYPE html_table.

  PERFORM f_add_html_line
    USING 'Filial'
          p_line_old-nu_compra
          p_line_old-werks
          p_line_new-werks
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Ponto de coleta'
          p_line_old-nu_compra
          p_line_old-id_ponto_coleta
          p_line_new-id_ponto_coleta
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Local destino'
          p_line_old-nu_compra
          p_line_old-id_local_destino
          p_line_new-id_local_destino
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Porto'
          p_line_old-nu_compra
          p_line_old-id_terminal
          p_line_new-id_terminal
 CHANGING lt_html.

  PERFORM f_add_html_line
    USING 'Quantidade'
          p_line_old-nu_compra
          p_line_old-saldo_entregar
          p_line_new-saldo_entregar
 CHANGING lt_html.

  PERFORM f_add_html_close CHANGING lt_html.

  CHECK lt_html IS NOT INITIAL.

  CALL METHOD zcl_send_email=>send_static
    EXPORTING
      i_receivers = 'EMPRESA_P&L_EMAIL'
      i_subject   = 'Lotes de compra com  alteração de dados'
      i_body      = lt_html.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_check_change_email
*&---------------------------------------------------------------------*
FORM f_add_html_line USING p_field TYPE c
                           p_nu_compra TYPE num8
                           p_line_old TYPE any
                           p_line_new TYPE any
                  CHANGING p_html TYPE html_table.

  DATA lw_html TYPE w3html.

  DATA lv_old TYPE c LENGTH 100.
  DATA lv_new TYPE c LENGTH 100.

  CHECK p_line_new IS NOT INITIAL.

  CHECK p_line_old NE p_line_new.

  WRITE p_line_old TO lv_old LEFT-JUSTIFIED.
  WRITE p_line_new TO lv_new LEFT-JUSTIFIED.

  IF p_html IS INITIAL.
    PERFORM f_add_html_ini CHANGING p_html.
  ENDIF.

  lw_html-line = `<tr style="height: 36px;">`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 135.516px; height: 36px;" height="20">` && p_nu_compra && `</td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 115.297px; height: 36px;">` && p_field && `</td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 210.094px; height: 36px;">` && lv_old && `</td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 209.094px; height: 36px;">` && lv_new && `</td></tr>`.
  APPEND lw_html TO p_html.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_check_change_email
*&---------------------------------------------------------------------*
FORM f_add_html_ini CHANGING p_html TYPE html_table.

  DATA lw_html TYPE w3html.

  lw_html-line = `<table style="height: 201px;background-color:#FCFCFC;" width="100%"><tbody>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<tr style="height: 21px;background-color:#D9D9D9" >`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 135.516px; height: 21px;" height="21"><strong>Lote</strong></td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 115.297px; height: 21px;"><strong>Dado</strong></td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 210.094px; height: 21px;"><strong>Antes</strong></td>`.
  APPEND lw_html TO p_html.

  lw_html-line = `<td style="width: 209.094px; height: 21px;"><strong>Atual</strong></td></tr>`.
  APPEND lw_html TO p_html.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_check_change_email
*&---------------------------------------------------------------------*
FORM f_add_html_close CHANGING p_html TYPE html_table.

  CHECK p_html IS NOT INITIAL.

  APPEND INITIAL LINE TO p_html ASSIGNING FIELD-SYMBOL(<fs_html>).

  <fs_html>-line = `</tr></tbody></table>`.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_SIGAM
*&---------------------------------------------------------------------*
FORM f_dados_sigam CHANGING p_erro TYPE c.

  CLEAR gt_lotes.

  gt_lotes = p_tab[].

  PERFORM f_carrega_dados_popup.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_LOTE_TELA
*&---------------------------------------------------------------------*
FORM f_dados_lote_tela USING p_nu_lote TYPE any
                             p_produtor TYPE lifnr
                             p_prod_desc TYPE name1_gp
                             p_qtde TYPE any.

  CLEAR wa_lote.

  wa_lote-lote = p_nu_lote.
  wa_lote-produtor = p_produtor.
  wa_lote-desc_produtor = p_prod_desc.
  wa_lote-qtde = p_qtde.

  IF wa_lote-desc_produtor IS INITIAL.

    SELECT SINGLE name1 FROM lfa1
      INTO wa_lote-desc_produtor
        WHERE lifnr = wa_lote-produtor.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_CHECK_BOX
*&---------------------------------------------------------------------*
FORM f_fill_check_box .

  CASE 'X'.
    WHEN wa_saida-indefinido.
      wa_zsdt0158_saida-tp_solicitacao = ''.
    WHEN wa_saida-form_lote.
      wa_zsdt0158_saida-tp_solicitacao = 'O'.
    WHEN wa_saida-indust.
      wa_zsdt0158_saida-tp_solicitacao = 'O'.
    WHEN wa_saida-ped_trans.
      wa_zsdt0158_saida-tp_solicitacao = 'P'.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILL_CHECK_BOX
*&---------------------------------------------------------------------*
FORM f_after_check_box.

  IF wa_zsdt0158_saida-tp_solicitacao = 'O'.

    IF wa_lfa1_z1-lifnr IS INITIAL.

      READ TABLE gt_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>) WITH KEY selec = 'X'.

      IF sy-subrc EQ 0.

        IF <fs_lote>-porto CA ','.
          PERFORM altera_zona_z1 USING <fs_lote>-porto CHANGING wa_lfa1_z1-lifnr.
        ELSE.
          UNPACK <fs_lote>-porto TO wa_lfa1_z1-lifnr.
        ENDIF.

      ENDIF.

    ENDIF.

  ELSE.

    CLEAR wa_lfa1_z1-lifnr.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_TRATA_CAMPOS_TERMINAL
*&---------------------------------------------------------------------*
FORM f_trata_campos_terminal USING p_input TYPE c.

  DATA lv_invi TYPE i.
  DATA lv_input TYPE i.
  DATA: lva_lifnr TYPE lfa1-lifnr.

  lv_input = COND int4( WHEN ( p_input IS INITIAL ) THEN 0
                       WHEN ( p_input IS NOT INITIAL ) THEN 1 ).

  LOOP AT SCREEN.

    IF wa_saida-form_lote = 'X' OR wa_saida-indust = 'X'.

      lv_invi = 0.

      IF screen-name = 'WA_LFA1_Z1-LIFNR' AND p_input = 'X'.
        lv_input = 1.
      ELSE.
        lv_input = 0.
      ENDIF.

    ELSE.

      lv_invi = 1.
      lv_input = 0.

    ENDIF.

    "// WBARBOSA US 152850 10/10/2024
    CASE screen-name.
      WHEN 'TEXT014' OR 'WA_SAIDA-EUDR'.

        CASE wa_zsdt0158-tipo.
          WHEN 'I' OR 'E'. "Inclusão/Edição

            IF is_processo_eudr EQ abap_true AND
               is_filial_classificacao_eudr EQ 'H' AND
               ( wa_saida-form_lote EQ abap_true OR
                 wa_saida-ped_trans EQ abap_true ).

              "Determina se é Processo CIF ou FOB pelo Ponto de Coleta da Solicitação
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input      = wa_lfa1_pc-lifnr
                IMPORTING
                 OUTPUT      = lva_lifnr.

              TRY.
                  zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = lva_lifnr
                  )->ck_parceiro_local_negocio(  IMPORTING e_j_1bbranch = DATA(is_branch) ).

                  DATA(lc_tipo_processo) = 'CIF'.
                CATCH zcx_parceiros.
                  lc_tipo_processo = 'FOB'.
              ENDTRY.

              IF lc_tipo_processo = 'CIF'.
                lv_invi  = 0.  "0-Visivel 1-Invisivel
                lv_input = 1. "0-Fechado 1-Aberto
              ELSE.
                lv_invi = 1.  "0-Visivel 1-Invisivel
                lv_input = 0. "0-Fechado 1-Aberto
              ENDIF.

            ELSE.
              lv_invi = 1.  "0-Visivel 1-Invisivel
              lv_input = 0. "0-Fechado 1-Aberto
            ENDIF.

          WHEN OTHERS. "Consulta

            IF wa_zsdt0158_saida-eudr IS NOT INITIAL.
              lv_invi = 0.  "0-Visivel 1-Invisivel
              lv_input = 0. "0-Fechado 1-Aberto
            ELSE.
              lv_invi = 1.  "0-Visivel 1-Invisivel
              lv_input = 0. "0-Fechado 1-Aberto
            ENDIF.
        ENDCASE.

    ENDCASE.
    "// WBARBOSA US 152850 10/10/2024

    CHECK screen-name = 'CHECK_Z1' OR
          screen-name = 'T20' OR
          screen-name = 'WA_LFA1_Z1-LIFNR' OR
          screen-name = 'WA_LFA1_Z1-NAME1' OR
          screen-name = 'T21' OR
          screen-name = 'WA_LFA1_Z1-STRAS' OR
          screen-name = 'T22' OR
          screen-name = 'WA_LFA1_Z1-ORT01' OR
          screen-name = 'T23' OR
          screen-name = 'WA_LFA1_Z1-REGIO' OR
          screen-name = 'T24' OR
          screen-name = 'WA_LFA1_Z1-STCD1' OR
          screen-name = 'T25' OR
          screen-name = 'WA_LFA1_Z1-STCD3' OR
          screen-name = '%#AUTOTEXT005' OR
          screen-name = 'WA_SAIDA-EUDR' OR "// WBARBOSA US 152850 10/10/2024
          screen-name = 'TEXT014'. "// WBARBOSA US 152850 10/10/2024

    screen-invisible  = lv_invi.
    screen-input = lv_input.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONTROLE_TELA_0102
*&---------------------------------------------------------------------*
FORM f_controle_tela_0102 .

  DATA lv_visible TYPE i.

  IF ( wa_zsdt0158-tipo IS INITIAL ) AND ( wa_zsdt0158-include-status NE 'L' ).

    LOOP AT SCREEN.

      CASE screen-name.
        WHEN 'T1' OR 'WA_SAIDA-NRO_SOL_OV'.

          " 22.09.2022 - RAMON 19450 -->
          IF wa_zsdt0158_saida-tp_solicitacao = 'O'.

            IF wa_saida-nro_sol_ov IS INITIAL.
              screen-invisible  = 1.
              MODIFY SCREEN.
            ELSE.
              screen-invisible = 0.
              MODIFY SCREEN.
            ENDIF.

          ELSEIF wa_zsdt0158_saida-tp_solicitacao = 'P'.

            " se for pedido então deixa esses campos invisiveis
            screen-invisible  = 1.
            MODIFY SCREEN.

          ELSE.

            screen-invisible  = 1.
            MODIFY SCREEN.

          ENDIF.
          " 22.09.2022 - RAMON 19450 --<

          " 22.09.2022 - RAMON 19450 -->
        WHEN 'T933' OR 'WA_SAIDA-EBELN'.

          IF wa_zsdt0158_saida-tp_solicitacao = 'P'.
            IF wa_saida-ebeln IS INITIAL.
              screen-invisible  = 1.
              MODIFY SCREEN.
            ELSE.
              screen-invisible = 0.
              MODIFY SCREEN.
            ENDIF.

          ELSE.

            " se for OV então deixa esses campos invisiveis
            screen-invisible  = 1.
            MODIFY SCREEN.
          ENDIF.

          " 22.09.2022 - RAMON 19450 --<

        WHEN 'WA_T001W-WERKS'    OR 'WA_LFA1_PC-LIFNR' OR 'WA_LFA1_PC-LZONE'
          OR 'WA_KNA1-KUNNR'     OR 'WA_KNA1-LZONE'    OR 'WA_LFA1_Z1-LIFNR'
          OR 'WA_FRETE-TP_FRETE' OR 'WA_MAKT-MATNR'    OR 'WA_ZSDT0158_SAIDA-QUANTIDADE'
          OR 'WA_MAKT-TIPO_PROD' OR 'WA_LFA1_Z1-LZONE' OR 'WA_ZSDT0158_SAIDA-SAFRA'
          OR 'CHECK_Z1'          " OR 'CHECK_DCO'        "OR 'WA_FRETE-AG_FRETE'
          OR 'TVV3T-KVGR3'       OR 'TINCT-INCO1'      OR 'WA_ZSDT0158_SAIDA-KVGR4' OR 'WA_ZSDT0158_SAIDA-CK_TROCA_NOTA'
          OR 'PR_PRODUTO_F'      OR 'PR_PRODUTO_A'     OR 'WA_ID_COMPRA-QUANTIDADE'
          OR 'WA_ID_COMPRA-UNIDADE' OR 'WA_ZSDT0158_SAIDA-KVGR5'

          " 20.07.2022 - RAMON - AJUSTE EXIBIÇÃO CAMPOS NOVOS -->
          OR 'WA_SAIDA-FORM_LOTE' OR 'WA_SAIDA-INDUST' OR 'WA_SAIDA-INDEFINIDO' OR 'WA_SAIDA-PED_TRANS'
          " 20.07.2022 - RAMON - AJUSTE EXIBIÇÃO CAMPOS NOVOS --<
          .

          screen-input  = 0.
          MODIFY SCREEN.

        WHEN 'WA_SAIDA-SEQUENCIAL'.

          screen-input  = 1.
          MODIFY SCREEN.

        WHEN 'ALTERA_ZONA'  OR 'ALTERA_ZONA_PC' "OR 'WA_ID_COMPRA-QUANTIDADE' OR 'WA_ID_COMPRA-UNIDADE'
          OR 'TXT33'        OR 'TXT34'          OR 'BTN_BUSCA_ID'            OR 'BTN_LIMPA_ID'.

          screen-input      = 0.
          screen-invisible  = 1.
          MODIFY SCREEN.

          " 26.09.2022 - RAMON - 19450 -->
        WHEN 'WA_KONP-KBETR' OR 'T28'.

          IF wa_zsdt0158_saida-tp_solicitacao = 'O'.

            screen-invisible  = 0.
            MODIFY SCREEN.

          ELSEIF wa_zsdt0158_saida-tp_solicitacao = 'P'.

            screen-invisible  = 1.
            MODIFY SCREEN.

          ENDIF.

        WHEN 'CHECK_DCO' OR 'WA_ZDCO_PROD-NR_DCO' OR 'T29'.

          IF wa_zsdt0158_saida-tp_solicitacao = 'P'.

            IF screen-name = 'CHECK_DCO'.

              screen-input  = 1.
              MODIFY SCREEN.

            ENDIF.

            IF wa_saida-form_lote = 'X' OR wa_saida-indefinido = 'X'.

              screen-invisible  = 0.
              MODIFY SCREEN.

            ELSE.

              screen-invisible  = 1.
              MODIFY SCREEN.

            ENDIF.

          ENDIF.
          " 26.09.2022 - RAMON - 19450 --<

      ENDCASE.

*      CASE SCREEN-GROUP1.
*        WHEN 'COM'.
*          IF ( TINCT-INCO1 EQ 'FOB' ) OR ( PR_PRODUTO_A EQ ABAP_TRUE ).
*            SCREEN-ACTIVE     = 1.
*            SCREEN-INVISIBLE  = 0.
*            SCREEN-INPUT      = 0.
*            MODIFY SCREEN.
*          ENDIF.
*      ENDCASE.


    ENDLOOP.

    PERFORM f_trata_campos_terminal USING space.

  ELSEIF wa_zsdt0158-tipo EQ 'I' OR wa_zsdt0158-tipo EQ 'E'.

    LOOP AT SCREEN.

      IF ( ( screen-name CS 'TXT33' ) OR ( screen-name CS 'TXT34' ) ) AND ( tinct-inco1 EQ 'FOB' ).
        screen-input      = 1.
        screen-active     = 1.
        screen-invisible  = 0.
        MODIFY SCREEN.
      ENDIF.

      CASE screen-name.
        WHEN 'WA_SAIDA-SEQUENCIAL'.

          " 26.09.2022 - RAMON - 19450 -->
          IF wa_saida-sequencial IS NOT INITIAL.
            screen-input  = 0.
            MODIFY SCREEN.
          ENDIF.
          " 26.09.2022 - RAMON - 19450 --<


        WHEN 'T1' OR 'WA_SAIDA-NRO_SOL_OV'.

          " 22.09.2022 - RAMON 19450 -->
          IF wa_zsdt0158_saida-tp_solicitacao = 'O'.
            IF wa_saida-nro_sol_ov IS INITIAL.
              screen-invisible  = 1.
              MODIFY SCREEN.
            ELSE.
              screen-invisible = 0.
              MODIFY SCREEN.
            ENDIF.

          ELSE.

            " se for pedido então deixa esses campos invisiveis
            screen-invisible  = 1.
            MODIFY SCREEN.
          ENDIF.

          " 22.09.2022 - RAMON 19450 -->
        WHEN 'T933' OR 'WA_SAIDA-EBELN'.

          IF wa_zsdt0158_saida-tp_solicitacao = 'P'.
            IF wa_saida-ebeln IS INITIAL.
              screen-invisible  = 1.
              MODIFY SCREEN.
            ELSE.
              screen-invisible = 0.
              MODIFY SCREEN.
            ENDIF.

          ELSE.

            " se for OV então deixa esses campos invisiveis
            screen-invisible  = 1.
            MODIFY SCREEN.
          ENDIF.

          " 22.09.2022 - RAMON 19450 --<


        WHEN 'WA_T001W-WERKS'    OR 'WA_LFA1_PC-LIFNR' OR 'WA_KNA1-KUNNR'
          OR 'WA_FRETE-TP_FRETE' OR 'WA_MAKT-MATNR'    OR 'WA_ZSDT0158_SAIDA-QUANTIDADE'
          OR 'WA_MAKT-TIPO_PROD' OR 'WA_ZSDT0158_SAIDA-SAFRA'
          OR 'CHECK_Z1'          OR 'TVV3T-KVGR3'
          OR 'TINCT-INCO1'       OR 'WA_ZSDT0158_SAIDA-KVGR4' OR 'WA_ZSDT0158_SAIDA-CK_TROCA_NOTA'
          OR 'PR_PRODUTO_F'      OR 'PR_PRODUTO_F'     OR 'WA_ID_COMPRA-QUANTIDADE'
          OR 'WA_ID_COMPRA-UNIDADE'

          " 20.07.2022 - RAMON - AJUSTE EXIBIÇÃO CAMPOS NOVOS -->
          OR 'WA_SAIDA-FORM_LOTE' OR 'WA_SAIDA-INDUST' OR 'WA_SAIDA-INDEFINIDO' OR 'WA_SAIDA-PED_TRANS'
          " 20.07.2022 - RAMON - AJUSTE EXIBIÇÃO CAMPOS NOVOS --<
          .

          screen-input  = 1.
          MODIFY SCREEN.

        WHEN 'WA_LFA1_Z1-LIFNR'.  "----> DESABILITA CAMPOS DO PORTO QUANDO = LR.

          IF check_z1 IS NOT INITIAL.
            screen-input = 0.
            MODIFY SCREEN.
          ELSE.
            screen-input = 1.
            MODIFY SCREEN.
          ENDIF.

          " 26.09.2022 - RAMON - 19450 -->
        WHEN 'WA_KONP-KBETR' OR 'T28'.

          IF wa_zsdt0158_saida-tp_solicitacao = 'O'.

            screen-invisible  = 0.
            MODIFY SCREEN.

          ELSEIF wa_zsdt0158_saida-tp_solicitacao = 'P'.

            screen-invisible  = 1.
            MODIFY SCREEN.

          ENDIF.

        WHEN 'CHECK_DCO' OR 'WA_ZDCO_PROD-NR_DCO' OR 'T29'.

          IF screen-name = 'CHECK_DCO'.

            screen-input  = 1.
            MODIFY SCREEN.

          ENDIF.

          IF wa_saida-form_lote = 'X' OR wa_saida-indefinido = 'X'.

            screen-invisible  = 0.
            MODIFY SCREEN.

          ELSE.

            screen-invisible  = 1.
            MODIFY SCREEN.

          ENDIF.


          " 26.09.2022 - RAMON - 19450 --<

*       05/11/2018 - REMOVIDO REGRAS P/ ID DE COMPRA
*        WHEN 'WA_ID_COMPRA-QUANTIDADE' OR 'WA_ID_COMPRA-UNIDADE'
*          OR 'TXT33' OR 'TXT34' OR 'BTN_BUSCA_ID'  OR 'BTN_LIMPA_ID'..
*
*          IF  ( TINCT-INCO1 EQ 'FOB' ).
*            SCREEN-INPUT      = 1.
*            SCREEN-INVISIBLE  = 0.
*            MODIFY SCREEN.
*          ENDIF.





      ENDCASE.

*       05/11/2018 - REMOVIDO REGRAS P/ ID DE COMPRA
*      CASE SCREEN-GROUP1.
*        WHEN 'COM'.
*          IF ( TINCT-INCO1 EQ 'FOB' ) OR ( PR_PRODUTO_A EQ ABAP_TRUE ).
*            SCREEN-ACTIVE     = 1.
*            SCREEN-INVISIBLE  = 0.
*            SCREEN-INPUT      = 1.
*            MODIFY SCREEN.
*          ENDIF.
*      ENDCASE.


    ENDLOOP.

    PERFORM f_trata_campos_terminal USING 'X'.

    SET CURSOR FIELD vg_focus.

  ELSEIF ( wa_zsdt0158-tipo IS INITIAL ) AND ( wa_zsdt0158-include-status EQ 'L' ).
*       05/11/2018 - REMOVIDO REGRAS P/ ID DE COMPRA
*    LOOP AT SCREEN.
*      CASE SCREEN-GROUP1.
*        WHEN 'COM'.
*          IF ( TINCT-INCO1 EQ 'FOB' ) OR ( PR_PRODUTO_A EQ ABAP_TRUE ).
*            SCREEN-ACTIVE     = 1.
*            SCREEN-INVISIBLE  = 0.
*            SCREEN-INPUT      = 0.
*            MODIFY SCREEN.
*          ENDIF.
*      ENDCASE.
*    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SCREEN_0102_CONTROL
*&---------------------------------------------------------------------*
FORM f_screen_0102_control .

  IF ( wa_zsdt0158-tipo IS INITIAL ) AND ( wa_zsdt0158-include-status NE 'L' ).

    LOOP AT SCREEN.

      CASE screen-name.
        WHEN 'T1' OR 'WA_SAIDA-NRO_SOL_OV'.

          IF wa_saida-nro_sol_ov IS INITIAL.
            screen-invisible  = 1.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.

        WHEN 'WA_T001W-WERKS'    OR 'WA_LFA1_PC-LIFNR' OR 'WA_LFA1_PC-LZONE'
          OR 'WA_KNA1-KUNNR'     OR 'WA_KNA1-LZONE'    OR 'WA_LFA1_Z1-LIFNR'
          OR 'WA_FRETE-TP_FRETE' OR 'WA_MAKT-MATNR'    OR 'WA_ZSDT0158_SAIDA-QUANTIDADE'
          OR 'WA_MAKT-TIPO_PROD' OR 'WA_LFA1_Z1-LZONE' OR 'WA_ZSDT0158_SAIDA-SAFRA'
          OR 'CHECK_Z1'          OR 'CHECK_DCO'        "OR 'WA_FRETE-AG_FRETE'
          OR 'TVV3T-KVGR3'       OR 'TINCT-INCO1'      OR 'WA_ZSDT0158_SAIDA-KVGR4' OR 'WA_ZSDT0158_SAIDA-CK_TROCA_NOTA'
          OR 'PR_PRODUTO_F'      OR 'PR_PRODUTO_A'     OR 'WA_ID_COMPRA-QUANTIDADE'
          OR 'WA_ID_COMPRA-UNIDADE' OR 'WA_ZSDT0158_SAIDA-KVGR5'

          " 20.07.2022 - RAMON - AJUSTE EXIBIÇÃO CAMPOS NOVOS -->
          OR 'WA_SAIDA-FORM_LOTE' OR 'WA_SAIDA-INDUST' OR 'WA_SAIDA-INDEFINIDO'
          " 20.07.2022 - RAMON - AJUSTE EXIBIÇÃO CAMPOS NOVOS --<
          .

          screen-input  = 0.
          MODIFY SCREEN.

        WHEN 'WA_SAIDA-SEQUENCIAL'.

          screen-input  = 1.
          MODIFY SCREEN.

        WHEN 'ALTERA_ZONA'  OR 'ALTERA_ZONA_PC' "OR 'WA_ID_COMPRA-QUANTIDADE' OR 'WA_ID_COMPRA-UNIDADE'
          OR 'TXT33'        OR 'TXT34'          OR 'BTN_BUSCA_ID'            OR 'BTN_LIMPA_ID'.

          screen-input      = 0.
          screen-invisible  = 1.
          MODIFY SCREEN.

      ENDCASE.

    ENDLOOP.

    " 26.09.2022 -->
    PERFORM f_screen_102_pedido.
    " 26.09.2022 --<

  ELSEIF wa_zsdt0158-tipo EQ 'I' OR wa_zsdt0158-tipo EQ 'E'.

    LOOP AT SCREEN.

      IF ( ( screen-name CS 'TXT33' ) OR ( screen-name CS 'TXT34' ) ) AND ( tinct-inco1 EQ 'FOB' ).
        screen-input      = 1.
        screen-active     = 1.
        screen-invisible  = 0.
        MODIFY SCREEN.
      ENDIF.

      CASE screen-name.
        WHEN 'WA_SAIDA-SEQUENCIAL'.

          screen-input  = 0.
          MODIFY SCREEN.

        WHEN 'T1' OR 'WA_SAIDA-NRO_SOL_OV'.

          IF wa_saida-nro_sol_ov IS INITIAL.
            screen-invisible  = 1.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.

        WHEN 'WA_T001W-WERKS'    OR 'WA_LFA1_PC-LIFNR' OR 'WA_KNA1-KUNNR'
          OR 'WA_FRETE-TP_FRETE' OR 'WA_MAKT-MATNR'    OR 'WA_ZSDT0158_SAIDA-QUANTIDADE'
          OR 'WA_MAKT-TIPO_PROD' OR 'WA_ZSDT0158_SAIDA-SAFRA'
          OR 'CHECK_Z1'          OR 'CHECK_DCO'        OR 'TVV3T-KVGR3'
          OR 'TINCT-INCO1'       OR 'WA_ZSDT0158_SAIDA-KVGR4' OR 'WA_ZSDT0158_SAIDA-CK_TROCA_NOTA'
          OR 'PR_PRODUTO_F'      OR 'PR_PRODUTO_F'     OR 'WA_ID_COMPRA-QUANTIDADE'
          OR 'WA_ID_COMPRA-UNIDADE'

          " 20.07.2022 - RAMON - AJUSTE EXIBIÇÃO CAMPOS NOVOS -->
          OR 'WA_SAIDA-FORM_LOTE' OR 'WA_SAIDA-INDUST' OR 'WA_SAIDA-INDEFINIDO'
          " 20.07.2022 - RAMON - AJUSTE EXIBIÇÃO CAMPOS NOVOS --<
          .

          screen-input  = 1.
          MODIFY SCREEN.

        WHEN 'WA_LFA1_Z1-LIFNR'.  "----> DESABILITA CAMPOS DO PORTO QUANDO = LR.

          IF check_z1 IS NOT INITIAL.
            screen-input = 0.
            MODIFY SCREEN.
          ELSE.
            screen-input = 1.
            MODIFY SCREEN.
          ENDIF.

*       05/11/2018 - REMOVIDO REGRAS P/ ID DE COMPRA
*        WHEN 'WA_ID_COMPRA-QUANTIDADE' OR 'WA_ID_COMPRA-UNIDADE'
*          OR 'TXT33' OR 'TXT34' OR 'BTN_BUSCA_ID'  OR 'BTN_LIMPA_ID'..
*
*          IF  ( TINCT-INCO1 EQ 'FOB' ).
*            SCREEN-INPUT      = 1.
*            SCREEN-INVISIBLE  = 0.
*            MODIFY SCREEN.
*          ENDIF.

      ENDCASE.

*       05/11/2018 - REMOVIDO REGRAS P/ ID DE COMPRA
*      CASE SCREEN-GROUP1.
*        WHEN 'COM'.
*          IF ( TINCT-INCO1 EQ 'FOB' ) OR ( PR_PRODUTO_A EQ ABAP_TRUE ).
*            SCREEN-ACTIVE     = 1.
*            SCREEN-INVISIBLE  = 0.
*            SCREEN-INPUT      = 1.
*            MODIFY SCREEN.
*          ENDIF.
*      ENDCASE.

    ENDLOOP.

    " 26.09.2022 -->
    PERFORM f_screen_102_pedido.
    " 26.09.2022 --<

    SET CURSOR FIELD vg_focus.

  ELSEIF ( wa_zsdt0158-tipo IS INITIAL ) AND ( wa_zsdt0158-include-status EQ 'L' ).
*       05/11/2018 - REMOVIDO REGRAS P/ ID DE COMPRA
*    LOOP AT SCREEN.
*      CASE SCREEN-GROUP1.
*        WHEN 'COM'.
*          IF ( TINCT-INCO1 EQ 'FOB' ) OR ( PR_PRODUTO_A EQ ABAP_TRUE ).
*            SCREEN-ACTIVE     = 1.
*            SCREEN-INVISIBLE  = 0.
*            SCREEN-INPUT      = 0.
*            MODIFY SCREEN.
*          ENDIF.
*      ENDCASE.
*    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SCREEN_102_PEDIDO
*&---------------------------------------------------------------------*
FORM f_screen_102_pedido .

  LOOP AT SCREEN.
    IF screen-name = 'T1' OR screen-name = 'WA_SAIDA-NRO_SOL_OV'.

      IF wa_zsdt0158_saida-tp_solicitacao = 'O'.

        IF wa_saida-nro_sol_ov IS INITIAL.
          screen-invisible  = 1.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.

      ELSEIF wa_zsdt0158_saida-tp_solicitacao = 'P'.

        " se for pedido então deixa esses campos invisiveis
        screen-invisible  = 1.
        MODIFY SCREEN.

      ELSEIF wa_zsdt0158_saida-tp_solicitacao = ''.

        " se for pedido então deixa esses campos invisiveis
        screen-invisible  = 1.
        MODIFY SCREEN.

      ENDIF.

    ENDIF.

    IF screen-name = 'T933' OR screen-name = 'WA_SAIDA-EBELN'.

      IF wa_zsdt0158_saida-tp_solicitacao = 'P'.
        IF wa_saida-ebeln IS INITIAL.
          screen-invisible  = 1.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.

      ELSE.

        " se for OV então deixa esses campos invisiveis
        screen-invisible  = 1.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_PO_ITEM
*&---------------------------------------------------------------------*
*FORM f_preenche_po_item USING p_item_tab TYPE bapiekpo_tp
*                     CHANGING p_poitem_tab TYPE bapimepoitem_tp
*                              p_poitemx_tab TYPE bapimepoitemx_tp.
FORM f_preenche_po_item USING p_item_tab TYPE bapimepoitem_tp
                     CHANGING p_poitem_tab TYPE bapimepoitem_tp
                              p_poitemx_tab TYPE bapimepoitemx_tp.

  LOOP AT p_item_tab ASSIGNING FIELD-SYMBOL(<fs_item>).

    APPEND INITIAL LINE TO p_poitem_tab ASSIGNING FIELD-SYMBOL(<fs_poitem>).
    APPEND INITIAL LINE TO p_poitemx_tab ASSIGNING FIELD-SYMBOL(<fs_poitemx>).

    MOVE-CORRESPONDING <fs_item> TO <fs_poitem>.

*    <fs_poitem>-po_unit = <fs_item>-unit.
    <fs_poitem>-po_unit = <fs_item>-po_unit.

    <fs_poitem>-quantity = wa_zsdt0158_qt-qtdade_nova.

    <fs_poitemx>-po_item = <fs_item>-po_item.
    <fs_poitemx>-quantity = 'X'.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_PO_ITEM
*&---------------------------------------------------------------------*
FORM f_preenche_po_sched USING p_sched_tab TYPE bapieket_tp
                     CHANGING p_posched_tab TYPE bapimeposchedule_tp
                              p_poschedx_tab TYPE bapimeposchedulx_tp.

  LOOP AT p_sched_tab ASSIGNING FIELD-SYMBOL(<fs_sched>).

    APPEND INITIAL LINE TO p_posched_tab ASSIGNING FIELD-SYMBOL(<fs_posched>).
    APPEND INITIAL LINE TO p_poschedx_tab ASSIGNING FIELD-SYMBOL(<fs_poschedx>).

    MOVE-CORRESPONDING <fs_sched> TO <fs_posched>.

    <fs_posched>-sched_line = <fs_sched>-serial_no.
    <fs_posched>-del_datcat_ext = space.

    <fs_poschedx>-sched_line = <fs_posched>-sched_line.

    <fs_poschedx>-quantity = 'X'.

  ENDLOOP.

ENDFORM.

FORM atualiza_controles_eudr .

  IF is_filial_participante_eudr EQ 'S' AND is_material_eudr IS NOT INITIAL.
    is_processo_eudr = abap_true.
  ELSE.
    is_processo_eudr = abap_false.
  ENDIF.

ENDFORM.
