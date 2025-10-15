FUNCTION zles_set_data_bp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BAPI_FORNECE) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_BAPI_CLIENTE) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_BUT000) TYPE  BUT000 OPTIONAL
*"     REFERENCE(I_LFA1) TYPE  LFA1 OPTIONAL
*"     REFERENCE(T_LFB1) TYPE  CVIS_LFB1_T OPTIONAL
*"     REFERENCE(T_LFBK) TYPE  LFBK_T OPTIONAL
*"     REFERENCE(T_LFBW) TYPE  CVIS_LFBW_T OPTIONAL
*"     REFERENCE(T_LFM1) TYPE  CVIS_LFM1_T OPTIONAL
*"     REFERENCE(I_KNA1) TYPE  KNA1 OPTIONAL
*"     REFERENCE(I_KNVK) TYPE  KNVK OPTIONAL
*"     REFERENCE(T_KNB1) TYPE  CVIS_KNB1_T OPTIONAL
*"     REFERENCE(T_KNBW) TYPE  CVIS_KNBW_T OPTIONAL
*"     REFERENCE(T_KNVI) TYPE  CVIS_KNVI_T OPTIONAL
*"     REFERENCE(T_KNVV) TYPE  CVIS_KNVV_T OPTIONAL
*"----------------------------------------------------------------------

  DATA: lv_tabix TYPE sy-tabix.

  gs_bapi_fornece = i_bapi_fornece.
  gs_bapi_cliente = i_bapi_cliente.

  gs_but000       = i_but000. "*-CS2021000253-#149089-07.01.2025-JT
*
  gs_lfa1         = i_lfa1.
  gt_lfb1[]       = t_lfb1[].
  gt_lfbk[]       = t_lfbk[].
  gt_lfbw[]       = t_lfbw[].
  gt_lfm1[]       = t_lfm1[].
*
  gs_kna1         = i_kna1.
  gs_knvk         = i_knvk.
  gt_knb1[]       = t_knb1[].
  gt_knbw[]       = t_knbw[].
  gt_knvi[]       = t_knvi[].
  gt_knvv[]       = t_knvv[].

*-CS2021000253-#149089-07.01.2025-JT-inicio
*----------------------------
*-BUT000
*----------------------------
  IF gs_but000 IS NOT INITIAL.
    SELECT SINGLE *
      INTO @DATA(_but000)
      FROM but000
     WHERE partner = @gs_but000-partner.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING   _but000
                               CHANGING gs_but000.
    ENDIF.
  ENDIF.

*----------------------------
*-lfa1
*----------------------------
  IF gs_lfa1 IS NOT INITIAL.
    SELECT SINGLE *
      INTO @DATA(_lfa1)
      FROM lfa1
     WHERE lifnr = @gs_lfa1-lifnr.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING   _lfa1
                               CHANGING gs_lfa1.
    ENDIF.
  ENDIF.

*----------------------------
*-lfb1
*----------------------------
  LOOP AT gt_lfb1 INTO DATA(w_lfb1).
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      INTO @DATA(_lfb1)
      FROM lfb1
     WHERE lifnr = @w_lfb1-lifnr
       AND bukrs = @w_lfb1-bukrs.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING  _lfb1
                               CHANGING w_lfb1.
      MODIFY gt_lfb1 FROM w_lfb1 INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

*----------------------------
*-lfbk
*----------------------------
  LOOP AT gt_lfbk INTO DATA(w_lfbk).
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      INTO @DATA(_lfbk)
      FROM lfbk
     WHERE lifnr = @w_lfbk-lifnr
       AND banks = @w_lfbk-banks
       AND bankl = @w_lfbk-bankl
       AND bankn = @w_lfbk-bankn.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING  _lfbk
                               CHANGING w_lfbk.
      MODIFY gt_lfbk FROM w_lfbk INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

*----------------------------
*-lfbw
*----------------------------
  LOOP AT gt_lfbw INTO DATA(w_lfbw).
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      INTO @DATA(_lfbw)
      FROM lfbw
     WHERE lifnr = @w_lfbw-lifnr
       AND bukrs = @w_lfbw-bukrs
       AND witht = @w_lfbw-witht.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING  _lfbw
                               CHANGING w_lfbw.
      MODIFY gt_lfbw FROM w_lfbw INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

*----------------------------
*-lfm1
*----------------------------
  LOOP AT gt_lfm1 INTO DATA(w_lfm1).
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      INTO @DATA(_lfm1)
      FROM lfm1
     WHERE lifnr = @w_lfm1-lifnr
       AND ekorg = @w_lfm1-ekorg.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING  _lfm1
                               CHANGING w_lfm1.
      MODIFY gt_lfm1 FROM w_lfm1 INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

*----------------------------
*-kna1
*----------------------------
  IF gs_kna1 IS NOT INITIAL.
    SELECT SINGLE *
      INTO @DATA(_kna1)
      FROM kna1
     WHERE kunnr = @gs_kna1-kunnr.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING   _kna1
                               CHANGING gs_kna1.
    ENDIF.
  ENDIF.

*----------------------------
*-knvk
*----------------------------
  IF gs_knvk IS NOT INITIAL.
    SELECT SINGLE *
      INTO @DATA(_knvk)
      FROM knvk
     WHERE parnr = @gs_knvk-parnr.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING   _knvk
                               CHANGING gs_knvk.
    ENDIF.
  ENDIF.

*----------------------------
*-knb1
*----------------------------
  LOOP AT gt_knb1 INTO DATA(w_knb1).
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      INTO @DATA(_knb1)
      FROM knb1
     WHERE kunnr = @w_knb1-kunnr
       AND bukrs = @w_knb1-bukrs.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING  _knb1
                               CHANGING w_knb1.
      MODIFY gt_knb1 FROM w_knb1 INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

*----------------------------
*-knbw
*----------------------------
  LOOP AT gt_knbw INTO DATA(w_knbw).
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      INTO @DATA(_knbw)
      FROM knbw
     WHERE kunnr = @w_knbw-kunnr
       AND bukrs = @w_knbw-bukrs
       AND witht = @w_knbw-witht.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING  _knbw
                               CHANGING w_knbw.
      MODIFY gt_knbw FROM w_knbw INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

*----------------------------
*-knvi
*----------------------------
  LOOP AT gt_knvi INTO DATA(w_knvi).
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      INTO @DATA(_knvi)
      FROM knvi
     WHERE kunnr = @w_knvi-kunnr
       AND aland = @w_knvi-aland
       AND tatyp = @w_knvi-tatyp.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING  _knvi
                               CHANGING w_knvi.
      MODIFY gt_knvi FROM w_knvi INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

*----------------------------
*-knvv
*----------------------------
  LOOP AT gt_knvv INTO DATA(w_knvv).
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      INTO @DATA(_knvv)
      FROM knvv
     WHERE kunnr = @w_knvv-kunnr
       AND vkorg = @w_knvv-vkorg
       AND vtweg = @w_knvv-vtweg
       AND spart = @w_knvv-spart.

    IF sy-subrc = 0.
      PERFORM f_move_estrutura    USING  _knvv
                               CHANGING w_knvv.
      MODIFY gt_knvv FROM w_knvv INDEX lv_tabix.
    ENDIF.
  ENDLOOP.
*-CS2021000253-#149089-07.01.2025-JT-fim

ENDFUNCTION.
