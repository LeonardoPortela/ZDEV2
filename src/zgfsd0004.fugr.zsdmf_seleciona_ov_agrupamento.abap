FUNCTION zsdmf_seleciona_ov_agrupamento .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(IR_VBELN_RANGE) TYPE  SHP_VBELN_RANGE_T OPTIONAL
*"     REFERENCE(IV_SEM_POPUP) TYPE  FLAG DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(EV_CANC) TYPE  FLAG
*"  TABLES
*"      ET_OVS STRUCTURE  ZSDS084
*"----------------------------------------------------------------------

  DATA lr_vbeln_range TYPE shp_vbeln_range_t.

  PERFORM f_refresh_9000.

  CLEAR: et_ovs[], gt_9000_alv[].

  lr_vbeln_range = ir_vbeln_range.

  IF iv_vbeln IS NOT INITIAL.
    APPEND 'IEQ' && iv_vbeln TO lr_vbeln_range.
  ENDIF.

  SELECT vbelv, SUM( rfmng ) AS qtd_fat FROM vbfa
     INTO TABLE @DATA(lt_fat)
       WHERE vbelv IN @lr_vbeln_range
         AND vbtyp_v = 'C'
         AND vbtyp_n <> 'J'
         GROUP BY vbelv.

  "DELETE lt_fat WHERE qtd_fat > 0.

  LOOP AT lr_vbeln_range ASSIGNING FIELD-SYMBOL(<fs_range>).

    READ TABLE lt_fat ASSIGNING FIELD-SYMBOL(<fs_fat>)
      WITH KEY vbelv = <fs_range>-low.

    DATA(lv_index) = sy-tabix.

    CHECK sy-subrc EQ 0.

    CHECK <fs_fat>-qtd_fat > 0.

    DELETE lr_vbeln_range INDEX lv_index.

  ENDLOOP.

  CHECK lr_vbeln_range IS NOT INITIAL.

  PERFORM f_selec_0041 USING lr_vbeln_range CHANGING gt_9000g_alv[].

  PERFORM f_selec_0090 USING lr_vbeln_range CHANGING gt_9000g_alv[].

  PERFORM f_dados_std_ov CHANGING gt_9000g_alv[].

  SELECT vbelv FROM zsdt0090
    INTO TABLE @DATA(lt_0090)
      FOR ALL ENTRIES IN @gt_9000g_alv
      WHERE vbelv = @gt_9000g_alv-vbeln
        AND estorno = @space
        AND categoria = 'C'
        AND trav_camb_utilizada = @space.

  LOOP AT gt_9000g_alv ASSIGNING FIELD-SYMBOL(<fs_agrup>).

    CASE <fs_agrup>-auart.
      WHEN 'ZFTE' OR 'ZOFE'.
        <fs_agrup>-flag_fert = abap_true.
      WHEN  'ZSEM' OR 'ZOSM'.
        <fs_agrup>-flag_seme = abap_true.
      WHEN 'ZDEF' OR 'ZODF'.
        <fs_agrup>-flag_defe = abap_true.
    ENDCASE.

    READ TABLE lt_0090 TRANSPORTING NO FIELDS
      WITH KEY vbelv = <fs_agrup>-vbeln.

    IF sy-subrc EQ 0.
      <fs_agrup>-c_trava = abap_true.
    ENDIF.

  ENDLOOP.

  SORT gt_9000g_alv.

  DELETE ADJACENT DUPLICATES FROM gt_9000g_alv COMPARING ALL FIELDS.

  "gt_9000g_alv[] = et_ovs_agrup[].

  zsds084-flag_fert = abap_true.
  zsds084-flag_defe = abap_false.
  zsds084-flag_seme = abap_false.

  IF iv_sem_popup IS INITIAL.

    CALL SCREEN 9000 STARTING AT 20 1.

    IF gv_ucomm_9000 = 'ESC'.

      CLEAR gt_9000g_alv[].

      ev_canc = abap_true.

      EXIT.

    ENDIF.

  ELSE.

    LOOP AT gt_9000g_alv ASSIGNING FIELD-SYMBOL(<fs_alv_g>) WHERE vbeln IN lr_vbeln_range.

      <fs_alv_g>-selec = abap_true.

      APPEND <fs_alv_g> TO gt_9000_alv.

    ENDLOOP.

    PERFORM f_verificar_campos USING ev_canc.

  ENDIF.

  CHECK ev_canc IS INITIAL.

  DELETE gt_9000_alv WHERE selec IS INITIAL.

  " devolve as ovs escolhidas na tela
  et_ovs[] = gt_9000_alv[].

ENDFUNCTION.
