class ZCL_MM_MR21_MR22 definition
  public
  final
  create public .

public section.

  interfaces IF_SALV_GUI_OM_GRID_LISTENER .
  interfaces IF_SALV_GUI_OM_EDIT_STRCT_LSTR .

  types:
    BEGIN OF ty_alv,
      data_vencimento  TYPE zmmt0178-data_vencimento,
      tipo             TYPE zmmt0178-tipo,
      status           TYPE icon-id,
      werks            TYPE zmmt0178-werks,
      matnr            TYPE zmmt0178-matnr,
      maktx            TYPE zmmt0178-maktx,
      vprsv            TYPE zmmt0178-vprsv,
      vlr_brl          TYPE zmmt0178-vlr_brl,
      vlr_usd          TYPE zmmt0178-vlr_usd,
      stprs            TYPE zmmt0178-stprs,
      aval_atual_brl   TYPE zmmt0178-aval_atual_brl,
      aval_atual_usd   TYPE zmmt0178-aval_atual_usd,
      aval_atual_stprs TYPE zmmt0178-aval_atual_stprs,
      vlr_brl_old      TYPE zmmt0178-vlr_brl_old,
      vlr_brl_new      TYPE zmmt0178-vlr_brl_new,
      vlr_usd_old      TYPE zmmt0178-vlr_usd_old,
      vlr_usd_new      TYPE zmmt0178-vlr_usd_new,
      stprs_old        TYPE zmmt0178-stprs_old,
      stprs_new        TYPE zmmt0178-stprs_new,
      data_ref         TYPE zmmt0178-data_vencimento,
      doc_mr22         TYPE zmmt0178-doc_mr22,
      message          TYPE char100,
      usuario          TYPE zmmt0178-usuario,
      data_modif       TYPE zmmt0178-data_modif,
      timestamp        TYPE zmmt0178-timestamp,
      modify           TYPE char1,
    END   OF ty_alv .

  methods CONSTRUCTOR .
  methods EXECUTE
    importing
      !IV_BUKRS type J_1BBRANCH-BUKRS optional
      !IV_DATA type BUDAT optional
      !IV_REF type XBLNR optional
      !IB_MR21 type CHAR1 optional
      !IB_MR22 type CHAR1 optional .
  methods SET_DADOS_SHDB
    importing
      !I_BUKRS type BUKRS
      !I_DATA type BUDAT
      !I_REFERENCIA type XBLNR .
  methods BATCH_INPUT_MR21
    importing
      !IS_ALV type TY_ALV .
  methods CALL_TRANSACTION_MR21_MR22 .
  methods SUBMIT_MR21_MR22 .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_makt,
      matnr TYPE makt-matnr,
      maktx TYPE makt-maktx,
    END   OF ty_makt .
  types:
    BEGIN OF ty_mbew,
      matnr TYPE mbew-matnr,
      bwkey TYPE mbew-bwkey,
      vprsv TYPE mbew-vprsv,
      verpr TYPE mbew-verpr,
      salk3 TYPE mbew-salk3,
      lbkum TYPE mbew-lbkum,
    END   OF ty_mbew .
  types:
    BEGIN OF ty_ckmlhd,
      kalnr TYPE ckmlhd-kalnr,
      matnr TYPE ckmlhd-matnr,
      bwkey TYPE ckmlhd-bwkey,
    END   OF ty_ckmlhd .
  types:
    BEGIN OF ty_t001w,
      werks      TYPE t001w-werks,
      j_1bbranch TYPE t001w-j_1bbranch,
      land1      TYPE t001w-land1,
    END   OF ty_t001w .
  types:
    BEGIN OF ty_ckmlcr,
      stprs TYPE ckmlcr-stprs,
      kalnr TYPE ckmlcr-kalnr,
      pvprs TYPE ckmlcr-pvprs,
      curtp TYPE ckmlcr-curtp,
      salk3 TYPE ckmlcr-salk3,
      bdatj TYPE ckmlcr-bdatj,
      poper TYPE ckmlcr-poper,
    END   OF ty_ckmlcr .
  types:
    tt_makt   TYPE TABLE OF ty_makt   WITH EMPTY KEY .
  types:
    tt_alv    TYPE TABLE OF ty_alv    WITH EMPTY KEY .
  types:
    tt_mbew   TYPE TABLE OF ty_mbew   WITH EMPTY KEY .
  types:
    tt_ckmlhd TYPE TABLE OF ty_ckmlhd WITH EMPTY KEY .
  types:
    tt_ckmlcr TYPE TABLE OF ty_ckmlcr WITH EMPTY KEY .
  types:
    tt_t001w  TYPE TABLE OF ty_t001w  WITH EMPTY KEY .

  data T_MSG_AUX type TAB_BDCMSGCOLL .
  data O_ALV type ref to CL_SALV_TABLE .
  data T_ALV type TT_ALV .
  data S_BDCDATA type BDCDATA .
  data T_BDCDATA type BDCDATA_TAB .
  data V_BUKRS type J_1BBRANCH-BUKRS .
  data V_DATA type BUDAT .
  data V_REF type XBLNR .
  data RB_MR21 type BOOLEAN .
  data RB_MR22 type BOOLEAN .
  data S_EDIT type ref to IF_SALV_GUI_OM_EDIT_RESTRICTED .
  data T_MBEW type TT_MBEW .
  data T_MAKT type TT_MAKT .
  data T_CKMLCR type TT_CKMLCR .
  data T_CKMLHD type TT_CKMLHD .
  data T_ROWS type SALV_T_ROW .
  data T_T001W type TT_T001W .

  methods CHANGE_VALUE_CONVERT
    importing
      value(IS_ALV) type TY_ALV
    returning
      value(E_RETURN) type TY_ALV .
  methods BUILD_ALV .
  methods ON_USER_COMMAND
    for event IF_SALV_EVENTS_FUNCTIONS~ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
  methods BATCH_INPUT_MR22
    importing
      !IS_ALV type TY_ALV .
  methods START_DYNPRO
    importing
      !IV_PROG type BDCDATA-PROGRAM
      !IV_DYNPRO type BDCDATA-DYNPRO .
  methods FILL_FIELD
    importing
      !IV_FNAME type BDCDATA-FNAM
      !IV_FVALUE type BDCDATA-FVAL .
  methods FILL_OKCODE
    importing
      !IV_OKCODE type BDCDATA-FVAL .
  methods MODIFY_ZMMT0178 .
  methods PROCESS_DATA .
  methods SHOW_FUNCTION_INFO .
  methods CALL_ALV .
  methods SET_COLUMNS_TECHNICAL
    importing
      !IR_COLUMNS type ref to CL_SALV_COLUMNS .
  methods GET_MESSAGE
    returning
      value(RETURN) type TY_ALV .
  methods GET_EXCHANGE_RATE
    returning
      value(RETURN) type ZMMT0178-VLR_USD .
  methods SET_ENABLED_CELLS
    importing
      !IV_ENABLED type BOOLEAN .
  methods GET_DATA_ZMMT0178
    importing
      value(IS_ALV) type TY_ALV optional .
  methods GET_DATA .
  methods CONVERT_VALUES
    importing
      !IS_ALV type TY_ALV
    returning
      value(RETURN) type TY_ALV .
  methods SET_COLOR_COLUMN .
ENDCLASS.



CLASS ZCL_MM_MR21_MR22 IMPLEMENTATION.


  METHOD batch_input_mr21.

    DATA lv_data_fvalue(10).

    WRITE v_data TO lv_data_fvalue.

    DATA(lv_valor_brl)   = CONV bdc_fval( is_alv-vlr_brl ).
    DATA(lv_valor_usd)   = CONV bdc_fval( is_alv-vlr_usd ).
    DATA(lv_valor_stprs) = CONV bdc_fval( is_alv-stprs ).
    DATA(lv_matnr) = is_alv-matnr.

    CONDENSE lv_matnr.
    CONDENSE lv_valor_brl.
    CONDENSE lv_valor_usd.
    CONDENSE lv_valor_stprs.

    REPLACE '.' IN lv_valor_brl   WITH ','.
    REPLACE '.' IN lv_valor_usd   WITH ','.
    REPLACE '.' IN lv_valor_stprs WITH ','.

    start_dynpro( iv_prog   = 'SAPRCKM_MR21' iv_dynpro = '0201'      ).
    fill_field( iv_fname = 'BDC_CURSOR' iv_fvalue = 'MR21HEAD-BKTXT' ).
    fill_okcode( iv_okcode = '=ENTR' ).

    fill_field( iv_fname = 'MR21HEAD-BUDAT' iv_fvalue = CONV #( lv_data_fvalue ) ). "'29.05.2024' ).
    fill_field( iv_fname = 'MR21HEAD-BUKRS' iv_fvalue = CONV #( v_bukrs ) ). "'0001'       ).
    fill_field( iv_fname = 'MR21HEAD-WERKS' iv_fvalue = CONV #( is_alv-werks ) )." '0111'       ).
    fill_field( iv_fname = 'MR21HEAD-XBLNR' iv_fvalue = CONV #( v_ref ) ). "'ref'        ).
    fill_field( iv_fname = 'MR21HEAD-BKTXT' iv_fvalue = 'cabec'      ).

    fill_field( iv_fname = 'BDC_SUBSCR' iv_fvalue = 'SAPRCKM_MR21                            0250MR21_SUB'   ).
    fill_field( iv_fname = 'MR21HEAD-SCREEN_VARIANT' iv_fvalue = 'MR21_LAGERMATERIAL_0250' ).

    start_dynpro( iv_prog   = 'SAPRCKM_MR21' iv_dynpro = '0201'      ).
    fill_okcode( iv_okcode = '=ENTR' ).

    fill_field( iv_fname = 'BDC_SUBSCR' iv_fvalue = 'SAPRCKM_MR21                            0250MR21_SUB' ).
    fill_field( iv_fname = 'BDC_CURSOR' iv_fvalue = 'CKI_MR21_0250-MATNR(01)' ).
    fill_field( iv_fname = 'MR21HEAD-SCREEN_VARIANT' iv_fvalue = 'MR21_LAGERMATERIAL_BWKEY_0250' ).
    fill_field( iv_fname = 'CKI_MR21_0250-MATNR(01)' iv_fvalue = CONV #( lv_matnr ) )."'88' ).

    start_dynpro( iv_prog   = 'SAPRCKM_MR21' iv_dynpro = '0201'      ).
    fill_okcode( iv_okcode = '=TAB2' ).

    fill_field( iv_fname = 'BDC_SUBSCR' iv_fvalue = 'SAPRCKM_MR21                            0250MR21_SUB' ).
    fill_field( iv_fname = 'BDC_CURSOR' iv_fvalue = 'CKI_MR21_0250-NEWVALPR(01)'   ).

    IF lv_valor_brl > '0,00'.
      fill_field( iv_fname = 'CKI_MR21_0250-NEWVALPR(01)' iv_fvalue = CONV #( lv_valor_brl ) ). "'4,10'   ).

    ENDIF.

    start_dynpro( iv_prog   = 'SAPRCKM_MR21' iv_dynpro = '0201'      ).
    fill_okcode( iv_okcode = '=TAB3' ).

    fill_field( iv_fname = 'BDC_SUBSCR' iv_fvalue = 'SAPRCKM_MR21                            0250MR21_SUB'   ).
    fill_field( iv_fname = 'BDC_CURSOR' iv_fvalue = 'CKI_MR21_0250-NEWVALPR(01)'   ).

    IF lv_valor_usd > '0,00'.
      fill_field( iv_fname = 'CKI_MR21_0250-NEWVALPR(01)' iv_fvalue = CONV #( lv_valor_usd ) ). "'0,79'   ).

    ENDIF.

    start_dynpro( iv_prog   = 'SAPRCKM_MR21' iv_dynpro = '0201'      ).

    fill_field( iv_fname = 'BDC_SUBSCR' iv_fvalue = 'SAPRCKM_MR21                            0250MR21_SUB'   ).
    fill_field( iv_fname = 'BDC_CURSOR' iv_fvalue = 'CKI_MR21_0250-NEWVALPR(01)'   ).

    IF lv_valor_stprs > '0,00'.
      fill_field( iv_fname = 'CKI_MR21_0250-NEWVALPR(01)' iv_fvalue = CONV #( lv_valor_stprs ) ). "'4,70'   ).

    ENDIF.

    start_dynpro( iv_prog   = 'SAPRCKM_MR21' iv_dynpro = '0201'      ).
    fill_okcode( iv_okcode = '=SAVE' ).

    start_dynpro( iv_prog   = 'SAPLSPO1' iv_dynpro = '0100'      ).
    fill_okcode( iv_okcode = '=YES' ).

  ENDMETHOD.


  METHOD batch_input_mr22.

    DATA lv_data_fvalue(10).

    WRITE v_data TO lv_data_fvalue.

    DATA(lv_valor_brl)   = CONV bdc_fval( is_alv-vlr_brl ).
    DATA(lv_valor_usd)   = CONV bdc_fval( is_alv-vlr_usd ).
    DATA(lv_valor_stprs) = CONV bdc_fval( is_alv-stprs ).
    DATA(lv_matnr) = is_alv-matnr.

    CONDENSE lv_matnr.
    CONDENSE lv_valor_brl.
    CONDENSE lv_valor_usd.
    CONDENSE lv_valor_stprs.

    REPLACE '.' IN lv_valor_brl   WITH ','.
    REPLACE '.' IN lv_valor_usd   WITH ','.
    REPLACE '.' IN lv_valor_stprs WITH ','.

    start_dynpro( iv_prog = 'SAPRCKM_MR22' iv_dynpro = '0201' ).
    fill_field( iv_fname  =  'BDC_CURSOR'  iv_fvalue = 'MR21HEAD-XBLNR' ).
    fill_okcode( iv_okcode = '=ENTR' ).

    fill_field( iv_fname = 'MR21HEAD-BUDAT'          iv_fvalue = CONV #( lv_data_fvalue ) ).
    fill_field( iv_fname = 'MR21HEAD-BUKRS'          iv_fvalue = CONV #( v_bukrs ) ).
    fill_field( iv_fname = 'MR21HEAD-WERKS'          iv_fvalue = CONV #( is_alv-werks ) ).
    fill_field( iv_fname = 'MR21HEAD-XBLNR'          iv_fvalue = CONV #( v_ref ) ).
    fill_field( iv_fname = 'BDC_SUBSCR'              iv_fvalue = 'SAPRCKM_MR22                           0250MR22_SUB' ).
    fill_field( iv_fname = 'MR21HEAD-SCREEN_VARIANT' iv_fvalue = 'LAGERMATERIAL_0250' ) .

    start_dynpro( iv_prog = 'SAPRCKM_MR22' iv_dynpro = '0201' ).
    fill_okcode( iv_okcode =  '=ENTR' ).
    fill_field( iv_fname = 'BDC_SUBSCR'              iv_fvalue = 'SAPRCKM_MR22                            0250MR22_SUB' ).
    fill_field( iv_fname = 'BDC_CURSOR'              iv_fvalue = 'CKI_MR22_0250-ZUUMB(01)' ).
    fill_field( iv_fname = 'MR21HEAD-SCREEN_VARIANT' iv_fvalue = 'LAGERMATERIAL - OHNE BWKEY_025' ).
    fill_field( iv_fname = 'CKI_MR22_0250-MATNR(01)' iv_fvalue = CONV #( lv_matnr ) ).

    IF lv_valor_brl > '0.00'.
      fill_field( iv_fname = 'CKI_MR22_0250-ZUUMB(01)' iv_fvalue = CONV #( lv_valor_brl ) ).
    ENDIF.

    start_dynpro( iv_prog = 'SAPRCKM_MR22' iv_dynpro = '0201' ).
    fill_okcode( iv_okcode = '=TAB2' ).
    fill_field( iv_fname = 'BDC_SUBSCR'            iv_fvalue = 'SAPRCKM_MR22                            0250MR22_SUB' ).
    fill_field( iv_fname = 'BDC_CURSOR'            iv_fvalue = 'CKI_MR22_0250-MATNR(02)' ).
    fill_field( iv_fname ='MR21HEAD-SCREEN_VARIANT' iv_fvalue = 'LAGERMATERIAL - OHNE BWKEY_025' ).

    start_dynpro( iv_prog = 'SAPRCKM_MR22' iv_dynpro = '0201' ).
    fill_okcode( iv_okcode =  '=TAB3' ).
    fill_field( iv_fname = 'BDC_SUBSCR'              iv_fvalue = 'SAPRCKM_MR22                            0250MR22_SUB' ).
    fill_field( iv_fname = 'BDC_CURSOR'              iv_fvalue = 'CKI_MR22_0250-ZUUMB(01)' ).
    fill_field( iv_fname = 'MR21HEAD-SCREEN_VARIANT' iv_fvalue = 'LAGERMATERIAL - OHNE BWKEY_025' ).

    IF lv_valor_usd > '0.00'.
      fill_field( iv_fname = 'CKI_MR22_0250-ZUUMB(01)' iv_fvalue = CONV #( lv_valor_usd ) ).
    ENDIF.

    start_dynpro( iv_prog = 'SAPRCKM_MR22'  iv_dynpro = '0201' ).
    fill_okcode( iv_okcode =  '=ENTR' ).
    fill_field( iv_fname = 'BDC_SUBSCR'              iv_fvalue = 'SAPRCKM_MR22                            0250MR22_SUB' ).
    fill_field( iv_fname = 'BDC_CURSOR'              iv_fvalue = 'CKI_MR22_0250-ZUUMB(01)' ).
    fill_field( iv_fname = 'MR21HEAD-SCREEN_VARIANT' iv_fvalue = 'LAGERMATERIAL - OHNE BWKEY_025' ).

    IF lv_valor_stprs > '0.00'.
      fill_field( iv_fname = 'CKI_MR22_0250-ZUUMB(01)' iv_fvalue = CONV #( lv_valor_stprs ) ).
    ENDIF.

    start_dynpro( iv_prog =  'SAPRCKM_MR22' iv_dynpro = '0201' ).
    fill_okcode( iv_okcode = '=SAVE' ).
    fill_field( iv_fname = 'BDC_SUBSCR'              iv_fvalue = 'SAPRCKM_MR22                            0250MR22_SUB' ).
    fill_field( iv_fname = 'BDC_CURSOR'              iv_fvalue = 'CKI_MR22_0250-MATNR(02)' ).
    fill_field( iv_fname ='MR21HEAD-SCREEN_VARIANT' iv_fvalue = 'LAGERMATERIAL - OHNE BWKEY_025' ).

    start_dynpro( iv_prog   = 'SAPLSPO1' iv_dynpro = '0100'      ).
    fill_okcode( iv_okcode = '=YES' ).

  ENDMETHOD.


  METHOD build_alv.

    call_alv( ).

*... §3.1 activate ALV generic Functions
    DATA(lr_functions) = o_alv->get_functions( ).

    lr_functions->set_all( abap_true ).

    DATA(lr_columns) = o_alv->get_columns( ).
    lr_columns->set_optimize( abap_true ).

    set_columns_technical( lr_columns ).
    set_color_column( ).

    SELECT SINGLE land1
      FROM t001
      INTO @DATA(lv_land1)
      WHERE bukrs  = @v_bukrs.

    IF rb_mr21  = abap_true AND lv_land1 <> 'BR'.

      lr_columns->set_column_position( columnname = 'STPRS'
                                       position   = 8 ).

      lr_columns->set_column_position( columnname = 'AVAL_ATUAL_STPRS'
                                       position   = 11 ).
      "ELSEIF rb_mr22  = abap_true  AND   lv_land1 <> 'BR'.  "147975 - PSA Ajuste na conversão ZMM0224

    ELSEIF rb_mr22  = abap_true.  "147975 - PSA Ajuste na conversão ZMM0224
      ""BREAK-POINT.

      IF lv_land1 <> 'BR'.  "147975 - PSA Ajuste na conversão ZMM0224

        lr_columns->set_column_position( columnname = 'STPRS'
                                         position   = 8 ).
        lr_columns->set_column_position( columnname = 'VLR_BRL'
                                         position   = 9 ).

        lr_columns->set_column_position( columnname = 'VLR_USD'
                                         position   = 10 ).

        lr_columns->set_column_position( columnname = 'STPRS_NEW'
                                         position   = 11 ).

        lr_columns->set_column_position( columnname = 'VLR_BRL_NEW'
                                         position   = 12 ).

        lr_columns->set_column_position( columnname = 'VLR_USD_NEW'
                                         position   = 13 ).

        lr_columns->set_column_position( columnname = 'STPRS_OLD'
                                         position   = 14 ).

        lr_columns->set_column_position( columnname = 'VLR_BRL_OLD'
                                         position   = 15 ).

        lr_columns->set_column_position( columnname = 'VLR_USD_OLD'
                                         position   = 16 ).

      ELSEIF lv_land1 = 'BR'. "147975 - PSA Ajuste na conversão ZMM0224

        lr_columns->set_column_position( columnname = 'VLR_BRL'
                                         position   = 8 ).

        lr_columns->set_column_position( columnname = 'VLR_USD'
                                         position   = 9 ).

        lr_columns->set_column_position( columnname = 'STPRS'
                                         position   = 10 ).

        lr_columns->set_column_position( columnname = 'VLR_BRL_NEW'
                                         position   = 11 ).

        lr_columns->set_column_position( columnname = 'VLR_USD_NEW'
                                         position   = 12 ).

        lr_columns->set_column_position( columnname = 'STPRS_NEW'
                                         position   = 13 ).

        lr_columns->set_column_position( columnname = 'VLR_BRL_OLD'
                                         position   = 14 ).

        lr_columns->set_column_position( columnname = 'VLR_USD_OLD'
                                         position   = 15 ).

        lr_columns->set_column_position( columnname = 'STPRS_OLD'
                                         position   = 16 ).
      ENDIF.

    ENDIF.

    DATA(ls_api) = o_alv->extended_grid_api( ).
    s_edit = ls_api->editable_restricted( ).

    set_enabled_cells( abap_true ).

    o_alv->set_screen_status( pfstatus      = 'ZFS_MMR200'
                              report        = 'ZMMR200'
                              set_functions = o_alv->c_functions_all ).

    DATA(lo_edit) = o_alv->extended_grid_api( )->editable_restricted( ).

    lo_edit->set_listener( me ).

    DATA(lr_events) = me->o_alv->get_event( ).

    SET HANDLER on_user_command FOR lr_events.

    o_alv->display( ).

  ENDMETHOD.


  METHOD call_alv.

    TRY .
        cl_salv_table=>factory(
          EXPORTING
             list_display = abap_false
          IMPORTING
            r_salv_table = o_alv
          CHANGING
            t_table      = t_alv ).
      CATCH cx_salv_msg.
        WRITE: / 'Erro na execução do ALV'.
    ENDTRY.

  ENDMETHOD.


  METHOD call_transaction_mr21_mr22.

    DATA lv_mode TYPE char1.
    lv_mode = 'N'.

    DATA(lv_transaction) = COND #( WHEN rb_mr21 IS NOT INITIAL THEN 'MR21'
                                   ELSE 'MR22' ).

    CALL TRANSACTION lv_transaction
              USING t_bdcdata
              MODE lv_mode
              UPDATE 'S'
              MESSAGES INTO t_msg_aux.

  ENDMETHOD.


  METHOD constructor.

  ENDMETHOD.


  METHOD execute.

    v_bukrs = iv_bukrs.
    v_data  = iv_data.
    v_ref   = iv_ref.
    rb_mr21 = ib_mr21.
    rb_mr22 = ib_mr22.

    EXPORT v_bukrs = v_bukrs TO MEMORY ID 'ZMM_BUKRS'.

    get_data_zmmt0178( ).
    build_alv( ).

  ENDMETHOD.


  METHOD fill_field.

    CLEAR s_bdcdata.
    s_bdcdata-fnam  = iv_fname.
    s_bdcdata-fval  = iv_fvalue.
    APPEND s_bdcdata TO t_bdcdata.

  ENDMETHOD.


  METHOD fill_okcode.

    CLEAR s_bdcdata.
    s_bdcdata-fnam     = 'BDC_OKCODE'.
    s_bdcdata-fval     =  iv_okcode.
    APPEND s_bdcdata TO t_bdcdata.

  ENDMETHOD.


  METHOD get_data.

    CHECK t_alv IS NOT INITIAL.

    SELECT matnr maktx
      FROM makt
      INTO TABLE t_makt
      FOR ALL ENTRIES IN t_alv
      WHERE spras = 'P'
        AND matnr = t_alv-matnr.

    SELECT matnr
           bwkey
           vprsv
           verpr
           salk3
           lbkum
      FROM mbew
      INTO TABLE t_mbew
      FOR ALL ENTRIES IN t_alv
      WHERE matnr = t_alv-matnr
        AND bwkey = t_alv-werks.

    SELECT kalnr matnr bwkey
      FROM ckmlhd
      INTO TABLE t_ckmlhd
      FOR ALL ENTRIES IN t_alv
      WHERE matnr = t_alv-matnr
        AND bwkey = t_alv-werks.

  ENDMETHOD.


  METHOD get_data_zmmt0178.

    DATA(lv_tipo) = COND #( WHEN rb_mr21 IS NOT INITIAL
                             THEN 'MR21'
                            WHEN rb_mr22 IS NOT INITIAL
                             THEN 'MR22' ).

    SELECT bukrs, branch
      FROM j_1bbranch
      INTO TABLE @DATA(lt_1bbranch)
      WHERE bukrs  = @v_bukrs.

    CHECK lt_1bbranch IS NOT INITIAL.

    SELECT werks, j_1bbranch, land1
      FROM t001w
      INTO TABLE @t_t001w
      FOR ALL ENTRIES IN @lt_1bbranch
      WHERE j_1bbranch = @lt_1bbranch-branch.

    CHECK t_t001w IS NOT INITIAL.

    SELECT *
      FROM zmmt0178
      INTO TABLE @DATA(lt_zmmt0178)
      FOR ALL ENTRIES IN @t_t001w
      WHERE tipo            = @lv_tipo
        AND data_vencimento = @v_data
        AND werks           = @t_t001w-werks.

    LOOP AT lt_zmmt0178 INTO DATA(ls_zmmt0178).

      APPEND VALUE #( status         = COND #( WHEN ls_zmmt0178-doc_mr22 IS NOT INITIAL
                                               THEN '@08@' )
                      data_vencimento  = ls_zmmt0178-data_vencimento
                      tipo             = ls_zmmt0178-tipo
                      werks            = ls_zmmt0178-werks
                      matnr            = ls_zmmt0178-matnr
                      maktx            = ls_zmmt0178-maktx
                      vprsv            = ls_zmmt0178-vprsv
                      vlr_brl          = ls_zmmt0178-vlr_brl
                      vlr_usd          = ls_zmmt0178-vlr_usd
                      stprs            = ls_zmmt0178-stprs
                      aval_atual_brl   = ls_zmmt0178-aval_atual_brl
                      aval_atual_usd   = ls_zmmt0178-aval_atual_usd
                      aval_atual_stprs = ls_zmmt0178-aval_atual_stprs
                      vlr_brl_old      = ls_zmmt0178-vlr_brl_old
                      vlr_brl_new      = ls_zmmt0178-vlr_brl_new
                      vlr_usd_old      = ls_zmmt0178-vlr_usd_old
                      vlr_usd_new      = ls_zmmt0178-vlr_usd_new
                      stprs_old        = ls_zmmt0178-stprs_old
                      stprs_new        = ls_zmmt0178-stprs_new
                      data_ref         = ls_zmmt0178-data_ref
                      doc_mr22         = ls_zmmt0178-doc_mr22
                      usuario          = ls_zmmt0178-usuario
                      data_modif       = ls_zmmt0178-data_modif
                      message          = COND #( WHEN ls_zmmt0178-doc_mr22 IS NOT INITIAL
                                                  THEN 'Documento já foi registrado.' ) ) TO t_alv.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_exchange_rate.

    DATA lv_input(10).

    DATA(lv_date_aux) = v_data.
*    ADD 1 TO lv_date_aux.

    WRITE lv_date_aux TO lv_input.

    DATA(lv_output) = CONV tcurr-gdatu( v_data ).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lv_input
      IMPORTING
        output = lv_output.

    SELECT SINGLE
           ukurs
      FROM tcurr
      INTO @DATA(lv_ukurs)
      WHERE kurst = 'B'
        AND fcurr = 'BRL'
        AND tcurr = 'USD'
        AND gdatu = @lv_output.

    return = lv_ukurs * -1.

  ENDMETHOD.


  METHOD get_message.

    DATA: ls_msg_aux1 TYPE bdcmsgcoll,
          lt_msg_aux1 TYPE tab_bdcmsgcoll.

    DATA(ls_t100)     = VALUE t100( ).
    DATA(ls_balm)     = VALUE balm( ).
    DATA(lv_wmessage) = CONV bapi_msg( '' ).

    IF t_msg_aux IS NOT INITIAL.

      LOOP AT t_msg_aux INTO DATA(ls_msg_aux).

        IF line_exists( t_msg_aux[ msgtyp = 'S'
                                    msgid = 'CKPRCH'
                                    msgnr = '019' ] ).

          IF NOT ( ls_msg_aux-msgv1 CA '0123456789' ) OR
                  ls_msg_aux-msgv1 = ''.

            return-message = CONV #( ls_msg_aux-msgv1 ).
            return-status  = '@0A@'."Erro'@09@'."Aviso

          ELSE.

            return-doc_mr22 = CONV #( ls_msg_aux-msgv1 ).
            return-status   = '@08@'."sucesso
            return-message  = 'Documento Registrado'.

          ENDIF.

        ELSE.

          ls_t100 = VALUE #( arbgb = ls_msg_aux-msgid
                             msgnr = ls_msg_aux-msgnr ).

          ls_balm = VALUE #( msgv1 = ls_msg_aux-msgv1
                             msgv2 = ls_msg_aux-msgv2
                             msgv3 = ls_msg_aux-msgv3
                             msgv4 = ls_msg_aux-msgv4 ).

          CALL FUNCTION 'MESSAGE_PREPARE'
            EXPORTING
              language = 'P'
              msg_id   = ls_t100-arbgb
              msg_no   = ls_t100-msgnr
              msg_var1 = ls_balm-msgv1
              msg_var2 = ls_balm-msgv2
              msg_var3 = ls_balm-msgv3
              msg_var4 = ls_balm-msgv4
            IMPORTING
              msg_text = lv_wmessage.

          return-message = CONV #( lv_wmessage ).
          return-status  = '@0A@'."Erro'@09@'."Aviso

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  method if_salv_gui_om_edit_strct_lstr~on_check_changed_data.

    data: ls_mbew      type ty_mbew,
          ls_ckmlhd    type ty_ckmlhd,
          t_ckmlcr_aux type table of ty_ckmlcr,
          ls_t001w     type ty_t001w,
          lt_mlit      type table of mlit.

    data(lv_status)           = conv ty_alv-status( '' ).
    data(lv_matnr)            = conv zmmt0178-matnr( '' ).
    data(lv_maktx)            = conv makt-maktx( '' ).
    data(lv_werks)            = conv zmmt0178-werks( '' ).
    data(lv_vlr_brl)          = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_usd)          = conv zmmt0178-vlr_usd( '' ).
    data(lv_stprs)            = conv zmmt0178-stprs( '' ).
    data(lv_stprs_old)        = conv zmmt0178-stprs( '' ).
    data(lv_stprs_new)        = conv zmmt0178-stprs( '' ).
    data(lv_aval_atual_brl)   = conv zmmt0178-vlr_brl( '' ).
    data(lv_total_preco_novo) = conv zmmt0178-vlr_brl( '' ).
    data(lv_aval_atual_usd)   = conv zmmt0178-vlr_usd( '' ).
    data(lv_aval_atual_stprs) = conv zmmt0178-stprs( '' ).
    data(lv_vlr_brl_old)      = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_brl_new)      = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_usd_old)      = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_usd_new)      = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_stprs_old)    = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_stprs_new)    = conv zmmt0178-vlr_brl( '' ).
    data(lv_screen)           = conv zmmt0178-vlr_brl( '' ).
    data(lv_total_standard)   = conv mlcrf-prdif( '' ).
    data(lv_lbkum)            = conv ckmlpp-lbkum( '' ).
    data(lv_poper)            = conv mlcrf-poper( '' ).
    data(lv_curin)            = conv t001a-curtp( '' ).
    data(lv_curfo)            = conv t001a-curtp( '' ).
    data(lv_curgr)            = conv t001a-curtp( '' ).
    data(lv_DATA_REF)         = conv zmmt0178-data_ref( '' ). "DATA_REF

    o_ui_data_modify->get_ui_changes( importing t_modified_cells = data(lt_modified) ).

    loop at lt_modified assigning field-symbol(<data>)
                  group by ( value = <data>-row_id )
                             into data(group).

      clear: lv_matnr,
             lv_werks,
             lv_maktx,
             ls_mbew,
             lv_aval_atual_brl,
             lv_aval_atual_usd,
             lv_aval_atual_stprs,
             lv_vlr_brl,
             lv_vlr_usd,
             lv_stprs,
             lv_lbkum,
             lv_screen,
             lv_data_ref.

      o_ui_data_modify->get_cell_value( exporting row_id     = group-value
                                                  fieldname  = 'STATUS'
                                        importing cell_value = lv_status ).

      o_ui_data_modify->get_cell_value( exporting row_id     = group-value
                                                  fieldname  = 'MATNR'
                                        importing cell_value = lv_matnr ).

      o_ui_data_modify->get_cell_value( exporting row_id     = group-value
                                                  fieldname  = 'WERKS'
                                        importing cell_value = lv_werks ).

      o_ui_data_modify->get_cell_value( exporting row_id     = group-value
                                                  fieldname  = 'VLR_BRL'
                                        importing cell_value = lv_vlr_brl ).

      o_ui_data_modify->get_cell_value( exporting row_id     = group-value
                                                  fieldname  = 'VLR_USD'
                                        importing cell_value = lv_vlr_usd ).

      o_ui_data_modify->get_cell_value( exporting row_id     = group-value
                                                  fieldname  = 'STPRS'
                                        importing cell_value = lv_stprs ).

      o_ui_data_modify->get_cell_value( exporting row_id     = group-value
                                                  fieldname  = 'DATA_REF'
                                        importing cell_value = lv_data_ref ).

      select single  finsc_ld_cmp~curtph finsc_ld_cmp~curtpk finsc_ld_cmp~curtpo
        into ( lv_curin, lv_curfo, lv_curgr )
        from t001w
        inner join j_1bbranch on j_1bbranch~branch = t001w~j_1bbranch
        inner join finsc_ld_cmp  on  finsc_ld_cmp~bukrs  =  j_1bbranch~bukrs
                                 and finsc_ld_cmp~rldnr  = '0L'
        where werks = lv_werks.

      select single maktx
        from makt
        into @lv_maktx
        where spras = 'P'
          and matnr = @lv_matnr.

      select single matnr,
                    bwkey,
                    vprsv,
                    verpr,
                    salk3,
                    lbkum
        from mbew
        into @ls_mbew
        where matnr = @lv_matnr
          and bwkey = @lv_werks.

      select single kalnr, matnr, bwkey
        from ckmlhd
        into @ls_ckmlhd
        where matnr = @lv_matnr
          and bwkey = @lv_werks.

      select stprs,
             kalnr,
             pvprs,
             curtp,
             salk3,
             bdatj,
             poper
        from ckmlcr
        into table @t_ckmlcr
        where kalnr = @ls_ckmlhd-kalnr
*          AND bdatj = @v_data
*          AND poper = @v_data+4(2)
          and curtp in ( '50' , '40' , '30' , '10' ).

      t_ckmlcr_aux = t_ckmlcr.

      sort t_ckmlcr_aux by bdatj descending poper descending.

      data(ls_data_ckmlcr) = value #( t_ckmlcr_aux[ 1 ] optional ).

      delete t_ckmlcr where poper < ls_data_ckmlcr-poper.
      delete t_ckmlcr where bdatj < ls_data_ckmlcr-bdatj.

      select single lbkum
        from ckmlpp
        into @lv_lbkum
        where kalnr = @ls_ckmlhd-kalnr
        and   bdatj = @ls_data_ckmlcr-bdatj
        and   poper = @ls_data_ckmlcr-poper.
*        AND   bdatj = @v_data(4)
*        AND   poper = @v_data+4(2).

      select single werks,
                    j_1bbranch,
                    land1
        from t001w
        into @ls_t001w
        where werks = @lv_werks.

      o_ui_data_modify->modify_cell_value( row_id     = group-value
                                           fieldname  = 'MAKTX'
                                           cell_value = lv_maktx ).

      o_ui_data_modify->modify_cell_value( row_id     = group-value
                                           fieldname  = 'VPRSV'
                                           cell_value = ls_mbew-vprsv ).

      if lv_data_ref is initial.
        o_ui_data_modify->modify_cell_value( row_id     = group-value
                                             fieldname  = 'DATA_REF'
                                             cell_value = v_data ).
      else.
        v_data = lv_data_ref.
      endif.

      lv_status = '@09@'."Aviso

      o_ui_data_modify->modify_cell_value( row_id     = group-value
                                           fieldname  = 'STATUS'
                                           cell_value = lv_status ).

      if ls_mbew-vprsv = 'S' and
         rb_mr21 is not initial.

        loop at t_ckmlcr into data(ls_ckmlcr).

          lv_aval_atual_brl = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                        then ls_ckmlcr-stprs
                                      when ls_ckmlcr-curtp = '30'
                                        then ls_ckmlcr-stprs
                                      else lv_aval_atual_brl ) .

          lv_aval_atual_usd = cond #( when ls_ckmlcr-curtp = lv_curfo
                                        then ls_ckmlcr-stprs
                                      else lv_aval_atual_usd ) .

          lv_aval_atual_stprs = cond #( when ls_ckmlcr-curtp = lv_curgr
                                          then ls_ckmlcr-stprs
                                        when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                          then ls_ckmlcr-stprs
                                        when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                          then ls_ckmlcr-stprs
                                        else lv_aval_atual_stprs  ) .

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'AVAL_ATUAL_BRL'
                                               cell_value = lv_aval_atual_brl ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'AVAL_ATUAL_USD'
                                               cell_value = lv_aval_atual_usd ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'AVAL_ATUAL_STPRS'
                                               cell_value = lv_aval_atual_stprs ).

        endloop.

      elseif ls_mbew-vprsv = 'V' and
         rb_mr21 is not initial.

        loop at t_ckmlcr into ls_ckmlcr.

          lv_aval_atual_brl = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                       then ls_ckmlcr-pvprs
                                      when ls_ckmlcr-curtp = '30'
                                        then ls_ckmlcr-pvprs
                                      else lv_aval_atual_brl ) .

          lv_aval_atual_usd = cond #( when ls_ckmlcr-curtp = lv_curfo
                                        then ls_ckmlcr-pvprs
                                      else lv_aval_atual_usd ) .

          lv_aval_atual_stprs = cond #( when ls_ckmlcr-curtp = lv_curgr
                                          then ls_ckmlcr-pvprs
                                        when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                          then ls_ckmlcr-pvprs
                                        when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                          then ls_ckmlcr-pvprs
                                        else lv_aval_atual_stprs ) .

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'AVAL_ATUAL_BRL'
                                               cell_value = lv_aval_atual_brl ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'AVAL_ATUAL_USD'
                                               cell_value = lv_aval_atual_usd ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'AVAL_ATUAL_STPRS'
                                               cell_value = lv_aval_atual_stprs ).

        endloop.

      elseif ls_mbew-vprsv = 'S' and
         rb_mr22 is not initial.

        loop at t_ckmlcr into ls_ckmlcr.
          lv_poper = |{ v_data+4(2) alpha = in }|.
          clear lv_total_standard.
          select sum( b~prdif )
              into lv_total_standard
              from mlit as a
              inner join mlcrf as b on b~belnr = a~belnr
               and   b~posnr = a~posnr
               and   b~poper = lv_poper
               and   b~curtp = ls_ckmlcr-curtp
               where a~kalnr = ls_ckmlhd-kalnr
               and   a~matnr = lv_matnr
               and   a~kjahr = v_data(4).

*            lv_screen = COND #( WHEN lv_vlr_brl IS NOT INITIAL
*                                 THEN lv_vlr_brl
*                                WHEN lv_vlr_usd IS NOT INITIAL
*                                  THEN lv_vlr_usd
*                                WHEN lv_stprs   IS NOT INITIAL
*                                  THEN lv_stprs ).
          lv_screen = cond #( when ls_ckmlcr-curtp eq lv_curin
                            then lv_vlr_brl
                           when ls_ckmlcr-curtp eq lv_curfo
                             then lv_vlr_usd
                           when ls_ckmlcr-curtp eq lv_curgr
                             then lv_stprs ).

          if lv_lbkum is not initial.
            lv_total_preco_novo = conv #( ( ls_ckmlcr-salk3 +  lv_screen + lv_total_standard ) / lv_lbkum ).
          endif.

          lv_vlr_brl_new = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                    then lv_total_preco_novo
                                   when ls_ckmlcr-curtp = '30'
                                    then lv_total_preco_novo
                                   else lv_vlr_brl_new ) .

          lv_vlr_brl_old = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                     then ls_ckmlcr-stprs
                                   when ls_ckmlcr-curtp = '30'
                                     then ls_ckmlcr-stprs
                                   else lv_vlr_brl_old ) .

          lv_vlr_usd_new = cond #( when ls_ckmlcr-curtp = lv_curfo and ls_t001w-land1 = 'BR'
                                     then lv_total_preco_novo
                                   else lv_vlr_usd_new ) .

          lv_vlr_usd_old = cond #( when ls_ckmlcr-curtp = lv_curfo
                                     then ls_ckmlcr-stprs
                                   else lv_vlr_usd_old ) .

          lv_stprs_old = cond #( when ls_ckmlcr-curtp = lv_curgr
                                   then ls_ckmlcr-stprs
                                 when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                   then ls_ckmlcr-stprs
                                 when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                  then ls_ckmlcr-stprs
                                 else lv_stprs_old ) .

          lv_stprs_new = cond #( when ls_ckmlcr-curtp = lv_curgr
                                   then lv_total_preco_novo
                                 when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                  then lv_total_preco_novo
                                 when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                  then lv_total_preco_novo
                                 else lv_stprs_new ) .

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'VLR_BRL_NEW'
                                               cell_value = lv_vlr_brl_new ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'VLR_BRL_OLD'
                                               cell_value = lv_vlr_brl_old ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'VLR_USD_NEW'
                                               cell_value = lv_vlr_usd_new ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'VLR_USD_OLD'
                                               cell_value = lv_vlr_usd_old ).


          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'STPRS_NEW'
                                               cell_value = lv_stprs_new ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'STPRS_OLD'
                                               cell_value = lv_stprs_old ).

        endloop.

      elseif ls_mbew-vprsv = 'V' and
           rb_mr22 is not initial.

        loop at t_ckmlcr into ls_ckmlcr.
          lv_poper = |{ v_data+4(2) alpha = in }|.
          clear lv_total_standard.
          select sum( b~prdif )
              into lv_total_standard
              from mlit as a
              inner join mlcrf as b on b~belnr = a~belnr
               and   b~posnr = a~posnr
               and   b~poper = lv_poper
               and   b~curtp = ls_ckmlcr-curtp
               where a~kalnr = ls_ckmlhd-kalnr
               and   a~matnr = lv_matnr
               and   a~kjahr = v_data(4).
          clear lv_screen.

*          lv_screen = COND #( WHEN lv_vlr_brl IS NOT INITIAL
*                               THEN lv_vlr_brl
*                              WHEN lv_vlr_usd IS NOT INITIAL
*                                THEN lv_vlr_usd
*                              WHEN lv_stprs   IS NOT INITIAL
*                                THEN lv_stprs
*                              ELSE lv_screen ).
          lv_screen = cond #( when ls_ckmlcr-curtp eq lv_curin
                              then lv_vlr_brl
                             when ls_ckmlcr-curtp eq lv_curfo
                               then lv_vlr_usd
                             when ls_ckmlcr-curtp eq lv_curgr
                               then lv_stprs
                             else lv_screen ).
          if lv_lbkum is not initial.
            lv_total_preco_novo = conv #( ( ls_ckmlcr-salk3 +  lv_screen + lv_total_standard ) / lv_lbkum ).
          endif.

          lv_vlr_brl_old = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                     then ls_ckmlcr-pvprs
                                   when ls_ckmlcr-curtp = '30'
                                     then ls_ckmlcr-pvprs
                                   else lv_vlr_brl_old ) .

          lv_vlr_brl_new = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                     then lv_total_preco_novo
                                   when ls_ckmlcr-curtp = '30'
                                     then lv_total_preco_novo
                                   else lv_vlr_brl_new ) .

          lv_vlr_usd_new = cond #( when ls_ckmlcr-curtp = lv_curfo and ls_t001w-land1 = 'BR'
                                     then lv_total_preco_novo
                                   else  lv_vlr_usd_new ) .

          lv_vlr_usd_old = cond #( when ls_ckmlcr-curtp =  lv_curfo
                                     then ls_ckmlcr-pvprs
                                   else lv_vlr_usd_old ) .

          lv_stprs_old = cond #( when ls_ckmlcr-curtp = lv_curgr
                                   then ls_ckmlcr-pvprs
                                 when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                   then ls_ckmlcr-pvprs
                                 when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                   then ls_ckmlcr-pvprs
                                 else lv_stprs_old ) .

          lv_stprs_new = cond #( when ls_ckmlcr-curtp = lv_curgr
                                   then lv_total_preco_novo
                                 when ls_ckmlcr-curtp = lv_curin  and ls_t001w-land1 = 'AR'
                                   then lv_total_preco_novo
                                 when ls_ckmlcr-curtp = lv_curin  and ls_t001w-land1 = 'PY'
                                   then lv_total_preco_novo
                                 else lv_stprs_new ) .

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'VLR_BRL_NEW'
                                               cell_value = lv_vlr_brl_new ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'VLR_BRL_OLD'
                                               cell_value = lv_vlr_brl_old ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'VLR_USD_NEW'
                                               cell_value = lv_vlr_usd_new ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'VLR_USD_OLD'
                                               cell_value = lv_vlr_usd_old ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'STPRS_NEW'
                                               cell_value = lv_stprs_new ).

          o_ui_data_modify->modify_cell_value( row_id     = group-value
                                               fieldname  = 'STPRS_OLD'
                                               cell_value = lv_stprs_old ).

        endloop.

      endif.

    endloop.

  endmethod.


  METHOD modify_zmmt0178.

    TYPES tt_zmmt0178 TYPE TABLE OF zmmt0178 WITH EMPTY KEY.

    CHECK t_alv IS NOT INITIAL.

    DATA(lt_alv_aux) = t_alv.

    DELETE lt_alv_aux WHERE modify = abap_true.

    DATA(lt_zmmt0178) = CORRESPONDING tt_zmmt0178( lt_alv_aux ).

    DELETE lt_zmmt0178 WHERE doc_mr22 IS INITIAL.

    MODIFY zmmt0178 FROM TABLE lt_zmmt0178.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  method on_user_command.

    case e_salv_function.
**********************************************************************147975 Ajuste na conversão ZMM0224 - PSA
      when 'LIMPAR'.
        ""BREAK-POINT.
        t_rows = o_alv->get_selections( )->get_selected_rows( ).
        if t_rows is initial.
          data(lv_msg001) = | Selecione ao menos uma linha! |.
          message lv_msg001 type 'I'.
        else.
          loop at t_rows into data(ls_rows_clear).
            read table t_alv assigning field-symbol(<fs_alv_clear>) index  ls_rows_clear.
            if sy-subrc = 0 and <fs_alv_clear>-status <> '@08@'.
              clear: <fs_alv_clear>-vlr_brl,<fs_alv_clear>-vlr_usd,<fs_alv_clear>-stprs.
            endif.
          endloop.
        endif.
**********************************************************************

      when 'GERAR'.

        t_rows = o_alv->get_selections( )->get_selected_rows( ).

        if t_rows is initial.
          data(lv_msg) = | Selecione ao menos uma linha! |.
          message lv_msg type 'I'.

        endif.

        show_function_info( ).

      when 'INSERIR'.

        append value #( status   = '@09@'  "Aviso
                        data_ref = v_data ) to t_alv.

      when 'REMOVER'.

        t_rows = o_alv->get_selections( )->get_selected_rows( ).

        if t_rows is initial.
          lv_msg = | Selecione ao menos uma linha! |.
          message lv_msg type 'I'.

        else.

          loop at t_rows into data(ls_rows).

            data(ls_alv_index) = value #( t_alv[ ls_rows ] optional ).

            if ls_alv_index-doc_mr22 is not initial.
              continue.

            elseif ls_alv_index is not initial.

              delete t_alv index ls_rows.

            endif.

          endloop.

        endif.

      when 'CONVERTER'.
        t_rows = o_alv->get_selections( )->get_selected_rows( ).

        if t_rows is initial.
          lv_msg = | Selecione ao menos uma linha! |.
          message lv_msg type 'I'.

        else.
          ""BREAK-POINT.
          loop at t_rows into ls_rows.

            read table t_alv assigning field-symbol(<fs_alv_index>) index  ls_rows.
*            ls_alv_index = VALUE #( t_alv[ ls_rows ] OPTIONAL ).
            check <fs_alv_index> is assigned.
**********************************************************************147975 Ajuste na conversão ZMM0224 - PSA
            select single land1 from t001 into @data(lv_land1) where bukrs  = @v_bukrs.

            data(_erro) = abap_false.

            if rb_mr21  = abap_true or rb_mr22  = abap_true .
              if <fs_alv_index>-doc_mr22 is not initial.
                continue.
              elseif <fs_alv_index>-stprs  is initial and <fs_alv_index>-vlr_brl is initial and <fs_alv_index>-vlr_usd is initial .
                <fs_alv_index>-status = '@0A@'."Erro
                <fs_alv_index>-message = | Somente permitida a conversão para registros com um valor preenchido. |.
                _erro = abap_true.
                continue.
              elseif <fs_alv_index>-stprs   is not initial or <fs_alv_index>-vlr_brl is not initial or <fs_alv_index>-vlr_usd is not initial .

                if lv_land1 <> 'BR'.  "147975 - PSA Ajuste na conversão ZMM0224
                  if <fs_alv_index>-stprs  is initial.
                    <fs_alv_index>-status = '@0A@'."Erro
                    <fs_alv_index>-message = | Somente permitida a conversão quando a MOEDA EMPRESA estiver preenchida! |.
                    _erro = abap_true.
                    continue.
                  endif.
                elseif lv_land1 = 'BR'. "147975 - PSA Ajuste na conversão ZMM0224
                  if <fs_alv_index>-vlr_brl  is initial.
                    <fs_alv_index>-status = '@0A@'."Erro
                    <fs_alv_index>-message = | Somente permitida a conversão quando a MOEDA EMPRESA estiver preenchida! |.
                    _erro = abap_true.
                    continue.
                  endif.
                endif.

                check _erro is initial.
                ""BREAK-POINT.
                data(ls_alv_conv_values) = convert_values( <fs_alv_index> ).

                if ls_alv_conv_values-message is not initial.
                  <fs_alv_index>-message = ls_alv_conv_values-message.
                  <fs_alv_index>-status = ls_alv_conv_values-status.
                  continue.
                else.
                  <fs_alv_index>-vlr_brl = ls_alv_conv_values-vlr_brl.
                  <fs_alv_index>-vlr_usd = ls_alv_conv_values-vlr_usd.
                  <fs_alv_index>-stprs   = ls_alv_conv_values-stprs.

                <fs_alv_index>           = change_value_convert( <fs_alv_index> ).
                endif.
              endif.
            endif.

********************************************************************** Original

*            IF <fs_alv_index>-doc_mr22 IS NOT INITIAL.
*              CONTINUE.
*
*            ELSEIF <fs_alv_index>-stprs   IS INITIAL AND
*                   <fs_alv_index>-vlr_brl IS INITIAL AND
*                   <fs_alv_index>-vlr_usd IS INITIAL .
*
*              <fs_alv_index>-status = '@0A@'."Erro
*              <fs_alv_index>-message = | Somente permitida a conversão para registros com um valor preenchido. |.
*
*              CONTINUE.
*
*            ELSEIF <fs_alv_index>-vlr_brl  IS NOT INITIAL AND
*                  ( <fs_alv_index>-vlr_usd IS NOT INITIAL OR
*                    <fs_alv_index>-stprs   IS NOT INITIAL ).
*
*              <fs_alv_index>-status = '@0A@'."Erro
*              <fs_alv_index>-message = | Somente permitida a conversão para registros com um valor preenchido. |.
*
*              CONTINUE.
*            ELSEIF <fs_alv_index>-vlr_usd  IS NOT INITIAL AND
*                  ( <fs_alv_index>-vlr_brl IS NOT INITIAL OR
*                    <fs_alv_index>-stprs   IS NOT INITIAL ).
*
*              <fs_alv_index>-status = '@0A@'."Erro
*              <fs_alv_index>-message = | Somente permitida a conversão para registros com um valor preenchido. |.
*
*              CONTINUE.
*            ELSEIF <fs_alv_index>-stprs    IS NOT INITIAL AND
*                  ( <fs_alv_index>-vlr_brl IS NOT INITIAL OR
*                    <fs_alv_index>-vlr_usd IS NOT INITIAL ).
*
*              <fs_alv_index>-status = '@0A@'."Erro
*              <fs_alv_index>-message = | Somente permitida a conversão para registros com um valor preenchido. |.
*
*              CONTINUE.
**            ELSEIF <fs_alv_index>-vprsv = 'V' AND
*            ELSEIF <fs_alv_index>-stprs   IS NOT INITIAL OR
*                   <fs_alv_index>-vlr_brl IS NOT INITIAL OR
*                   <fs_alv_index>-vlr_usd IS NOT INITIAL .
*
*              DATA(ls_alv_conv_values) = convert_values( <fs_alv_index> ).
*
*              IF ls_alv_conv_values-message IS NOT INITIAL.
*
*                <fs_alv_index>-message = ls_alv_conv_values-message.
*                <fs_alv_index>-status = ls_alv_conv_values-status.
*
*                CONTINUE.
*              ELSE.
*
*                <fs_alv_index>-vlr_brl = ls_alv_conv_values-vlr_brl.
*                <fs_alv_index>-vlr_usd = ls_alv_conv_values-vlr_usd.
*                <fs_alv_index>-stprs   = ls_alv_conv_values-stprs.
*              ENDIF.
*
*            ENDIF.

**********************************************************************

          endloop.

        endif.

    endcase.

    o_alv->refresh( ).

  endmethod.


  METHOD process_data.

    SELECT werks,
           j_1bbranch,
           land1
      FROM t001w
      INTO TABLE @DATA(lt_t001w)
      FOR ALL ENTRIES IN @t_alv
      WHERE werks  = @t_alv-werks.

    IF lt_t001w IS NOT INITIAL.

      SELECT bukrs, branch
        FROM j_1bbranch
        INTO TABLE @DATA(lt_1bbranch)
        FOR ALL ENTRIES IN @lt_t001w
        WHERE bukrs  = @v_bukrs
          AND branch = @lt_t001w-j_1bbranch.

    ENDIF.

    LOOP AT t_rows INTO DATA(ls_rows).

      CLEAR: t_bdcdata,
             t_msg_aux.

      READ TABLE t_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX ls_rows.

      CHECK <fs_alv> IS ASSIGNED.

      " ...validando Centro
      DATA(ls_t001w) = VALUE #( lt_t001w[ werks = <fs_alv>-werks ] OPTIONAL ).

      DATA(ls_1bbranch) = VALUE #( lt_1bbranch[ branch = ls_t001w-j_1bbranch ] OPTIONAL ).

      IF ls_t001w IS INITIAL.

        <fs_alv>-status = '@0A@'."Erro
        <fs_alv>-message = | 'Este centro nao existe.' |.

        CONTINUE.

      ELSEIF ls_1bbranch IS INITIAL.

        <fs_alv>-message = 'Centro não pertence a empresa.'.
        <fs_alv>-status = '@0A@'."Erro

        CONTINUE.

      ELSEIF <fs_alv>-doc_mr22 IS NOT INITIAL.

        <fs_alv>-status  = '@08@'."sucesso
        <fs_alv>-modify  = 'X'.
        <fs_alv>-message = 'Não é permitido alterar dados de documentos já registrado.'.

        CONTINUE.
      ENDIF.

      <fs_alv>-tipo = COND #( WHEN rb_mr21 IS NOT INITIAL
                               THEN 'MR21'
                              WHEN rb_mr22 IS NOT INITIAL
                               THEN 'MR22' ).

      DATA(ls_makt) = VALUE #( t_makt[ matnr = <fs_alv>-matnr ] OPTIONAL ).

      IF ls_makt IS NOT INITIAL.

        <fs_alv>-maktx = ls_makt-maktx.

      ENDIF.

      DATA(ls_mbew) = VALUE #( t_mbew[ matnr = <fs_alv>-matnr
                                       bwkey = <fs_alv>-werks ] OPTIONAL ).

      IF ls_mbew IS NOT INITIAL.

        <fs_alv>-vprsv = ls_mbew-vprsv.

      ENDIF.

      IF <fs_alv>-stprs IS INITIAL.

        DATA(ls_ckmlhd) = VALUE #( t_ckmlhd[ matnr = <fs_alv>-matnr
                                             bwkey = <fs_alv>-werks ] OPTIONAL ).

        IF ls_ckmlhd      IS NOT INITIAL AND
           <fs_alv>-stprs IS INITIAL.
          DATA(ls_ckmlcr) = VALUE #( t_ckmlcr[ kalnr = ls_ckmlhd-kalnr ] OPTIONAL ).

          <fs_alv>-stprs = COND #( WHEN ls_ckmlcr-curtp = '50'
                                          THEN ls_ckmlcr-stprs
                                        WHEN ls_ckmlcr-curtp = '10' AND ls_t001w-land1 = 'AR'
                                          THEN ls_ckmlcr-stprs
                                        WHEN ls_ckmlcr-curtp = '10' AND ls_t001w-land1 = 'PY'
                                          THEN ls_ckmlcr-stprs
                                        ELSE <fs_alv>-stprs ) . " Preço UFIR

        ENDIF.

      ENDIF.

      IF rb_mr21 IS NOT INITIAL
         AND <fs_alv>-vlr_brl < '0.00' .

        <fs_alv>-message = | 'Preencher campo -Preço BRL, com valores positivos.' |.
        <fs_alv>-status = '@0A@'."Erro."'@09@'."Aviso

        CONTINUE.

      ELSEIF rb_mr21          IS NOT INITIAL
         AND ( <fs_alv>-vlr_brl IS NOT INITIAL
          OR <fs_alv>-vlr_usd   IS NOT INITIAL
          OR <fs_alv>-stprs     IS NOT INITIAL ).

        batch_input_mr21( <fs_alv> ).

      ELSEIF rb_mr22          IS NOT INITIAL
         AND ( <fs_alv>-vlr_brl IS NOT INITIAL
          OR <fs_alv>-vlr_usd   IS NOT INITIAL
          OR <fs_alv>-stprs     IS NOT INITIAL ).

        batch_input_mr22( <fs_alv> ).

      ENDIF.

      call_transaction_mr21_mr22( ).

      DATA(ls_return_message) = get_message( ).

      IF ls_return_message-doc_mr22 IS NOT INITIAL.

        <fs_alv>-data_vencimento = v_data.
        <fs_alv>-status          = ls_return_message-status.
        <fs_alv>-doc_mr22        = ls_return_message-doc_mr22.
        <fs_alv>-message         = ls_return_message-message.
        <fs_alv>-timestamp       = CONV #( |{ sy-datum }{ sy-uzeit }| ).
        <fs_alv>-usuario         = sy-uname.
        <fs_alv>-data_modif      = sy-datum.

      ELSE.
        <fs_alv>-data_vencimento = v_data.
        <fs_alv>-status          = ls_return_message-status.
        <fs_alv>-message         = ls_return_message-message.

      ENDIF.

    ENDLOOP.

    o_alv->display( ).
    modify_zmmt0178( ).

  ENDMETHOD.


  METHOD set_columns_technical.

    DATA: lo_column TYPE REF TO cl_salv_column_table.

    lo_column ?= ir_columns->get_column( 'STATUS' ).
    lo_column->set_short_text( 'Status' ).
    lo_column->set_medium_text( 'Status' ).
    lo_column->set_long_text( 'Status' ).

    lo_column ?= ir_columns->get_column( 'MAKTX' ).
    lo_column->set_short_text( 'Descr.' ).
    lo_column->set_medium_text( 'Descrição' ).
    lo_column->set_long_text( 'Descrição' ).

    lo_column ?= ir_columns->get_column( 'VPRSV' ).
    lo_column->set_short_text( 'Ctr.Pr.' ).
    lo_column->set_medium_text( 'Ctr.Preço' ).
    lo_column->set_long_text( 'Controle Preço' ).

    lo_column ?= ir_columns->get_column( 'DATA_REF' ).
    lo_column->set_short_text( 'Dt.Ref' ).
    lo_column->set_medium_text( 'Dt. Ref' ).
    lo_column->set_long_text( 'Data Referência' ).

    lo_column ?= ir_columns->get_column( 'MESSAGE' ).
    lo_column->set_short_text( 'Mensagem' ).
    lo_column->set_medium_text( 'Mensagem' ).
    lo_column->set_long_text( 'Mensagem' ).

    lo_column ?= ir_columns->get_column( 'USUARIO' ).
    lo_column->set_short_text( 'Usuário' ).
    lo_column->set_medium_text( 'Usuário' ).
    lo_column->set_long_text( 'Usuário' ).

    lo_column ?= ir_columns->get_column( 'DATA_MODIF' ).
    lo_column->set_short_text( 'Dt Mod' ).
    lo_column->set_medium_text( 'Dt Modificação' ).
    lo_column->set_long_text( 'Data Modificação' ).

    ir_columns->get_column( 'DATA_VENCIMENTO' )->set_visible( abap_false ).
    ir_columns->get_column( 'TIPO' )->set_visible( abap_false ).
    ir_columns->get_column( 'MODIFY' )->set_visible( abap_false ).

    DATA: p_ddic TYPE salv_s_ddic_reference.

    p_ddic-table = 'ZMMT0178'.
    p_ddic-field = 'WERKS'.

    lo_column ?= ir_columns->get_column( p_ddic-field ).
    lo_column->set_ddic_reference( p_ddic ).
    lo_column->set_f4( if_salv_c_bool_sap=>true ).

    SELECT SINGLE land1
      FROM t001
      INTO @DATA(lv_land1)
      WHERE bukrs  = @v_bukrs.

    IF lv_land1 = 'BR' AND
      rb_mr21 IS NOT INITIAL.

      lo_column ?= ir_columns->get_column( 'VLR_BRL' ).
      lo_column->set_short_text( 'Moeda Emp' ).
      lo_column->set_medium_text( 'Moeda da Emp' ).
      lo_column->set_long_text( 'Moeda da Empresa' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD' ).
      lo_column->set_short_text( 'Moeda Fort' ).
      lo_column->set_medium_text( 'Moeda Forte' ).
      lo_column->set_long_text( 'Moeda Forte' ).

      lo_column ?= ir_columns->get_column( 'STPRS' ).
      lo_column->set_short_text( 'Moeda Índ' ).
      lo_column->set_medium_text( 'Moeda Índice' ).
      lo_column->set_long_text( 'Moeda Índice' ).

    ELSEIF lv_land1 = 'AR' AND
      rb_mr21 IS NOT INITIAL.

      lo_column ?= ir_columns->get_column( 'STPRS' ).
      lo_column->set_short_text( 'Moeda Emp' ).
      lo_column->set_medium_text( 'Moeda da Emp' ).
      lo_column->set_long_text( 'Moeda da Empresa' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL' ).
      lo_column->set_short_text( 'Md gr Emp' ).
      lo_column->set_medium_text( 'Moeda grp Emp' ).
      lo_column->set_long_text( 'Moeda grupo Empresas' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD' ).
      lo_column->set_short_text( 'Moeda Fort' ).
      lo_column->set_medium_text( 'Moeda Forte' ).
      lo_column->set_long_text( 'Moeda Forte' ).

      lo_column ?= ir_columns->get_column( 'AVAL_ATUAL_STPRS' ).
      lo_column->set_short_text( 'Pr Av ARS' ).
      lo_column->set_medium_text( 'Pr Aval.ARS' ).
      lo_column->set_long_text( 'Pr Avaliação Atual ARS ' ).

    ELSEIF lv_land1 = 'PY' AND
      rb_mr21 IS NOT INITIAL.

      lo_column ?= ir_columns->get_column( 'STPRS' ).
      lo_column->set_short_text( 'Moeda Emp' ).
      lo_column->set_medium_text( 'Moeda da Emp' ).
      lo_column->set_long_text( 'Moeda da Empresa' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL' ).
      lo_column->set_short_text( 'Md gr Emp' ).
      lo_column->set_medium_text( 'Moeda grp Emp' ).
      lo_column->set_long_text( 'Moeda grupo Empresas' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD' ).
      lo_column->set_short_text( 'Moeda Fort' ).
      lo_column->set_medium_text( 'Moeda Forte' ).
      lo_column->set_long_text( 'Moeda Forte' ).

      lo_column ?= ir_columns->get_column( 'AVAL_ATUAL_STPRS' ).
      lo_column->set_short_text( 'Pr Av PYG' ).
      lo_column->set_medium_text( 'Pr Aval.PYG' ).
      lo_column->set_long_text( 'Pr Avaliação Atual PYG ' ).

    ENDIF.

    ir_columns->get_column( 'TIMESTAMP' )->set_visible( abap_false ).

    IF rb_mr21 IS NOT INITIAL.

      lo_column ?= ir_columns->get_column( 'AVAL_ATUAL_BRL' ).
      lo_column->set_short_text( 'Pr Av BRL' ).
      lo_column->set_medium_text( 'Pr Aval. BRL' ).
      lo_column->set_long_text( 'Pr Avaliação Atual BRL ' ).

      lo_column ?= ir_columns->get_column( 'AVAL_ATUAL_USD' ).
      lo_column->set_short_text( 'Pr Av USD' ).
      lo_column->set_medium_text( 'Pr Aval. USD' ).
      lo_column->set_long_text( 'Pr Avaliação Atual USD' ).

      lo_column ?= ir_columns->get_column( 'DOC_MR22' ).
      lo_column->set_short_text( 'Doc MR21' ).
      lo_column->set_medium_text( 'Doc MR21' ).
      lo_column->set_long_text( 'Documento MR21' ).

      ir_columns->get_column( 'VLR_BRL_OLD' )->set_visible( abap_false ).
      ir_columns->get_column( 'VLR_BRL_NEW' )->set_visible( abap_false ).
      ir_columns->get_column( 'VLR_USD_OLD' )->set_visible( abap_false ).
      ir_columns->get_column( 'VLR_USD_NEW' )->set_visible( abap_false ).
      ir_columns->get_column( 'STPRS_OLD' )->set_visible( abap_false ).
      ir_columns->get_column( 'STPRS_NEW' )->set_visible( abap_false ).

    ELSEIF rb_mr22 IS NOT INITIAL.

      lo_column ?= ir_columns->get_column( 'DOC_MR22' ).
      lo_column->set_short_text( 'Doc MR22' ).
      lo_column->set_medium_text( 'Doc MR22' ).
      lo_column->set_long_text( 'Documento MR22' ).

      ir_columns->get_column( 'AVAL_ATUAL_BRL' )->set_visible( abap_false ).
      ir_columns->get_column( 'AVAL_ATUAL_USD' )->set_visible( abap_false ).
      ir_columns->get_column( 'AVAL_ATUAL_STPRS' )->set_visible( abap_false ).

    ENDIF.

    IF lv_land1 = 'BR' AND
      rb_mr22 IS NOT INITIAL.

      lo_column ?= ir_columns->get_column( 'VLR_BRL' ).
      lo_column->set_short_text( 'Moeda Emp' ).
      lo_column->set_medium_text( 'Moeda da Emp' ).
      lo_column->set_long_text( 'Moeda da Empresa' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD' ).
      lo_column->set_short_text( 'Moeda Fort' ).
      lo_column->set_medium_text( 'Moeda Forte' ).
      lo_column->set_long_text( 'Moeda Forte' ).

      lo_column ?= ir_columns->get_column( 'STPRS' ).
      lo_column->set_short_text( 'Moeda Índ' ).
      lo_column->set_medium_text( 'Moeda Índice' ).
      lo_column->set_long_text( 'Moeda Índice' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL_OLD' ).
      lo_column->set_short_text( 'Vlr BRL' ).
      lo_column->set_medium_text( 'Valor BRL' ).
      lo_column->set_long_text( 'Valor BRL' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL_NEW' ).
      lo_column->set_short_text( 'Vlr Nv BRL' ).
      lo_column->set_medium_text( 'Valor Nv BRL' ).
      lo_column->set_long_text( 'Valor Novo BRL' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD_OLD' ).
      lo_column->set_short_text( 'Vlr USD' ).
      lo_column->set_medium_text( 'Valor USD' ).
      lo_column->set_long_text( 'Valor USD' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD_NEW' ).
      lo_column->set_short_text( 'Vlr Nv USD' ).
      lo_column->set_medium_text( 'Valor Nv USD' ).
      lo_column->set_long_text( 'Valor Novo USD' ).

      lo_column ?= ir_columns->get_column( 'STPRS_OLD' ).
      lo_column->set_short_text( 'Vlr UFIR' ).
      lo_column->set_medium_text( 'Valor UFIR' ).
      lo_column->set_long_text( 'Valor UFIR' ).

      lo_column ?= ir_columns->get_column( 'STPRS_NEW' ).
      lo_column->set_short_text( 'Vl Nv UFIR' ).
      lo_column->set_medium_text( 'Valor Nv UFIR' ).
      lo_column->set_long_text( 'Valor Novo UFIR' ).

    ELSEIF lv_land1 = 'AR' AND
      rb_mr22 IS NOT INITIAL.

      lo_column ?= ir_columns->get_column( 'STPRS' ).
      lo_column->set_short_text( 'Moeda Emp' ).
      lo_column->set_medium_text( 'Moeda da Emp' ).
      lo_column->set_long_text( 'Moeda da Empresa' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL' ).
      lo_column->set_short_text( 'Md gr Emp' ).
      lo_column->set_medium_text( 'Moeda grp Emp' ).
      lo_column->set_long_text( 'Moeda grupo Empresas' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD' ).
      lo_column->set_short_text( 'Moeda Fort' ).
      lo_column->set_medium_text( 'Moeda Forte' ).
      lo_column->set_long_text( 'Moeda Forte' ).

      lo_column ?= ir_columns->get_column( 'STPRS_OLD' ).
      lo_column->set_short_text( 'Vlr ARS' ).
      lo_column->set_medium_text( 'Valor ARS' ).
      lo_column->set_long_text( 'Valor ARS' ).

      lo_column ?= ir_columns->get_column( 'STPRS_NEW' ).
      lo_column->set_short_text( 'Vlr Nv ARS' ).
      lo_column->set_medium_text( 'Valor Nv ARS' ).
      lo_column->set_long_text( 'Valor Novo ARS' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL_OLD' ).
      lo_column->set_short_text( 'Vlr BRL' ).
      lo_column->set_medium_text( 'Valor BRL' ).
      lo_column->set_long_text( 'Valor BRL' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL_NEW' ).
      lo_column->set_short_text( 'Vlr Nv BRL' ).
      lo_column->set_medium_text( 'Valor Nv BRL' ).
      lo_column->set_long_text( 'Valor Novo BRL' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD_OLD' ).
      lo_column->set_short_text( 'Vlr USD' ).
      lo_column->set_medium_text( 'Valor USD' ).
      lo_column->set_long_text( 'Valor USD' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD_NEW' ).
      lo_column->set_short_text( 'Vlr Nv USD' ).
      lo_column->set_medium_text( 'Valor Nv USD' ).
      lo_column->set_long_text( 'Valor Novo USD' ).

    ELSEIF lv_land1 = 'PY' AND
      rb_mr22 IS NOT INITIAL.

      lo_column ?= ir_columns->get_column( 'STPRS' ).
      lo_column->set_short_text( 'Moeda Emp' ).
      lo_column->set_medium_text( 'Moeda da Emp' ).
      lo_column->set_long_text( 'Moeda da Empresa' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL' ).
      lo_column->set_short_text( 'Md gr Emp' ).
      lo_column->set_medium_text( 'Moeda grp Emp' ).
      lo_column->set_long_text( 'Moeda grupo Empresas' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD' ).
      lo_column->set_short_text( 'Moeda Fort' ).
      lo_column->set_medium_text( 'Moeda Forte' ).
      lo_column->set_long_text( 'Moeda Forte' ).

      lo_column ?= ir_columns->get_column( 'STPRS_OLD' ).
      lo_column->set_short_text( 'Vlr PYG' ).
      lo_column->set_medium_text( 'Valor PYG' ).
      lo_column->set_long_text( 'Valor PYG' ).

      lo_column ?= ir_columns->get_column( 'STPRS_NEW' ).
      lo_column->set_short_text( 'Vlr Nv PYG' ).
      lo_column->set_medium_text( 'Valor Nv PYG' ).
      lo_column->set_long_text( 'Valor Novo PYG' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL_OLD' ).
      lo_column->set_short_text( 'Vlr BRL' ).
      lo_column->set_medium_text( 'Valor BRL' ).
      lo_column->set_long_text( 'Valor BRL' ).

      lo_column ?= ir_columns->get_column( 'VLR_BRL_NEW' ).
      lo_column->set_short_text( 'Vlr Nv BRL' ).
      lo_column->set_medium_text( 'Valor Nv BRL' ).
      lo_column->set_long_text( 'Valor Novo BRL' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD_OLD' ).
      lo_column->set_short_text( 'Vlr USD' ).
      lo_column->set_medium_text( 'Valor USD' ).
      lo_column->set_long_text( 'Valor USD' ).

      lo_column ?= ir_columns->get_column( 'VLR_USD_NEW' ).
      lo_column->set_short_text( 'Vlr Nv USD' ).
      lo_column->set_medium_text( 'Valor Nv USD' ).
      lo_column->set_long_text( 'Valor Novo USD' ).

    ENDIF.

  ENDMETHOD.


  METHOD set_enabled_cells.

    s_edit->set_attributes_for_columnname(
                EXPORTING
                  columnname = 'WERKS'
                  all_cells_input_enabled = iv_enabled ).

    s_edit->set_attributes_for_columnname(
                EXPORTING
                  columnname = 'MATNR'
                  all_cells_input_enabled = iv_enabled ).

    s_edit->set_attributes_for_columnname(
                EXPORTING
                  columnname = 'VLR_BRL'
                  all_cells_input_enabled = iv_enabled ).

    s_edit->set_attributes_for_columnname(
                 EXPORTING
                   columnname = 'VLR_USD'
                   all_cells_input_enabled = iv_enabled ).

    s_edit->set_attributes_for_columnname(
                 EXPORTING
                   columnname = 'STPRS'
                   all_cells_input_enabled = iv_enabled ).

    s_edit->set_attributes_for_columnname(
                 EXPORTING
                   columnname = 'DATA_REF'
                   all_cells_input_enabled = iv_enabled ).

  ENDMETHOD.


  METHOD show_function_info.

    set_enabled_cells( abap_true ).
    get_data( ).
    process_data( ).

    o_alv->refresh( ).

  ENDMETHOD.


  METHOD start_dynpro.

    CLEAR s_bdcdata.
    s_bdcdata-program  = iv_prog.
    s_bdcdata-dynpro   = iv_dynpro.
    s_bdcdata-dynbegin = 'X'.
    APPEND s_bdcdata TO t_bdcdata.

  ENDMETHOD.


  METHOD convert_values.

    DATA lv_input(10).

    DATA(lv_date_aux) = is_alv-data_ref.
*    lv_date_aux = is_alv-data_ref - 1.

    WRITE lv_date_aux TO lv_input.

    DATA(lv_output) = CONV tcurr-gdatu( v_data ).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lv_input
      IMPORTING
        output = lv_output.

    SELECT SINGLE
           ukurs
      FROM tcurr
      INTO @DATA(lv_ukurs)
      WHERE kurst = 'B'
        AND fcurr = 'USD'
        AND tcurr = 'BRL'
        AND gdatu = @lv_output.

*     lv_ukurs * -1.

    SELECT SINGLE
           ukurs
      FROM tcurr
      INTO @DATA(lv_ukurs_ufir)
      WHERE kurst = 'B'
        AND fcurr = 'UFIR'.

    ""BREAK-POINT.
    IF lv_ukurs IS NOT INITIAL.

      return-vlr_brl = COND #(
      WHEN is_alv-vlr_brl IS INITIAL AND is_alv-vlr_usd IS NOT INITIAL THEN is_alv-vlr_usd * lv_ukurs
      WHEN is_alv-vlr_brl IS INITIAL AND is_alv-stprs IS NOT INITIAL THEN is_alv-stprs * lv_ukurs_ufir
      ELSE is_alv-vlr_brl
                               ).
      return-vlr_usd = COND #(
      WHEN is_alv-vlr_usd IS INITIAL AND is_alv-vlr_brl IS NOT INITIAL THEN is_alv-vlr_brl / lv_ukurs
      WHEN is_alv-vlr_usd IS INITIAL AND is_alv-stprs IS NOT INITIAL THEN is_alv-stprs * lv_ukurs_ufir
      ELSE is_alv-vlr_usd
      ).
      return-stprs   = COND #(
      WHEN is_alv-stprs   IS INITIAL AND is_alv-vlr_brl IS NOT INITIAL THEN is_alv-vlr_brl / lv_ukurs_ufir
      WHEN is_alv-stprs   IS INITIAL AND is_alv-vlr_usd IS NOT INITIAL THEN is_alv-vlr_usd / lv_ukurs_ufir
      ELSE is_alv-stprs ).

    ELSE.

      return-message = 'Não existe Dolar cadastrado para data referência.'.
      return-status = '@0A@'."Erro

    ENDIF.

  ENDMETHOD.


  METHOD set_color_column.

    CHECK rb_mr22 = abap_true.

    DATA(o_col) = CAST cl_salv_column_table( o_alv->get_columns( )->get_column( 'VLR_BRL_NEW' ) ).

    o_col->set_color( VALUE #( col = 1
                               int = 0
                               inv = 0 ) ).

    o_col = CAST cl_salv_column_table( o_alv->get_columns( )->get_column( 'VLR_USD_NEW' ) ).

    o_col->set_color( VALUE #( col = 1
                               int = 0
                               inv = 0 ) ).

    o_col = CAST cl_salv_column_table( o_alv->get_columns( )->get_column( 'STPRS_NEW' ) ).

    o_col->set_color( VALUE #( col = 1
                               int = 0
                               inv = 0 ) ).

    o_col = CAST cl_salv_column_table( o_alv->get_columns( )->get_column( 'VLR_BRL_OLD' ) ).

    o_col->set_color( VALUE #( col = 5
                               int = 0
                               inv = 0 ) ).

    o_col = CAST cl_salv_column_table( o_alv->get_columns( )->get_column( 'VLR_USD_OLD' ) ).

    o_col->set_color( VALUE #( col = 5
                               int = 0
                               inv = 0 ) ).

    o_col = CAST cl_salv_column_table( o_alv->get_columns( )->get_column( 'STPRS_OLD' ) ).

    o_col->set_color( VALUE #( col = 5
                               int = 0
                               inv = 0 ) ).

  ENDMETHOD.


  method change_value_convert.

    check is_alv is not initial.

    data: ls_mbew      type ty_mbew,
          ls_ckmlhd    type ty_ckmlhd,
          t_ckmlcr_aux type table of ty_ckmlcr,
          ls_t001w     type ty_t001w,
          lt_mlit      type table of mlit.

    data(lv_status)           = conv ty_alv-status( '' ).
    data(lv_matnr)            = conv zmmt0178-matnr( '' ).
    data(lv_maktx)            = conv makt-maktx( '' ).
    data(lv_werks)            = conv zmmt0178-werks( '' ).
    data(lv_vlr_brl)          = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_usd)          = conv zmmt0178-vlr_usd( '' ).
    data(lv_stprs)            = conv zmmt0178-stprs( '' ).
    data(lv_stprs_old)        = conv zmmt0178-stprs( '' ).
    data(lv_stprs_new)        = conv zmmt0178-stprs( '' ).
    data(lv_aval_atual_brl)   = conv zmmt0178-vlr_brl( '' ).
    data(lv_total_preco_novo) = conv zmmt0178-vlr_brl( '' ).
    data(lv_aval_atual_usd)   = conv zmmt0178-vlr_usd( '' ).
    data(lv_aval_atual_stprs) = conv zmmt0178-stprs( '' ).
    data(lv_vlr_brl_old)      = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_brl_new)      = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_usd_old)      = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_usd_new)      = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_stprs_old)    = conv zmmt0178-vlr_brl( '' ).
    data(lv_vlr_stprs_new)    = conv zmmt0178-vlr_brl( '' ).
    data(lv_screen)           = conv zmmt0178-vlr_brl( '' ).
    data(lv_total_standard)   = conv mlcrf-prdif( '' ).
    data(lv_lbkum)            = conv ckmlpp-lbkum( '' ).
    data(lv_poper)            = conv mlcrf-poper( '' ).
    data(lv_curin)            = conv t001a-curtp( '' ).
    data(lv_curfo)            = conv t001a-curtp( '' ).
    data(lv_curgr)            = conv t001a-curtp( '' ).



    clear: lv_matnr,
           lv_werks,
           lv_maktx,
           ls_mbew,
           lv_aval_atual_brl,
           lv_aval_atual_usd,
           lv_aval_atual_stprs,
           lv_vlr_brl,
           lv_vlr_usd,
           lv_stprs,
           lv_lbkum,
           lv_screen.


    select single  finsc_ld_cmp~curtph finsc_ld_cmp~curtpk finsc_ld_cmp~curtpo
      into ( lv_curin, lv_curfo, lv_curgr )
      from t001w
      inner join j_1bbranch on j_1bbranch~branch = t001w~j_1bbranch
      inner join finsc_ld_cmp  on  finsc_ld_cmp~bukrs  =  j_1bbranch~bukrs
                               and finsc_ld_cmp~rldnr  = '0L'
      where werks = is_alv-werks.


    select single maktx
      from makt
      into @lv_maktx
      where spras = 'P'
        and matnr = @is_alv-matnr.

    select single matnr,
                  bwkey,
                  vprsv,
                  verpr,
                  salk3,
                  lbkum
      from mbew
      into @ls_mbew
      where matnr = @is_alv-matnr
        and bwkey = @is_alv-werks.


    select single kalnr, matnr, bwkey
    from ckmlhd
    into @ls_ckmlhd
    where matnr = @is_alv-matnr
      and bwkey = @is_alv-werks.

    select stprs,
           kalnr,
           pvprs,
           curtp,
           salk3,
           bdatj,
           poper
      from ckmlcr
      into table @t_ckmlcr
      where kalnr = @ls_ckmlhd-kalnr
        and curtp in ( '50' , '40' , '30' , '10' ).

    t_ckmlcr_aux = t_ckmlcr.



    sort t_ckmlcr_aux by bdatj descending poper descending.

    data(ls_data_ckmlcr) = value #( t_ckmlcr_aux[ 1 ] optional ).

    delete t_ckmlcr where poper < ls_data_ckmlcr-poper.
    delete t_ckmlcr where bdatj < ls_data_ckmlcr-bdatj.

    select single lbkum
      from ckmlpp
      into @lv_lbkum
      where kalnr = @ls_ckmlhd-kalnr
      and   bdatj = @ls_data_ckmlcr-bdatj
      and   poper = @ls_data_ckmlcr-poper.

    select single werks,
                j_1bbranch,
                land1
    from t001w
    into @ls_t001w
    where werks = @is_alv-werks.


    if ls_mbew-vprsv = 'S' and
     rb_mr21 is not initial.

      loop at t_ckmlcr into data(ls_ckmlcr).

        is_alv-aval_atual_brl = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                      then ls_ckmlcr-stprs
                                    when ls_ckmlcr-curtp = '30'
                                      then ls_ckmlcr-stprs
                                    else is_alv-aval_atual_brl ).

        is_alv-aval_atual_usd = cond #( when ls_ckmlcr-curtp = lv_curfo
                                      then ls_ckmlcr-stprs
                                    else is_alv-aval_atual_usd ).

        is_alv-aval_atual_stprs = cond #( when ls_ckmlcr-curtp = lv_curgr
                                        then ls_ckmlcr-stprs
                                      when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                        then ls_ckmlcr-stprs
                                      when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                        then ls_ckmlcr-stprs
                                      else is_alv-aval_atual_stprs  ).

      endloop.


    elseif ls_mbew-vprsv = 'V' and
     rb_mr21 is not initial.


      loop at t_ckmlcr into ls_ckmlcr.

        is_alv-aval_atual_brl = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                     then ls_ckmlcr-pvprs
                                    when ls_ckmlcr-curtp = '30'
                                      then ls_ckmlcr-pvprs
                                    else is_alv-aval_atual_brl ).

        is_alv-aval_atual_usd = cond #( when ls_ckmlcr-curtp = lv_curfo
                                      then ls_ckmlcr-pvprs
                                    else is_alv-aval_atual_usd ).

        is_alv-aval_atual_stprs = cond #( when ls_ckmlcr-curtp = lv_curgr
                                        then ls_ckmlcr-pvprs
                                      when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                        then ls_ckmlcr-pvprs
                                      when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                        then ls_ckmlcr-pvprs
                                      else is_alv-aval_atual_stprs ).
      endloop.

    elseif ls_mbew-vprsv = 'S' and
   rb_mr22 is not initial.

      loop at t_ckmlcr into ls_ckmlcr.
        lv_poper = |{ is_alv-data_ref+4(2) alpha = in }|.
        clear lv_total_standard.
        select sum( b~prdif )
            into lv_total_standard
            from mlit as a
            inner join mlcrf as b on b~belnr = a~belnr
             and   b~posnr = a~posnr
             and   b~poper = lv_poper
             and   b~curtp = ls_ckmlcr-curtp
             where a~kalnr = ls_ckmlhd-kalnr
             and   a~matnr = is_alv-matnr
             and   a~kjahr = is_alv-data_ref(4).


        lv_screen = cond #( when ls_ckmlcr-curtp eq lv_curin
                             then is_alv-vlr_brl
                            when ls_ckmlcr-curtp eq lv_curfo
                              then is_alv-vlr_usd
                            when ls_ckmlcr-curtp eq lv_curgr
                              then is_alv-stprs ).

        if lv_lbkum is not initial.
          lv_total_preco_novo = conv #( ( ls_ckmlcr-salk3 +  lv_screen + lv_total_standard ) / lv_lbkum ).
        endif.


        is_alv-vlr_brl_new = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                  then lv_total_preco_novo
                                 when ls_ckmlcr-curtp = '30'
                                  then lv_total_preco_novo
                                 else is_alv-vlr_brl_new ).

        is_alv-vlr_brl_old = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                   then ls_ckmlcr-stprs
                                 when ls_ckmlcr-curtp = '30'
                                   then ls_ckmlcr-stprs
                                 else is_alv-vlr_brl_old ).

        is_alv-vlr_usd_new = cond #( when ls_ckmlcr-curtp = lv_curfo and ls_t001w-land1 = 'BR'
                                   then lv_total_preco_novo
                                 else is_alv-vlr_usd_new ).

        is_alv-vlr_usd_old = cond #( when ls_ckmlcr-curtp = lv_curfo
                                   then ls_ckmlcr-stprs
                                 else is_alv-vlr_usd_old ).

        is_alv-stprs_old = cond #( when ls_ckmlcr-curtp = lv_curgr
                                 then ls_ckmlcr-stprs
                               when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                 then ls_ckmlcr-stprs
                               when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                then ls_ckmlcr-stprs
                               else is_alv-stprs_old ).

        is_alv-stprs_new = cond #( when ls_ckmlcr-curtp = lv_curgr
                                 then lv_total_preco_novo
                               when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                then lv_total_preco_novo
                               when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                then lv_total_preco_novo
                               else is_alv-stprs_new ).
      endloop.

    elseif ls_mbew-vprsv = 'V' and
       rb_mr22 is not initial.

      loop at t_ckmlcr into ls_ckmlcr.
        lv_poper = |{ is_alv-data_ref+4(2) alpha = in }|.
        clear lv_total_standard.
        select sum( b~prdif )
            into lv_total_standard
            from mlit as a
            inner join mlcrf as b on b~belnr = a~belnr
             and   b~posnr = a~posnr
             and   b~poper = lv_poper
             and   b~curtp = ls_ckmlcr-curtp
             where a~kalnr = ls_ckmlhd-kalnr
             and   a~matnr = is_alv-matnr
             and   a~kjahr = is_alv-data_ref(4).
        clear lv_screen.


        lv_screen = cond #( when ls_ckmlcr-curtp eq lv_curin
                               then is_alv-vlr_brl
                              when ls_ckmlcr-curtp eq lv_curfo
                                then is_alv-vlr_usd
                              when ls_ckmlcr-curtp eq lv_curgr
                                then is_alv-stprs
                              else lv_screen ).

        if lv_lbkum is not initial.
          lv_total_preco_novo = conv #( ( ls_ckmlcr-salk3 +  lv_screen + lv_total_standard ) / lv_lbkum ).
        endif.

        is_alv-vlr_brl_old = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                     then ls_ckmlcr-pvprs
                                   when ls_ckmlcr-curtp = '30'
                                     then ls_ckmlcr-pvprs
                                   else is_alv-vlr_brl_old ).

        is_alv-vlr_brl_new = cond #( when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'BR'
                                   then lv_total_preco_novo
                                 when ls_ckmlcr-curtp = '30'
                                   then lv_total_preco_novo
                                 else is_alv-vlr_brl_new ).


        is_alv-vlr_usd_new = cond #( when ls_ckmlcr-curtp = lv_curfo and ls_t001w-land1 = 'BR'
                                   then lv_total_preco_novo
                                 else  is_alv-vlr_usd_new ).

        is_alv-vlr_usd_old = cond #( when ls_ckmlcr-curtp =  lv_curfo
                                   then ls_ckmlcr-pvprs
                                 else is_alv-vlr_usd_old ).

        is_alv-stprs_old = cond #( when ls_ckmlcr-curtp = lv_curgr
                                 then ls_ckmlcr-pvprs
                               when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'AR'
                                 then ls_ckmlcr-pvprs
                               when ls_ckmlcr-curtp = lv_curin and ls_t001w-land1 = 'PY'
                                 then ls_ckmlcr-pvprs
                               else is_alv-stprs_old ).

        is_alv-stprs_new = cond #( when ls_ckmlcr-curtp = lv_curgr
                                 then lv_total_preco_novo
                               when ls_ckmlcr-curtp = lv_curin  and ls_t001w-land1 = 'AR'
                                 then lv_total_preco_novo
                               when ls_ckmlcr-curtp = lv_curin  and ls_t001w-land1 = 'PY'
                                 then lv_total_preco_novo
                               else is_alv-stprs_new ).

      endloop.

    endif.


    e_return = is_alv.


  endmethod.


  METHOD set_dados_shdb.

    v_bukrs = i_bukrs.
    v_data  = i_data.
    v_ref   = i_referencia.
    rb_mr21 = abap_true.

    EXPORT v_bukrs = v_bukrs TO MEMORY ID 'ZMM_BUKRS'.

  ENDMETHOD.


  METHOD submit_mr21_mr22.

    DATA: lv_jobname TYPE tbtcjob-jobname,
          lv_name    TYPE tbtcjob-jobname,
          lv_number  TYPE tbtcjob-jobcount,
          lv_user    TYPE uname,
          lv_json    TYPE string.

*-------------------
*-- recuperar usuario e monta json
*-------------------
    lv_user = zcl_job=>get_user_job( ).
    lv_json = /ui2/cl_json=>serialize( data = t_bdcdata ).

*-------------------
*-- p/ debug
*-------------------
    IF 1 = 2.
      SUBMIT zmmr_executa_mr21 WITH p_bdctab = lv_json
                               WITH p_mr21   = rb_mr21
                                AND RETURN.
    ENDIF.

*------------------------------------
*---- criar job execucao
*------------------------------------
    lv_jobname = |CALL_MR21|.
    lv_name    = lv_jobname && '_' && lv_user.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_name
      IMPORTING
        jobcount         = lv_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

*-------------------
*-- executa MR21
*-------------------
    SUBMIT zmmr_executa_mr21 WITH p_bdctab = lv_json
                             WITH p_mr21   = rb_mr21
                          VIA JOB lv_name
                           NUMBER lv_number
                             USER lv_user
                              AND RETURN.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_number
        jobname              = lv_name
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

  ENDMETHOD.
ENDCLASS.
