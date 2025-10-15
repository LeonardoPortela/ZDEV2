class ZCL_SIMULADOR_INSUMOS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_NR_SIMU type ZSDED003 optional
      !IV_OV type VBELN optional
    exceptions
      SEM_PARAMETROS .
  methods GET_NR_SIMULADOR
    returning
      value(RV_SIMU) type ZSDED003 .
  methods CALL_TRANSACTION_ZSDT0087
    exceptions
      SEM_DADOS .
protected section.
private section.

  types:
    ty_simu_ovs_tab TYPE TABLE OF zi_in_simu_ov .

  data GO_SIMULADOR type ref to ZCL_SIMULADOR_INSUMOS .
  data GV_NR_SIMU type ZSDED003 .
  data GV_OV_PARAM type VBELN .
  data GT_SIMU_OVS type TY_SIMU_OVS_TAB .
  data GS_0040 type ZSDT0040 .

  methods POPUP_GET_OV .
  methods CREATE_OBJECT .
  methods BDC_APPEND
    importing
      !IV_SCREEN type FLAG optional
      !IV_PARAM_01 type BDC_FVAL
      !IV_PARAM_02 type BDC_FVAL
    changing
      !CT_TAB type TAB_BDCDATA .
ENDCLASS.



CLASS ZCL_SIMULADOR_INSUMOS IMPLEMENTATION.


  METHOD bdc_append.

    APPEND INITIAL LINE TO ct_tab ASSIGNING FIELD-SYMBOL(<fs_bdc>).

    IF iv_screen = abap_true.

      <fs_bdc>-program  = iv_param_01.
      <fs_bdc>-dynpro   = iv_param_02.
      <fs_bdc>-dynbegin = abap_true.

    ELSE.

      <fs_bdc>-fnam = iv_param_01.
      <fs_bdc>-fval = iv_param_02.

    ENDIF.

  ENDMETHOD.


  METHOD call_transaction_zsdt0087.

    DATA: lwa_params TYPE ctu_params .

*    DATA lr_simu LIKE RANGE OF gv_nr_simu.
*    DATA lr_vkorg LIKE RANGE OF gs_0040-vkorg.
*    DATA lr_vkbur LIKE RANGE OF gs_0040-vkbur.
*
*    AUTHORITY-CHECK OBJECT 'S_TCODE'
*      ID 'TCD' FIELD 'ZSDT0087'.
*
*    IF sy-subrc = 0.
*
*      APPEND 'IEQ' && gv_nr_simu TO lr_simu.
*      APPEND 'IEQ' && gs_0040-vkorg TO lr_vkorg.
*      APPEND 'IEQ' && gs_0040-vkbur TO lr_vkbur.
*
*      SUBMIT zsdr0042
*        WITH s_docsi IN lr_simu
*        WITH s_vkbur IN lr_vkbur
*        WITH s_vkorg IN lr_vkorg
*          AND RETURN.
*
*    ELSE.
*      MESSAGE s016(ds) WITH 'Você não tem autorização'
*                            'para executar a transação ZSDT0087.'
*        DISPLAY LIKE 'E'.
*
*    ENDIF.


    DATA lt_bdcdata TYPE tab_bdcdata.

    IF gs_0040-vkbur IS INITIAL.
      RAISE sem_dados.
    ENDIF.

    bdc_append(
      EXPORTING
        iv_screen   = abap_true
        iv_param_01 = 'ZSDR0042'
        iv_param_02 = '1000'
      CHANGING
        ct_tab      = lt_bdcdata ).

    bdc_append(
      EXPORTING
        iv_param_01 = 'BDC_OKCODE'
        iv_param_02 = '=ONLI'
      CHANGING
        ct_tab      = lt_bdcdata ).

    bdc_append(
      EXPORTING
        iv_param_01 = 'S_VKORG-LOW'
        iv_param_02 = CONV #( gs_0040-vkorg )
      CHANGING
        ct_tab      = lt_bdcdata ).

    bdc_append(
      EXPORTING
        iv_param_01 = 'S_VKBUR-LOW'
        iv_param_02 = CONV #( gs_0040-vkbur )
      CHANGING
        ct_tab      = lt_bdcdata ).

    bdc_append(
      EXPORTING
        iv_param_01 = 'S_DOCSI-LOW'
        iv_param_02 = CONV #( gv_nr_simu )
      CHANGING
        ct_tab      = lt_bdcdata ).


*    bdc_append(
*       EXPORTING
*         iv_param_01 = 'S_VBELN-LOW'
*         iv_param_02 = CONV #( gv_ov_param )
*       CHANGING
*         ct_tab      = lt_bdcdata ).


    lwa_params-racommit = 'X'.
    lwa_params-updmode  = 'S'.
    lwa_params-dismode  = 'E'.

    SET PARAMETER ID 'ZRT' FIELD 'X'.

    CALL TRANSACTION 'ZSDT0087'
      WITH AUTHORITY-CHECK
        USING lt_bdcdata
         OPTIONS FROM lwa_params.


  ENDMETHOD.


  METHOD constructor.

    gv_nr_simu = iv_nr_simu.
    gv_ov_param = iv_ov.

    IF gv_nr_simu IS INITIAL AND gv_ov_param IS INITIAL.

      popup_get_ov( ).

    ENDIF.

    me->create_object( ).

  ENDMETHOD.


  METHOD create_object.

    DATA lr_simu TYPE RANGE OF zsded003.
    DATA lr_ov TYPE RANGE OF vbeln.

    IF gv_nr_simu IS NOT INITIAL.

      READ TABLE gt_simu_ovs TRANSPORTING NO FIELDS
        WITH KEY doc_simulacao = gv_nr_simu.

      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.

      APPEND 'IEQ' && gv_nr_simu TO lr_simu.
    ENDIF.

    IF gv_ov_param IS NOT INITIAL.

      READ TABLE gt_simu_ovs TRANSPORTING NO FIELDS
         WITH KEY vbeln_p = gv_ov_param.

      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.

      READ TABLE gt_simu_ovs TRANSPORTING NO FIELDS
         WITH KEY vbeln = gv_ov_param.

      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.

      APPEND 'IEQ' && gv_ov_param TO lr_ov.
    ENDIF.

    CHECK gv_nr_simu IS NOT INITIAL OR gv_ov_param IS NOT INITIAL.

    SELECT * FROM zi_in_simu_ov
      INTO TABLE @gt_simu_ovs
        WHERE doc_simulacao IN @lr_simu
          AND ( vbeln_p IN @lr_ov OR vbeln IN @lr_ov ).

    IF sy-subrc EQ 0.

      gv_nr_simu = gt_simu_ovs[ 1 ]-doc_simulacao.

      SELECT SINGLE * FROM zsdt0040
        INTO gs_0040 WHERE doc_simulacao = gv_nr_simu.

    ENDIF.

  ENDMETHOD.


  METHOD get_nr_simulador.

    rv_simu = gv_nr_simu.

  ENDMETHOD.


  METHOD popup_get_ov.

    DATA lv_ret.
    DATA lt_sval TYPE TABLE OF sval.

    CHECK sy-batch IS INITIAL.

    lt_sval = VALUE #( ( tabname = 'ZSDT0040' fieldname = 'DOC_SIMULACAO' )
                       ( tabname = 'VBAK' fieldname = 'VBELN' )  ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Doc.Vendas do Simulador'
      IMPORTING
        returncode      = lv_ret
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    CHECK lt_sval IS NOT INITIAL.

    gv_nr_simu = VALUE #( lt_sval[ fieldname = 'DOC_SIMULACAO' ]-value DEFAULT '' ).
    gv_ov_param = VALUE #( lt_sval[ fieldname = 'VBELN' ]-value DEFAULT '' ).

  ENDMETHOD.
ENDCLASS.
